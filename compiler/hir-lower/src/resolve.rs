use arena::Ix;
use diagnostics::{FilePath, FileSpan, Span};
use hir::ident::AbsPath;
use rustc_hash::FxHashMap;
use syntax::{ast, token, AstElement};
use text::Text;
use verde::{Ctx, Id};

use crate::{
	index::{
		canonical::{CanonicalTree, Declaration, ModuleTree},
		NameTy,
	},
	VisiblePackages,
};

#[derive(Clone)]
pub enum NameExprResolution {
	Struct(Id<AbsPath>),
	Expr(hir::ExprKind),
	Error,
}

pub struct ResPath {
	pub root: Option<token::Dot>,
	/// In reverse!
	pub elems: Vec<ResName>,
}

#[derive(Clone)]
pub struct ResName {
	pub name: Text,
	pub ast: ast::Name,
}

impl PartialEq for ResName {
	fn eq(&self, other: &Self) -> bool { self.name == other.name }
}
impl Eq for ResName {}

impl ResName {
	fn new(ast: Option<ast::Name>) -> Option<Self> {
		let ast = ast?;
		let name = ast.text().unwrap_or_else(|| Text::new("<error>"));
		Some(Self { name, ast })
	}
}

impl PartialEq for ResPath {
	fn eq(&self, other: &Self) -> bool { self.root.is_some() && other.root.is_some() && self.elems == other.elems }
}
impl Eq for ResPath {}

impl ResPath {
	pub fn full_span(&self) -> Option<FileSpan> {
		let spans = self
			.root
			.iter()
			.map(|x| x.span())
			.chain(self.elems.iter().map(|x| x.ast.span()));
		let mut ret: Option<FileSpan> = None;
		for s in spans {
			ret = Some(match ret {
				Some(x) => FileSpan {
					start: x.start.min(s.start),
					end: x.end.max(s.end),
					relative: (),
				},
				None => s,
			})
		}
		ret
	}

	fn from_ast(path: ast::Path) -> Self {
		let mut segs = path.path_segments();
		let first = segs.next().unwrap();
		let (root, first) = match first {
			ast::PathSegment::Dot(d) => (Some(d), None),
			_ => (None, Some(first)),
		};

		let mut elems = Vec::new();
		for seg in first.into_iter().chain(segs) {
			match seg {
				ast::PathSegment::Dot(_) => {},
				ast::PathSegment::Name(n) => {
					elems.extend(ResName::new(Some(n)));
				},
			}
		}
		elems.reverse();

		ResPath { root, elems }
	}

	fn from_field_expr(mut expr: ast::FieldExpr) -> Option<Self> {
		let mut path = ResPath {
			root: None,
			elems: Vec::new(),
		};
		loop {
			path.elems.extend(ResName::new(expr.name()));
			match expr.expr() {
				Some(ast::Expr::FieldExpr(f)) => expr = f,
				Some(ast::Expr::NameExpr(n)) => {
					path.root = n.dot();
					path.elems.extend(ResName::new(expr.name()));
					break Some(path);
				},
				_ => return None,
			}
		}
	}
}

pub enum Resolution {
	Local {
		local: Ix<hir::Local>,
		unresolved: ResPath,
	},
	Param {
		param: Ix<hir::Param>,
		unresolved: ResPath,
	},
	Item {
		path: Id<AbsPath>,
		ty: NameTy,
		unresolved: ResPath,
	},
	Error,
}

pub struct NameResolver<'a> {
	ctx: &'a Ctx<'a>,
	this: Id<AbsPath>,
	packages: &'a VisiblePackages,
	tree: &'a CanonicalTree,
	scopes: Vec<FxHashMap<Text, Ix<hir::Local>>>,
	params: FxHashMap<Text, Ix<hir::Param>>,
	file: FilePath,
}

impl<'a> NameResolver<'a> {
	pub fn new(
		ctx: &'a Ctx, this: Id<AbsPath>, packages: &'a VisiblePackages, tree: &'a CanonicalTree, file: FilePath,
	) -> Self {
		Self {
			ctx,
			this,
			packages,
			tree,
			scopes: Vec::new(),
			params: FxHashMap::default(),
			file,
		}
	}

	pub fn resolve_path(&mut self, path: ast::Path) -> Resolution {
		let path = ResPath::from_ast(path);
		self.resolve_inner(path)
	}

	pub fn resolve_field_expr(&mut self, expr: ast::FieldExpr) -> Option<Resolution> {
		ResPath::from_field_expr(expr).map(|path| self.resolve_inner(path))
	}

	pub fn resolve_name(&mut self, name: ast::NameExpr) -> Resolution {
		let root = name.dot();
		let Some(ast) = name.name() else {
			return Resolution::Error;
		};
		self.resolve_inner(ResPath {
			root,
			elems: vec![ResName {
				name: ast.text().unwrap_or_else(|| Text::new("<error>")),
				ast,
			}],
		})
	}

	fn resolve_inner(&mut self, mut path: ResPath) -> Resolution {
		if let Some(d) = path.root.take() {
			if let Some(name) = path.elems.last() {
				let Some(pkg) = self.packages.packages.get(&name.name) else {
					let span = name.ast.span().with(self.file);
					self.ctx.push(
						span.error("unknown package")
							.label(span.label("404: this package was not found")),
					);
					return Resolution::Error;
				};
				let tree = self.tree.packages[pkg];
				self.resolve_from_mod(path, tree)
			} else {
				// TODO: Map `.` to the current item.
				let span = d.span().with(self.file);
				self.ctx
					.push(span.error("self `.` is not supported").label(span.mark()));
				Resolution::Error
			}
		} else {
			let first = path.elems.last().expect("empty path");
			if let Some(local) = self.resolve_local(first.name) {
				path.elems.pop();
				Resolution::Local {
					local,
					unresolved: path,
				}
			} else if let Some(param) = self.resolve_param(first.name) {
				path.elems.pop();
				Resolution::Param {
					param,
					unresolved: path,
				}
			} else {
				let tree = self.tree.modules[&self.this];
				let source = match *self.ctx.geti(self.this) {
					AbsPath::Name { name, .. } => name,
					AbsPath::Package(_) => Text::new("root"),
				};
				path.elems.push(ResName {
					name: source,
					ast: first.ast.clone(),
				});
				self.resolve_from_mod(path, tree)
			}
		}
	}

	fn resolve_from_mod(&mut self, mut path: ResPath, mut module: Id<ModuleTree>) -> Resolution {
		let mut source = path.elems.pop().unwrap();
		loop {
			let tree = self.ctx.get(module);
			let Some(name) = path.elems.pop() else {
				let span = source.ast.span().with(self.file);
				self.ctx
					.push(span.error("expected item").label(span.label("found module")));
				return Resolution::Error;
			};

			let private = is_child_of(self.ctx, tree.path, self.this);
			let index = if private { tree.private } else { tree.public };
			let index = self.ctx.get(index);
			let Some(decl) = index.names.get(&name.name) else {
				let span = name.ast.span().with(self.file);
				self.ctx.push(
					span.error("unknown name")
						.label(span.label(format!("404: name does not exist in `{}`", source.name.as_str()))),
				);
				return Resolution::Error;
			};

			match *decl {
				Declaration::Module(m) => module = m,
				Declaration::Name { path: item, ty, .. } => {
					return Resolution::Item {
						path: item,
						ty,
						unresolved: path,
					}
				},
			}

			source = name;
		}
	}

	pub fn declare_param(&mut self, name: Text, param: Ix<hir::Param>) { self.params.insert(name, param); }

	pub fn declare_local(&mut self, name: Text, local: Ix<hir::Local>) {
		let scope = self.scopes.last_mut().expect("let without any scope");
		scope.insert(name, local);
	}

	pub fn resolve_local(&self, name: Text) -> Option<Ix<hir::Local>> {
		for scope in self.scopes.iter().rev() {
			if let Some(&local) = scope.get(&name) {
				return Some(local);
			}
		}
		None
	}

	pub fn resolve_param(&self, name: Text) -> Option<Ix<hir::Param>> { self.params.get(&name).copied() }

	pub fn push_scope(&mut self) { self.scopes.push(FxHashMap::default()); }

	pub fn pop_scope(&mut self) { self.scopes.pop().expect("pop without any scope"); }
}

fn is_child_of(ctx: &Ctx, parent: Id<AbsPath>, mut child: Id<AbsPath>) -> bool {
	loop {
		if parent == child {
			return true;
		}

		child = match *ctx.geti(child) {
			AbsPath::Package(_) => return false,
			AbsPath::Name { prec, .. } => prec,
		};
	}
}

