use arena::Ix;
use diagnostics::{FilePath, FileSpan, FullSpan, Span};
use hir::ident::AbsPath;
use rustc_hash::FxHashMap;
use syntax::{ast, token, AstElement};
use text::Text;
use verde::{Ctx, Id};

use crate::{
	index::{Declaration, ErasedTempId, ModuleTree, NameTy, PackageTree, RelPath, TempName},
	is_child_of,
	prelude::{Prelude, PreludeItem},
	VisiblePackages,
};

#[derive(Clone)]
pub enum NameExprResolution {
	Struct(Id<AbsPath>),
	Expr(hir::ExprKind),
	Error,
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

pub struct ResPath {
	pub root: Option<token::Dot>,
	pub elems: Vec<ResName>,
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

	fn empty() -> Self {
		Self {
			root: None,
			elems: Vec::new(),
		}
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
					path.elems.reverse();
					break Some(path);
				},
				_ => return None,
			}
		}
	}
}

trait ResolutionPath {
	type Iter<'a>: Iterator<Item = (Text, Self::Span)> + 'a;
	type Span: Copy + Send + Span + 'static;

	fn take_dot(&mut self) -> bool;

	fn take_first(&mut self, file: Option<FilePath>) -> (Text, Self::Span);

	fn elems<'a>(&'a mut self, file: Option<FilePath>) -> Self::Iter<'a>;

	fn remaining(iter: Self::Iter<'_>);
}

struct ResDrain<'a> {
	inner: std::vec::Drain<'a, ResName>,
	file: FilePath,
}
impl Iterator for ResDrain<'_> {
	type Item = (Text, FullSpan);

	fn next(&mut self) -> Option<Self::Item> { self.inner.next().map(|x| (x.name, x.ast.span().with(self.file))) }
}
impl ResolutionPath for ResPath {
	type Iter<'a> = ResDrain<'a>;
	type Span = FullSpan;

	fn take_dot(&mut self) -> bool { self.root.take().is_some() }

	fn take_first(&mut self, file: Option<FilePath>) -> (Text, FullSpan) {
		let x = self.elems.remove(0);
		(x.name, x.ast.span().with(file.unwrap()))
	}

	fn elems<'a>(&'a mut self, file: Option<FilePath>) -> Self::Iter<'a> {
		ResDrain {
			inner: self.elems.drain(..),
			file: file.unwrap(),
		}
	}

	fn remaining(iter: Self::Iter<'_>) { iter.inner.keep_rest(); }
}

pub struct RelDrain<'a> {
	inner: std::vec::Drain<'a, TempName>,
}
impl Iterator for RelDrain<'_> {
	type Item = (Text, ErasedTempId);

	fn next(&mut self) -> Option<Self::Item> { self.inner.next().map(|x| (x.name, x.id.erased())) }
}
impl ResolutionPath for RelPath {
	type Iter<'a> = RelDrain<'a>;
	type Span = ErasedTempId;

	fn take_dot(&mut self) -> bool { self.dot.take().is_some() }

	fn take_first(&mut self, _: Option<FilePath>) -> (Text, ErasedTempId) {
		let x = self.names.remove(0);
		(x.name, x.id.erased())
	}

	fn elems<'a>(&'a mut self, _: Option<FilePath>) -> Self::Iter<'a> {
		RelDrain {
			inner: self.names.drain(..),
		}
	}

	fn remaining(iter: Self::Iter<'_>) { iter.inner.keep_rest(); }
}

pub enum GlobalResolution<P> {
	Module(Id<ModuleTree>),
	Item {
		path: Id<AbsPath>,
		ty: NameTy,
		unresolved: P,
	},
	Error,
}

pub struct GlobalResolver<'a> {
	ctx: &'a Ctx<'a>,
	this: Id<AbsPath>,
	file: Option<FilePath>,
	prelude: Option<&'a Prelude>,
	packages: &'a VisiblePackages,
	tree: &'a PackageTree,
}

impl<'a> GlobalResolver<'a> {
	pub fn new(
		ctx: &'a Ctx<'a>, this: Id<AbsPath>, file: Option<FilePath>, prelude: Option<&'a Prelude>,
		packages: &'a VisiblePackages, tree: &'a PackageTree,
	) -> Self {
		Self {
			ctx,
			this,
			file,
			prelude,
			packages,
			tree,
		}
	}

	pub fn resolve_prelude(&self, module: Id<ModuleTree>, path: RelPath) -> GlobalResolution<RelPath> {
		self.resolve_inner(module, path)
	}

	pub fn verify_imports(mut self) {
		let module = self.get_module(self.this);
		if let Some(index) = self.ctx.get(module).index {
			let index = self.ctx.get(index);
			for (_, decl) in index.decls.iter() {
				match decl {
					// TODO: group together imports.
					Declaration::Import { path, is_prelude, .. } if *is_prelude => {
						let p = self.prelude.take();
						self.resolve_inner(module, path.clone());
						self.prelude = p;
					},
					Declaration::Import { path, .. } => {
						self.resolve_inner(module, path.clone());
					},
					_ => {},
				}
			}
		}
	}

	fn resolve_from_module(&self, module: Id<AbsPath>, path: ResPath) -> GlobalResolution<ResPath> {
		self.resolve_inner(self.get_module(module), path)
	}

	fn resolve_global(&self, path: ResPath) -> GlobalResolution<ResPath> {
		self.resolve_inner(self.get_module(self.this), path)
	}

	fn get_module(&self, path: Id<AbsPath>) -> Id<ModuleTree> {
		let (pkg, names) = AbsPath::segments(self.ctx, path);
		let mut module = *self.tree.packages.get(&pkg).unwrap();
		for name in names {
			module = *self.ctx.get(module).children.get(&name).unwrap();
		}
		module
	}

	fn resolve_inner<P: ResolutionPath>(&self, mut module: Id<ModuleTree>, mut path: P) -> GlobalResolution<P> {
		if path.take_dot() {
			let (name, name_span) = path.take_first(self.file);
			let Some(pkg) = self.packages.packages.get(&name) else {
				if self.this == self.ctx.get(module).path {
					self.ctx.push(
						name_span
							.error("unknown package")
							.label(name_span.label("404: this package was not found")),
					);
				}
				return GlobalResolution::Error;
			};
			let &tree = self.tree.packages.get(pkg).unwrap();
			module = tree;
		}

		let mut elems = path.elems(self.file);
		loop {
			let tree = self.ctx.get(module);
			let Some((name, name_span)) = elems.next() else {
				return GlobalResolution::Module(module);
			};

			let prelude = self.prelude.and_then(|x| x.prelude.get(&name));
			let private = is_child_of(self.ctx, tree.path, self.this);
			let child = private.then(|| tree.children.get(&name)).flatten();
			let index = tree.index.map(|i| self.ctx.get(i));
			let decl = index.as_ref().and_then(|i| {
				let is_public = self.ctx.get(i.public).names.contains(&name);
				(is_public || private).then(|| i.decls.get(&name)).flatten()
			});
			match (child, decl, prelude) {
				(Some(&c), ..) => module = c,
				(_, Some(decl), _) => match *decl {
					Declaration::Name { path: item, ty, .. } => {
						P::remaining(elems);
						return GlobalResolution::Item {
							path: item,
							ty,
							unresolved: path,
						};
					},
					Declaration::Import { path: ref x, .. } => match self.resolve_inner(module, x.clone()) {
						GlobalResolution::Item { path: item, ty, .. } => {
							P::remaining(elems);
							return GlobalResolution::Item {
								path: item,
								ty,
								unresolved: path,
							};
						},
						GlobalResolution::Module(m) => module = m,
						GlobalResolution::Error => return GlobalResolution::Error,
					},
				},
				(_, _, Some(prelude)) => match *prelude {
					PreludeItem::Module(c) => module = c,
					PreludeItem::Item { path: item, ty } => {
						P::remaining(elems);
						return GlobalResolution::Item {
							path: item,
							ty,
							unresolved: path,
						};
					},
				},
				(None, None, None) => {
					if self.this == tree.path {
						self.ctx.push(
							name_span
								.error("unknown name")
								.label(name_span.label(format!("404: name does not exist"))),
						);
					}
					return GlobalResolution::Error;
				},
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
	global: GlobalResolver<'a>,
	item: Id<AbsPath>,
	ty: Option<NameTy>,
	scopes: Vec<FxHashMap<Text, Ix<hir::Local>>>,
	params: FxHashMap<Text, Ix<hir::Param>>,
}

impl<'a> NameResolver<'a> {
	pub fn new(
		ctx: &'a Ctx, file: FilePath, prelude: &'a Prelude, packages: &'a VisiblePackages, tree: &'a PackageTree,
		this: Id<AbsPath>, item: Id<AbsPath>, ty: Option<NameTy>,
	) -> Self {
		Self {
			global: GlobalResolver::new(ctx, this, Some(file), Some(prelude), packages, tree),
			item,
			ty,
			scopes: Vec::new(),
			params: FxHashMap::default(),
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
		if let Some(_) = path.root.take() {
			self.resolve_global(path)
		} else {
			let first = path.elems.first().expect("empty path");
			if let Some(local) = self.resolve_local(first.name) {
				path.elems.remove(0);
				Resolution::Local {
					local,
					unresolved: path,
				}
			} else if let Some(param) = self.resolve_param(first.name) {
				path.elems.remove(0);
				Resolution::Param {
					param,
					unresolved: path,
				}
			} else {
				let span = path.full_span().unwrap().with(self.global.file.unwrap());
				let res = self.global.resolve_from_module(self.global.this, path);
				self.wrap_global_res(span, res)
			}
		}
	}

	fn resolve_global(&self, path: ResPath) -> Resolution {
		let span = path.full_span().unwrap().with(self.global.file.unwrap());
		if !path.elems.is_empty() {
			let res = self.global.resolve_global(path);
			self.wrap_global_res(span, res)
		} else {
			self.ty
				.map(|ty| Resolution::Item {
					path: self.item,
					ty,
					unresolved: ResPath::empty(),
				})
				.unwrap_or(Resolution::Error)
		}
	}

	fn wrap_global_res(&self, span: FullSpan, res: GlobalResolution<ResPath>) -> Resolution {
		match res {
			GlobalResolution::Module(_) => {
				self.global
					.ctx
					.push(span.error("expected item").label(span.label("found module")));
				Resolution::Error
			},
			GlobalResolution::Item { path, ty, unresolved } => Resolution::Item { path, ty, unresolved },
			GlobalResolution::Error => Resolution::Error,
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
