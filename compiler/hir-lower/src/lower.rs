use arena::{Arena, Ix};
use diagnostics::{FilePath, RawSpan, Span};
use hir::{ast::AstId, ident::AbsPath, ExprKind};
use rustc_hash::FxHashMap;
use syntax::{ast, token, AstElement, AstToken, SyntaxElement};
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Db, Id, Tracked};

use crate::{
	index::{canonical::CanonicalTree, local::name_of_item, ItemBuilder, ModuleMap, NameTy},
	resolve::{NameExprResolution, NameResolver, ResPath, Resolution},
	Module,
	VisiblePackages,
};

pub fn build_hir_sea(
	db: &dyn Db, modules: impl IntoIterator<Item = Id<LoweredModule>>,
) -> FxHashMap<Id<AbsPath>, Id<hir::Item>> {
	let mut m = FxHashMap::default();
	for module in modules {
		let module = db.get(module);
		for &item in module.items.iter() {
			let i = db.get(item);
			m.insert(i.path, item);
		}
	}
	m
}

#[derive(Tracked)]
pub struct LoweredModule {
	#[id]
	module: Id<AbsPath>,
	items: Vec<Id<hir::Item>>,
}

impl Eq for LoweredModule {}
impl PartialEq for LoweredModule {
	fn eq(&self, _: &Self) -> bool {
		// We don't care about actually comparing each item, since this struct is just a dummy struct to generate the
		// real HIR sea.
		false
	}
}

/// Lower a module to HIR.
///
/// The `ModuleMap` should be the same one that was used to generate the index.
#[query]
pub fn lower_to_hir(
	ctx: &Ctx, module: Id<Module>, packages: Id<VisiblePackages>, tree: Id<CanonicalTree>,
	#[ignore] map: &mut ModuleMap,
) -> LoweredModule {
	let module = ctx.get(module);

	let s = span!(Level::DEBUG, "lower to HIR", path =%module.file);
	let _e = s.enter();

	let items = module
		.ast
		.items()
		.into_iter()
		.flat_map(|x| {
			name_of_item(&x).and_then(|(x, _)| x.text()).and_then(|name| {
				let builder = map.define(name);
				let item = lower_item(ctx, module.path, name, module.file, x, packages, tree, builder);
				item.map(|x| ctx.insert(x))
			})
		})
		.collect();

	LoweredModule {
		module: module.path,
		items,
	}
}

fn lower_item(
	ctx: &Ctx, module: Id<AbsPath>, name: Text, file: FilePath, item: ast::Item, packages: Id<VisiblePackages>,
	tree: Id<CanonicalTree>, builder: ItemBuilder,
) -> Option<hir::Item> {
	let packages = ctx.get(packages);
	let tree = ctx.get(tree);
	let mut lowerer = Lowerer::new(ctx, module, builder, &*packages, &*tree, file);
	let path = ctx.add(AbsPath::Name { prec: module, name });

	let attrs = item.attributes().into_iter().filter_map(|x| lowerer.attr(x)).collect();

	let kind = match item.item_kind() {
		Some(ast::ItemKind::Fn(f)) => hir::ItemKind::Fn(lowerer.fn_(f)),
		Some(ast::ItemKind::Struct(s)) => hir::ItemKind::Struct(lowerer.struct_(s)),
		Some(ast::ItemKind::Enum(e)) => hir::ItemKind::Enum(lowerer.enum_(e)),
		Some(ast::ItemKind::TypeAlias(t)) => hir::ItemKind::TypeAlias(lowerer.type_alias(t)),
		Some(ast::ItemKind::Static(s)) => hir::ItemKind::Static(lowerer.static_(s)),
		Some(ast::ItemKind::Import(_)) | None => return None,
	};

	Some(hir::Item {
		path,
		name: lowerer.name(name_of_item(&item).map(|x| x.0)),
		attrs,
		exprs: lowerer.exprs,
		types: lowerer.types,
		locals: lowerer.locals,
		kind,
	})
}

struct Lowerer<'a> {
	ctx: &'a Ctx<'a>,
	resolver: NameResolver<'a>,
	builder: ItemBuilder<'a>,
	exprs: Arena<hir::Expr>,
	types: Arena<hir::Type>,
	locals: Arena<hir::Local>,
	file: FilePath,
}

impl<'a> Lowerer<'a> {
	fn new(
		ctx: &'a Ctx<'a>, module: Id<AbsPath>, builder: ItemBuilder<'a>, packages: &'a VisiblePackages,
		tree: &'a CanonicalTree, file: FilePath,
	) -> Self {
		Self {
			ctx,
			builder,
			resolver: NameResolver::new(ctx, module, packages, tree, file),
			exprs: Arena::new(),
			types: Arena::new(),
			locals: Arena::new(),
			file,
		}
	}

	fn attr(&mut self, a: ast::Attribute) -> Option<hir::Attr> {
		let name = a.name()?;
		let text = name.text()?;
		let kind = match text.as_str() {
			"lang" => {
				let tt = a.token_tree()?;
				let mut tokens = tt.tokens();

				let Some(value) = tokens.next() else {
					let span = tt.span().with(self.file);
					self.ctx.push(span.error("expected lang item type").label(span.mark()));
					return None;
				};
				let Some(ident) = token::Ident::cast(SyntaxElement::Token(value.clone())) else {
					let span = value.text_range();
					let span = RawSpan {
						start: span.start().into(),
						end: span.end().into(),
						relative: self.file,
					};
					self.ctx
						.push(span.error("lang item type must be an `ident`").label(span.mark()));
					return None;
				};

				let mut span = None;
				for token in tokens {
					let range = token.text_range();
					let span = span.get_or_insert(RawSpan {
						start: range.start().into(),
						end: range.end().into(),
						relative: self.file,
					});
					span.end = range.end().into();
				}

				if let Some(span) = span {
					self.ctx.push(
						span.error("unexpected tokens in lang item attribute")
							.label(span.mark()),
					);
				}

				let lang = match ident.text().as_str() {
					"u8" => hir::LangItem::U8,
					"u16" => hir::LangItem::U16,
					"u32" => hir::LangItem::U32,
					"u64" => hir::LangItem::U64,
					"u128" => hir::LangItem::U128,
					"i8" => hir::LangItem::I8,
					"i16" => hir::LangItem::I16,
					"i32" => hir::LangItem::I32,
					"i64" => hir::LangItem::I64,
					"i128" => hir::LangItem::I128,
					"bool" => hir::LangItem::Bool,
					"char" => hir::LangItem::Char,
					"f32" => hir::LangItem::F32,
					"f64" => hir::LangItem::F64,
					_ => {
						let span = ident.span().with(self.file);
						self.ctx.push(span.error("unknown lang item").label(span.mark()));
						return None;
					},
				};
				hir::AttrKind::LangItem(lang)
			},
			_ => {
				let span = name.span().with(self.file);
				self.ctx.push(span.error("unknown attribute").label(span.mark()));
				return None;
			},
		};

		let id = self.builder.add(Some(a));

		Some(hir::Attr { kind, id })
	}

	fn fn_(&mut self, f: ast::Fn) -> hir::Fn {
		let abi = self.abi(f.abi());
		let name = self.name(f.name());
		let mut params = Arena::new();
		for p in f.param_list().iter().flat_map(|x| x.params()) {
			let p = self.param(p);
			let name = p.name.name;
			let param = params.push(p);
			self.resolver.declare_param(name, param);
		}
		let ret = f.ret_ty().map(|x| self.ty(x.type_()));
		let body = match f.fn_body() {
			Some(ast::FnBody::Block(b)) => Some(self.block(Some(b))),
			Some(ast::FnBody::Semi(_)) | None => None,
		};

		hir::Fn {
			abi,
			name,
			params,
			ret,
			body,
		}
	}

	fn struct_(&mut self, s: ast::Struct) -> hir::Struct {
		let name = self.name(s.name());
		let fields = s.fields().into_iter().map(|x| self.param(x)).collect();

		hir::Struct { name, fields }
	}

	fn enum_(&mut self, e: ast::Enum) -> hir::Enum {
		let name = self.name(e.name());
		let variants = e
			.variant_list()
			.iter()
			.flat_map(|x| x.variants())
			.map(|x| hir::Variant(self.name(Some(x))))
			.collect();

		hir::Enum { name, variants }
	}

	fn type_alias(&mut self, t: ast::TypeAlias) -> hir::TypeAlias {
		let name = self.name(t.name());
		let ty = self.ty(t.type_());

		hir::TypeAlias { name, ty }
	}

	fn static_(&mut self, s: ast::Static) -> hir::Static {
		let name = self.name(s.name());
		let ty = self.ty(s.type_());
		let init = self.expr(s.init());

		hir::Static { name, ty, init }
	}

	fn param(&mut self, p: ast::Param) -> hir::Param {
		let name = self.name(p.name());
		let ty = self.ty(p.type_());
		let id = self.builder.add(Some(p));

		hir::Param { name, ty, id }
	}

	fn ty(&mut self, t: Option<ast::Type>) -> Ix<hir::Type> {
		let kind: Option<_> = try {
			let Some(ref t) = t else { None? };
			match t {
				ast::Type::ArrayType(a) => {
					let ty = self.ty(a.type_());
					let len = self.expr(a.len());

					hir::TypeKind::Array(hir::ArrayType { ty, len })
				},
				ast::Type::FnType(f) => {
					let abi = self.abi(f.abi());
					let params = f
						.params()
						.iter()
						.flat_map(|x| x.types())
						.map(|x| self.ty(Some(x)))
						.collect();
					let ret = f.ret_ty().map(|x| self.ty(x.type_()));

					hir::TypeKind::Fn(hir::FnType { abi, params, ret })
				},
				ast::Type::InferType(_) => hir::TypeKind::Infer,
				ast::Type::PathType(p) => {
					let path = p.path()?;
					match self.resolver.resolve_path(path.clone()) {
						Resolution::Item {
							path: p,
							ty,
							unresolved,
						} => {
							if let Some(rest) = unresolved.full_span() {
								let span = rest.with(self.file);
								self.ctx
									.push(span.error("path continues after type").label(span.mark()));
							}

							match ty {
								NameTy::Struct => hir::TypeKind::Struct(p),
								NameTy::Enum => hir::TypeKind::Enum(p),
								NameTy::TypeAlias => hir::TypeKind::Alias(p),
								_ => {
									let span = path.span().with(self.file);
									self.ctx
										.push(span.error("expected type").label(span.label(format!("found {:?}", ty))));
									None?
								},
							}
						},
						Resolution::Local { .. } => {
							let span = path.span().with(self.file);
							self.ctx
								.push(span.error("expected type").label(span.label("found local")));
							None?
						},
						Resolution::Param { .. } => {
							let span = path.span().with(self.file);
							self.ctx
								.push(span.error("expected type").label(span.label("found param")));
							None?
						},
						Resolution::Error => None?,
					}
				},
				ast::Type::PtrType(p) => {
					let mutable = p.mut_kw().is_some();
					let ty = self.ty(p.type_());
					hir::TypeKind::Ptr(hir::PtrType { mutable, ty })
				},
			}
		};
		let kind = kind.unwrap_or(hir::TypeKind::Error);
		let id = self.builder.add(t.clone());

		self.types.push(hir::Type { kind, id })
	}

	fn block(&mut self, b: Option<ast::Block>) -> hir::Block {
		let mut discard = Vec::with_capacity(b.as_ref().map(|x| x.statements().count()).unwrap_or(0));
		let mut value = None;
		let mut errored = false;

		self.resolver.push_scope();
		let Some(b) = b else {
			return hir::Block::default();
		};
		for stmt in b.statements() {
			match stmt {
				ast::Stmt::Item(i) => {
					// TODO: support.
					let span = name_of_item(&i)
						.map(|(x, _)| x.span())
						.unwrap_or(i.span())
						.with(self.file);
					self.ctx
						.push(span.error("internal items are not supported").label(span.mark()));
				},
				ast::Stmt::Expr(e) => {
					if let Some(v) = value {
						if !errored {
							let span = e.span().with(self.file);
							self.ctx.push(
								span.error("expected `;` before expression")
									.label(span.label("expressions must be separated by `;`")),
							);
							errored = true;
						}
						discard.push(v);
					}
					value = Some(self.expr(Some(e)));
				},
				ast::Stmt::SemiExpr(e) => {
					if let Some(v) = value {
						if !errored {
							let span = e.span().with(self.file);
							self.ctx
								.push(span.error("expected `;` before expression").label(span.mark()));
							errored = true;
						}
						discard.push(v);
						value = None;
					}

					let e = self.expr(e.expr());
					discard.push(e);
				},
				ast::Stmt::Semi(_) => {},
			}
		}
		self.resolver.pop_scope();

		hir::Block { discard, value }
	}

	fn expr(&mut self, e: Option<ast::Expr>) -> Ix<hir::Expr> {
		let kind: Option<_> = try {
			let Some(ref e) = e else { None? };
			match e.clone() {
				ast::Expr::ArrayExpr(a) => hir::ExprKind::Array(self.array(a)),
				ast::Expr::InfixExpr(i) => hir::ExprKind::Infix(self.infix(i)),
				ast::Expr::Block(b) => hir::ExprKind::Block(self.block(Some(b))),
				ast::Expr::BreakExpr(b) => hir::ExprKind::Break(b.expr().map(|x| self.expr(Some(x)))),
				ast::Expr::CallExpr(c) => self.call(c),
				ast::Expr::CastExpr(c) => hir::ExprKind::Cast(self.cast(c)),
				ast::Expr::ContinueKw(_) => hir::ExprKind::Continue,
				ast::Expr::FieldExpr(f) => self.field(f),
				ast::Expr::ForExpr(_) => {
					// TODO: Support.
					let span = e.span().with(self.file);
					self.ctx.push(span.error("`for` expressions are not supported"));
					None?
				},
				ast::Expr::IfExpr(i) => hir::ExprKind::Match(self.if_(i)),
				ast::Expr::IndexExpr(i) => hir::ExprKind::Index(self.index(i)),
				ast::Expr::Literal(l) => hir::ExprKind::Literal(self.literal(l)),
				ast::Expr::LoopExpr(l) => hir::ExprKind::Loop(self.loop_(l)),
				ast::Expr::MatchExpr(m) => hir::ExprKind::Match(self.match_(m)),
				ast::Expr::NameExpr(n) => self.name_expr(n),
				ast::Expr::ParenExpr(e) => return self.expr(e.expr()),
				ast::Expr::PrefixExpr(p) => hir::ExprKind::Prefix(self.prefix(p)),
				ast::Expr::RefExpr(r) => hir::ExprKind::Ref(self.ref_(r)),
				ast::Expr::ReturnExpr(r) => hir::ExprKind::Return(r.expr().map(|x| self.expr(Some(x)))),
				ast::Expr::WhileExpr(w) => hir::ExprKind::Loop(self.while_(w)?),
				ast::Expr::LetExpr(l) => hir::ExprKind::Let(self.let_(l)),
			}
		};
		let kind = kind.unwrap_or(hir::ExprKind::Error);
		let id = self.builder.add(e);
		self.exprs.push(hir::Expr { kind, id })
	}

	fn array(&mut self, a: ast::ArrayExpr) -> hir::ArrayExpr {
		match a.array_init() {
			Some(ast::ArrayInit::ArrayList(l)) => {
				let elems = l.exprs().map(|e| self.expr(Some(e))).collect();
				hir::ArrayExpr { elems, repeat: false }
			},
			Some(ast::ArrayInit::ArrayRepeat(r)) => {
				let expr = self.expr(r.expr());
				let len = self.expr(r.len());
				hir::ArrayExpr {
					elems: vec![expr, len],
					repeat: true,
				}
			},
			None => hir::ArrayExpr {
				elems: vec![],
				repeat: false,
			},
		}
	}

	fn infix(&mut self, i: ast::InfixExpr) -> hir::InfixExpr {
		let lhs = self.expr(i.lhs());
		let op = i.op();
		let op_id = self.builder.add(op.clone());
		let op = self.infix_op(op);
		let rhs = self.expr(i.rhs());

		hir::InfixExpr { lhs, op, op_id, rhs }
	}

	fn infix_op(&mut self, o: Option<ast::InfixOp>) -> hir::InfixOp {
		let Some(o) = o else {
			return hir::InfixOp::Error;
		};
		match o {
			ast::InfixOp::PipePipe(_) => hir::InfixOp::Or,
			ast::InfixOp::AmpAmp(_) => hir::InfixOp::And,
			ast::InfixOp::EqEq(_) => hir::InfixOp::Eq,
			ast::InfixOp::Neq(_) => hir::InfixOp::NotEq,
			ast::InfixOp::Leq(_) => hir::InfixOp::Leq,
			ast::InfixOp::Geq(_) => hir::InfixOp::Geq,
			ast::InfixOp::Lt(_) => hir::InfixOp::Lt,
			ast::InfixOp::Gt(_) => hir::InfixOp::Gt,
			ast::InfixOp::Plus(_) => hir::InfixOp::Add,
			ast::InfixOp::Star(_) => hir::InfixOp::Mul,
			ast::InfixOp::Minus(_) => hir::InfixOp::Sub,
			ast::InfixOp::Slash(_) => hir::InfixOp::Div,
			ast::InfixOp::Percent(_) => hir::InfixOp::Mod,
			ast::InfixOp::Shl(_) => hir::InfixOp::Shl,
			ast::InfixOp::Shr(_) => hir::InfixOp::Shr,
			ast::InfixOp::Caret(_) => hir::InfixOp::Xor,
			ast::InfixOp::Pipe(_) => hir::InfixOp::Or,
			ast::InfixOp::Amp(_) => hir::InfixOp::And,
			ast::InfixOp::Eq(_) => hir::InfixOp::Assign,
			ast::InfixOp::PlusEq(_) => hir::InfixOp::AddAssign,
			ast::InfixOp::SlashEq(_) => hir::InfixOp::DivAssign,
			ast::InfixOp::StarEq(_) => hir::InfixOp::MulAssign,
			ast::InfixOp::PercentEq(_) => hir::InfixOp::ModAssign,
			ast::InfixOp::ShrEq(_) => hir::InfixOp::ShrAssign,
			ast::InfixOp::ShlEq(_) => hir::InfixOp::ShlAssign,
			ast::InfixOp::MinusEq(_) => hir::InfixOp::SubAssign,
			ast::InfixOp::PipeEq(_) => hir::InfixOp::BitOrAssign,
			ast::InfixOp::AmpEq(_) => hir::InfixOp::BitAndAssign,
			ast::InfixOp::CaretEq(_) => hir::InfixOp::XorAssign,
		}
	}

	fn call(&mut self, c: ast::CallExpr) -> hir::ExprKind {
		let args = c
			.arg_list()
			.iter()
			.flat_map(|x| x.exprs())
			.map(|e| self.expr(Some(e)))
			.collect();

		let callee = c.expr();
		let callee = match callee {
			Some(ast::Expr::NameExpr(n)) => match self.name_expr_inner(n) {
				NameExprResolution::Struct(struct_) => return hir::ExprKind::Struct(hir::StructExpr { struct_, args }),
				NameExprResolution::Expr(kind) => {
					let id = self.builder.add(c.expr());
					self.exprs.push(hir::Expr { kind, id })
				},
				NameExprResolution::Error => {
					let id = self.builder.add_errored();
					self.exprs.push(hir::Expr {
						kind: hir::ExprKind::Error,
						id,
					})
				},
			},
			_ => self.expr(c.expr()),
		};
		hir::ExprKind::Call(hir::CallExpr { callee, args })
	}

	fn cast(&mut self, c: ast::CastExpr) -> hir::CastExpr {
		let expr = self.expr(c.expr());
		let ty = self.ty(c.type_());
		hir::CastExpr { expr, ty }
	}

	/// A little complex because paths are ambiguous.
	///
	/// `a.b.c` is parsed as a field access, so we have to reconstruct it as a path first.
	fn field(&mut self, f: ast::FieldExpr) -> hir::ExprKind {
		match self.resolver.resolve_field_expr(f.clone()) {
			Some(res) => match res {
				Resolution::Item { path, ty, unresolved } => match ty {
					NameTy::Fn => {
						if let Some(rest) = unresolved.full_span() {
							let span = rest.with(self.file);
							self.ctx
								.push(span.error("path continues after function").label(span.mark()));
						}
						hir::ExprKind::Fn(path)
					},
					NameTy::Struct => {
						if let Some(rest) = unresolved.full_span() {
							let span = rest.with(self.file);
							self.ctx.push(span.error("expected value, found struct field").label(
								span.label("cannot access a field of a type - use an instance of the struct instead"),
							));
						} else {
							let span = f.span().with(self.file);
							self.ctx
								.push(span.error("expected value, found struct").label(span.mark()));
						}
						ExprKind::Error
					},
					NameTy::Enum => {
						// TODO: enum aaaa
						let span = f.span().with(self.file);
						self.ctx
							.push(span.error("enums are not supported yet").label(span.mark()));
						hir::ExprKind::Error
					},
					NameTy::TypeAlias => {
						// TODO: type alias aaaa
						let span = f.span().with(self.file);
						self.ctx
							.push(span.error("type aliases are not supported yet").label(span.mark()));
						hir::ExprKind::Error
					},
					NameTy::Static => self.field_access(hir::ExprKind::Static(path), unresolved),
				},
				Resolution::Local { local, unresolved } => self.field_access(hir::ExprKind::Local(local), unresolved),
				Resolution::Param { param, unresolved } => self.field_access(hir::ExprKind::Param(param), unresolved),
				Resolution::Error => ExprKind::Error,
			},
			None => {
				let expr = self.expr(f.expr());
				let field = self.name(f.name());

				hir::ExprKind::Field(hir::FieldExpr { expr, field })
			},
		}
	}

	fn field_access(&mut self, mut from: hir::ExprKind, mut path: ResPath) -> hir::ExprKind {
		while let Some(name) = path.elems.pop() {
			let parent = ast::Expr::cast(name.ast.clone().inner().parent().unwrap().clone().into()).unwrap();
			let expr = self.exprs.push(hir::Expr {
				kind: from,
				id: self.builder.add(Some(parent)),
			});
			from = hir::ExprKind::Field(hir::FieldExpr {
				expr,
				field: self.name(Some(name.ast)),
			});
		}

		from
	}

	fn if_(&mut self, i: ast::IfExpr) -> hir::MatchExpr {
		let cond = self.expr(i.cond());
		let id = self.builder.add(i.cond());
		let t = i.then();
		let then = self.block(t.clone());
		let t = self.builder.add(t);
		let then = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Block(then),
			id: self.builder.cast(t),
		});
		let else_ = i.else_().map(|e| self.expr(Some(e)));

		self.make_if(id, cond, then, else_)
	}

	fn index(&mut self, i: ast::IndexExpr) -> hir::IndexExpr {
		let expr = self.expr(i.base());
		let index = self.expr(i.index());

		hir::IndexExpr { expr, index }
	}

	fn literal(&mut self, l: ast::Literal) -> hir::Literal {
		let l = match l {
			ast::Literal::IntLit(i) => hir::Literal::Int(i.text().as_str().parse().unwrap()),
			ast::Literal::FloatLit(f) => hir::Literal::Float(f.text().as_str().parse::<f64>().unwrap().to_bits()),
			ast::Literal::CharLit(c) => hir::Literal::Char(c.text().as_str().as_bytes()[0]),
			ast::Literal::StringLit(s) => {
				let s = s.text().as_str();
				hir::Literal::String(&s[1..s.len() - 1])
			},
			ast::Literal::BoolLit(b) => hir::Literal::Bool(b.text().as_str() == "true"),
		};
		l
	}

	fn loop_(&mut self, l: ast::LoopExpr) -> hir::LoopExpr {
		let body = self.block(l.body());
		hir::LoopExpr { body }
	}

	fn match_(&mut self, m: ast::MatchExpr) -> hir::MatchExpr {
		let expr = self.expr(m.expr());
		let arms = m.arms().map(|a| self.arm(a)).collect();
		hir::MatchExpr { expr, arms }
	}

	fn name_expr(&mut self, n: ast::NameExpr) -> hir::ExprKind {
		let span = n.span().with(self.file);
		match self.name_expr_inner(n) {
			NameExprResolution::Expr(kind) => kind,
			NameExprResolution::Struct(_) => {
				self.ctx
					.push(span.error("expected value").label(span.label("found struct")));
				ExprKind::Error
			},
			NameExprResolution::Error => ExprKind::Error,
		}
	}

	fn name_expr_inner(&mut self, n: ast::NameExpr) -> NameExprResolution {
		match self.resolver.resolve_name(n.clone()) {
			Resolution::Item { path, ty, .. } => match ty {
				NameTy::Fn => NameExprResolution::Expr(hir::ExprKind::Fn(path)),
				NameTy::Static => NameExprResolution::Expr(hir::ExprKind::Static(path)),
				NameTy::Struct => NameExprResolution::Struct(path),
				_ => {
					let span = n.span().with(self.file);
					self.ctx.push(
						span.error("expected value")
							.label(span.label(format!("found {:?}", ty))),
					);
					NameExprResolution::Error
				},
			},
			Resolution::Local { local, .. } => NameExprResolution::Expr(hir::ExprKind::Local(local)),
			Resolution::Param { param, .. } => NameExprResolution::Expr(hir::ExprKind::Param(param)),
			Resolution::Error => NameExprResolution::Error,
		}
	}

	fn arm(&mut self, a: ast::MatchArm) -> hir::MatchArm {
		let value = self.expr(a.value());
		let then = self.expr(a.then());

		hir::MatchArm { value, then }
	}

	fn prefix(&mut self, p: ast::PrefixExpr) -> hir::PrefixExpr {
		let op = p.op();
		let op_id = self.builder.add(op.clone());
		let op = self.prefix_op(op);
		let expr = self.expr(p.expr());

		hir::PrefixExpr { op, op_id, expr }
	}

	fn prefix_op(&mut self, p: Option<ast::PrefixOp>) -> hir::PrefixOp {
		let Some(p) = p else {
			return hir::PrefixOp::Error;
		};
		match p {
			ast::PrefixOp::Minus(_) => hir::PrefixOp::Neg,
			ast::PrefixOp::Not(_) => hir::PrefixOp::Not,
			ast::PrefixOp::Star(_) => hir::PrefixOp::Deref,
		}
	}

	fn ref_(&mut self, r: ast::RefExpr) -> hir::RefExpr {
		let expr = self.expr(r.expr());
		let mutable = r.mut_kw().is_some();

		hir::RefExpr { expr, mutable }
	}

	fn while_(&mut self, w: ast::WhileExpr) -> Option<hir::LoopExpr> {
		let cond = self.expr(w.expr());
		let mut body = self.block(w.body());

		let id = self.builder.add(w.expr());
		let break_ = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Break(None),
			id,
		});
		let kind = hir::ExprKind::Match(self.make_if(id, cond, break_, None));
		let prec = self.exprs.push(hir::Expr { kind, id });
		body.discard.insert(0, prec);

		Some(hir::LoopExpr { body })
	}

	fn let_(&mut self, l: ast::LetExpr) -> hir::LetExpr {
		let name = self.name(l.name());
		let ty = l.type_().map(|t| self.ty(Some(t)));
		let init = l.init().map(|i| self.expr(Some(i)));
		let local = self.locals.push(hir::Local { decl: name });

		self.resolver.declare_local(name.name, local);

		hir::LetExpr { name, ty, init, local }
	}

	fn abi(&mut self, a: Option<ast::Abi>) -> Option<hir::Abi> {
		// If `hir::Abi` does not exist: `fn`
		// If `hir::Abi` exists: `extern fn`
		// If `hir::AbiDecl` also exists: `extern "abi" fn`
		a.and_then(|a| {
			let abi = a.string_lit().map(|lit| {
				let abi_with_quotes = lit.text().as_str();
				let abi = &abi_with_quotes[1..abi_with_quotes.len() - 1];
				let id = self.builder.add_conc(lit);

				hir::AbiDecl { abi, id }
			});
			let id = self.builder.add_conc(a);

			Some(hir::Abi { abi, id })
		})
	}

	fn name(&mut self, n: Option<ast::Name>) -> hir::Name {
		let id = self.builder.add(n.clone());
		let name = n.and_then(|x| x.text()).unwrap_or_else(|| Text::new("<error>"));
		hir::Name { name, id }
	}

	fn make_if(
		&mut self, id: AstId<ast::Expr>, cond: Ix<hir::Expr>, then: Ix<hir::Expr>, else_: Option<Ix<hir::Expr>>,
	) -> hir::MatchExpr {
		let else_ = else_.unwrap_or_else(|| {
			self.exprs.push(hir::Expr {
				kind: hir::ExprKind::Block(hir::Block::default()),
				id,
			})
		});

		let true_ = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Literal(hir::Literal::Bool(true)),
			id,
		});
		let false_ = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Literal(hir::Literal::Bool(false)),
			id,
		});

		hir::MatchExpr {
			expr: cond,
			arms: vec![
				hir::MatchArm { value: true_, then },
				hir::MatchArm {
					value: false_,
					then: else_,
				},
			],
		}
	}
}
