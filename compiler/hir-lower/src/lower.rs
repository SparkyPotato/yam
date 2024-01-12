use arena::{Arena, Ix};
use diagnostics::{FilePath, RawSpan, Span};
use hir::ident::AbsPath;
use rustc_hash::FxHashMap;
use syntax::{ast, token, AstElement, AstToken, SyntaxElement};
use text::Text;
use tracing::{span, Level};
use verde::{internal::storage::tracked::Get, query, Ctx, Db, Id, Interned, Tracked};

use crate::{
	index::{
		canonical::{CanonicalTree, Declaration, ModuleTree},
		local::name_of_item,
		ItemBuilder,
		ModuleMap,
		NameTy,
	},
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
	let mut lowerer = Lowerer::new(ctx, module, builder, packages, tree, file);
	let path = ctx.add(AbsPath::Name { prec: module, name });

	let attrs = item.attributes().into_iter().filter_map(|x| lowerer.attr(x)).collect();

	let kind = match item.item_kind() {
		Some(ast::ItemKind::Fn(f)) => hir::ItemKind::Fn(lowerer.fn_(f)?),
		Some(ast::ItemKind::Struct(s)) => hir::ItemKind::Struct(lowerer.struct_(s)?),
		Some(ast::ItemKind::Enum(e)) => hir::ItemKind::Enum(lowerer.enum_(e)?),
		Some(ast::ItemKind::TypeAlias(t)) => hir::ItemKind::TypeAlias(lowerer.type_alias(t)?),
		Some(ast::ItemKind::Static(s)) => hir::ItemKind::Static(lowerer.static_(s)?),
		Some(ast::ItemKind::Import(_)) | None => return None,
	};

	Some(hir::Item {
		path,
		name: lowerer.name(name_of_item(&item)?.0)?,
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

enum NameExprResolution {
	Struct(Id<AbsPath>),
	Expr(hir::ExprKind),
}

impl<'a> Lowerer<'a> {
	fn new(
		ctx: &'a Ctx<'a>, module: Id<AbsPath>, builder: ItemBuilder<'a>, packages: Id<VisiblePackages>,
		tree: Id<CanonicalTree>, file: FilePath,
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

		let id = self.builder.add(a);

		Some(hir::Attr { kind, id })
	}

	fn fn_(&mut self, f: ast::Fn) -> Option<hir::Fn> {
		let abi = self.abi(f.abi());
		let name = self.name(f.name()?)?;
		let mut params = Arena::new();
		for p in f.param_list().iter().flat_map(|x| x.params()) {
			let Some(p) = self.param(p) else {
				continue;
			};
			let name = p.name.name;
			let param = params.push(p);
			self.resolver.declare_param(name, param);
		}
		let params = f
			.param_list()
			.iter()
			.flat_map(|x| x.params())
			.filter_map(|x| self.param(x))
			.collect();
		let ret = f.ret_ty().and_then(|x| self.ty(x.type_()?));
		let body = match f.fn_body() {
			Some(ast::FnBody::Block(b)) => self.block(b),
			Some(ast::FnBody::Semi(_)) | None => None,
		};

		Some(hir::Fn {
			abi,
			name,
			params,
			ret,
			body,
		})
	}

	fn struct_(&mut self, s: ast::Struct) -> Option<hir::Struct> {
		let name = self.name(s.name()?)?;
		let fields = s.fields().into_iter().filter_map(|x| self.param(x)).collect();

		Some(hir::Struct { name, fields })
	}

	fn enum_(&mut self, e: ast::Enum) -> Option<hir::Enum> {
		let name = self.name(e.name()?)?;
		let variants = e
			.variant_list()
			.iter()
			.flat_map(|x| x.variants())
			.filter_map(|x| self.name(x).map(|x| hir::Variant(x)))
			.collect();

		Some(hir::Enum { name, variants })
	}

	fn type_alias(&mut self, t: ast::TypeAlias) -> Option<hir::TypeAlias> {
		let name = self.name(t.name()?)?;
		let ty = self.ty(t.type_()?)?;

		Some(hir::TypeAlias { name, ty })
	}

	fn static_(&mut self, s: ast::Static) -> Option<hir::Static> {
		let name = self.name(s.name()?)?;
		let ty = self.ty(s.type_()?)?;
		let init = self.expr(s.init()?)?;

		Some(hir::Static { name, ty, init })
	}

	fn param(&mut self, p: ast::Param) -> Option<hir::Param> {
		let name = self.name(p.name()?)?;
		let ty = self.ty(p.type_()?)?;
		let id = self.builder.add(p);

		Some(hir::Param { name, ty, id })
	}

	fn ty(&mut self, t: ast::Type) -> Option<Ix<hir::Type>> {
		let id = self.builder.add(t.clone());
		let kind = match t {
			ast::Type::ArrayType(a) => {
				let ty = self.ty(a.type_()?)?;
				let len = self.expr(a.len()?)?;

				hir::TypeKind::Array(hir::ArrayType { ty, len })
			},
			ast::Type::FnType(f) => {
				let abi = self.abi(f.abi());
				let params = f
					.params()
					.iter()
					.flat_map(|x| x.types())
					.flat_map(|x| self.ty(x))
					.collect();
				let ret = f.ret_ty().and_then(|x| x.type_()).and_then(|x| self.ty(x));

				hir::TypeKind::Fn(hir::FnType { abi, params, ret })
			},
			ast::Type::InferType(_) => hir::TypeKind::Infer,
			ast::Type::PathType(p) => {
				let path = p.path()?;
				match self.resolver.resolve_path(path.clone())? {
					Resolution::Item {
						path: p,
						ty,
						unresolved,
					} => {
						if let Some(rest) = unresolved {
							let rest = self.ctx.geti(rest);
							let span = rest.ast.span().with(self.file);
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
								return None;
							},
						}
					},
					Resolution::Local { .. } => {
						let span = path.span().with(self.file);
						self.ctx
							.push(span.error("expected type").label(span.label("found local")));
						return None;
					},
					Resolution::Param { .. } => {
						let span = path.span().with(self.file);
						self.ctx
							.push(span.error("expected type").label(span.label("found param")));
						return None;
					},
				}
			},
			ast::Type::PtrType(p) => {
				let mutable = p.mut_kw().is_some();
				let ty = self.ty(p.type_()?)?;

				hir::TypeKind::Ptr(hir::PtrType { mutable, ty })
			},
		};

		Some(self.types.push(hir::Type { kind, id }))
	}

	fn block(&mut self, b: ast::Block) -> Option<hir::Block> {
		let mut discard = Vec::with_capacity(b.statements().count());
		let mut value = None;
		let mut errored = false;

		self.resolver.push_scope();
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
					value = self.expr(e);
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

					let e = e.expr().and_then(|e| self.expr(e));
					discard.extend(e);
				},
				ast::Stmt::Semi(_) => {},
			}
		}
		self.resolver.pop_scope();

		Some(hir::Block { discard, value })
	}

	fn expr(&mut self, e: ast::Expr) -> Option<Ix<hir::Expr>> {
		let kind = match e.clone() {
			ast::Expr::ArrayExpr(a) => hir::ExprKind::Array(self.array(a)?),
			ast::Expr::InfixExpr(i) => hir::ExprKind::Infix(self.infix(i)?),
			ast::Expr::Block(b) => hir::ExprKind::Block(self.block(b).unwrap_or_default()),
			ast::Expr::BreakExpr(b) => hir::ExprKind::Break(b.expr().and_then(|x| self.expr(x))),
			ast::Expr::CallExpr(c) => self.call(c)?,
			ast::Expr::CastExpr(c) => hir::ExprKind::Cast(self.cast(c)?),
			ast::Expr::ContinueKw(_) => hir::ExprKind::Continue,
			ast::Expr::FieldExpr(f) => self.field(f)?,
			ast::Expr::ForExpr(_) => {
				// TODO: Support.
				let span = e.span().with(self.file);
				self.ctx.push(span.error("`for` expressions are not supported"));
				return None;
			},
			ast::Expr::IfExpr(i) => hir::ExprKind::Match(self.if_(i)?),
			ast::Expr::IndexExpr(i) => hir::ExprKind::Index(self.index(i)?),
			ast::Expr::Literal(l) => hir::ExprKind::Literal(self.literal(l)?),
			ast::Expr::LoopExpr(l) => hir::ExprKind::Loop(self.loop_(l)?),
			ast::Expr::MatchExpr(m) => hir::ExprKind::Match(self.match_(m)?),
			ast::Expr::NameExpr(n) => self.name_expr(n)?,
			ast::Expr::ParenExpr(e) => return self.expr(e.expr()?),
			ast::Expr::PrefixExpr(p) => hir::ExprKind::Prefix(self.prefix(p)?),
			ast::Expr::RefExpr(r) => hir::ExprKind::Ref(self.ref_(r)?),
			ast::Expr::ReturnExpr(r) => hir::ExprKind::Return(r.expr().and_then(|x| self.expr(x))),
			ast::Expr::WhileExpr(w) => hir::ExprKind::Loop(self.while_(w)?),
			ast::Expr::LetExpr(l) => hir::ExprKind::Let(self.let_(l)?),
		};
		let id = Some(self.builder.add(e));

		Some(self.exprs.push(hir::Expr { kind, id }))
	}

	fn array(&mut self, a: ast::ArrayExpr) -> Option<hir::ArrayExpr> {
		let init = a.array_init()?;
		let init = match init {
			ast::ArrayInit::ArrayList(l) => {
				let elems = l.exprs().flat_map(|e| self.expr(e)).collect();
				hir::ArrayExpr { elems, repeat: false }
			},
			ast::ArrayInit::ArrayRepeat(r) => {
				let expr = self.expr(r.expr()?)?;
				let len = self.expr(r.len()?)?;
				hir::ArrayExpr {
					elems: vec![expr, len],
					repeat: true,
				}
			},
		};
		Some(init)
	}

	fn infix(&mut self, i: ast::InfixExpr) -> Option<hir::InfixExpr> {
		let lhs = self.expr(i.lhs()?)?;
		let op = i.op()?;
		let op_id = self.builder.add(op.clone());
		let op = self.infix_op(op);
		let rhs = self.expr(i.rhs()?)?;

		Some(hir::InfixExpr { lhs, op, op_id, rhs })
	}

	fn infix_op(&mut self, o: ast::InfixOp) -> hir::InfixOp {
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

	fn call(&mut self, c: ast::CallExpr) -> Option<hir::ExprKind> {
		let args = c
			.arg_list()
			.iter()
			.flat_map(|x| x.exprs())
			.flat_map(|e| self.expr(e))
			.collect();

		let callee = c.expr()?;
		let kind = match callee {
			ast::Expr::NameExpr(n) => match self.name_expr_inner(n)? {
				NameExprResolution::Struct(struct_) => hir::ExprKind::Struct(hir::StructExpr { struct_, args }),
				NameExprResolution::Expr(kind) => {
					let id = Some(self.builder.add(c.expr()?));
					let callee = self.exprs.push(hir::Expr { kind, id });
					let args = c
						.arg_list()
						.iter()
						.flat_map(|x| x.exprs())
						.flat_map(|e| self.expr(e))
						.collect();

					hir::ExprKind::Call(hir::CallExpr { callee, args })
				},
			},
			_ => {
				let callee = self.expr(c.expr()?)?;
				hir::ExprKind::Call(hir::CallExpr { callee, args })
			},
		};
		Some(kind)
	}

	fn cast(&mut self, c: ast::CastExpr) -> Option<hir::CastExpr> {
		let expr = self.expr(c.expr()?)?;
		let ty = self.ty(c.type_()?)?;

		Some(hir::CastExpr { expr, ty })
	}

	/// A little complex because paths are ambiguous.
	///
	/// `a.b.c` is parsed as a field access, so we have to reconstruct it as a path first.
	fn field(&mut self, f: ast::FieldExpr) -> Option<hir::ExprKind> {
		match CachedPath::from_field_expr(self.ctx, f.clone()) {
			Some(path) => {
				let kind = match self.resolver.resolve_inner(path)? {
					Resolution::Item { path, ty, unresolved } => match ty {
						NameTy::Fn => {
							if let Some(rest) = unresolved {
								let rest = self.ctx.geti(rest);
								let span = rest.ast.span().with(self.file);
								self.ctx
									.push(span.error("path continues after function").label(span.mark()));
							}
							hir::ExprKind::Fn(path)
						},
						NameTy::Struct => {
							if let Some(rest) = unresolved {
								let rest = self.ctx.geti(rest);
								let span = rest.ast.span().with(self.file);
								self.ctx.push(span.error("expected value, found struct field").label(
									span.label(
										"cannot access a field of a type - use an instance of the struct instead",
									),
								));
							} else {
								let span = f.span().with(self.file);
								self.ctx
									.push(span.error("expected value, found struct").label(span.mark()));
							}
							return None;
						},
						NameTy::Enum => {
							if let Some(rest) = unresolved {
								let rest = self.ctx.geti(rest);
								let variant = self.name(rest.ast.clone())?;
								if let Some(rest) = rest.next {
									let rest = self.ctx.geti(rest);
									let span = rest.ast.span().with(self.file);
									self.ctx
										.push(span.error("path continues after enum variant").label(span.mark()));
								}

								hir::ExprKind::EnumVariant(hir::VariantExpr { path, variant })
							} else {
								let span = f.span().with(self.file);
								self.ctx
									.push(span.error("expected value, found enum").label(span.mark()));
								return None;
							}
						},
						NameTy::TypeAlias => {
							if let Some(rest) = unresolved {
								let rest = self.ctx.geti(rest);
								let variant = self.name(rest.ast.clone())?;
								if let Some(rest) = rest.next {
									let rest = self.ctx.geti(rest);
									let span = rest.ast.span().with(self.file);
									self.ctx
										.push(span.error("path continues after enum variant").label(span.mark()));
								}

								hir::ExprKind::EnumVariant(hir::VariantExpr { path, variant })
							} else {
								let span = f.span().with(self.file);
								self.ctx
									.push(span.error("expected value, found type alias").label(span.mark()));
								return None;
							}
						},
						NameTy::Static => self.field_access(hir::ExprKind::Static(path), unresolved),
					},
					Resolution::Local { local, unresolved } => {
						self.field_access(hir::ExprKind::Local(local), unresolved)
					},
					Resolution::Param { param, unresolved } => {
						self.field_access(hir::ExprKind::Param(param), unresolved)
					},
				};

				Some(kind)
			},
			None => {
				let expr = self.expr(f.expr()?)?;
				let field = self.name(f.name()?)?;

				Some(hir::ExprKind::Field(hir::FieldExpr { expr, field }))
			},
		}
	}

	fn field_access(&mut self, mut from: hir::ExprKind, mut path: Option<Id<CachedName>>) -> hir::ExprKind {
		while let Some(p) = path {
			let p = self.ctx.geti(p);
			let expr = self.exprs.push(hir::Expr { kind: from, id: None });
			from = hir::ExprKind::Field(hir::FieldExpr {
				expr,
				field: self.name(p.ast.clone()).unwrap(),
			});

			path = p.next;
		}

		from
	}

	fn if_(&mut self, i: ast::IfExpr) -> Option<hir::MatchExpr> {
		let cond = self.expr(i.cond()?)?;
		let t = i.then()?;
		let then = self.block(t.clone())?;
		let t = self.builder.add(t);
		let then = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Block(then),
			id: Some(self.builder.cast(t)),
		});
		let else_ = i.else_().and_then(|e| self.expr(e));

		Some(self.make_if(cond, then, else_))
	}

	fn index(&mut self, i: ast::IndexExpr) -> Option<hir::IndexExpr> {
		let expr = self.expr(i.base()?)?;
		let index = self.expr(i.index()?)?;

		Some(hir::IndexExpr { expr, index })
	}

	fn literal(&mut self, l: ast::Literal) -> Option<hir::Literal> {
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
		Some(l)
	}

	fn loop_(&mut self, l: ast::LoopExpr) -> Option<hir::LoopExpr> {
		let body = self.block(l.body()?)?;
		Some(hir::LoopExpr { body })
	}

	fn match_(&mut self, m: ast::MatchExpr) -> Option<hir::MatchExpr> {
		let expr = self.expr(m.expr()?)?;
		let arms = m.arms().flat_map(|a| self.arm(a)).collect();

		Some(hir::MatchExpr { expr, arms })
	}

	fn name_expr(&mut self, n: ast::NameExpr) -> Option<hir::ExprKind> {
		let span = n.span().with(self.file);
		let kind = match self.name_expr_inner(n)? {
			NameExprResolution::Expr(kind) => kind,
			NameExprResolution::Struct(_) => {
				self.ctx
					.push(span.error("expected value").label(span.label("found struct")));
				return None;
			},
		};

		Some(kind)
	}

	fn name_expr_inner(&mut self, n: ast::NameExpr) -> Option<NameExprResolution> {
		let res = match self.resolver.resolve_name(n.clone())? {
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
					return None;
				},
			},
			Resolution::Local { local, .. } => NameExprResolution::Expr(hir::ExprKind::Local(local)),
			Resolution::Param { param, .. } => NameExprResolution::Expr(hir::ExprKind::Param(param)),
		};

		Some(res)
	}

	fn arm(&mut self, a: ast::MatchArm) -> Option<hir::MatchArm> {
		let value = self.expr(a.value()?)?;
		let then = self.expr(a.then()?)?;

		Some(hir::MatchArm { value, then })
	}

	fn prefix(&mut self, p: ast::PrefixExpr) -> Option<hir::PrefixExpr> {
		let op = p.op()?;
		let op_id = self.builder.add(op.clone());
		let op = self.prefix_op(p.op()?);
		let expr = self.expr(p.expr()?)?;

		Some(hir::PrefixExpr { op, op_id, expr })
	}

	fn prefix_op(&mut self, p: ast::PrefixOp) -> hir::PrefixOp {
		match p {
			ast::PrefixOp::Minus(_) => hir::PrefixOp::Neg,
			ast::PrefixOp::Not(_) => hir::PrefixOp::Not,
			ast::PrefixOp::Star(_) => hir::PrefixOp::Deref,
		}
	}

	fn ref_(&mut self, r: ast::RefExpr) -> Option<hir::RefExpr> {
		let expr = self.expr(r.expr()?)?;
		let mutable = r.mut_kw().is_some();

		Some(hir::RefExpr { expr, mutable })
	}

	fn while_(&mut self, w: ast::WhileExpr) -> Option<hir::LoopExpr> {
		let cond = self.expr(w.expr()?)?;
		let mut body = self.block(w.body()?)?;

		let break_ = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Break(None),
			id: None,
		});
		let kind = hir::ExprKind::Match(self.make_if(cond, break_, None));
		let prec = self.exprs.push(hir::Expr { kind, id: None });
		body.discard.insert(0, prec);

		Some(hir::LoopExpr { body })
	}

	fn let_(&mut self, l: ast::LetExpr) -> Option<hir::LetExpr> {
		let name = self.name(l.name()?)?;
		let ty = l.type_().and_then(|t| self.ty(t));
		let init = l.init().and_then(|i| self.expr(i));
		let local = self.locals.push(hir::Local { decl: name });

		self.resolver.declare_local(name.name, local);

		Some(hir::LetExpr { name, ty, init, local })
	}

	fn abi(&mut self, a: Option<ast::Abi>) -> Option<hir::Abi> {
		// If `hir::Abi` does not exist: `fn`
		// If `hir::Abi` exists: `extern fn`
		// If `hir::AbiDecl` also exists: `extern "abi" fn`
		a.and_then(|a| {
			let abi = a.string_lit().map(|lit| {
				let abi_with_quotes = lit.text().as_str();
				let abi = &abi_with_quotes[1..abi_with_quotes.len() - 1];
				let id = self.builder.add(lit);

				hir::AbiDecl { abi, id }
			});
			let id = self.builder.add(a);

			Some(hir::Abi { abi, id })
		})
	}

	fn name(&mut self, n: ast::Name) -> Option<hir::Name> {
		let name = n.text()?;
		let id = self.builder.add(n);

		Some(hir::Name { name, id })
	}

	fn make_if(&mut self, cond: Ix<hir::Expr>, then: Ix<hir::Expr>, else_: Option<Ix<hir::Expr>>) -> hir::MatchExpr {
		let else_ = else_.unwrap_or_else(|| {
			self.exprs.push(hir::Expr {
				kind: hir::ExprKind::Block(hir::Block::default()),
				id: None,
			})
		});

		let true_ = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Literal(hir::Literal::Bool(true)),
			id: None,
		});
		let false_ = self.exprs.push(hir::Expr {
			kind: hir::ExprKind::Literal(hir::Literal::Bool(false)),
			id: None,
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

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct CachedPath {
	root: Option<token::Dot>,
	rest: Option<Id<CachedName>>,
}

#[derive(Interned, Clone, Eq, PartialEq, Hash)]
pub struct CachedName {
	name: Text,
	ast: ast::Name,
	next: Option<Id<CachedName>>,
}

impl CachedPath {
	fn from_ast(ctx: &Ctx, path: ast::Path) -> Option<Self> {
		let mut segs = path.path_segments().peekable();

		let first = segs.peek().unwrap();
		let root = match first {
			ast::PathSegment::Dot(d) => {
				let d = d.clone();
				segs.next();
				Some(d)
			},
			ast::PathSegment::Name(_) => None,
		};

		fn inner(ctx: &Ctx, segs: &mut impl Iterator<Item = ast::PathSegment>) -> Option<Id<CachedName>> {
			let (name, ast) = loop {
				match segs.next()? {
					ast::PathSegment::Name(name) => {
						if let Some(n) = name.text() {
							break (n, name);
						}
					},
					ast::PathSegment::Dot(_) => {},
				}
			};
			let next = inner(ctx, segs);

			Some(ctx.add(CachedName { name, ast, next }))
		}

		let rest = inner(ctx, &mut segs);
		Some(CachedPath { root, rest })
	}

	fn from_field_expr(ctx: &Ctx, mut expr: ast::FieldExpr) -> Option<Self> {
		let mut next = None;
		loop {
			let name = expr.name()?;
			let ast = name.clone();
			let name = name.text()?;
			next = Some(ctx.add(CachedName { name, ast, next }));

			match expr.expr()? {
				ast::Expr::FieldExpr(f) => expr = f,
				ast::Expr::NameExpr(n) => {
					let root = n.dot();
					let name = n.name()?;
					let ast = name.clone();
					let name = name.text()?;
					next = Some(ctx.add(CachedName { name, ast, next }));
					break Some(Self { root, rest: next });
				},
				_ => return None,
			}
		}
	}
}

enum Resolution {
	Local {
		local: Ix<hir::Local>,
		unresolved: Option<Id<CachedName>>,
	},
	Param {
		param: Ix<hir::Param>,
		unresolved: Option<Id<CachedName>>,
	},
	Item {
		path: Id<AbsPath>,
		ty: NameTy,
		unresolved: Option<Id<CachedName>>,
	},
}

struct NameResolver<'a> {
	ctx: &'a Ctx<'a>,
	this: Id<AbsPath>,
	packages: Get<'a, VisiblePackages>,
	tree: Get<'a, CanonicalTree>,
	scopes: Vec<FxHashMap<Text, Ix<hir::Local>>>,
	params: FxHashMap<Text, Ix<hir::Param>>,
	file: FilePath,
}

impl<'a> NameResolver<'a> {
	fn new(
		ctx: &'a Ctx, this: Id<AbsPath>, packages: Id<VisiblePackages>, tree: Id<CanonicalTree>, file: FilePath,
	) -> Self {
		Self {
			ctx,
			this,
			packages: ctx.get(packages),
			tree: ctx.get(tree),
			scopes: Vec::new(),
			params: FxHashMap::default(),
			file,
		}
	}

	fn resolve_path(&mut self, path: ast::Path) -> Option<Resolution> {
		let path = CachedPath::from_ast(self.ctx, path)?;
		self.resolve_inner(path)
	}

	fn resolve_name(&mut self, name: ast::NameExpr) -> Option<Resolution> {
		let root = name.dot();
		let ast = name.name()?;
		let path = CachedPath {
			root,
			rest: Some(self.ctx.add(CachedName {
				name: ast.text()?,
				ast,
				next: None,
			})),
		};
		self.resolve_inner(path)
	}

	fn resolve_inner(&mut self, path: CachedPath) -> Option<Resolution> {
		if let Some(d) = path.root {
			if let Some(rest) = path.rest {
				let rest = self.ctx.geti(rest);
				let Some(pkg) = self.packages.packages.get(&rest.name) else {
					let span = rest.ast.span().with(self.file);
					self.ctx.push(
						span.error("unknown package")
							.label(span.label("404: this package does not exist")),
					);
					return None;
				};
				let tree = self.tree.packages[pkg];
				let Some(next) = rest.next else {
					let span = rest.ast.span().with(self.file);
					self.ctx
						.push(span.error("expected item").label(span.label("found package")));
					return None;
				};

				let (path, ty, unresolved) = self.resolve_from_module(next, tree, rest.name)?;
				Some(Resolution::Item { path, ty, unresolved })
			} else {
				let span = d.span().with(self.file);
				self.ctx
					.push(span.error("bare `.`s are not supported").label(span.mark()));
				None
			}
		} else {
			let rest = path.rest.expect("empty path");
			let r = self.ctx.geti(rest);
			if let Some(local) = self.resolve_local(r.name) {
				return Some(Resolution::Local {
					local,
					unresolved: r.next,
				});
			} else if let Some(param) = self.resolve_param(r.name) {
				return Some(Resolution::Param {
					param,
					unresolved: r.next,
				});
			}

			let tree = self.tree.modules[&self.this];
			let prev = match *self.ctx.geti(self.this) {
				AbsPath::Name { name, .. } => name,
				AbsPath::Package(_) => Text::new("root"),
			};
			let (path, ty, unresolved) = self.resolve_from_module(rest, tree, prev)?;
			Some(Resolution::Item { path, ty, unresolved })
		}
	}

	fn resolve_from_module(
		&mut self, path: Id<CachedName>, tree: Id<ModuleTree>, mut prev: Text,
	) -> Option<(Id<AbsPath>, NameTy, Option<Id<CachedName>>)> {
		let mut search = self.ctx.geti(path);
		let mut tree = self.ctx.get(tree);

		loop {
			let is_private = is_child_of(self.ctx, tree.path, self.this);
			let index = if is_private { tree.private } else { tree.public };
			let index = self.ctx.get(index);
			let Some(decl) = index.names.get(&search.name) else {
				let span = search.ast.span().with(self.file);
				self.ctx.push(
					span.error("unknown name")
						.label(span.label(format!("404: name does not exist in `{}`", prev.as_str()))),
				);
				return None;
			};

			match decl {
				Declaration::Module(t) => {
					tree = self.ctx.get(*t);
					let Some(next) = search.next else {
						let span = search.ast.span().with(self.file);
						self.ctx
							.push(span.error("expected item").label(span.label("found module")));
						return None;
					};
					search = self.ctx.geti(next);
				},
				Declaration::Name { path, ty, .. } => return Some((*path, *ty, search.next)),
			}

			prev = search.name;
		}
	}

	fn declare_param(&mut self, name: Text, param: Ix<hir::Param>) { self.params.insert(name, param); }

	fn declare_local(&mut self, name: Text, local: Ix<hir::Local>) {
		let scope = self.scopes.last_mut().expect("let without any scope");
		scope.insert(name, local);
	}

	fn resolve_local(&self, name: Text) -> Option<Ix<hir::Local>> {
		for scope in self.scopes.iter().rev() {
			if let Some(&local) = scope.get(&name) {
				return Some(local);
			}
		}
		None
	}

	fn resolve_param(&self, name: Text) -> Option<Ix<hir::Param>> { self.params.get(&name).copied() }

	fn push_scope(&mut self) { self.scopes.push(FxHashMap::default()); }

	fn pop_scope(&mut self) { self.scopes.pop().expect("pop without any scope"); }
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
