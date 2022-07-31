use diag::{Diagnostics, Span};
pub use parse::Rodeo;
use parse::{
	ast::{
		Attr,
		Block,
		Expr,
		ExprKind,
		Fn,
		Ident,
		ItemKind,
		Let,
		LitKind,
		Module,
		Pat,
		PatKind,
		Stmt,
		StmtKind,
		Struct,
		Token,
		Visibility,
	},
	token::TokenKind,
	Spur,
};

use crate::{
	ctx::*,
	hir::*,
	lang_item::{LangItem, LangItemIdents},
	types::Type,
};

pub mod ctx;
pub mod hir;
pub mod lang_item;
pub mod types;

fn register_module_items(module: &Module, prefix: Path, builder: &mut HirBuilder, diags: &mut Diagnostics) {
	for item in module.items.iter() {
		let curr_span = item.span;
		match &item.kind {
			ItemKind::Fn(ident, _) | ItemKind::Struct(ident, _) => {
				Some(builder.decl_val(prefix.with(*ident), curr_span, diags))
			},
			ItemKind::Const(l) | ItemKind::Static(l) => Some(match l.pat.node {
				PatKind::Binding(binding) => builder.decl_val(
					prefix.with(Ident {
						node: binding.binding,
						span: l.pat.span,
					}),
					curr_span,
					diags,
				),
			}),
			ItemKind::Import(_) => {
				diags.push(item.span.error("imports are not supported").label(item.span.mark()));
				None
			},
		};
	}
}

pub fn resolve(module: Module, mut rodeo: Rodeo, diagnostics: &mut Diagnostics) -> Hir {
	let mut builder = HirBuilder::new();

	register_module_items(&module, Path::default(), &mut builder, diagnostics);

	let mut resolver = Resolver {
		idents: LangItemIdents::new(&mut rodeo),
		rodeo: &rodeo,
		builder: &mut builder,
		diagnostics,
		local: LocalBuilder::new(),
		in_fn: false,
		in_loop: false,
	};

	for item in module.items {
		let span = item.span;

		let mut lang_item: Option<(LangItem, Span)> = None;
		for attr in item.attrs {
			let attr_span = attr.span;
			let item = resolver.lang_item(attr);
			if item.is_some() {
				if let Some((_, span)) = lang_item {
					resolver.diagnostics.push(
						attr_span
							.error("multiple lang attributes on one item")
							.label(span.label("first lang attribute"))
							.label(attr_span.label("duplicate lang attribute")),
					);
				}
				lang_item = item.map(|x| (x, attr_span));
			}
		}

		match item.kind {
			ItemKind::Fn(ident, f) => {
				let path = Path::from_ident(ident);
				let val = resolver.builder.resolve(&path).unwrap();

				let (sig, block) = resolver.resolve_fn(f, item.span);

				let def = ValDef {
					path,
					kind: match block {
						Some(block) => ValDefKind::Fn(hir::Fn { sig, block }),
						None => ValDefKind::FnDecl(sig),
					},
					span,
				};
				if let Some((lang, _)) = lang_item {
					resolver.builder.define_lang_item(lang, val, resolver.diagnostics);
					lang.verify_item(&def, val, resolver.diagnostics);
				}

				resolver.builder.define_val(val, def);
			},
			ItemKind::Const(l) => match l.pat.node {
				PatKind::Binding(b) => {
					let path = Path::from_ident(Ident {
						node: b.binding,
						span: l.pat.span,
					});
					let val = resolver.builder.resolve(&path).unwrap();

					let def = ValDef {
						path,
						kind: ValDefKind::Const(resolver.resolve_global_let(l, span)),
						span,
					};
					if let Some((lang, _)) = lang_item {
						resolver.builder.define_lang_item(lang, val, resolver.diagnostics);
						lang.verify_item(&def, val, resolver.diagnostics);
					}

					resolver.builder.define_val(val, def);
				},
			},
			ItemKind::Static(l) => match l.pat.node {
				PatKind::Binding(b) => {
					let path = Path::from_ident(Ident {
						node: b.binding,
						span: l.pat.span,
					});
					let val = resolver.builder.resolve(&path).unwrap();

					let def = ValDef {
						path,
						kind: ValDefKind::Static(resolver.resolve_global_let(l, span)),
						span,
					};
					if let Some((lang, _)) = lang_item {
						resolver.builder.define_lang_item(lang, val, resolver.diagnostics);
						lang.verify_item(&def, val, resolver.diagnostics);
					}

					resolver.builder.define_val(val, def);
				},
			},
			ItemKind::Struct(ident, s) => {
				let path = Path::from_ident(ident);
				let val = resolver.builder.resolve(&path).unwrap();

				let def = ValDef {
					path,
					kind: ValDefKind::Struct(resolver.resolve_struct(s)),
					span,
				};
				if let Some((lang, _)) = lang_item {
					resolver.builder.define_lang_item(lang, val, resolver.diagnostics);
					lang.verify_item(&def, val, resolver.diagnostics);
				}

				resolver.builder.define_val(val, def);
			},
			ItemKind::Import(_) => {},
		}
	}

	builder.finish(rodeo, diagnostics, module.source)
}

struct Resolver<'a> {
	rodeo: &'a Rodeo,
	builder: &'a mut HirBuilder,
	diagnostics: &'a mut Diagnostics,
	local: LocalBuilder,
	idents: LangItemIdents,
	in_fn: bool,
	in_loop: bool,
}

impl Resolver<'_> {
	fn lang_item(&mut self, attr: Attr) -> Option<LangItem> {
		if attr.name.node != self.idents.lang {
			self.diagnostics
				.push(attr.name.span.error("unknown attribute").label(attr.name.span.mark()));
		}

		match attr.values.as_slice() {
			&[Token {
				kind: TokenKind::Ident,
				data,
				span,
			}] => {
				if let Some(item) = self.idents.resolve_lang_item(data) {
					Some(item)
				} else {
					self.diagnostics
						.push(span.error("unknown lang item").label(span.mark()));

					None
				}
			},
			_ => {
				self.diagnostics.push(
					attr.span
						.error("lang item attribute expects one ident")
						.label(attr.name.span.mark()),
				);

				None
			},
		}
	}

	fn resolve_struct(&mut self, s: Struct) -> hir::Struct {
		hir::Struct {
			fields: s
				.fields
				.into_iter()
				.map(|x| {
					if x.is_const {
						self.diagnostics
							.push(x.span.error("structs cannot have `const` fields").label(x.span.mark()));
					}

					Field {
						visibility: x.visibility,
						name: match x.pat.node {
							PatKind::Binding(binding) => {
								if binding.mutability {
									self.diagnostics.push(
										x.span
											.error("structs cannot have `mut` fields")
											.label(x.pat.span.mark()),
									);
								}

								Ident {
									node: binding.binding,
									span: x.pat.span,
								}
							},
						},
						ty: self.resolve_expr(x.ty),
						span: x.span,
					}
				})
				.collect(),
		}
	}

	fn resolve_fn(&mut self, f: Fn, span: Span) -> (FnSignature, Option<hir::Block>) {
		self.local.push_scope();

		let args = f
			.args
			.into_iter()
			.map(|x| {
				if x.is_const {
					self.diagnostics.push(
						x.span
							.error("functions cannot have `const` arguments")
							.label(x.span.mark()),
					);
				}

				if x.visibility == Visibility::Public {
					self.diagnostics.push(
						x.span
							.error("functions cannot have `pub` arguments")
							.label(x.span.mark()),
					);
				}

				Arg {
					is_const: x.is_const,
					pat: self.resolve_pat(x.pat),
					ty: self.resolve_expr(x.ty),
					span: x.span,
				}
			})
			.collect();
		let ret_expr = f.ret.map(|expr| Box::new(self.resolve_expr(*expr)));

		if matches!(f.abi, Abi::None) && f.block.is_none() {
			self.diagnostics
				.push(span.error("functions must have a body").label(span.mark()));
		}

		let sig = FnSignature {
			abi: f.abi,
			args,
			ret_expr,
			ret: Type::Unknown,
		};

		self.in_fn = true;

		let ret = (sig, f.block.map(|block| self.resolve_block(block)));

		self.in_fn = false;

		self.local.pop_scope();
		self.local.reset();

		ret
	}

	fn resolve_global_let(&mut self, l: Let, span: Span) -> GlobalLet {
		GlobalLet {
			ty: Type::Unknown,
			ty_expr: l.ty.map(|expr| self.resolve_expr(*expr)),
			expr: if let Some(expr) = l.expr {
				self.resolve_expr(*expr)
			} else {
				self.diagnostics
					.push(span.error("globals must have initializers").label(span.mark()));

				hir::Expr {
					node: ExprData {
						kind: hir::ExprKind::Err,
						ty: Type::Void,
					},
					span,
				}
			},
		}
	}

	fn resolve_expr(&mut self, expr: Expr) -> hir::Expr {
		hir::Expr {
			node: ExprData {
				kind: self.resolve_expr_kind(expr.node, expr.span),
				ty: Type::Unknown,
			},
			span: expr.span,
		}
	}

	fn resolve_expr_kind(&mut self, expr: ExprKind, span: Span) -> hir::ExprKind {
		match expr {
			ExprKind::Lit(lit) => hir::ExprKind::Lit(match lit.kind {
				LitKind::Bool => Lit::Bool(self.rodeo.resolve(&lit.sym) == "true"),
				LitKind::Char => Lit::Char(
					self.rodeo
						.resolve(&lit.sym)
						.parse()
						.expect("failed to parse char literal"),
				),
				LitKind::Int => Lit::Int(
					self.rodeo
						.resolve(&lit.sym)
						.parse()
						.expect("failed to parse int literal"),
				),
				LitKind::Float => Lit::Float(
					self.rodeo
						.resolve(&lit.sym)
						.parse()
						.expect("failed to parse float literal"),
				),
				LitKind::String => Lit::String(lit.sym),
			}),
			ExprKind::Block(block) => hir::ExprKind::Block(self.resolve_block(block)),
			ExprKind::Ident(x) => {
				if let Some(local) = self.local.resolve(x) {
					return hir::ExprKind::LocalRef(local);
				}

				match self.builder.resolve(&Path::from_ident(Ident { node: x, span })) {
					Some(val) => hir::ExprKind::ValRef(val),
					None => {
						self.diagnostics
							.push(span.error("undefined identifier").label(span.mark()));

						hir::ExprKind::Err
					},
				}
			},
			ExprKind::Let(l) => hir::ExprKind::Let(hir::Let {
				pat: self.resolve_pat(l.pat),
				ty: Type::Unknown,
				ty_expr: l.ty.map(|expr| Box::new(self.resolve_expr(*expr))),
				expr: l.expr.map(|expr| Box::new(self.resolve_expr(*expr))),
				span,
			}),
			ExprKind::List(list) => hir::ExprKind::List(list.into_iter().map(|x| self.resolve_expr(x)).collect()),
			ExprKind::Array(array) => hir::ExprKind::Array(Array {
				expr: Box::new(self.resolve_expr(*array.expr)),
				count: Box::new(self.resolve_expr(*array.count)),
			}),
			ExprKind::Cast(cast) => hir::ExprKind::Cast(Cast {
				expr: Box::new(self.resolve_expr(*cast.expr)),
				ty: Box::new(self.resolve_expr(*cast.ty)),
			}),
			ExprKind::Never => hir::ExprKind::Never,
			ExprKind::Type => hir::ExprKind::Type,
			ExprKind::TypeOf(e) => hir::ExprKind::TypeOf(Box::new(self.resolve_expr(*e))),
			ExprKind::Ptr(ptr) => hir::ExprKind::Ptr(Ptr {
				mutability: ptr.mutability,
				to: Box::new(self.resolve_expr(*ptr.to)),
			}),
			ExprKind::Fn(_) => {
				self.diagnostics
					.push(span.error("closures are not supported yet").label(span.mark()));
				hir::ExprKind::Err
			},
			ExprKind::Call(call) => hir::ExprKind::Call(Call {
				target: Box::new(self.resolve_expr(*call.target)),
				args: call.args.into_iter().map(|x| self.resolve_expr(x)).collect(),
			}),
			ExprKind::Index(index) => hir::ExprKind::Index(Index {
				target: Box::new(self.resolve_expr(*index.target)),
				index: Box::new(self.resolve_expr(*index.index)),
			}),
			ExprKind::Access(access) => hir::ExprKind::Access(Access {
				target: Box::new(self.resolve_expr(*access.target)),
				field: access.field,
			}),
			ExprKind::Unary(unary) => hir::ExprKind::Unary(Unary {
				op: unary.op,
				expr: Box::new(self.resolve_expr(*unary.expr)),
			}),
			ExprKind::Binary(binary) => hir::ExprKind::Binary(Binary {
				op: binary.op,
				lhs: Box::new(self.resolve_expr(*binary.lhs)),
				rhs: Box::new(self.resolve_expr(*binary.rhs)),
			}),
			ExprKind::Break(expr) => {
				if !self.in_loop {
					self.diagnostics
						.push(span.error("`break` is only allowed inside loops").label(span.mark()));
				}

				hir::ExprKind::Break(expr.map(|expr| Box::new(self.resolve_expr(*expr))))
			},
			ExprKind::Continue => {
				if !self.in_loop {
					self.diagnostics
						.push(span.error("`continue` is only allowed inside loops").label(span.mark()));
				}

				hir::ExprKind::Continue
			},
			ExprKind::Return(expr) => {
				if !self.in_fn {
					self.diagnostics
						.push(span.error("`return` not allowed here").label(span.mark()));
				}

				hir::ExprKind::Return(expr.map(|expr| Box::new(self.resolve_expr(*expr))))
			},
			ExprKind::If(if_) => {
				let else_ = if_.else_.map(|x| *x);

				hir::ExprKind::If(If {
					cond: Box::new(self.resolve_expr(*if_.cond)),
					then: self.resolve_block(if_.then),
					else_: match else_ {
						Some(Expr {
							node: ExprKind::Block(block),
							..
						}) => Some(self.resolve_block(block)),
						Some(Expr { span, .. }) => {
							self.diagnostics.push(
								span.error("else must have a block")
									.label(span.label("surround this `{<expr>}`")),
							);
							None
						},
						None => None,
					},
				})
			},
			ExprKind::Loop(loop_) => hir::ExprKind::Loop(Loop {
				block: {
					self.in_loop = true;
					let ret = self.resolve_block(loop_.block);
					self.in_loop = false;
					ret
				},
				while_: loop_.while_.map(|cond| Box::new(self.resolve_expr(*cond))),
			}),
			ExprKind::While(while_) => hir::ExprKind::While(While {
				cond: Box::new(self.resolve_expr(*while_.cond)),
				block: {
					self.in_loop = true;
					let ret = self.resolve_block(while_.block);
					self.in_loop = false;
					ret
				},
			}),
			ExprKind::For(for_) => hir::ExprKind::For(For {
				pat: self.resolve_pat(for_.pat),
				iter: Box::new(self.resolve_expr(*for_.iter)),
				block: {
					self.in_loop = true;
					let ret = self.resolve_block(for_.block);
					self.in_loop = false;
					ret
				},
			}),
			ExprKind::Err => hir::ExprKind::Err,
			ExprKind::Tuple(x) => hir::ExprKind::Tuple(x.into_iter().map(|x| self.resolve_expr(x)).collect()),
			ExprKind::Infer => hir::ExprKind::Infer,
		}
	}

	fn resolve_pat(&mut self, pat: Pat) -> hir::Pat {
		match pat.node {
			PatKind::Binding(binding) => {
				let local = self.local.declare(binding.binding);

				hir::Pat {
					node: hir::PatKind::Binding(Binding {
						mutability: binding.mutability,
						ty: Type::Unknown,
						binding: local,
					}),
					span: pat.span,
				}
			},
		}
	}

	fn resolve_block(&mut self, block: Block) -> hir::Block {
		self.local.push_scope();

		let ret = hir::Block {
			is_const: block.is_const,
			stmts: block.stmts.into_iter().map(|stmt| self.resolve_stmt(stmt)).collect(),
			ty: Type::Unknown,
			span: block.span,
		};

		self.local.pop_scope();

		ret
	}

	fn resolve_stmt(&mut self, stmt: Stmt) -> hir::Stmt {
		match stmt.node {
			StmtKind::Item(_) => {
				self.diagnostics.push(
					stmt.span
						.error("items are not supported in this position")
						.label(stmt.span.mark()),
				);
				hir::Stmt {
					node: hir::StmtKind::Err,
					span: stmt.span,
				}
			},
			StmtKind::Expr(expr) => hir::Stmt {
				node: hir::StmtKind::Expr(ExprData {
					kind: self.resolve_expr_kind(expr, stmt.span),
					ty: Type::Unknown,
				}),
				span: stmt.span,
			},
			StmtKind::Semi(expr) => hir::Stmt {
				node: hir::StmtKind::Semi(ExprData {
					kind: self.resolve_expr_kind(expr, stmt.span),
					ty: Type::Unknown,
				}),
				span: stmt.span,
			},
			StmtKind::Err => hir::Stmt {
				node: hir::StmtKind::Err,
				span: stmt.span,
			},
		}
	}
}
