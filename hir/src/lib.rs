use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
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
	Rodeo,
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

fn register_module_items(module: &Module, prefix: Path, builder: &mut HirBuilder, diags: &mut Vec<Report<Span>>) {
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
				diags.push(
					item.span
						.report(ReportKind::Error)
						.with_message("imports are not supported")
						.with_label(Label::new(item.span))
						.finish(),
				);
				None
			},
		};
	}
}

pub fn resolve(module: Module, mut rodeo: Rodeo, diagnostics: &mut Vec<Report<Span>>) -> Hir {
	let mut builder = HirBuilder::new();

	register_module_items(&module, Path::default(), &mut builder, diagnostics);

	let mut resolver = Resolver {
		idents: LangItemIdents::new(&mut rodeo),
		rodeo: &rodeo,
		builder: &mut builder,
		diagnostics,
		local: LocalBuilder::new(),
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
							.report(ReportKind::Error)
							.with_message("multiple lang attributes on one item")
							.with_label(Label::new(span).with_message("first lang attribute"))
							.with_label(Label::new(attr_span).with_message("duplicate lang attribute"))
							.finish(),
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
	diagnostics: &'a mut Vec<Report<Span>>,
	local: LocalBuilder,
	idents: LangItemIdents,
}

impl Resolver<'_> {
	fn lang_item(&mut self, attr: Attr) -> Option<LangItem> {
		if attr.name.node != self.idents.lang {
			self.diagnostics.push(
				attr.name
					.span
					.report(ReportKind::Error)
					.with_message("unknown attribute")
					.with_label(Label::new(attr.name.span))
					.finish(),
			);
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
					self.diagnostics.push(
						span.report(ReportKind::Error)
							.with_message("unknown lang item")
							.with_label(Label::new(span))
							.finish(),
					);

					None
				}
			},
			_ => {
				self.diagnostics.push(
					attr.span
						.report(ReportKind::Error)
						.with_message("lang item attribute expects one ident")
						.with_label(Label::new(attr.name.span))
						.finish(),
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
						self.diagnostics.push(
							x.span
								.report(ReportKind::Error)
								.with_message("structs cannot have `const` fields")
								.with_label(Label::new(x.span))
								.finish(),
						);
					}

					Field {
						visibility: x.visibility,
						name: match x.pat.node {
							PatKind::Binding(binding) => {
								if binding.mutability {
									self.diagnostics.push(
										x.span
											.report(ReportKind::Error)
											.with_message("structs cannot have `mut` fields")
											.with_label(Label::new(x.pat.span))
											.finish(),
									);
								}

								Ident {
									node: binding.binding,
									span: x.pat.span,
								}
							},
						},
						ty: Type::Unknown,
						ty_expr: self.resolve_expr(x.ty),
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
							.report(ReportKind::Error)
							.with_message("functions cannot have `const` arguments")
							.with_label(Label::new(x.span))
							.finish(),
					);
				}

				if x.visibility == Visibility::Public {
					self.diagnostics.push(
						x.span
							.report(ReportKind::Error)
							.with_message("functions cannot have `pub` arguments")
							.with_label(Label::new(x.span))
							.finish(),
					);
				}

				Arg {
					is_const: x.is_const,
					pat: self.resolve_pat(x.pat),
					ty: Type::Unknown,
					ty_expr: self.resolve_expr(x.ty),
					span: x.span,
				}
			})
			.collect();
		let ret_expr = f.ret.map(|expr| Box::new(self.resolve_expr(*expr)));

		if matches!(f.abi, Abi::None) && f.block.is_none() {
			self.diagnostics.push(
				span.report(ReportKind::Error)
					.with_message("functions must have a body")
					.with_label(Label::new(span))
					.finish(),
			);
		}

		let sig = FnSignature {
			abi: f.abi,
			args,
			ret_expr,
			ret: Type::Unknown,
		};

		let ret = (sig, f.block.map(|block| self.resolve_block(block)));

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
				self.diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("globals must have initializers")
						.with_label(Label::new(span))
						.finish(),
				);

				hir::Expr {
					kind: hir::ExprKind::Err,
					ty: Type::Void,
					span,
				}
			},
		}
	}

	fn resolve_expr(&mut self, expr: Expr) -> hir::Expr {
		hir::Expr {
			kind: self.resolve_expr_kind(expr.node, expr.span),
			ty: Type::Unknown,
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
						self.diagnostics.push(
							span.report(ReportKind::Error)
								.with_message("undefined identifier")
								.with_label(Label::new(span))
								.finish(),
						);

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
				ty: Type::Unknown,
				ty_expr: Box::new(self.resolve_expr(*cast.ty)),
			}),
			ExprKind::Never => hir::ExprKind::Never,
			ExprKind::Type => hir::ExprKind::Type,
			ExprKind::TypeOf(e) => hir::ExprKind::TypeOf(Box::new(self.resolve_expr(*e))),
			ExprKind::Ptr(ptr) => hir::ExprKind::Ptr(Ptr {
				mutability: ptr.mutability,
				to: Box::new(self.resolve_expr(*ptr.to)),
			}),
			ExprKind::Fn(_) => {
				self.diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("closures are not support yet")
						.with_label(Label::new(span))
						.finish(),
				);
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
			ExprKind::Break(expr) => hir::ExprKind::Break(expr.map(|expr| Box::new(self.resolve_expr(*expr)))),
			ExprKind::Continue => hir::ExprKind::Continue,
			ExprKind::Return(expr) => hir::ExprKind::Return(expr.map(|expr| Box::new(self.resolve_expr(*expr)))),
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
								span.report(ReportKind::Error)
									.with_message("else must be a block")
									.with_label(Label::new(span).with_message("surround this with `{<expr>}`"))
									.finish(),
							);
							None
						},
						None => None,
					},
				})
			},
			ExprKind::Loop(loop_) => hir::ExprKind::Loop(Loop {
				block: self.resolve_block(loop_.block),
				while_: loop_.while_.map(|cond| Box::new(self.resolve_expr(*cond))),
			}),
			ExprKind::While(while_) => hir::ExprKind::While(While {
				cond: Box::new(self.resolve_expr(*while_.cond)),
				block: self.resolve_block(while_.block),
			}),
			ExprKind::For(for_) => hir::ExprKind::For(For {
				pat: self.resolve_pat(for_.pat),
				iter: Box::new(self.resolve_expr(*for_.iter)),
				block: self.resolve_block(for_.block),
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
						.report(ReportKind::Error)
						.with_message("items are not supported in this position")
						.with_label(Label::new(stmt.span))
						.finish(),
				);
				hir::Stmt {
					node: hir::StmtKind::Err,
					span: stmt.span,
				}
			},
			StmtKind::Expr(expr) => hir::Stmt {
				node: hir::StmtKind::Expr(self.resolve_expr_kind(expr, stmt.span)),
				span: stmt.span,
			},
			StmtKind::Semi(expr) => hir::Stmt {
				node: hir::StmtKind::Semi(self.resolve_expr_kind(expr, stmt.span)),
				span: stmt.span,
			},
			StmtKind::Err => hir::Stmt {
				node: hir::StmtKind::Err,
				span: stmt.span,
			},
		}
	}
}
