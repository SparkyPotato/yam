use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
pub use parse::Rodeo;
use parse::{
	ast::{
		Block,
		Expr,
		ExprKind,
		Fn,
		Ident,
		ItemKind,
		LitKind,
		Module,
		Pat,
		PatKind,
		Stmt,
		StmtKind,
		Struct,
		Visibility,
	},
	Spur,
};

use crate::{ctx::*, resolved::*};

pub mod ctx;
pub mod resolved;

// TODO: lang items

pub fn resolve(module: Module, rodeo: &Rodeo, diagnostics: &mut Vec<Report<Span>>) -> ResolveCtx {
	let mut builder = ResolveCtxBuilder::new();
	let mut items = HashMap::new();

	for item in &module.items {
		let curr_span = item.span;
		let existing = match &item.kind {
			ItemKind::Fn(ident, _) => items.insert(ident.node, (curr_span, builder.decl_val())),
			ItemKind::Const(l) | ItemKind::Static(l) => match l.pat.node {
				PatKind::Binding(binding) => items.insert(binding.binding, (curr_span, builder.decl_val())),
			},
			ItemKind::Struct(ident, _) => items.insert(ident.node, (curr_span, builder.decl_val())),
			ItemKind::Import(_) => {
				diagnostics.push(
					item.span
						.report(ReportKind::Error)
						.with_message("imports are not supported")
						.with_label(Label::new(item.span))
						.finish(),
				);
				None
			},
		};

		if let Some((span, _)) = existing {
			diagnostics.push(
				curr_span
					.report(ReportKind::Error)
					.with_message("duplicate definition")
					.with_label(Label::new(span).with_message("already defined here"))
					.with_label(Label::new(curr_span).with_message("redefined here"))
					.finish(),
			);
		}
	}

	let mut resolver = Resolver {
		rodeo,
		items: &items,
		diagnostics,
		local: LocalBuilder::new(),
	};

	for item in module.items {
		let span = item.span;

		match item.kind {
			ItemKind::Fn(ident, f) => {
				let (_, val) = items[&ident.node];
				builder.define_val(
					val,
					ValDef {
						path: vec![ident],
						kind: ValDefKind::Fn(resolver.resolve_fn(f)),
						span,
					},
				);
			},
			ItemKind::Const(l) => match l.pat.node {
				PatKind::Binding(b) => {
					let (_, val) = items[&b.binding];
					builder.define_val(
						val,
						ValDef {
							path: vec![Ident {
								node: b.binding,
								span: l.pat.span,
							}],
							kind: ValDefKind::Const(resolver.resolve_global_let(l, span)),
							span,
						},
					);
				},
			},
			ItemKind::Static(l) => match l.pat.node {
				PatKind::Binding(b) => {
					let (_, val) = items[&b.binding];
					builder.define_val(
						val,
						ValDef {
							path: vec![Ident {
								node: b.binding,
								span: l.pat.span,
							}],
							kind: ValDefKind::Static(resolver.resolve_global_let(l, span)),
							span,
						},
					);
				},
			},
			ItemKind::Struct(ident, s) => {
				let (_, val) = items[&ident.node];
				builder.define_val(
					val,
					ValDef {
						path: vec![ident],
						kind: ValDefKind::Struct(resolver.resolve_struct(s)),
						span,
					},
				);
			},
			ItemKind::Import(_) => {},
		}
	}

	builder.finish()
}

struct Resolver<'a> {
	rodeo: &'a Rodeo,
	items: &'a HashMap<Spur, (Span, ValRef)>,
	diagnostics: &'a mut Vec<Report<Span>>,
	local: LocalBuilder,
}

impl Resolver<'_> {
	fn resolve_struct(&mut self, s: Struct) -> resolved::Struct {
		resolved::Struct {
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
						ty: self.resolve_expr(x.ty),
						span: x.span,
					}
				})
				.collect(),
		}
	}

	fn resolve_fn(&mut self, f: Fn) -> resolved::Fn {
		self.local.push_scope();

		let ret = resolved::Fn {
			args: f
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
						ty: self.resolve_expr(x.ty),
						span: x.span,
					}
				})
				.collect(),
			ret: f.ret.map(|expr| Box::new(self.resolve_expr(*expr))),
			block: self.resolve_block(f.block),
		};

		self.local.pop_scope();
		self.local.reset();

		ret
	}

	fn resolve_global_let(&mut self, l: parse::ast::Let, span: Span) -> GlobalLet {
		GlobalLet {
			ty: l.ty.map(|expr| self.resolve_expr(*expr)),
			expr: if let Some(expr) = l.expr {
				self.resolve_expr(*expr)
			} else {
				self.diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("globals must have initializers")
						.with_label(Label::new(span))
						.finish(),
				);

				resolved::Expr {
					node: resolved::ExprKind::Err,
					span,
				}
			},
		}
	}

	fn resolve_expr(&mut self, expr: Expr) -> resolved::Expr {
		resolved::Expr {
			node: self.resolve_expr_kind(expr.node, expr.span),
			span: expr.span,
		}
	}

	fn resolve_expr_kind(&mut self, expr: ExprKind, span: Span) -> resolved::ExprKind {
		match expr {
			ExprKind::Lit(lit) => resolved::ExprKind::Lit(match lit.kind {
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
			ExprKind::Block(block) => resolved::ExprKind::Block(self.resolve_block(block)),
			ExprKind::Ident(x) => {
				if let Some(local) = self.local.resolve(x) {
					return resolved::ExprKind::LocalRef(local);
				}

				match self.items.get(&x) {
					Some((_, val)) => resolved::ExprKind::ValRef(*val),
					None => {
						self.diagnostics.push(
							span.report(ReportKind::Error)
								.with_message("undefined identifier")
								.with_label(Label::new(span))
								.finish(),
						);

						resolved::ExprKind::Err
					},
				}
			},
			ExprKind::Let(l) => resolved::ExprKind::Let(Let {
				pat: self.resolve_pat(l.pat),
				ty: l.ty.map(|expr| Box::new(self.resolve_expr(*expr))),
				expr: l.expr.map(|expr| Box::new(self.resolve_expr(*expr))),
				span,
			}),
			ExprKind::List(list) => resolved::ExprKind::List(list.into_iter().map(|x| self.resolve_expr(x)).collect()),
			ExprKind::Array(array) => resolved::ExprKind::Array(Array {
				expr: Box::new(self.resolve_expr(*array.expr)),
				count: Box::new(self.resolve_expr(*array.count)),
			}),
			ExprKind::Cast(cast) => resolved::ExprKind::Cast(Cast {
				expr: Box::new(self.resolve_expr(*cast.expr)),
				ty: Box::new(self.resolve_expr(*cast.ty)),
			}),
			ExprKind::Type => resolved::ExprKind::Type,
			ExprKind::TypeOf(e) => resolved::ExprKind::TypeOf(Box::new(self.resolve_expr(*e))),
			ExprKind::Ptr(ptr) => resolved::ExprKind::Ptr(Ptr {
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
				resolved::ExprKind::Err
			},
			ExprKind::Call(call) => resolved::ExprKind::Call(Call {
				target: Box::new(self.resolve_expr(*call.target)),
				args: call.args.into_iter().map(|x| self.resolve_expr(x)).collect(),
			}),
			ExprKind::Index(index) => resolved::ExprKind::Index(Index {
				target: Box::new(self.resolve_expr(*index.target)),
				index: Box::new(self.resolve_expr(*index.index)),
			}),
			ExprKind::Access(access) => resolved::ExprKind::Access(Access {
				target: Box::new(self.resolve_expr(*access.target)),
				field: access.field,
			}),
			ExprKind::Unary(unary) => resolved::ExprKind::Unary(Unary {
				op: unary.op,
				expr: Box::new(self.resolve_expr(*unary.expr)),
			}),
			ExprKind::Binary(binary) => resolved::ExprKind::Binary(Binary {
				op: binary.op,
				lhs: Box::new(self.resolve_expr(*binary.lhs)),
				rhs: Box::new(self.resolve_expr(*binary.rhs)),
			}),
			ExprKind::Break(expr) => resolved::ExprKind::Break(expr.map(|expr| Box::new(self.resolve_expr(*expr)))),
			ExprKind::Continue(expr) => {
				resolved::ExprKind::Continue(expr.map(|expr| Box::new(self.resolve_expr(*expr))))
			},
			ExprKind::Return(expr) => resolved::ExprKind::Return(expr.map(|expr| Box::new(self.resolve_expr(*expr)))),
			ExprKind::If(if_) => resolved::ExprKind::If(If {
				cond: Box::new(self.resolve_expr(*if_.cond)),
				then: self.resolve_block(if_.then),
				else_: if_.else_.map(|else_| Box::new(self.resolve_expr(*else_))),
			}),
			ExprKind::Loop(loop_) => resolved::ExprKind::Loop(Loop {
				block: self.resolve_block(loop_.block),
				while_: loop_.while_.map(|cond| Box::new(self.resolve_expr(*cond))),
			}),
			ExprKind::While(while_) => resolved::ExprKind::While(While {
				cond: Box::new(self.resolve_expr(*while_.cond)),
				block: self.resolve_block(while_.block),
			}),
			ExprKind::For(for_) => resolved::ExprKind::For(For {
				pat: self.resolve_pat(for_.pat),
				iter: Box::new(self.resolve_expr(*for_.iter)),
				block: self.resolve_block(for_.block),
			}),
			ExprKind::Err => resolved::ExprKind::Err,
			ExprKind::Tuple(x) => resolved::ExprKind::Tuple(x.into_iter().map(|x| self.resolve_expr(x)).collect()),
			ExprKind::Infer => resolved::ExprKind::Infer,
		}
	}

	fn resolve_pat(&mut self, pat: Pat) -> resolved::Pat {
		match pat.node {
			PatKind::Binding(binding) => {
				let local = self.local.declare(binding.binding);

				resolved::Pat {
					node: resolved::PatKind::Binding(Binding {
						mutability: binding.mutability,
						binding: local,
					}),
					span: pat.span,
				}
			},
		}
	}

	fn resolve_block(&mut self, block: Block) -> resolved::Block {
		self.local.push_scope();

		let ret = resolved::Block {
			is_const: block.is_const,
			stmts: block.stmts.into_iter().map(|stmt| self.resolve_stmt(stmt)).collect(),
			span: block.span,
		};

		self.local.pop_scope();

		ret
	}

	fn resolve_stmt(&mut self, stmt: Stmt) -> resolved::Stmt {
		match stmt.node {
			StmtKind::Item(_) => {
				self.diagnostics.push(
					stmt.span
						.report(ReportKind::Error)
						.with_message("items are not supported in this position")
						.with_label(Label::new(stmt.span))
						.finish(),
				);
				resolved::Stmt {
					node: resolved::StmtKind::Err,
					span: stmt.span,
				}
			},
			StmtKind::Expr(expr) => resolved::Stmt {
				node: resolved::StmtKind::Expr(self.resolve_expr_kind(expr, stmt.span)),
				span: stmt.span,
			},
			StmtKind::Semi(expr) => resolved::Stmt {
				node: resolved::StmtKind::Semi(self.resolve_expr_kind(expr, stmt.span)),
				span: stmt.span,
			},
			StmtKind::Err => resolved::Stmt {
				node: resolved::StmtKind::Err,
				span: stmt.span,
			},
		}
	}
}
