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

use crate::resolved::{
	Access,
	Arg,
	Array,
	Binary,
	Binding,
	Call,
	Cast,
	Ctx,
	Field,
	For,
	GlobalLet,
	If,
	InbuiltType,
	Index,
	Let,
	Lit,
	LocalRef,
	Loop,
	Ptr,
	Ty,
	TyExpr,
	TyExprKind,
	TyRef,
	Unary,
	Val,
	ValExpr,
	ValExprKind,
	ValRef,
	While,
};

pub mod resolved;

enum Ref {
	Ty(TyRef),
	Val(ValRef),
}

pub fn resolve(module: Module, rodeo: &mut Rodeo, diagnostics: &mut Vec<Report<Span>>) -> Ctx {
	let mut items = HashMap::new();
	let mut types = Vec::new();
	let mut globals = Vec::new();

	for item in module.items {
		let curr_span = item.span;
		let existing = match &item.kind {
			ItemKind::Fn(ident, _) => {
				let val = ValRef(globals.len() as _);
				let ident = ident.node;
				globals.push(item);
				items.insert(ident, (curr_span, Ref::Val(val)))
			},
			ItemKind::Const(l) | ItemKind::Static(l) => match l.pat.node {
				PatKind::Binding(binding) => {
					let val = ValRef(globals.len() as _);
					globals.push(item);
					items.insert(binding.binding, (curr_span, Ref::Val(val)))
				},
			},
			ItemKind::Struct(ident, _) => {
				let ty = TyRef(types.len() as _);
				let ident = ident.node;
				types.push(item);
				items.insert(ident, (curr_span, Ref::Ty(ty)))
			},
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
		items: &mut items,
		diagnostics,
		scopes: Vec::new(),
	};

	let ty = types;
	let mut inbuilt_types = HashMap::new();
	let mut types = resolver.init_inbuilt_types(ty.len() as _, &mut inbuilt_types);
	types.extend(ty.into_iter().enumerate().map(|(i, x)| match x.kind {
		ItemKind::Struct(ident, s) => (TyRef(i as _), Ty::Struct(resolver.resolve_struct(ident, s, x.span))),
		_ => unreachable!("found value in type item list"),
	}));

	let globals = globals
		.into_iter()
		.enumerate()
		.map(|(i, x)| match x.kind {
			ItemKind::Fn(ident, f) => (ValRef(i as _), Val::Fn(resolver.resolve_fn(ident, f, x.span))),
			ItemKind::Const(l) => (ValRef(i as _), Val::Const(resolver.resolve_global_let(l, x.span))),
			ItemKind::Static(l) => (ValRef(i as _), Val::Static(resolver.resolve_global_let(l, x.span))),
			_ => unreachable!("found type in global item list"),
		})
		.collect();

	Ctx {
		types,
		globals,
		inbuilt_types,
	}
}

struct Resolver<'a> {
	rodeo: &'a mut Rodeo,
	items: &'a mut HashMap<Spur, (Span, Ref)>,
	diagnostics: &'a mut Vec<Report<Span>>,
	scopes: Vec<(HashMap<Spur, LocalRef>, u32)>,
}

impl Resolver<'_> {
	fn init_inbuilt_types(&mut self, start: u32, reverse_map: &mut HashMap<InbuiltType, TyRef>) -> HashMap<TyRef, Ty> {
		let span = Span {
			start: 0,
			end: 0,
			file: self.rodeo.get_or_intern("inbuilt"),
		};

		(start..)
			.zip([
				("bool", InbuiltType::Bool),
				("f32", InbuiltType::Float(32)),
				("f64", InbuiltType::Float(64)),
				("i8", InbuiltType::Int(8)),
				("i16", InbuiltType::Int(16)),
				("i32", InbuiltType::Int(32)),
				("i64", InbuiltType::Int(64)),
				("isize", InbuiltType::Int(0)),
				("u8", InbuiltType::Uint(8)),
				("u16", InbuiltType::Uint(16)),
				("u32", InbuiltType::Uint(32)),
				("u64", InbuiltType::Uint(64)),
				("usize", InbuiltType::Uint(0)),
			])
			.map(|(i, (name, ty))| {
				self.items
					.insert(self.rodeo.get_or_intern(name), (span, Ref::Ty(TyRef(i))));
				reverse_map.insert(ty, TyRef(i));

				(TyRef(i), Ty::Inbuilt(ty))
			})
			.collect()
	}

	fn push_block(&mut self) {
		let start = self
			.scopes
			.last()
			.map(|(map, start)| start + map.len() as u32)
			.unwrap_or(0);

		self.scopes.push((HashMap::new(), start));
	}

	fn pop_block(&mut self) { self.scopes.pop(); }

	fn resolve_struct(&mut self, ident: Ident, s: Struct, span: Span) -> resolved::Struct {
		resolved::Struct {
			path: vec![ident],
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
						ty: self.resolve_ty_expr(x.ty),
						span: x.span,
					}
				})
				.collect(),
			span,
		}
	}

	fn resolve_fn(&mut self, ident: Ident, f: Fn, span: Span) -> resolved::Fn {
		self.push_block();

		let ret = resolved::Fn {
			path: vec![ident],
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
						ty: self.resolve_ty_expr(x.ty),
						span: x.span,
					}
				})
				.collect(),
			ret: f.ret.map(|expr| Box::new(self.resolve_ty_expr(*expr))),
			block: self.resolve_block(f.block),
			span,
		};

		self.pop_block();

		ret
	}

	fn resolve_global_let(&mut self, l: parse::ast::Let, span: Span) -> GlobalLet {
		GlobalLet {
			ty: l.ty.map(|expr| self.resolve_ty_expr(*expr)),
			expr: if let Some(expr) = l.expr {
				self.resolve_val_expr(*expr)
			} else {
				self.diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("globals must have initializers")
						.with_label(Label::new(span))
						.finish(),
				);

				ValExpr {
					node: ValExprKind::Err,
					span,
				}
			},
			span,
		}
	}

	fn resolve_ty_expr(&mut self, expr: Expr) -> TyExpr {
		let resolved = self.resolve_expr(expr);
		match resolved.node {
			resolved::ExprKind::Ty(ty) => TyExpr {
				node: ty,
				span: resolved.span,
			},
			resolved::ExprKind::Val(_) => {
				self.diagnostics.push(
					resolved
						.span
						.report(ReportKind::Error)
						.with_message("expected type")
						.with_label(Label::new(resolved.span).with_message("found value"))
						.finish(),
				);
				TyExpr {
					node: TyExprKind::Err,
					span: resolved.span,
				}
			},
			resolved::ExprKind::Err => TyExpr {
				node: TyExprKind::Err,
				span: resolved.span,
			},
		}
	}

	fn resolve_val_expr(&mut self, expr: Expr) -> ValExpr {
		ValExpr {
			node: self.resolve_val_expr_kind(expr.node, expr.span),
			span: expr.span,
		}
	}

	fn resolve_val_expr_kind(&mut self, expr: ExprKind, span: Span) -> ValExprKind {
		match self.resolve_expr_kind(expr, span) {
			resolved::ExprKind::Val(v) => v,
			resolved::ExprKind::Ty(_) => {
				self.diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("expected value")
						.with_label(Label::new(span).with_message("found type"))
						.finish(),
				);
				ValExprKind::Err
			},
			resolved::ExprKind::Err => ValExprKind::Err,
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
			ExprKind::Lit(lit) => resolved::ExprKind::Val(ValExprKind::Lit(match lit.kind {
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
			})),
			ExprKind::Block(block) => resolved::ExprKind::Val(ValExprKind::Block(self.resolve_block(block))),
			ExprKind::Ident(x) => {
				for (scope, _) in self.scopes.iter().rev() {
					if let Some(r) = scope.get(&x) {
						return resolved::ExprKind::Val(ValExprKind::LocalRef(*r));
					}
				}

				match self.items.get(&x) {
					Some((_, Ref::Ty(ty))) => resolved::ExprKind::Ty(TyExprKind::TyRef(*ty)),
					Some((_, Ref::Val(val))) => resolved::ExprKind::Val(ValExprKind::ValRef(*val)),
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
			ExprKind::Let(l) => resolved::ExprKind::Val(ValExprKind::Let(Let {
				pat: self.resolve_pat(l.pat),
				ty: l.ty.map(|expr| Box::new(self.resolve_ty_expr(*expr))),
				expr: l.expr.map(|expr| Box::new(self.resolve_val_expr(*expr))),
				span,
			})),
			ExprKind::List(list) => resolved::ExprKind::Val(ValExprKind::List(
				list.into_iter().map(|x| self.resolve_val_expr(x)).collect(),
			)),
			ExprKind::Array(array) => resolved::ExprKind::Val(ValExprKind::Array(Array {
				expr: Box::new(self.resolve_val_expr(*array.expr)),
				count: Box::new(self.resolve_val_expr(*array.count)),
			})),
			ExprKind::Cast(cast) => resolved::ExprKind::Val(ValExprKind::Cast(Cast {
				expr: Box::new(self.resolve_val_expr(*cast.expr)),
				ty: Box::new(self.resolve_ty_expr(*cast.ty)),
			})),
			ExprKind::Type => resolved::ExprKind::Ty(TyExprKind::Type),
			ExprKind::TypeOf(e) => resolved::ExprKind::Ty(TyExprKind::TypeOf(Box::new(self.resolve_expr(*e)))),
			ExprKind::Ptr(ptr) => resolved::ExprKind::Ty(TyExprKind::Ptr(Ptr {
				mutability: ptr.mutability,
				to: Box::new(self.resolve_ty_expr(*ptr.to)),
			})),
			ExprKind::Fn(_) => {
				self.diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("closures are not support yet")
						.with_label(Label::new(span))
						.finish(),
				);
				resolved::ExprKind::Err
			},
			ExprKind::MacroRef(spur) => resolved::ExprKind::Val(ValExprKind::MacroRef(spur)),
			ExprKind::Call(call) => resolved::ExprKind::Val(ValExprKind::Call(Call {
				target: Box::new(self.resolve_val_expr(*call.target)),
				args: call.args.into_iter().map(|x| self.resolve_expr(x)).collect(),
			})),
			ExprKind::Index(index) => resolved::ExprKind::Val(ValExprKind::Index(Index {
				target: Box::new(self.resolve_val_expr(*index.target)),
				index: Box::new(self.resolve_val_expr(*index.index)),
			})),
			ExprKind::Access(access) => resolved::ExprKind::Val(ValExprKind::Access(Access {
				target: Box::new(self.resolve_expr(*access.target)),
				field: access.field,
			})),
			ExprKind::Unary(unary) => resolved::ExprKind::Val(ValExprKind::Unary(Unary {
				op: unary.op,
				expr: Box::new(self.resolve_val_expr(*unary.expr)),
			})),
			ExprKind::Binary(binary) => resolved::ExprKind::Val(ValExprKind::Binary(Binary {
				op: binary.op,
				lhs: Box::new(self.resolve_val_expr(*binary.lhs)),
				rhs: Box::new(self.resolve_val_expr(*binary.rhs)),
			})),
			ExprKind::Break(expr) => {
				resolved::ExprKind::Val(ValExprKind::Break(expr.map(|expr| Box::new(self.resolve_expr(*expr)))))
			},
			ExprKind::Continue(expr) => resolved::ExprKind::Val(ValExprKind::Continue(
				expr.map(|expr| Box::new(self.resolve_expr(*expr))),
			)),
			ExprKind::Return(expr) => {
				resolved::ExprKind::Val(ValExprKind::Return(expr.map(|expr| Box::new(self.resolve_expr(*expr)))))
			},
			ExprKind::If(if_) => resolved::ExprKind::Val(ValExprKind::If(If {
				cond: Box::new(self.resolve_val_expr(*if_.cond)),
				then: self.resolve_block(if_.then),
				else_: if_.else_.map(|else_| Box::new(self.resolve_expr(*else_))),
			})),
			ExprKind::Loop(loop_) => resolved::ExprKind::Val(ValExprKind::Loop(Loop {
				block: self.resolve_block(loop_.block),
				while_: loop_.while_.map(|cond| Box::new(self.resolve_val_expr(*cond))),
			})),
			ExprKind::While(while_) => resolved::ExprKind::Val(ValExprKind::While(While {
				cond: Box::new(self.resolve_val_expr(*while_.cond)),
				block: self.resolve_block(while_.block),
			})),
			ExprKind::For(for_) => resolved::ExprKind::Val(ValExprKind::For(For {
				pat: self.resolve_pat(for_.pat),
				iter: Box::new(self.resolve_val_expr(*for_.iter)),
				block: self.resolve_block(for_.block),
			})),
			ExprKind::Err => resolved::ExprKind::Err,
		}
	}

	fn resolve_pat(&mut self, pat: Pat) -> resolved::Pat {
		let scope = self.scopes.last_mut().expect("pattern without scope");
		let r = scope.0.len() as u32 + scope.1;

		match pat.node {
			PatKind::Binding(binding) => {
				scope.0.insert(binding.binding, LocalRef(r));

				resolved::Pat {
					node: resolved::PatKind::Binding(Binding {
						mutability: binding.mutability,
						binding: LocalRef(r),
					}),
					span: pat.span,
				}
			},
		}
	}

	fn resolve_block(&mut self, block: Block) -> resolved::Block {
		self.push_block();

		let ret = resolved::Block {
			is_const: block.is_const,
			stmts: block.stmts.into_iter().map(|stmt| self.resolve_stmt(stmt)).collect(),
			span: block.span,
		};

		self.pop_block();

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
				node: resolved::StmtKind::Expr(self.resolve_val_expr_kind(expr, stmt.span)),
				span: stmt.span,
			},
			StmtKind::Semi(expr) => resolved::Stmt {
				node: resolved::StmtKind::Semi(self.resolve_val_expr_kind(expr, stmt.span)),
				span: stmt.span,
			},
			StmtKind::Err => resolved::Stmt {
				node: resolved::StmtKind::Err,
				span: stmt.span,
			},
		}
	}
}
