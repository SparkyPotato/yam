use diag::{Diagnostics, Span};
use hir::{
	ctx::{Hir, LocalRef, ValRef},
	hir::*,
	lang_item::LangItem,
	types::{Type, TypeId},
};
use id::{DenseMapBuilder, DenseMut};

use crate::check::{Constraint, InferenceEngine, TypeInfo};

mod check;

pub fn type_check(hir: &mut Hir, diags: &mut Diagnostics) {
	let globals = std::mem::take(&mut hir.globals).make_mut();

	let mut checker = TypeChecker {
		engine: InferenceEngine::new(&hir.rodeo, &globals, &hir.lang_items, &hir.val_to_lang_item, diags),
		locals: DenseMapBuilder::new(),
		globals: &globals,
		ret_ty: None,
		loop_ty: None,
	};

	for (val, mut def) in globals.iter_mut() {
		if hir.val_to_lang_item.get(val).is_some() {
			continue;
		}

		match &mut def.kind {
			ValDefKind::Static(_) | ValDefKind::Const(_) => {
				unreachable!("static and const globals are not supported yet");
			},
			ValDefKind::Fn(f) => checker.sig_init_decl_types(&mut f.sig),
			ValDefKind::FnDecl(sig) => checker.sig_init_decl_types(sig),
			ValDefKind::Struct(s) => checker.struct_init_decl_types(s),
		}
	}

	for (val, mut def) in globals.iter_mut() {
		if hir.val_to_lang_item.get(val).is_some() {
			continue;
		}

		match &mut def.kind {
			ValDefKind::Static(_) | ValDefKind::Const(_) => unreachable!(),
			ValDefKind::Fn(f) => checker.check_fn(f),
			ValDefKind::FnDecl(_) => {},
			ValDefKind::Struct(_) => {},
		}
	}

	hir.globals = globals.make_imm();
}

struct TypeChecker<'a> {
	engine: InferenceEngine<'a>,
	locals: DenseMapBuilder<LocalRef, TypeId>,
	globals: &'a DenseMut<ValRef, ValDef>,
	ret_ty: Option<(Type, Span)>,
	loop_ty: Option<TypeId>,
}

impl TypeChecker<'_> {
	fn sig_init_decl_types(&mut self, sig: &mut FnSignature) {
		for arg in sig.args.iter_mut() {
			self.evaluate_expr_as_ty(&mut arg.ty.node, arg.ty.span);

			if matches!(arg.ty.node.ty, Type::Unknown) {
				self.engine.diags.push(
					arg.ty
						.span
						.error("function arguments cannot be inferred")
						.label(arg.ty.span.label("change this to a concrete type")),
				);
			}
		}

		let ret = sig
			.ret_expr
			.as_mut()
			.map(|x| {
				self.evaluate_expr_as_ty(&mut x.node, x.span);
				if matches!(x.node.ty, Type::Unknown) {
					self.engine.diags.push(
						x.span
							.error("function return values cannot be inferred")
							.label(x.span.label("change this to a concrete type")),
					);
				}
				x.node.ty.clone()
			})
			.unwrap_or(Type::Void);
		sig.ret = ret;

		self.reset();
	}

	fn struct_init_decl_types(&mut self, s: &mut Struct) {
		for field in s.fields.iter_mut() {
			self.evaluate_expr_as_ty(&mut field.ty.node, field.ty.span);
			if matches!(field.ty.node.ty, Type::Unknown) {
				self.engine.diags.push(
					field
						.ty
						.span
						.error("struct fields cannot be inferred")
						.label(field.ty.span.label("change this to a concrete type")),
				);
			}
		}

		self.reset();
	}

	fn check_fn(&mut self, f: &mut Fn) {
		for arg in f.sig.args.iter() {
			match &arg.pat.node {
				PatKind::Binding(binding) => {
					let id = self.ty_to_id(&arg.ty.node.ty, arg.ty.span);

					self.locals.insert_at(binding.binding, id);
				},
			}
		}

		let ret_span = f.sig.ret_expr.as_ref().map(|x| x.span).unwrap_or(f.block.span);
		self.ret_ty = Some((f.sig.ret.clone(), ret_span));

		self.infer_block(&mut f.block);
		let id = self.ty_to_id(&f.block.ty, f.block.span);
		let ret_id = self.ty_to_id(&f.sig.ret, ret_span);
		self.engine.constraint(Constraint::Eq(id, ret_id));

		self.engine.solve();
		self.reconstruct_block(&mut f.block);

		self.ret_ty = None;

		self.reset();
	}

	fn infer_block(&mut self, block: &mut Block) {
		let last_stmt = block.stmts.len().wrapping_sub(1);

		let mut last_stmt_ty = None;
		for (i, stmt) in block.stmts.iter_mut().enumerate() {
			if matches!(
				&stmt.node,
				StmtKind::Expr(ExprData {
					kind,
					..
				}) if !matches!(kind, ExprKind::Block(_) | ExprKind::While(_) | ExprKind::For(_) | ExprKind::If(_))
			) && i != last_stmt
			{
				self.engine.diags.push(
					stmt.span
						.error("expected `;` after statement")
						.label(stmt.span.label("add a `;` after this statement")),
				);
			}

			match &mut stmt.node {
				StmtKind::Expr(expr) => {
					self.infer_expr(expr, stmt.span);
					if i == last_stmt {
						last_stmt_ty = Some(expr.ty.clone());
					}
				},
				StmtKind::Semi(expr) => self.infer_expr(expr, stmt.span),
				StmtKind::Err => {},
			}
		}

		block.ty = last_stmt_ty.unwrap_or(Type::Void);
	}

	fn reconstruct_block(&mut self, block: &mut Block) {
		for stmt in block.stmts.iter_mut() {
			match &mut stmt.node {
				StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
					self.reconstruct_expr(expr);
				},
				StmtKind::Err => {},
			}
		}

		match &mut block.ty {
			Type::Unresolved(id) => {
				block.ty = self.engine.reconstruct(*id);
			},
			_ => {},
		}
	}

	fn infer_expr(&mut self, expr: &mut ExprData, span: Span) {
		let info = match &mut expr.kind {
			ExprKind::ValRef(val) => {
				if let Some(def) = self.globals.try_get(*val) {
					return expr.ty = match &def.kind {
						ValDefKind::Static(_) | ValDefKind::Const(_) => {
							unreachable!("statics and consts are not supported")
						},
						ValDefKind::Fn(Fn { sig, .. }) | ValDefKind::FnDecl(sig) => Type::Fn {
							args: sig.args.iter().map(|x| x.ty.node.ty.clone()).collect(),
							ret: Box::new(sig.ret.clone()),
						},
						ValDefKind::Struct(s) => Type::Type,
					};
				} else {
					self.engine
						.diags
						.push(span.error("recursion doesn't work yet").label(span.mark()));
					TypeInfo::Unknown
				}
			},
			ExprKind::LocalRef(l) => TypeInfo::EqTo(self.locals[*l]),
			ExprKind::Never => TypeInfo::Type,
			ExprKind::Type => TypeInfo::Type,
			ExprKind::TypeOf(ex) => {
				self.infer_expr(&mut ex.node, ex.span);
				return expr.ty = ex.node.ty.clone();
			},
			ExprKind::Ptr(_) => TypeInfo::Type,
			ExprKind::Tuple(tuple) => {
				TypeInfo::Tuple(tuple.iter().map(|x| self.ty_to_id(&x.node.ty, x.span)).collect())
			},
			ExprKind::Lit(lit) => match lit {
				Lit::Int(_) => TypeInfo::IntLit,
				Lit::Float(_) => TypeInfo::FloatLit,
				Lit::Bool(_) => return expr.ty = Type::Ty(self.engine.lang_items[LangItem::Bool]),
				Lit::String(_) => {
					return expr.ty = Type::Ptr {
						mutable: false,
						to: Box::new(Type::Ty(self.engine.lang_items[LangItem::U8])),
					}
				},
				Lit::Char(_) => return expr.ty = Type::Ty(self.engine.lang_items[LangItem::U32]),
			},
			ExprKind::Block(block) => {
				return {
					self.infer_block(block);
					expr.ty = block.ty.clone();
				}
			},
			ExprKind::Let(l) => {
				let ty = l.ty_expr.as_mut().map(|x| {
					self.evaluate_expr_as_ty(&mut x.node, x.span);
					(x.node.ty.clone(), x.span)
				});

				let init_ty = l.expr.as_mut().map(|x| {
					self.infer_expr(&mut x.node, x.span);
					(x.node.ty.clone(), x.span)
				});

				let r = match &l.pat.node {
					PatKind::Binding(binding) => binding.binding,
				};

				let id = match (ty, init_ty) {
					(Some((ty, ty_span)), Some((init_ty, init_span))) => {
						let ty_id = self.ty_to_id(&ty, ty_span);
						let init_id = self.ty_to_id(&init_ty, init_span);
						self.engine.constraint(Constraint::Eq(ty_id, init_id));
						l.ty = ty;

						ty_id
					},
					(Some((ty, span)), None) => {
						let ty_id = self.ty_to_id(&ty, span);
						l.ty = ty;
						ty_id
					},
					(None, Some((init_ty, init_span))) => {
						let init_id = self.ty_to_id(&init_ty, init_span);
						l.ty = Type::Unresolved(init_id);
						init_id
					},
					(None, None) => self.engine.insert(TypeInfo::Unknown, span),
				};

				self.locals.insert_at(r, id);

				return expr.ty = Type::Void;
			},
			ExprKind::List(_) => unreachable!("arrays not implemented"),
			ExprKind::Array(_) => unreachable!("arrays not implemented"),
			ExprKind::Cast(c) => {
				self.infer_expr(&mut c.expr.node, c.expr.span);
				self.evaluate_expr_as_ty(&mut c.ty.node, c.ty.span);

				return expr.ty = c.ty.node.ty.clone();
			},
			ExprKind::Fn(_) => unreachable!("closures not implemented"),
			ExprKind::Call(c) => {
				self.infer_expr(&mut c.target.node, c.target.span);
				let fn_id = self.ty_to_id(&c.target.node.ty, c.target.span);

				let args = c
					.args
					.iter_mut()
					.map(|x| {
						self.infer_expr(&mut x.node, x.span);
						self.ty_to_id(&x.node.ty, x.span)
					})
					.collect();

				let ret_id = self.engine.insert(TypeInfo::Unknown, span);

				self.engine.constraint(Constraint::FnCall { fn_id, args, ret_id });

				return expr.ty = Type::Unresolved(ret_id);
			},
			ExprKind::Index(_) => unreachable!("indexing not implemented"),
			ExprKind::Access(access) => {
				self.infer_expr(&mut access.target.node, access.target.span);
				let struct_id = self.ty_to_id(&access.target.node.ty, access.target.span);

				let id = self.engine.insert(TypeInfo::Unknown, span);

				self.engine.constraint(Constraint::Field {
					id,
					struct_id,
					field: access.field,
				});

				return expr.ty = Type::Unresolved(id);
			},
			ExprKind::Unary(unary) => {
				self.infer_expr(&mut unary.expr.node, unary.expr.span);
				let ex = self.ty_to_id(&unary.expr.node.ty, unary.expr.span);

				let id = self.engine.insert(TypeInfo::Unknown, span);

				self.engine.constraint(Constraint::Unary {
					id,
					op: unary.op,
					expr: ex,
				});

				return expr.ty = Type::Unresolved(id);
			},
			ExprKind::Binary(binary) => {
				self.infer_expr(&mut binary.lhs.node, binary.lhs.span);
				self.infer_expr(&mut binary.rhs.node, binary.rhs.span);
				let lhs = self.ty_to_id(&binary.lhs.node.ty, binary.lhs.span);
				let rhs = self.ty_to_id(&binary.rhs.node.ty, binary.rhs.span);

				let id = self.engine.insert(TypeInfo::Unknown, span);

				self.engine.constraint(Constraint::Binary {
					id,
					op: binary.op,
					lhs,
					rhs,
				});

				return expr.ty = Type::Unresolved(id);
			},
			ExprKind::Break(b) => {
				let id = if let Some(expr) = b {
					self.infer_expr(&mut expr.node, expr.span);
					self.ty_to_id(&expr.node.ty, expr.span)
				} else {
					self.engine.insert(TypeInfo::Void, span)
				};

				self.engine.constraint(Constraint::Eq(id, self.loop_ty.unwrap()));

				return expr.ty = Type::Never;
			},
			ExprKind::Continue => return expr.ty = Type::Never,
			ExprKind::Return(r) => {
				let ty = r
					.as_mut()
					.map(|x| {
						self.infer_expr(&mut x.node, x.span);
						x.node.ty.clone()
					})
					.unwrap_or(Type::Void);

				let id = self.ty_to_id(&ty, span);
				let (ret, span) = self.ret_ty.clone().unwrap();
				let ret_id = self.ty_to_id(&ret, span);
				self.engine.constraint(Constraint::Eq(id, ret_id));
				return expr.ty = Type::Never;
			},
			ExprKind::If(i) => {
				self.infer_expr(&mut i.cond.node, i.cond.span);
				self.infer_block(&mut i.then);

				i.else_
					.as_mut()
					.map(|x| {
						self.infer_expr(&mut x.node, x.span);

						let then_id = self.ty_to_id(&i.then.ty, i.then.span);
						let else_id = self.ty_to_id(&x.node.ty, x.span);
						self.engine.constraint(Constraint::Eq(then_id, else_id));

						TypeInfo::EqTo(then_id)
					})
					.unwrap_or(TypeInfo::Void)
			},
			ExprKind::Loop(l) => {
				let info = if l.while_.is_some() {
					TypeInfo::Void
				} else {
					TypeInfo::Never
				};

				let id = self.engine.insert(info, span);

				self.loop_ty = Some(id);
				self.infer_block(&mut l.block);
				self.loop_ty = None;

				return expr.ty = Type::Unresolved(id);
			},
			ExprKind::While(w) => {
				self.infer_expr(&mut w.cond.node, w.cond.span);
				self.infer_block(&mut w.block);
				return expr.ty = Type::Void;
			},
			ExprKind::For(_) => unreachable!("for is not supported"),
			ExprKind::Infer => return expr.ty = Type::Err,
			ExprKind::Err => return expr.ty = Type::Err,
		};

		let id = self.engine.insert(info, span);
		expr.ty = Type::Unresolved(id);
	}

	fn reconstruct_expr(&mut self, expr: &mut ExprData) {
		self.reconstruct(&mut expr.ty);

		match &mut expr.kind {
			ExprKind::ValRef(_) => {},
			ExprKind::LocalRef(_) => {},
			ExprKind::Never => {},
			ExprKind::Type => {},
			ExprKind::TypeOf(ex) => {
				self.reconstruct_expr(&mut ex.node);
			},
			ExprKind::Ptr(_) => {},
			ExprKind::Tuple(tuple) => {
				for expr in tuple.iter_mut() {
					self.reconstruct_expr(&mut expr.node);
				}
			},
			ExprKind::Lit(_) => {},
			ExprKind::Block(b) => self.reconstruct_block(b),
			ExprKind::Let(l) => {
				l.expr.as_mut().map(|x| self.reconstruct_expr(&mut x.node));
				self.reconstruct(&mut l.ty);
			},
			ExprKind::List(_) => unreachable!(),
			ExprKind::Array(_) => unreachable!(),
			ExprKind::Cast(c) => {
				self.reconstruct_expr(&mut c.expr.node);
			},
			ExprKind::Fn(_) => unreachable!(),
			ExprKind::Call(c) => {
				self.reconstruct_expr(&mut c.target.node);
				for arg in c.args.iter_mut() {
					self.reconstruct_expr(&mut arg.node);
				}
			},
			ExprKind::Index(_) => unreachable!(),
			ExprKind::Access(a) => {
				self.reconstruct_expr(&mut a.target.node);
			},
			ExprKind::Unary(unary) => {
				self.reconstruct_expr(&mut unary.expr.node);
			},
			ExprKind::Binary(binary) => {
				self.reconstruct_expr(&mut binary.lhs.node);
				self.reconstruct_expr(&mut binary.rhs.node);
			},
			ExprKind::Break(b) => {
				b.as_mut().map(|x| self.reconstruct_expr(&mut x.node));
			},
			ExprKind::Continue => {},
			ExprKind::Return(r) => {
				r.as_mut().map(|x| self.reconstruct_expr(&mut x.node));
			},
			ExprKind::If(if_) => {
				self.reconstruct_expr(&mut if_.cond.node);

				if if_.cond.node.ty != Type::Ty(self.engine.lang_items[LangItem::Bool]) {
					self.engine
						.diags
						.push(if_.cond.span.error("type mismatch").label(if_.cond.span.label(format!(
							"expected `bool`, found `{}`",
							{
								let mut s = String::new();
								self.engine.fmt_ty(&if_.cond.node.ty, &mut s);
								s
							}
						))));
				}

				self.reconstruct_block(&mut if_.then);
				if_.else_.as_mut().map(|x| self.reconstruct_expr(&mut x.node));
			},
			ExprKind::Loop(l) => {
				self.reconstruct_block(&mut l.block);
				l.while_.as_mut().map(|x| {
					self.reconstruct_expr(&mut x.node);

					if x.node.ty != Type::Ty(self.engine.lang_items[LangItem::Bool]) {
						self.engine
							.diags
							.push(x.span.error("type mismatch").label(x.span.label(format!(
								"expected `bool`, found `{}`",
								{
									let mut s = String::new();
									self.engine.fmt_ty(&x.node.ty, &mut s);
									s
								}
							))));
					}
				});
			},
			ExprKind::While(w) => {
				self.reconstruct_expr(&mut w.cond.node);

				if w.cond.node.ty != Type::Ty(self.engine.lang_items[LangItem::Bool]) {
					self.engine
						.diags
						.push(w.cond.span.error("type mismatch").label(w.cond.span.label(format!(
							"expected `bool`, found `{}`",
							{
								let mut s = String::new();
								self.engine.fmt_ty(&w.cond.node.ty, &mut s);
								s
							}
						))));
				}

				self.reconstruct_block(&mut w.block);
			},
			ExprKind::For(_) => unreachable!(),
			ExprKind::Infer => {},
			ExprKind::Err => {},
		}
	}

	fn reconstruct(&mut self, ty: &mut Type) {
		match ty {
			Type::Unresolved(id) => {
				let id = *id;
				*ty = self.engine.reconstruct(id);
			},
			_ => {},
		}
	}

	fn evaluate_expr_as_ty(&mut self, expr: &mut ExprData, span: Span) {
		let ty = match &mut expr.kind {
			ExprKind::ValRef(val) => {
				if self.engine.inv_lang_items.get(*val).is_some() {
					Type::Ty(*val)
				} else if let Some(ty) = self.globals.try_get(*val) {
					match &ty.kind {
						ValDefKind::Struct(_) => Type::Ty(*val),
						ValDefKind::Static(_) | ValDefKind::Const(_) | ValDefKind::Fn(_) | ValDefKind::FnDecl(_) => {
							self.engine
								.diags
								.push(span.error("expected type, found value").label(span.mark()));
							Type::Err
						},
					}
				} else {
					self.engine.diags.push(span.error("cycle detected").label(span.mark()));
					Type::Err
				}
			},
			ExprKind::Never => Type::Never,
			ExprKind::Type => Type::Type,
			ExprKind::TypeOf(expr) => {
				self.infer_expr(&mut expr.node, expr.span);
				self.engine.solve();
				self.reconstruct_expr(&mut expr.node);
				self.reset();

				expr.node.ty.clone()
			},
			ExprKind::Ptr(ptr) => Type::Ptr {
				mutable: ptr.mutability,
				to: {
					self.evaluate_expr_as_ty(&mut ptr.to.node, ptr.to.span);
					Box::new(ptr.to.node.ty.clone())
				},
			},
			ExprKind::Tuple(tys) => {
				for ty in tys.iter_mut() {
					self.evaluate_expr_as_ty(&mut ty.node, ty.span);
				}

				Type::Tuple(tys.iter().map(|x| x.node.ty.clone()).collect())
			},
			ExprKind::Infer => Type::Unknown,
			_ => {
				self.engine
					.diags
					.push(span.error("expected type, found value").label(span.mark()));

				Type::Err
			},
		};

		expr.ty = ty;
	}

	fn ty_to_id(&mut self, ty: &Type, span: Span) -> TypeId {
		let info = match ty {
			Type::Void => TypeInfo::Void,
			Type::Never => TypeInfo::Never,
			Type::Type => TypeInfo::Type,
			Type::Ty(id) => TypeInfo::Ty(*id),
			Type::Ptr { mutable, to } => TypeInfo::Ptr {
				mutable: *mutable,
				to: self.ty_to_id(to, span),
			},
			Type::Tuple(tys) => TypeInfo::Tuple(tys.iter().map(|x| self.ty_to_id(x, span)).collect()),
			Type::Fn { args, ret } => TypeInfo::Fn {
				args: args.iter().map(|x| self.ty_to_id(x, span)).collect(),
				ret: self.ty_to_id(ret, span),
			},
			Type::Unresolved(id) => return *id,
			Type::Unknown | Type::Err => TypeInfo::Unknown,
		};

		self.engine.insert(info, span)
	}

	fn reset(&mut self) {
		self.locals.reset();
		self.engine.reset();
	}
}
