use arena::{Arena, Ix};
use diagnostics::{Label, Span};
use hir::{ast::ErasedAstId, ident::AbsPath, LangItem};
use rustc_hash::{FxHashMap, FxHashSet};
use verde::{Ctx, Id};

pub struct TypeSolver {
	partial: Arena<Partial>,
	constraints: Vec<Constraint>,
}

#[derive(Debug)]
pub struct Partial {
	ty: PartialType,
	span: Option<ErasedAstId>,
}

#[derive(Debug)]
pub enum PartialType {
	Concrete(Id<thir::Type>),
	Array {
		ty: Ix<Partial>,
		len: u64,
	},
	Fn {
		abi: Option<&'static str>,
		params: Vec<Ix<Partial>>,
		ret: Ix<Partial>,
	},
	Ptr {
		mutable: bool,
		ty: Ix<Partial>,
	},
	Infer,
}

#[derive(Debug)]
pub enum Constraint {
	Unify(Ix<Partial>, Ix<Partial>),
	InfixOp {
		lhs: Ix<Partial>,
		rhs: Ix<Partial>,
		op: hir::InfixOp,
		result: Ix<Partial>,
	},
	Call {
		callee: Ix<Partial>,
		args: Vec<Ix<Partial>>,
		result: Ix<Partial>,
	},
	Cast {
		expr: Ix<Partial>,
		ty: Ix<Partial>,
	},
	Field {
		expr: Ix<Partial>,
		field: hir::Name,
		result: Ix<Partial>,
	},
	Index {
		expr: Ix<Partial>,
		index: Ix<Partial>,
		result: Ix<Partial>,
	},
	Prefix {
		op: hir::PrefixOp,
		expr: Ix<Partial>,
		result: Ix<Partial>,
	},
	Float(Ix<Partial>),
	Int(Ix<Partial>),
}

impl TypeSolver {
	pub fn new() -> Self {
		Self {
			partial: Arena::new(),
			constraints: Vec::new(),
		}
	}

	pub fn unify(&mut self, a: Ix<Partial>, b: Ix<Partial>) { self.constraints.push(Constraint::Unify(a, b)); }

	pub fn infix_op(&mut self, lhs: Ix<Partial>, rhs: Ix<Partial>, op: hir::InfixOp, result: Ix<Partial>) {
		self.constraints.push(Constraint::InfixOp { lhs, rhs, op, result });
	}

	pub fn call(&mut self, callee: Ix<Partial>, args: Vec<Ix<Partial>>, result: Ix<Partial>) {
		self.constraints.push(Constraint::Call { callee, args, result });
	}

	pub fn cast(&mut self, expr: Ix<Partial>, ty: Ix<Partial>) { self.constraints.push(Constraint::Cast { expr, ty }); }

	pub fn field(&mut self, expr: Ix<Partial>, field: hir::Name, result: Ix<Partial>) {
		self.constraints.push(Constraint::Field { expr, field, result });
	}

	pub fn index(&mut self, expr: Ix<Partial>, index: Ix<Partial>, result: Ix<Partial>) {
		self.constraints.push(Constraint::Index { expr, index, result });
	}

	pub fn prefix(&mut self, op: hir::PrefixOp, expr: Ix<Partial>, result: Ix<Partial>) {
		self.constraints.push(Constraint::Prefix { op, expr, result });
	}

	pub fn float(&mut self, expr: Ix<Partial>) { self.constraints.push(Constraint::Float(expr)); }

	pub fn int(&mut self, expr: Ix<Partial>) { self.constraints.push(Constraint::Int(expr)); }

	pub fn concrete(&mut self, ty: Id<thir::Type>, span: Option<ErasedAstId>) -> Ix<Partial> {
		self.add(PartialType::Concrete(ty), span)
	}

	pub fn array(&mut self, ty: Ix<Partial>, len: u64, span: Option<ErasedAstId>) -> Ix<Partial> {
		self.add(PartialType::Array { ty, len }, span)
	}

	pub fn fn_(
		&mut self, abi: Option<&'static str>, params: Vec<Ix<Partial>>, ret: Ix<Partial>, span: Option<ErasedAstId>,
	) -> Ix<Partial> {
		self.add(PartialType::Fn { abi, params, ret }, span)
	}

	pub fn ptr(&mut self, mutable: bool, ty: Ix<Partial>, span: Option<ErasedAstId>) -> Ix<Partial> {
		self.add(PartialType::Ptr { mutable, ty }, span)
	}

	pub fn infer(&mut self, span: Option<ErasedAstId>) -> Ix<Partial> { self.add(PartialType::Infer, span) }

	pub fn add(&mut self, ty: PartialType, span: Option<ErasedAstId>) -> Ix<Partial> {
		self.partial.push(Partial { ty, span })
	}

	pub fn get(&self, ctx: &Ctx, i: Ix<Partial>) -> Id<thir::Type> {
		let partial = &self.partial[i];
		match partial.ty {
			PartialType::Concrete(ty) => ty,
			_ => ctx.add(thir::Type::Error),
		}
	}

	pub fn solve(
		&mut self, ctx: &Ctx, items: &FxHashMap<Id<AbsPath>, Id<hir::Item>>,
		decls: &FxHashMap<Id<AbsPath>, Id<thir::ItemDecl>>,
	) {
		let mut constraints = self.solve_loop(ctx, items, decls);
		constraints.retain(|c| !c.finalize(self, ctx));
		self.constraints = constraints;
		constraints = self.solve_loop(ctx, items, decls);

		let mut errored = FxHashSet::default();
		for i in 0..self.partial.len() {
			self.infer_error(ctx, Ix::new(i), None, &mut errored);
		}
		for c in constraints {
			c.error(self, ctx);
		}
	}

	fn infer_error(&self, ctx: &Ctx, ty: Ix<Partial>, span: Option<ErasedAstId>, errored: &mut FxHashSet<ErasedAstId>) {
		let p = &self.partial[ty];
		let span = p.span.or(span);
		match p.ty {
			PartialType::Concrete(_) => {},
			PartialType::Array { ty, .. } => self.infer_error(ctx, ty, span, errored),
			PartialType::Fn { ref params, ret, .. } => {
				for &p in params {
					self.infer_error(ctx, p, span, errored);
				}
				self.infer_error(ctx, ret, span, errored);
			},
			PartialType::Ptr { ty, .. } => self.infer_error(ctx, ty, span, errored),
			PartialType::Infer => {
				let Some(span) = span else { return };
				if errored.insert(span) {
					ctx.push(span.error("cannot infer type").label(span.mark()));
				}
			},
		}
	}

	fn concretize(&mut self, ctx: &Ctx) {
		for i in 0..self.partial.len() {
			let i = Ix::new(i);
			let partial = &self.partial[i];
			match partial.ty {
				PartialType::Concrete(_) => {},
				PartialType::Infer => {},
				PartialType::Array { ty, len } => {
					let PartialType::Concrete(ty) = self.partial[ty].ty else {
						continue;
					};
					let ty = ctx.add(thir::Type::Array(thir::ArrayType { ty, len }));
					self.partial[i].ty = PartialType::Concrete(ty);
				},
				PartialType::Fn { abi, ref params, ret } => {
					let p: Vec<_> = params
						.iter()
						.filter_map(|&p| match self.partial[p].ty {
							PartialType::Concrete(ty) => Some(ty),
							_ => None,
						})
						.collect();
					if p.len() != params.len() {
						continue;
					}
					let PartialType::Concrete(ret) = self.partial[ret].ty else {
						continue;
					};
					let ty = ctx.add(thir::Type::Fn(thir::FnType { abi, params: p, ret }));
					self.partial[i].ty = PartialType::Concrete(ty);
				},
				PartialType::Ptr { mutable, ty } => {
					let PartialType::Concrete(ty) = self.partial[ty].ty else {
						continue;
					};
					let ty = ctx.add(thir::Type::Ptr(thir::PtrType { mutable, ty }));
					self.partial[i].ty = PartialType::Concrete(ty);
				},
			}
		}
	}

	fn solve_loop(
		&mut self, ctx: &Ctx, items: &FxHashMap<Id<AbsPath>, Id<hir::Item>>,
		decls: &FxHashMap<Id<AbsPath>, Id<thir::ItemDecl>>,
	) -> Vec<Constraint> {
		let c = loop {
			self.concretize(ctx);

			let mut constraints = std::mem::take(&mut self.constraints);
			let initial_len = constraints.len();
			constraints.retain(|c| !c.solve(self, ctx, items, decls));

			if constraints.len() == initial_len {
				break constraints;
			}

			self.constraints.extend(constraints);
		};
		self.concretize(ctx);
		c
	}

	fn fmt_type(&self, ctx: &Ctx, ty: Ix<Partial>) -> String {
		match self.partial[ty].ty {
			PartialType::Infer => "_".to_string(),
			PartialType::Concrete(ty) => fmt_type(ctx, ty),
			PartialType::Array { ty, len } => format!("[{}; {}]", self.fmt_type(ctx, ty), len),
			PartialType::Fn { abi, ref params, ret } => {
				let abi = abi.map(|x| format!("extern \"{}\" ", x)).unwrap_or(String::new());
				let params = params
					.iter()
					.map(|&p| self.fmt_type(ctx, p))
					.collect::<Vec<_>>()
					.join(", ");
				let ret = self.fmt_type(ctx, ret);
				format!("{} fn({}) -> {}", abi, params, ret)
			},
			PartialType::Ptr { mutable, ty } => {
				if mutable {
					format!("*mut {}", self.fmt_type(ctx, ty))
				} else {
					format!("*{}", self.fmt_type(ctx, ty))
				}
			},
		}
	}

	fn ty_label(&self, ctx: &Ctx, ty: Ix<Partial>) -> Label<ErasedAstId> {
		let s = self.partial[ty].span.unwrap();
		s.label(format!("this is of type `{}`", self.fmt_type(ctx, ty)))
	}

	fn is_infer(&self, ty: Ix<Partial>) -> bool { matches!(self.partial[ty].ty, PartialType::Infer) }
}

impl Constraint {
	fn solve(
		&self, solver: &mut TypeSolver, ctx: &Ctx, items: &FxHashMap<Id<AbsPath>, Id<hir::Item>>,
		decls: &FxHashMap<Id<AbsPath>, Id<thir::ItemDecl>>,
	) -> bool {
		match *self {
			Constraint::Unify(a, b) if a == b => true,
			Constraint::Unify(a, b) => match (&solver.partial[a].ty, &solver.partial[b].ty) {
				(&PartialType::Concrete(a), &PartialType::Concrete(b)) => a == b,
				(&PartialType::Concrete(a), PartialType::Infer) => {
					solver.partial[b].ty = PartialType::Concrete(a);
					true
				},
				(PartialType::Infer, &PartialType::Concrete(b)) => {
					solver.partial[a].ty = PartialType::Concrete(b);
					true
				},
				_ => false,
			},
			Constraint::InfixOp { lhs, rhs, op, result } => {
				let conc = |ty: &PartialType| match ty {
					&PartialType::Concrete(ty) => Some(ty),
					_ => None,
				};
				let check = match op {
					hir::InfixOp::Assign => |_: &thir::Type| true,
					hir::InfixOp::Or | hir::InfixOp::And => {
						|ty: &thir::Type| matches!(ty, thir::Type::LangItem(LangItem::Bool))
					},
					hir::InfixOp::Eq | hir::InfixOp::NotEq => |_: &thir::Type| true,
					hir::InfixOp::Lt | hir::InfixOp::Leq | hir::InfixOp::Gt | hir::InfixOp::Geq => {
						|ty: &thir::Type| matches!(ty, thir::Type::LangItem(_))
					},
					hir::InfixOp::Add | hir::InfixOp::AddAssign | hir::InfixOp::Sub | hir::InfixOp::SubAssign => {
						|ty: &thir::Type| {
							matches!(
								ty,
								thir::Type::LangItem(
									LangItem::U8
										| LangItem::U16 | LangItem::U32 | LangItem::U64
										| LangItem::U128 | LangItem::I8 | LangItem::I16
										| LangItem::I32 | LangItem::I64 | LangItem::I128
										| LangItem::F32 | LangItem::F64
								) | thir::Type::Ptr(_)
							)
						}
					},
					hir::InfixOp::Mul
					| hir::InfixOp::MulAssign
					| hir::InfixOp::Div
					| hir::InfixOp::DivAssign
					| hir::InfixOp::Mod
					| hir::InfixOp::ModAssign => |ty: &thir::Type| {
						matches!(
							ty,
							thir::Type::LangItem(
								LangItem::U8
									| LangItem::U16 | LangItem::U32 | LangItem::U64
									| LangItem::U128 | LangItem::I8 | LangItem::I16
									| LangItem::I32 | LangItem::I64 | LangItem::I128
									| LangItem::F32 | LangItem::F64
							)
						)
					},
					hir::InfixOp::Shl
					| hir::InfixOp::ShlAssign
					| hir::InfixOp::Shr
					| hir::InfixOp::ShrAssign
					| hir::InfixOp::Xor
					| hir::InfixOp::XorAssign
					| hir::InfixOp::BitOr
					| hir::InfixOp::BitOrAssign
					| hir::InfixOp::BitAnd
					| hir::InfixOp::BitAndAssign => |ty: &thir::Type| {
						matches!(
							ty,
							thir::Type::LangItem(
								LangItem::U8
									| LangItem::U16 | LangItem::U32 | LangItem::U64
									| LangItem::U128 | LangItem::I8 | LangItem::I16
									| LangItem::I32 | LangItem::I64 | LangItem::I128
							)
						)
					},
					hir::InfixOp::Error => |_: &thir::Type| true,
				};

				let conc = conc(&solver.partial[lhs].ty)
					.map(|x| (x, 0))
					.or_else(|| conc(&solver.partial[rhs].ty).map(|x| (x, 1)))
					.or_else(|| conc(&solver.partial[result].ty).map(|x| (x, 2)));
				match conc {
					Some((ty, i)) if check(&*ctx.geti(ty)) => {
						match i {
							0 => {
								solver.unify(lhs, rhs);
								solver.unify(lhs, result);
							},
							1 => {
								solver.unify(rhs, lhs);
								solver.unify(rhs, result);
							},
							2 => {
								solver.unify(result, lhs);
								solver.unify(result, rhs);
							},
							_ => unreachable!(),
						}
						true
					},
					_ => false,
				}
			},
			Constraint::Call {
				callee,
				ref args,
				result,
			} => {
				let ret = match solver.partial[callee].ty {
					PartialType::Concrete(callee) => {
						let callee = &*ctx.geti(callee);
						match callee {
							thir::Type::Fn(f) => {
								if f.params.len() != args.len() {
									return false;
								}

								for (&p, &a) in f.params.iter().zip(args.iter()) {
									let p = solver.concrete(p, None);
									solver.unify(p, a);
								}
								let ret = solver.concrete(f.ret, None);
								solver.unify(ret, result);

								true
							},
							_ => false,
						}
					},
					PartialType::Fn { ref params, ret, .. } => {
						if params.len() != args.len() {
							return false;
						}

						for (p, &a) in params.clone().into_iter().zip(args.iter()) {
							solver.unify(p, a);
						}
						solver.unify(ret, result);

						true
					},
					_ => false,
				};
				ret
			},
			Constraint::Cast { expr, ty } => {
				let from = match &solver.partial[expr].ty {
					&PartialType::Concrete(ty) => ty,
					_ => return false,
				};
				let to = match &solver.partial[ty].ty {
					&PartialType::Concrete(ty) => ty,
					_ => return false,
				};
				let from = &*ctx.geti(from);
				let to = &*ctx.geti(to);
				return match (from, to) {
					(
						thir::Type::LangItem(
							LangItem::U8
							| LangItem::U16
							| LangItem::U32
							| LangItem::U64
							| LangItem::U128
							| LangItem::I8
							| LangItem::I16
							| LangItem::I32
							| LangItem::I64
							| LangItem::I128,
						),
						thir::Type::LangItem(_),
					) => true,
					(
						thir::Type::LangItem(LangItem::Bool | LangItem::Char),
						thir::Type::LangItem(
							LangItem::U8
							| LangItem::U16
							| LangItem::U32
							| LangItem::U64
							| LangItem::U128
							| LangItem::I8
							| LangItem::I16
							| LangItem::I32
							| LangItem::I64
							| LangItem::I128
							| LangItem::F32
							| LangItem::F64,
						),
					) => true,
					(
						thir::Type::LangItem(LangItem::F32 | LangItem::F64),
						thir::Type::LangItem(
							LangItem::U8
							| LangItem::U16
							| LangItem::U32
							| LangItem::U64
							| LangItem::U128
							| LangItem::I8
							| LangItem::I16
							| LangItem::I32
							| LangItem::I64
							| LangItem::I128,
						),
					) => true,
					(thir::Type::LangItem(x), thir::Type::LangItem(y)) => x == y,
					_ => false,
				};
			},
			Constraint::Field { expr, field, result } => {
				let expr = match &solver.partial[expr].ty {
					&PartialType::Concrete(expr) => expr,
					_ => return false,
				};
				let expr = &*ctx.geti(expr);
				match expr {
					thir::Type::Struct(p) => {
						let &s = decls.get(p).unwrap();
						let s = ctx.get(s);
						let &si = items.get(p).unwrap();
						let si = ctx.get(si);
						let Some((f, (_, &ty))) = (match (&s.kind, &si.kind) {
							(thir::ItemDeclKind::Struct(s), hir::ItemKind::Struct(si)) => {
								si.fields.iter().zip(s.fields.iter()).find(|(f, _)| f.name == field)
							},
							_ => return false,
						}) else {
							return false;
						};
						let ty = solver.concrete(ty, Some(f.id.erased()));
						solver.unify(ty, result);
						true
					},
					_ => false,
				}
			},
			Constraint::Index { expr, index, result } => {
				let index = match &solver.partial[index].ty {
					&PartialType::Concrete(index) => index,
					_ => return false,
				};
				let index = &*ctx.geti(index);
				match index {
					thir::Type::LangItem(
						LangItem::U8 | LangItem::U16 | LangItem::U32 | LangItem::U64 | LangItem::U128,
					) => {},
					_ => return false,
				}

				match &solver.partial[expr].ty {
					&PartialType::Concrete(expr) => {
						let expr = &*ctx.geti(expr);
						match expr {
							thir::Type::Array(a) => {
								let ty = solver.concrete(a.ty, None);
								solver.unify(ty, result);
								true
							},
							_ => false,
						}
					},
					&PartialType::Array { ty, .. } => {
						solver.unify(ty, result);
						true
					},
					_ => false,
				}
			},
			Constraint::Prefix { op, expr, result } => {
				let conc = |ty: &PartialType| match ty {
					&PartialType::Concrete(ty) => Some(ty),
					_ => None,
				};
				let check = match op {
					hir::PrefixOp::Not => |ty: &thir::Type| {
						matches!(
							ty,
							thir::Type::LangItem(
								LangItem::Bool
									| LangItem::U8 | LangItem::U16 | LangItem::U32
									| LangItem::U64 | LangItem::U128 | LangItem::I8
									| LangItem::I16 | LangItem::I32 | LangItem::I64
									| LangItem::I128 | LangItem::F32 | LangItem::F64,
							)
						)
					},
					hir::PrefixOp::Neg => |ty: &thir::Type| {
						matches!(
							ty,
							thir::Type::LangItem(
								LangItem::I8
									| LangItem::I16 | LangItem::I32 | LangItem::I64
									| LangItem::I128 | LangItem::F32 | LangItem::F64
							)
						)
					},
					hir::PrefixOp::Deref => |ty: &thir::Type| matches!(ty, thir::Type::Ptr(_)),
					hir::PrefixOp::Error => |_: &thir::Type| true,
				};

				let conc = conc(&solver.partial[expr].ty)
					.map(|x| (x, true))
					.or_else(|| conc(&solver.partial[result].ty).map(|x| (x, false)));
				match conc {
					Some((ty, e)) if check(&*ctx.geti(ty)) => {
						if e {
							solver.unify(expr, result);
						} else {
							solver.unify(result, expr);
						}
						true
					},
					_ => false,
				}
			},
			Constraint::Float(_) | Constraint::Int(_) => false,
		}
	}

	fn finalize(&self, solver: &mut TypeSolver, ctx: &Ctx) -> bool {
		match *self {
			Constraint::Float(expr) => match &solver.partial[expr].ty {
				&PartialType::Concrete(expr) => {
					let expr = &*ctx.geti(expr);
					matches!(expr, thir::Type::LangItem(LangItem::F32 | LangItem::F64))
				},
				&PartialType::Infer => {
					let ty = ctx.add(thir::Type::LangItem(LangItem::F32));
					solver.partial[expr].ty = PartialType::Concrete(ty);
					true
				},
				_ => return false,
			},
			Constraint::Int(expr) => match &solver.partial[expr].ty {
				&PartialType::Concrete(expr) => {
					let expr = &*ctx.geti(expr);
					matches!(
						expr,
						thir::Type::LangItem(
							LangItem::U8
								| LangItem::U16 | LangItem::U32 | LangItem::U64
								| LangItem::U128 | LangItem::I8 | LangItem::I16
								| LangItem::I32 | LangItem::I64 | LangItem::I128,
						)
					)
				},
				&PartialType::Infer => {
					let ty = ctx.add(thir::Type::LangItem(LangItem::I32));
					solver.partial[expr].ty = PartialType::Concrete(ty);
					true
				},
				_ => return false,
			},
			_ => false,
		}
	}

	fn error(self, solver: &mut TypeSolver, ctx: &Ctx) {
		match self {
			Constraint::Unify(a, b) => {
				let a_s = &solver.partial[a].span;
				let b_s = &solver.partial[b].span;
				let a = solver.fmt_type(ctx, a);
				let b = solver.fmt_type(ctx, b);
				match (a_s, b_s) {
					(Some(a_s), Some(b_s)) => ctx.push(
						a_s.error("mismatched types")
							.label(a_s.label(format!("expected `{}`", a)))
							.label(b_s.label(format!("found `{}`", b))),
					),
					(Some(s), None) | (None, Some(s)) => {
						ctx.push(
							s.error(format!("expected type `{}`", a))
								.label(s.label(format!("found `{}`", b))),
						);
					},
					(None, None) => {},
				}
			},
			Constraint::InfixOp { lhs, rhs, .. } if !solver.is_infer(lhs) && !solver.is_infer(rhs) => {
				let lhss = &solver.partial[lhs].span.unwrap();
				ctx.push(
					lhss.error("invalid operands for infix operator")
						.label(solver.ty_label(ctx, lhs))
						.label(solver.ty_label(ctx, rhs)),
				);
			},
			Constraint::Call { callee, .. } if !solver.is_infer(callee) => {
				let callees = &solver.partial[callee].span.unwrap();
				ctx.push(callees.error("cannot call").label(solver.ty_label(ctx, callee)));
			},
			Constraint::Cast { expr, ty } if !solver.is_infer(expr) && !solver.is_infer(ty) => {
				let exprs = &solver.partial[expr].span.unwrap();
				ctx.push(
					exprs
						.error("invalid operands for cast")
						.label(solver.ty_label(ctx, expr))
						.label(solver.ty_label(ctx, ty)),
				);
			},
			Constraint::Field { expr, field, .. } if !solver.is_infer(expr) => {
				let exprs = &solver.partial[expr].span.unwrap();
				let f = field.id.erased();
				ctx.push(
					exprs
						.error("unknown field")
						.label(solver.ty_label(ctx, expr))
						.label(f.mark()),
				);
			},
			Constraint::Index { expr, index, .. } if !solver.is_infer(expr) && !solver.is_infer(index) => {
				let exprs = &solver.partial[expr].span.unwrap();
				ctx.push(
					exprs
						.error("invalid operands for indexing")
						.label(solver.ty_label(ctx, expr))
						.label(solver.ty_label(ctx, index)),
				);
			},
			Constraint::Prefix { expr, .. } if !solver.is_infer(expr) => {
				let exprs = &solver.partial[expr].span.unwrap();
				ctx.push(
					exprs
						.error("invalid operand for prefix operator")
						.label(solver.ty_label(ctx, expr)),
				);
			},
			Constraint::Float(f) => {
				let s = &solver.partial[f].span.unwrap();
				ctx.push(
					s.error(format!("expected `{}`", solver.fmt_type(ctx, f)))
						.label(s.label("found `{{float}}`")),
				);
			},
			Constraint::Int(i) => {
				let s = &solver.partial[i].span.unwrap();
				ctx.push(
					s.error(format!("expected `{}`", solver.fmt_type(ctx, i)))
						.label(s.label("found `{{int}}`")),
				);
			},
			_ => {},
		}
	}
}

fn fmt_type(ctx: &Ctx, ty: Id<thir::Type>) -> String {
	match &*ctx.geti(ty) {
		thir::Type::LangItem(LangItem::Bool) => "bool".to_string(),
		thir::Type::LangItem(LangItem::Char) => "char".to_string(),
		thir::Type::LangItem(LangItem::U8) => "u8".to_string(),
		thir::Type::LangItem(LangItem::U16) => "u16".to_string(),
		thir::Type::LangItem(LangItem::U32) => "u32".to_string(),
		thir::Type::LangItem(LangItem::U64) => "u64".to_string(),
		thir::Type::LangItem(LangItem::U128) => "u128".to_string(),
		thir::Type::LangItem(LangItem::I8) => "i8".to_string(),
		thir::Type::LangItem(LangItem::I16) => "i16".to_string(),
		thir::Type::LangItem(LangItem::I32) => "i32".to_string(),
		thir::Type::LangItem(LangItem::I64) => "i64".to_string(),
		thir::Type::LangItem(LangItem::I128) => "i128".to_string(),
		thir::Type::LangItem(LangItem::F32) => "f32".to_string(),
		thir::Type::LangItem(LangItem::F64) => "f64".to_string(),
		thir::Type::Array(a) => {
			let ty = fmt_type(ctx, a.ty);
			format!("[{}; {}]", ty, a.len)
		},
		thir::Type::Fn(f) => {
			let abi = f.abi.map(|x| format!("extern \"{}\" ", x)).unwrap_or(String::new());
			let params = f
				.params
				.iter()
				.map(|&p| fmt_type(ctx, p))
				.collect::<Vec<_>>()
				.join(", ");
			let ret = fmt_type(ctx, f.ret);
			format!("{} fn({}) -> {}", abi, params, ret)
		},
		&thir::Type::Struct(p) | &thir::Type::Enum(p) => {
			let p = ctx.geti(p);
			match *p {
				AbsPath::Name { name, .. } => name.as_str().to_string(),
				_ => unreachable!(),
			}
		},
		thir::Type::Ptr(p) => {
			let ty = fmt_type(ctx, p.ty);
			if p.mutable {
				format!("*mut {}", ty)
			} else {
				format!("*{}", ty)
			}
		},
		thir::Type::Error => "<error>".to_string(),
		thir::Type::Void => "void".to_string(),
	}
}
