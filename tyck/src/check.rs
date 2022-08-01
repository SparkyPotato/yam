use std::{collections::VecDeque, fmt::Write};

use diag::{Diagnostics, Span};
use hir::{
	ctx::ValRef,
	hir::{BinOp, Ident, Spanned, UnOp, ValDef},
	lang_item::LangItem,
	types::{Type, TypeId},
	Rodeo,
};
use id::{DenseMap, DenseMapBuilder, DenseMut, SparseMap};

pub struct InferenceEngine<'a> {
	info: DenseMapBuilder<TypeId, Spanned<TypeInfo>>,
	constraints: VecDeque<Constraint>,
	ty_to_ident: DenseMap<ValRef, Ident>,
	rodeo: &'a Rodeo,
	globals: &'a DenseMut<ValRef, ValDef>,
	pub lang_items: &'a DenseMap<LangItem, ValRef>,
	pub inv_lang_items: &'a SparseMap<ValRef, LangItem>,
	pub diags: &'a mut Diagnostics,
}

impl<'a> InferenceEngine<'a> {
	pub fn new(
		rodeo: &'a Rodeo, globals: &'a DenseMut<ValRef, ValDef>, lang_items: &'a DenseMap<LangItem, ValRef>,
		inv_lang_items: &'a SparseMap<ValRef, LangItem>, diags: &'a mut Diagnostics,
	) -> Self {
		Self {
			info: DenseMapBuilder::new(),
			constraints: VecDeque::new(),
			ty_to_ident: {
				let mut builder = DenseMapBuilder::new();
				for (val, def) in globals.iter_mut() {
					builder.insert_at(val, *def.path.ident());
				}
				builder.build()
			},
			rodeo,
			globals,
			lang_items,
			inv_lang_items,
			diags,
		}
	}
}

impl InferenceEngine<'_> {
	pub fn insert(&mut self, info: TypeInfo, span: Span) -> TypeId { self.info.add(Spanned { node: info, span }) }

	pub fn reset(&mut self) {
		self.info.reset();
		self.constraints.clear();
	}

	pub fn resolve(&self, id: TypeId) -> &TypeInfo {
		match &self.info[id].node {
			TypeInfo::EqTo(id) => self.resolve(*id),
			x => x,
		}
	}

	pub fn get_mut(&mut self, id: TypeId) -> &mut TypeInfo { &mut self.info[id].node }

	pub fn constraint(&mut self, c: Constraint) { self.constraints.push_back(c); }

	pub fn solve(&mut self) {
		loop {
			let mut solved = false;
			for _ in 0..self.constraints.len() {
				if let Some(c) = self.constraints.pop_front() {
					if self.solve_constraint(&c) {
						solved = true
					} else {
						self.constraints.push_back(c);
					}
				}
			}

			if !solved {
				for c in std::mem::take(&mut self.constraints) {
					self.report_constraint_error(c);
				}

				self.ensure_all_types_known();

				break;
			}
		}
	}

	pub fn reconstruct(&self, id: TypeId) -> Type {
		let info = &self.info[id];
		match &info.node {
			TypeInfo::EqTo(a) => self.reconstruct(*a),
			TypeInfo::IntLit => Type::Ty(self.lang_items[LangItem::Isize]),
			TypeInfo::FloatLit => Type::Ty(self.lang_items[LangItem::F32]),
			TypeInfo::Void => Type::Void,
			TypeInfo::Never => Type::Never,
			TypeInfo::Type => Type::Type,
			TypeInfo::Tuple(tys) => Type::Tuple(tys.iter().map(|id| self.reconstruct(*id)).collect()),
			TypeInfo::Fn { args, ret } => Type::Fn {
				args: args.iter().map(|id| self.reconstruct(*id)).collect(),
				ret: Box::new(self.reconstruct(*ret)),
			},
			TypeInfo::Ty(id) => Type::Ty(*id),
			TypeInfo::Ptr { mutable, to } => Type::Ptr {
				mutable: *mutable,
				to: Box::new(self.reconstruct(*to)),
			},
			TypeInfo::Unknown => Type::Unknown,
		}
	}

	fn solve_constraint(&mut self, c: &Constraint) -> bool {
		match c {
			Constraint::Eq(a, b) => self.unify(*a, *b),
			Constraint::FnCall { fn_id, args, ret_id } => match self.resolve(*fn_id) {
				TypeInfo::Fn { args: a, ret } => {
					let ret = *ret;
					let ret_id = *ret_id;
					let a = a.clone();
					let args = args.clone();

					if a.len() != args.len() {
						let span = self.info[*fn_id].span;
						self.diags.push(
							span.error("wrong number of arguments")
								.label(span.label(format!("expected {} arguments", a.len()))),
						);
						return true;
					}

					for (a, b) in args.into_iter().zip(a) {
						self.constraints.push_back(Constraint::Eq(a, b));
					}

					self.constraints.push_back(Constraint::Eq(ret, ret_id));

					true
				},
				_ => false,
			},
			Constraint::Field { id, struct_id, field } => false,
			Constraint::Unary { id, op, expr } => self.unary(*id, *op, *expr),
			Constraint::Binary { id, lhs, op, rhs } => self.binary(*id, *lhs, *op, *rhs),
		}
	}

	fn report_constraint_error(&mut self, c: Constraint) {
		match c {
			Constraint::Eq(a, b) => {
				let info_a = &self.info[a];
				let info_b = &self.info[b];

				let found = {
					let mut s = String::new();
					self.fmt_ty_info(&info_a.node, &mut s);
					s
				};
				let expected = {
					let mut s = String::new();
					self.fmt_ty_info(&info_b.node, &mut s);
					s
				};

				self.diags.push(
					info_a
						.span
						.error("type mismatch")
						.label(info_a.span.label(format!("found `{}`", found)))
						.label(info_b.span.label(format!("expected `{}`", expected))),
				);
			},
			Constraint::FnCall { fn_id, .. } => {
				let info = &self.info[fn_id];
				self.diags
					.push(
						info.span
							.error("cannot call")
							.label(info.span.label(format!("this has type `{}`", {
								let mut s = String::new();
								self.fmt_ty_info(&info.node, &mut s);
								s
							}))),
					);
			},
			Constraint::Field { .. } => {},
			Constraint::Unary { op, expr, .. } => {
				let info = &self.info[expr];
				self.diags
					.push(info.span.error("invalid operation").label(info.span.label(format!(
						"`{}` cannot be used on type `{}`",
						op,
						{
							let mut s = String::new();
							self.fmt_ty_info(&info.node, &mut s);
							s
						}
					))));
			},
			Constraint::Binary { lhs, op, rhs, .. } => {
				let info_lhs = &self.info[lhs];
				let info_rhs = &self.info[rhs];
				let span = info_lhs.span + info_rhs.span;
				self.diags.push(
					span.error(format!("invalid use of `{}`", op))
						.label(info_lhs.span.label(format!("this has type `{}`", {
							let mut s = String::new();
							self.fmt_ty_info(&info_lhs.node, &mut s);
							s
						})))
						.label(info_rhs.span.label(format!("this has type `{}`", {
							let mut s = String::new();
							self.fmt_ty_info(&info_rhs.node, &mut s);
							s
						}))),
				);
			},
		}
	}

	fn ensure_all_types_known(&mut self) {
		for (_, info) in self.info.iter() {
			if let TypeInfo::Unknown = &info.node {
				self.diags.push(info.span.error("cannot infer").label(info.span.mark()));
			}
		}
	}

	fn unify(&mut self, a: TypeId, b: TypeId) -> bool {
		let ai = &self.info[a];
		let bi = &self.info[b];

		match (&ai.node, &bi.node) {
			(TypeInfo::EqTo(a), _) => return self.unify(*a, b),
			(_, TypeInfo::EqTo(b)) => return self.unify(a, *b),
			(TypeInfo::Never | TypeInfo::Unknown, _) => *self.get_mut(a) = TypeInfo::EqTo(b),
			(_, TypeInfo::Never | TypeInfo::Unknown) => *self.get_mut(b) = TypeInfo::EqTo(a),
			(TypeInfo::IntLit, TypeInfo::Ty(val))
				if self.inv_lang_items.get(*val).map(|x| x.is_integer()).unwrap_or(false) =>
			{
				*self.get_mut(a) = TypeInfo::EqTo(b);
			},
			(TypeInfo::Ty(val), TypeInfo::IntLit)
				if self.inv_lang_items.get(*val).map(|x| x.is_integer()).unwrap_or(false) =>
			{
				*self.get_mut(b) = TypeInfo::EqTo(a);
			},
			(TypeInfo::FloatLit, TypeInfo::Ty(val))
				if self.inv_lang_items.get(*val).map(|x| x.is_float()).unwrap_or(false) =>
			{
				*self.get_mut(a) = TypeInfo::EqTo(b);
			},
			(TypeInfo::Ty(val), TypeInfo::FloatLit)
				if self.inv_lang_items.get(*val).map(|x| x.is_integer()).unwrap_or(false) =>
			{
				*self.get_mut(a) = TypeInfo::EqTo(b);
			},
			(TypeInfo::Tuple(tys_a), TypeInfo::Tuple(tys_b)) if tys_a.len() == tys_b.len() => {
				let tys_a = tys_a.clone();
				let tys_b = tys_b.clone();
				let mut ret = true;
				for (a, b) in tys_a.into_iter().zip(tys_b) {
					if !self.unify(a, b) {
						ret = false;
					}
				}
				return ret;
			},
			(
				TypeInfo::Fn {
					args: args_a,
					ret: ret_a,
				},
				TypeInfo::Fn {
					args: args_b,
					ret: ret_b,
				},
			) if args_a.len() == args_b.len() => {
				let args_a = args_a.clone();
				let args_b = args_b.clone();
				let ret_a = *ret_a;
				let ret_b = *ret_b;

				let mut ret = true;
				for (a, b) in args_a.into_iter().zip(args_b) {
					if !self.unify(a, b) {
						ret = false;
					}
				}

				return self.unify(ret_a, ret_b) && ret;
			},
			(
				TypeInfo::Ptr {
					mutable: mut_a,
					to: toa,
				},
				TypeInfo::Ptr {
					mutable: mut_b,
					to: tob,
				},
			) if mut_a == mut_b => return self.unify(*toa, *tob),
			(a, b) if a == b => {},
			(a, b) => self.diags.push(
				ai.span
					.error("type mismatch")
					.label(ai.span.label({
						let mut ty = String::new();
						self.fmt_ty_info(a, &mut ty);
						format!("this has type `{}`", ty)
					}))
					.label(bi.span.label({
						let mut ty = String::new();
						self.fmt_ty_info(b, &mut ty);
						format!("this has type `{}`", ty)
					})),
			),
		}

		if matches!(self.info[a].node, TypeInfo::Unknown) && matches!(self.info[b].node, TypeInfo::Unknown) {
			false
		} else {
			true
		}
	}

	fn unary(&mut self, id: TypeId, op: UnOp, expr: TypeId) -> bool {
		let span = self.info[id].span;

		let cons = match op {
			UnOp::Not | UnOp::Neg => Constraint::Eq(id, expr),
			UnOp::Addr => {
				let res = self.insert(
					TypeInfo::Ptr {
						mutable: false,
						to: expr,
					},
					span,
				);
				Constraint::Eq(res, id)
			},
			UnOp::DoubleAddr => {
				let to = self.insert(
					TypeInfo::Ptr {
						mutable: false,
						to: expr,
					},
					span,
				);
				let res = self.insert(TypeInfo::Ptr { mutable: false, to }, span);

				Constraint::Eq(res, id)
			},
			UnOp::AddrMut => {
				let res = self.insert(
					TypeInfo::Ptr {
						mutable: true,
						to: expr,
					},
					span,
				);
				Constraint::Eq(res, id)
			},
			UnOp::DoubleAddrMut => {
				let to = self.insert(
					TypeInfo::Ptr {
						mutable: true,
						to: expr,
					},
					span,
				);
				let res = self.insert(TypeInfo::Ptr { mutable: false, to }, span);

				Constraint::Eq(res, id)
			},
			UnOp::Deref => match self.resolve(expr) {
				TypeInfo::Ptr { to, .. } => Constraint::Eq(*to, id),
				_ => return false,
			},
		};

		self.constraints.push_back(cons);

		true
	}

	fn binary(&mut self, id: TypeId, lhs: TypeId, op: BinOp, rhs: TypeId) -> bool {
		let lhsi = self.resolve(lhs).clone();
		let rhsi = self.resolve(rhs).clone();

		let int = |this: &mut Self, lhsi: &TypeInfo, rhsi: &TypeInfo| {
			Some(match (lhsi, rhsi) {
				(TypeInfo::IntLit, TypeInfo::IntLit) => {
					this.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::Ty(l), TypeInfo::Ty(r))
					if this.inv_lang_items.get(*l).map(|x| x.is_int()).unwrap_or(false)
						&& this.inv_lang_items.get(*r).map(|x| x.is_int()).unwrap_or(false) =>
				{
					let l = *this.inv_lang_items.get(*l).unwrap();
					let r = *this.inv_lang_items.get(*r).unwrap();
					let larger = LangItem::larger_int(l, r);
					if larger == l {
						Constraint::Eq(id, lhs)
					} else {
						Constraint::Eq(id, rhs)
					}
				},
				(TypeInfo::Ty(l), TypeInfo::Ty(r))
					if this.inv_lang_items.get(*l).map(|x| x.is_uint()).unwrap_or(false)
						&& this.inv_lang_items.get(*r).map(|x| x.is_uint()).unwrap_or(false) =>
				{
					let l = *this.inv_lang_items.get(*l).unwrap();
					let r = *this.inv_lang_items.get(*r).unwrap();
					let larger = LangItem::larger_uint(l, r);
					if larger == l {
						Constraint::Eq(id, lhs)
					} else {
						Constraint::Eq(id, rhs)
					}
				},
				(TypeInfo::Ty(v), TypeInfo::IntLit)
					if this
						.inv_lang_items
						.get(*v)
						.map(|x| x.is_uint() || x.is_int())
						.unwrap_or(false) =>
				{
					this.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::IntLit, TypeInfo::Ty(v))
					if this
						.inv_lang_items
						.get(*v)
						.map(|x| x.is_uint() || x.is_int())
						.unwrap_or(false) =>
				{
					this.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, rhs)
				},
				_ => return None,
			})
		};

		let float = |this: &mut Self, lhsi: &TypeInfo, rhsi: &TypeInfo| {
			Some(match (lhsi, rhsi) {
				(TypeInfo::FloatLit, TypeInfo::FloatLit) => {
					this.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::Ty(l), TypeInfo::Ty(r))
					if this.inv_lang_items.get(*l).map(|x| x.is_float()).unwrap_or(false)
						&& this.inv_lang_items.get(*r).map(|x| x.is_float()).unwrap_or(false) =>
				{
					let l = *this.inv_lang_items.get(*l).unwrap();
					let r = *this.inv_lang_items.get(*r).unwrap();
					let larger = LangItem::larger_float(l, r);
					if larger == l {
						Constraint::Eq(id, lhs)
					} else {
						Constraint::Eq(id, rhs)
					}
				},
				(TypeInfo::Ty(v), TypeInfo::FloatLit)
					if this.inv_lang_items.get(*v).map(|x| x.is_float()).unwrap_or(false) =>
				{
					this.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::FloatLit, TypeInfo::Ty(v))
					if this.inv_lang_items.get(*v).map(|x| x.is_float()).unwrap_or(false) =>
				{
					this.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, rhs)
				},
				_ => return None,
			})
		};

		let arith = |this: &mut Self, lhsi: &TypeInfo, rhsi: &TypeInfo| {
			int(this, lhsi, rhsi).or_else(|| float(this, lhsi, rhsi))
		};

		let cons = match op {
			BinOp::Add => {
				if let Some(c) = arith(self, &lhsi, &rhsi) {
					c
				} else {
					match (lhsi, rhsi) {
						(TypeInfo::Ptr { .. }, TypeInfo::Ty(r))
							if self
								.inv_lang_items
								.get(r)
								.map(|x| x.is_uint() || x.is_int())
								.unwrap_or(false) =>
						{
							Constraint::Eq(id, lhs)
						},
						(TypeInfo::Ptr { .. }, TypeInfo::IntLit) => Constraint::Eq(id, lhs),
						_ => return false,
					}
				}
			},
			BinOp::Sub => {
				if let Some(c) = arith(self, &lhsi, &rhsi) {
					c
				} else {
					match (lhsi, rhsi) {
						(TypeInfo::Ptr { to: tol, .. }, TypeInfo::Ptr { to: tor, .. }) => {
							let tol = tol;
							let tor = tor;
							self.constraint(Constraint::Eq(tol, tor));

							let i = self.insert(TypeInfo::Ty(self.lang_items[LangItem::Isize]), self.info[lhs].span);
							Constraint::Eq(id, i)
						},
						_ => return false,
					}
				}
			},
			BinOp::Mul | BinOp::Div | BinOp::Rem => {
				if let Some(c) = arith(self, &lhsi, &rhsi) {
					c
				} else {
					return false;
				}
			},
			BinOp::Shl | BinOp::Shr => match (lhsi, rhsi) {
				(TypeInfo::IntLit, TypeInfo::IntLit) => {
					self.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::Ty(l), TypeInfo::Ty(r))
					if self
						.inv_lang_items
						.get(l)
						.map(|x| x.is_uint() || x.is_int())
						.unwrap_or(false) && self.inv_lang_items.get(r).map(|x| x.is_uint()).unwrap_or(false) =>
				{
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::Ty(l), TypeInfo::IntLit)
					if self
						.inv_lang_items
						.get(l)
						.map(|x| x.is_uint() || x.is_int())
						.unwrap_or(false) =>
				{
					self.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				(TypeInfo::IntLit, TypeInfo::Ty(l))
					if self.inv_lang_items.get(l).map(|x| x.is_uint()).unwrap_or(false) =>
				{
					self.constraint(Constraint::Eq(lhs, rhs));
					Constraint::Eq(id, lhs)
				},
				_ => return false,
			},
			BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq | BinOp::Eq | BinOp::Neq => {
				match (lhsi, rhsi) {
					(TypeInfo::Ty(v), TypeInfo::IntLit) | (TypeInfo::IntLit, TypeInfo::Ty(v))
						if self
							.inv_lang_items
							.get(v)
							.map(|x| x.is_uint() || x.is_int())
							.unwrap_or(false) =>
					{
						self.constraint(Constraint::Eq(lhs, rhs));
					},
					(TypeInfo::Ty(v), TypeInfo::FloatLit) | (TypeInfo::FloatLit, TypeInfo::Ty(v))
						if self.inv_lang_items.get(v).map(|x| x.is_float()).unwrap_or(false) =>
					{
						self.constraint(Constraint::Eq(lhs, rhs));
					},
					(a, b) if a == b => {},
					_ => return false,
				}

				let i = self.insert(TypeInfo::Ty(self.lang_items[LangItem::Bool]), self.info[lhs].span);
				Constraint::Eq(id, i)
			},
			BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => {
				if let Some(c) = int(self, &lhsi, &rhsi) {
					c
				} else {
					return false;
				}
			},
			BinOp::And | BinOp::Or => match (lhsi, rhsi) {
				(TypeInfo::Ty(l), TypeInfo::Ty(r))
					if self
						.inv_lang_items
						.get(l)
						.map(|x| *x == LangItem::Bool)
						.unwrap_or(false) && self
						.inv_lang_items
						.get(r)
						.map(|x| *x == LangItem::Bool)
						.unwrap_or(false) =>
				{
					Constraint::Eq(id, lhs)
				},
				_ => return false,
			},
			BinOp::Assign => {
				let void = self.insert(TypeInfo::Void, self.info[lhs].span);
				self.constraint(Constraint::Eq(id, void));
				Constraint::Eq(lhs, rhs)
			},
			BinOp::AddAssign
			| BinOp::SubAssign
			| BinOp::MulAssign
			| BinOp::DivAssign
			| BinOp::RemAssign
			| BinOp::BitAndAssign
			| BinOp::BitOrAssign
			| BinOp::BitXorAssign
			| BinOp::ShlAssign
			| BinOp::ShrAssign => unreachable!("unsupported"),
			BinOp::PlaceConstruct => unreachable!("placement construction is not supported"),
		};

		self.constraint(cons);

		true
	}

	fn can_coerce(&mut self, from: &Type, to: &Type) -> bool {
		match (from, to) {
			(Type::Never, _) => true,
			(
				Type::Ptr { mutable: true, to: toa },
				Type::Ptr {
					mutable: false,
					to: tob,
				},
			) => self.can_coerce(toa, tob),
			(a, b) if a == b => true,
			_ => false,
		}
	}

	fn fmt_ty_info(&self, info: &TypeInfo, w: &mut impl Write) {
		match info {
			TypeInfo::EqTo(a) => {
				self.fmt_ty_info(&self.info[*a].node, w);
				Ok(())
			},
			TypeInfo::IntLit => w.write_str("<int>"),
			TypeInfo::FloatLit => w.write_str("<float>"),
			TypeInfo::Void => w.write_str("void"),
			TypeInfo::Never => w.write_str("!"),
			TypeInfo::Type => w.write_str("type"),
			TypeInfo::Tuple(tys) => {
				w.write_str("(").unwrap();
				for (i, ty) in tys.iter().enumerate() {
					if i > 0 {
						w.write_str(", ").unwrap();
					}
					self.fmt_ty_info(&self.info[*ty].node, w);
				}
				w.write_str(")")
			},
			TypeInfo::Fn { args, ret } => {
				w.write_str("fn").unwrap();
				w.write_str("(").unwrap();
				for (i, ty) in args.iter().enumerate() {
					if i > 0 {
						w.write_str(", ").unwrap();
					}
					self.fmt_ty_info(&self.info[*ty].node, w);
				}
				w.write_str(") -> ").unwrap();
				self.fmt_ty_info(&self.info[*ret].node, w);
				Ok(())
			},
			TypeInfo::Ty(ty) => w.write_str(self.rodeo.resolve(&self.ty_to_ident[*ty].node)),
			TypeInfo::Ptr { mutable, to } => {
				w.write_str("*").unwrap();
				w.write_str(if *mutable { "mut " } else { "const " }).unwrap();
				self.fmt_ty_info(&self.info[*to].node, w);
				Ok(())
			},
			TypeInfo::Unknown => w.write_str("<unknown>"),
		}
		.unwrap();
	}

	pub fn fmt_ty(&self, ty: &Type, w: &mut impl Write) {
		match ty {
			Type::Void => w.write_str("void"),
			Type::Never => w.write_str("!"),
			Type::Type => w.write_str("type"),
			Type::Tuple(tys) => {
				w.write_str("(").unwrap();
				for (i, ty) in tys.iter().enumerate() {
					if i > 0 {
						w.write_str(", ").unwrap();
					}
					self.fmt_ty(ty, w);
				}
				w.write_str(")")
			},
			Type::Fn { args, ret } => {
				w.write_str("fn").unwrap();
				w.write_str("(").unwrap();
				for (i, ty) in args.iter().enumerate() {
					if i > 0 {
						w.write_str(", ").unwrap();
					}
					self.fmt_ty(ty, w);
				}
				w.write_str(") -> ").unwrap();
				self.fmt_ty(ret, w);
				Ok(())
			},
			Type::Ty(ty) => w.write_str(self.rodeo.resolve(&self.ty_to_ident[*ty].node)),
			Type::Ptr { mutable, to } => {
				w.write_str("*").unwrap();
				w.write_str(if *mutable { "mut " } else { "const " }).unwrap();
				self.fmt_ty(to, w);
				Ok(())
			},
			Type::Unknown => w.write_str("<unknown>"),
			Type::Unresolved(id) => Ok(self.fmt_ty_info(&self.info[*id].node, w)),
			Type::Err => w.write_str("<err>"),
		}
		.unwrap();
	}
}

#[derive(Clone, Eq, PartialEq)]
pub enum TypeInfo {
	EqTo(TypeId),
	IntLit,
	FloatLit,
	Void,
	Never,
	Type,
	Tuple(Vec<TypeId>),
	Fn { args: Vec<TypeId>, ret: TypeId },
	Ty(ValRef),
	Ptr { mutable: bool, to: TypeId },
	Unknown,
}

pub enum Constraint {
	Eq(TypeId, TypeId),
	FnCall {
		fn_id: TypeId,
		args: Vec<TypeId>,
		ret_id: TypeId,
	},
	Field {
		id: TypeId,
		struct_id: TypeId,
		field: Ident,
	},
	Unary {
		id: TypeId,
		op: UnOp,
		expr: TypeId,
	},
	Binary {
		id: TypeId,
		op: BinOp,
		lhs: TypeId,
		rhs: TypeId,
	},
}
