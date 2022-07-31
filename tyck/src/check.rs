use std::{collections::VecDeque, fmt::Write};

use diag::{Diagnostics, Span};
use hir::{
	ctx::{Hir, ValRef},
	hir::{BinOp, Ident, Spanned, UnOp},
	lang_item::LangItem,
	types::{Type, TypeId},
	Rodeo,
};
use id::{DenseMap, DenseMapBuilder, SparseMap};

pub struct InferenceEngine<'a> {
	info: DenseMapBuilder<TypeId, Spanned<TypeInfo>>,
	constraints: VecDeque<Constraint>,
	ty_to_ident: DenseMap<ValRef, Ident>,
	rodeo: &'a Rodeo,
	pub lang_items: &'a DenseMap<LangItem, ValRef>,
	pub inv_lang_items: &'a SparseMap<ValRef, LangItem>,
	pub diags: &'a mut Diagnostics,
}

impl<'a> InferenceEngine<'a> {
	pub fn new(
		hir: &Hir, rodeo: &'a Rodeo, lang_items: &'a DenseMap<LangItem, ValRef>,
		inv_lang_items: &'a SparseMap<ValRef, LangItem>, diags: &'a mut Diagnostics,
	) -> Self {
		Self {
			info: DenseMapBuilder::new(),
			constraints: VecDeque::new(),
			ty_to_ident: {
				let mut builder = DenseMapBuilder::new();
				for (val, def) in hir.globals.iter() {
					builder.insert_at(val, *def.path.ident());
				}
				builder.build()
			},
			rodeo,
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

	pub fn get(&mut self, id: TypeId) -> &TypeInfo { &self.info[id].node }

	pub fn get_mut(&mut self, id: TypeId) -> &mut TypeInfo { &mut self.info[id].node }

	pub fn constraint(&mut self, c: Constraint) { self.constraints.push_back(c); }

	pub fn solve(&mut self) {
		let mut last_len = self.constraints.len();
		loop {
			for _ in 0..self.constraints.len() {
				if let Some(c) = self.constraints.pop_front() {
					if !self.solve_constraint(&c) {
						self.constraints.push_back(c);
					}
				}
			}

			if last_len == self.constraints.len() {
				for c in std::mem::take(&mut self.constraints) {
					self.report_constraint_error(c);
				}

				self.ensure_all_types_known();

				break;
			}

			last_len = self.constraints.len();
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
			Constraint::MustBe { id, ty, .. } => {
				let is = self.reconstruct(*id);
				self.can_coerce(&is, ty)
			},
			Constraint::Return { .. } => false,
			Constraint::Arg { .. } => false,
			Constraint::Field { .. } => false,
			Constraint::Unary { .. } => false,
			Constraint::Binary { .. } => false,
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
			Constraint::MustBe { id, ty, span } => {
				let info = &self.info[id];

				let found = {
					let mut s = String::new();
					self.fmt_ty_info(&info.node, &mut s);
					s
				};
				let expected = {
					let mut s = String::new();
					self.fmt_ty(&ty, &mut s);
					s
				};

				self.diags.push({
					let diag = info.span.error("type mismatch");
					if let Some(span) = span {
						diag.label(info.span.label(format!("found `{}`", found)))
							.label(span.label(format!("expected `{}`", expected)))
					} else {
						diag.label(info.span.label(format!("found `{}`, expected `{}`", found, expected)))
					}
				});
			},
			_ => {},
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
				if self.inv_lang_items.get(*val).map(|x| x.is_int()).unwrap_or(false) =>
			{
				*self.get_mut(a) = TypeInfo::EqTo(b);
			},
			(TypeInfo::Ty(val), TypeInfo::IntLit)
				if self.inv_lang_items.get(*val).map(|x| x.is_int()).unwrap_or(false) =>
			{
				*self.get_mut(b) = TypeInfo::EqTo(a);
			},
			(TypeInfo::FloatLit, TypeInfo::Ty(val))
				if self.inv_lang_items.get(*val).map(|x| x.is_float()).unwrap_or(false) =>
			{
				*self.get_mut(a) = TypeInfo::EqTo(b);
			},
			(TypeInfo::Ty(val), TypeInfo::FloatLit)
				if self.inv_lang_items.get(*val).map(|x| x.is_int()).unwrap_or(false) =>
			{
				*self.get_mut(a) = TypeInfo::EqTo(b);
			},
			(TypeInfo::Ty(av), TypeInfo::Ty(bv))
				if self.inv_lang_items.get(*av).map(|x| x.is_int()).unwrap_or(false)
					&& self.inv_lang_items.get(*bv).map(|x| x.is_int()).unwrap_or(false) =>
			{
				let la = *self.inv_lang_items.get(*av).unwrap();
				let lb = *self.inv_lang_items.get(*bv).unwrap();

				let larger = LangItem::larger_int(la, lb);

				if larger == la {
					*self.get_mut(b) = TypeInfo::EqTo(a);
				} else {
					*self.get_mut(a) = TypeInfo::EqTo(b);
				}
			},
			(TypeInfo::Ty(av), TypeInfo::Ty(bv))
				if self.inv_lang_items.get(*av).map(|x| x.is_float()).unwrap_or(false)
					&& self.inv_lang_items.get(*bv).map(|x| x.is_float()).unwrap_or(false) =>
			{
				let la = *self.inv_lang_items.get(*av).unwrap();
				let lb = *self.inv_lang_items.get(*bv).unwrap();

				let larger = LangItem::larger_float(la, lb);

				if larger == la {
					*self.get_mut(b) = TypeInfo::EqTo(a);
				} else {
					*self.get_mut(a) = TypeInfo::EqTo(b);
				}
			},
			(TypeInfo::Tuple(tys_a), TypeInfo::Tuple(tys_b)) if tys_a.len() == tys_b.len() => {
				let tys_a = tys_a.clone();
				let tys_b = tys_b.clone();
				for (a, b) in tys_a.into_iter().zip(tys_b) {
					if !self.unify(a, b) {
						return false;
					}
				}
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

				for (a, b) in args_a.into_iter().zip(args_b) {
					if !self.unify(a, b) {
						return false;
					}
				}

				return self.unify(ret_a, ret_b);
			},
			(TypeInfo::Ptr { mutable: true, to: toa }, TypeInfo::Ptr { mutable: true, to: tob }) => {
				return self.unify(*toa, *tob)
			},
			(
				TypeInfo::Ptr {
					mutable: false,
					to: toa,
				},
				TypeInfo::Ptr {
					mutable: false,
					to: tob,
				},
			) => return self.unify(*toa, *tob),
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

	fn fmt_ty(&self, ty: &Type, w: &mut impl Write) {
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
	MustBe {
		id: TypeId,
		ty: Type,
		span: Option<Span>,
	},
	Return {
		id: TypeId,
		fn_id: TypeId,
	},
	Arg {
		id: TypeId,
		arg_idx: usize,
		fn_id: TypeId,
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
