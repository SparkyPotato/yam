use std::collections::VecDeque;

use diag::{Diagnostics, Span};
use hir::{
	ctx::ValRef,
	hir::Spanned,
	types::{Type, TypeId},
};

#[derive(Default)]
pub struct TypeEngine {
	info: Vec<Spanned<TypeInfo>>,
	constraints: VecDeque<Constraint>,
}

impl TypeEngine {
	pub fn new() -> Self { Self::default() }

	pub fn insert(&mut self, info: TypeInfo, span: Span) -> TypeId {
		let id = self.info.len();
		self.info.push(Spanned { node: info, span });
		TypeId::new(id as _)
	}

	pub fn reset(&mut self) {
		self.info.clear();
		self.constraints.clear();
	}

	pub fn constraint(&mut self, c: Constraint) { self.constraints.push_back(c); }

	pub fn solve(&mut self, diags: &mut Diagnostics) {
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
					self.report_constraint_error(c, diags);
				}

				break;
			}

			last_len = self.constraints.len();
		}
	}

	pub fn reconstruct(&self, id: TypeId) -> Type {
		let info = &self.info[id.id() as usize];
		match &info.node {
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

	fn solve_constraint(&mut self, c: &Constraint) -> bool { true }

	fn report_constraint_error(&mut self, c: Constraint, diags: &mut Diagnostics) {}

	fn unify(&mut self, a: TypeId, b: TypeId, diags: &mut Diagnostics) {
		let a = &self.info[a.id() as usize];
		let b = &self.info[b.id() as usize];
	}
}

pub enum TypeInfo {
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
}
