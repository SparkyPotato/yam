use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use name_resolve::resolved::Ty;

use crate::{InbuiltType, Rodeo, TyRef, Type};

#[derive(Copy, Clone, Debug)]
pub struct TypeId(pub(crate) u32);

#[derive(Clone)]
pub enum TypeInfo {
	Ref(TypeId),
	Void,
	Fn { args: Vec<TypeId>, ret: TypeId },
	Ty(TyRef),
	Ptr { mutable: bool, to: TypeId },
	FnRet(TypeId),
	Deref(TypeId),
	Int,
	Float,
	Unknown,
}

pub struct TypeEngine<'a> {
	vars: Vec<TypeInfo>,
	scopes: Vec<usize>,
	types: &'a HashMap<TyRef, Ty>,
	inbuilts: &'a HashMap<InbuiltType, TyRef>,
	rodeo: &'a Rodeo,
}

impl<'a> TypeEngine<'a> {
	pub fn new(types: &'a HashMap<TyRef, Ty>, inbuilts: &'a HashMap<InbuiltType, TyRef>, rodeo: &'a Rodeo) -> Self {
		Self {
			vars: Vec::new(),
			scopes: Vec::new(),
			types,
			inbuilts,
			rodeo,
		}
	}
}

impl TypeEngine<'_> {
	pub fn push_scope(&mut self) { self.scopes.push(self.vars.len()); }

	pub fn pop_scope(&mut self) { self.vars.truncate(*self.scopes.last().unwrap_or(&0)) }

	pub fn reset(&mut self) {
		assert!(self.scopes.is_empty());
		assert!(self.vars.is_empty());
	}

	pub fn insert(&mut self, info: TypeInfo) -> TypeId {
		let id = self.vars.len() as _;
		self.vars.push(info);
		TypeId(id)
	}

	pub fn get(&self, id: TypeId) -> &TypeInfo { &self.vars[id.0 as usize] }

	pub fn get_mut(&mut self, id: TypeId) -> &mut TypeInfo { &mut self.vars[id.0 as usize] }

	pub fn assign(
		&mut self, lhs: TypeId, lhs_span: Span, rhs: TypeId, rhs_span: Span, diagnostics: &mut Vec<Report<Span>>,
	) {
		match (self.get(lhs), self.get(lhs)) {
			(TypeInfo::Ref(lhs), _) => self.assign(*lhs, lhs_span, rhs, rhs_span, diagnostics),
			(_, TypeInfo::Ref(rhs)) => self.assign(lhs, lhs_span, *rhs, rhs_span, diagnostics),
			(TypeInfo::Unknown, _) => *self.get_mut(lhs) = TypeInfo::Ref(rhs),
			(_, TypeInfo::Unknown) => *self.get_mut(rhs) = TypeInfo::Ref(lhs),
			(TypeInfo::Void, TypeInfo::Void) => {},
			(TypeInfo::Int, TypeInfo::Int) => {},
			(TypeInfo::Float, TypeInfo::Float) => {},
			(TypeInfo::Int, TypeInfo::Ty(x)) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Int(_))) => {
				*self.get_mut(lhs) = TypeInfo::Ref(rhs)
			},
			(TypeInfo::Ty(x), TypeInfo::Int) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Int(_))) => {
				*self.get_mut(rhs) = TypeInfo::Ref(lhs)
			},
			(TypeInfo::Float, TypeInfo::Ty(x)) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Float(_))) => {
				*self.get_mut(lhs) = TypeInfo::Ref(rhs)
			},
			(TypeInfo::Ty(x), TypeInfo::Float) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Float(_))) => {
				*self.get_mut(rhs) = TypeInfo::Ref(lhs)
			},
			(
				TypeInfo::Ptr {
					mutable: false,
					to: to_lhs,
				},
				TypeInfo::Ptr { mutable: _, to: to_rhs },
			) => self.assign(*to_lhs, lhs_span, *to_rhs, rhs_span, diagnostics),
			(
				TypeInfo::Ptr {
					mutable: true,
					to: to_lhs,
				},
				TypeInfo::Ptr {
					mutable: true,
					to: to_rhs,
				},
			) => self.assign(*to_lhs, lhs_span, *to_rhs, rhs_span, diagnostics),
			(a, b) => diagnostics.push(
				lhs_span
					.report(ReportKind::Error)
					.with_message("cannot assign")
					.with_label(Label::new(lhs_span).with_message(format!("this has type `{}`", self.fmt_type(a))))
					.with_label(Label::new(rhs_span).with_message(format!("this has type `{}`", self.fmt_type(b))))
					.finish(),
			),
		}
	}

	pub fn reconstruct(&mut self, id: TypeId, span: Span, diagnostics: &mut Vec<Report<Span>>) -> Type {
		match self.get(id) {
			TypeInfo::Unknown => {
				diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("cannot infer")
						.with_label(Label::new(span).with_message("consider giving this an explicit type"))
						.finish(),
				);
				Type::Err
			},
			TypeInfo::FnRet(id) => {
				let fn_ty = self.reconstruct(*id, span, diagnostics);
				match fn_ty {
					Type::Fn { ret, .. } => *ret,
					_ => Type::Err,
				}
			},
			TypeInfo::Deref(id) => {
				let to_deref = self.reconstruct(*id, span, diagnostics);
				match to_deref {
					Type::Ptr { to, .. } => *to,
					_ => Type::Err,
				}
			},
			TypeInfo::Fn { args, ret } => {
				let mut args = args
					.iter()
					.map(|&x| self.reconstruct(x, span, diagnostics))
					.collect::<Vec<_>>();
				let ret = Box::new(self.reconstruct(*ret, span, diagnostics));
				Type::Fn { args, ret }
			},
			TypeInfo::Void => Type::Void,
			TypeInfo::Ty(ty) => Type::TyRef(*ty),
			TypeInfo::Ptr { mutable, to } => Type::Ptr {
				mutable: *mutable,
				to: Box::new(self.reconstruct(*to, span, diagnostics)),
			},
			TypeInfo::Ref(id) => self.reconstruct(*id, span, diagnostics),
			TypeInfo::Int => Type::TyRef(self.inbuilts[&InbuiltType::Int(0)]),
			TypeInfo::Float => Type::TyRef(self.inbuilts[&InbuiltType::Float(32)]),
		}
	}

	pub fn fmt_type(&self, info: &TypeInfo) -> String {
		match info {
			TypeInfo::Unknown => "<unknown>".to_string(),
			TypeInfo::Void => "void".to_string(),
			TypeInfo::Fn { args, ret } => format!(
				"fn({}) -> {}",
				args.iter()
					.map(|x| self.fmt_type(self.get(*x)))
					.collect::<Vec<_>>()
					.join(", "),
				self.fmt_type(self.get(*ret))
			),
			TypeInfo::FnRet(id) => match self.get(*id) {
				TypeInfo::Fn { ret, .. } => self.fmt_type(self.get(*ret)),
				_ => unreachable!(),
			},
			TypeInfo::Deref(id) => match self.get(*id) {
				TypeInfo::Ptr { to, .. } => self.fmt_type(self.get(*to)),
				_ => unreachable!(),
			},
			TypeInfo::Ty(ty) => self.types[&ty].to_string(self.rodeo),
			TypeInfo::Ptr { mutable, to } => format!(
				"*{} {}",
				if *mutable { "mut" } else { "const" },
				self.fmt_type(&self.get(*to))
			),
			TypeInfo::Ref(id) => self.fmt_type(&self.get(*id)),
			TypeInfo::Int => "integer".to_string(),
			TypeInfo::Float => "float".to_string(),
		}
	}
}
