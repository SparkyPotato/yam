use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use name_resolve::{
	resolved::{Ctx, InbuiltType, Ty, TyRef},
	Rodeo,
};

use crate::typed::{Field, Ptr, Struct, Type};

pub mod typed;

type TypeId = u32;

#[derive(Copy, Clone)]
enum TypeInfo {
	Unknown,
	Ty(TyRef),
	Ptr { mutable: bool, to: TypeId },
	Ref(TypeId),
	Int,
	Float,
}

struct TypingEngine<'a> {
	vars: Vec<TypeInfo>,
	types: &'a HashMap<TyRef, Ty>,
	inbuilts: &'a HashMap<InbuiltType, TyRef>,
	rodeo: &'a Rodeo,
}

impl TypingEngine<'_> {
	fn reset(&mut self) { self.vars.clear(); }

	fn insert(&mut self, info: TypeInfo) -> TypeId {
		let id = self.vars.len() as _;
		self.vars.push(info);
		id
	}

	fn unify(&mut self, a: TypeId, a_span: Span, b: TypeId, b_span: Span, diagnostics: &mut Vec<Report<Span>>) {
		match (self.vars[a as usize], self.vars[b as usize]) {
			(TypeInfo::Ref(a), _) => self.unify(a, a_span, b, b_span, diagnostics),
			(_, TypeInfo::Ref(b)) => self.unify(a, a_span, b, b_span, diagnostics),
			(TypeInfo::Unknown, _) => self.vars[a as usize] = TypeInfo::Ref(b),
			(_, TypeInfo::Unknown) => self.vars[b as usize] = TypeInfo::Ref(a),
			(TypeInfo::Int, TypeInfo::Int) => {},
			(TypeInfo::Float, TypeInfo::Float) => {},
			(TypeInfo::Int, TypeInfo::Ty(x)) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Int(_))) => {
				self.vars[a as usize] = TypeInfo::Ref(b)
			},
			(TypeInfo::Ty(x), TypeInfo::Int) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Int(_))) => {
				self.vars[b as usize] = TypeInfo::Ref(a)
			},
			(TypeInfo::Float, TypeInfo::Ty(x)) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Float(_))) => {
				self.vars[a as usize] = TypeInfo::Ref(b)
			},
			(TypeInfo::Ty(x), TypeInfo::Float) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Float(_))) => {
				self.vars[b as usize] = TypeInfo::Ref(a)
			},
			(
				TypeInfo::Ptr {
					mutable: false,
					to: to_a,
				},
				TypeInfo::Ptr { mutable: _, to: to_b },
			) => self.unify(to_a, a_span, to_b, b_span, diagnostics),
			(
				TypeInfo::Ptr {
					mutable: true,
					to: to_a,
				},
				TypeInfo::Ptr {
					mutable: true,
					to: to_b,
				},
			) => self.unify(to_a, a_span, to_b, b_span, diagnostics),
			(a, b) => diagnostics.push(
				a_span
					.report(ReportKind::Error)
					.with_message("cannot assign")
					.with_label(Label::new(a_span).with_message(format!("this has type `{}`", self.fmt_type(a))))
					.with_label(Label::new(b_span).with_message(format!("this has type `{}`", self.fmt_type(b))))
					.finish(),
			),
		}
	}

	fn reconstruct(&mut self, id: TypeId, span: Span, diagnostics: &mut Vec<Report<Span>>) -> Type {
		match self.vars[id as usize] {
			TypeInfo::Unknown => {
				diagnostics.push(
					span.report(ReportKind::Error)
						.with_message("cannot infer")
						.with_label(Label::new(span).with_message("consider giving this an explicit type"))
						.finish(),
				);
				Type::Err
			},
			TypeInfo::Ty(ty) => Type::TyRef(ty),
			TypeInfo::Ptr { mutable, to } => Type::Ptr(Ptr {
				mutability: mutable,
				to: Box::new(self.reconstruct(to, span, diagnostics)),
			}),
			TypeInfo::Ref(id) => self.reconstruct(id, span, diagnostics),
			TypeInfo::Int => Type::TyRef(self.inbuilts[&InbuiltType::Int(0)]),
			TypeInfo::Float => Type::TyRef(self.inbuilts[&InbuiltType::Float(32)]),
		}
	}

	fn fmt_type(&self, info: TypeInfo) -> String {
		match info {
			TypeInfo::Unknown => "<unknown>".to_string(),
			TypeInfo::Ty(ty) => self.types[&ty].to_string(self.rodeo),
			TypeInfo::Ptr { mutable, to } => format!(
				"*{} {}",
				if mutable { "mut" } else { "const" },
				self.fmt_type(self.vars[to as usize])
			),
			TypeInfo::Ref(id) => self.fmt_type(self.vars[id as usize]),
			TypeInfo::Int => "integer".to_string(),
			TypeInfo::Float => "float".to_string(),
		}
	}
}

pub fn type_check(ctx: Ctx, rodeo: &Rodeo, diagnostics: &mut Vec<Report<Span>>) -> typed::Ctx {
	let mut solver = TypeSolver {
		engine: TypingEngine {
			vars: Vec::new(),
			types: &ctx.types,
			inbuilts: &ctx.inbuilt_types,
			rodeo,
		},
		diagnostics,
	};


}

struct TypeSolver<'a> {
	engine: TypingEngine<'a>,
	diagnostics: &'a mut Vec<Report<Span>>,
}

impl TypeSolver<'_> {
	fn solve_ty(&mut self, ty: Ty) -> typed::Ty {
		match ty {
			Ty::Inbuilt(i) => typed::Ty::Inbuilt(i),
			Ty::Struct(s) => typed::Ty::Struct(Struct {
				path: s.path,
				fields: s.fields.into_iter().map(|x| Field {
					name: x.name,
					ty: ,
					span: x.span,
				}).collect(),
				span: s.span,
			}),
		}
	}
}
