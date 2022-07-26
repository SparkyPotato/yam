use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use name_resolve::resolved::{LocalRef, Pat, Ty};

use crate::{BinOp, InbuiltType, Lit, Rodeo, TyRef, Type, UnOp, ValRef};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TypeId(u32);

#[derive(Clone, Eq, PartialEq)]
pub enum TypeInfo {
	Ref(TypeId),
	Void,
	Never,
	Fn { args: Vec<TypeId>, ret: TypeId },
	Ty(TyRef),
	Ptr { mutable: bool, to: TypeId },
	FnRet(TypeId),
	Deref(TypeId),
	Int,
	Float,
	Unknown,
}

#[derive(Debug, Clone)]
pub struct Block {
	pub exprs: Vec<Expr>,
	pub ty: TypeId,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expr {
	pub kind: ExprKind,
	pub ty: TypeId,
	pub span: Span,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
	Lit(Lit),
	Block(Block),
	ValRef(ValRef),
	LocalRef(LocalRef),
	Let(Let),
	Cast(Cast),
	Call(Call),
	Unary(Unary),
	Binary(Binary),
	Break(Option<Box<Expr>>),
	Continue(Option<Box<Expr>>),
	Return(Option<Box<Expr>>),
	If(If),
	Loop(Loop),
	While(While),
	For(For),
	Err,
}

#[derive(Debug, Clone)]
pub struct Let {
	pub pat: Pat,
	pub expr: Option<Box<Expr>>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Cast {
	pub expr: Box<Expr>,
	pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct Call {
	pub target: Box<Expr>,
	pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Unary {
	pub op: UnOp,
	pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
	pub lhs: Box<Expr>,
	pub op: BinOp,
	pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
	pub cond: Box<Expr>,
	pub then: Block,
	pub else_: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Loop {
	pub block: Block,
	pub while_: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct While {
	pub cond: Box<Expr>,
	pub block: Block,
}

#[derive(Debug, Clone)]
pub struct For {
	pub pat: Pat,
	pub iter: Box<Expr>,
	pub block: Block,
}

pub struct TypeEngine<'a> {
	vars: Vec<TypeInfo>,
	types: &'a HashMap<TyRef, Ty>,
	inbuilts: &'a HashMap<InbuiltType, TyRef>,
	rodeo: &'a Rodeo,
}

impl<'a> TypeEngine<'a> {
	pub fn new(types: &'a HashMap<TyRef, Ty>, inbuilts: &'a HashMap<InbuiltType, TyRef>, rodeo: &'a Rodeo) -> Self {
		Self {
			vars: Vec::new(),
			types,
			inbuilts,
			rodeo,
		}
	}
}

impl TypeEngine<'_> {
	pub fn reset(&mut self) { self.vars.clear(); }

	pub fn insert(&mut self, info: TypeInfo) -> TypeId {
		let id = self.vars.len() as _;
		self.vars.push(info);
		TypeId(id)
	}

	pub fn get(&self, id: TypeId) -> &TypeInfo { &self.vars[id.0 as usize] }

	pub fn get_mut(&mut self, id: TypeId) -> &mut TypeInfo { &mut self.vars[id.0 as usize] }

	pub fn unify(&mut self, a: TypeId, a_span: Span, b: TypeId, b_span: Span, diagnostics: &mut Vec<Report<Span>>) {
		match (self.get(a), self.get(a)) {
			(a, b) if a == b => {},
			(TypeInfo::Ref(a), _) => self.unify(*a, a_span, b, b_span, diagnostics),
			(_, TypeInfo::Ref(b)) => self.unify(a, a_span, *b, b_span, diagnostics),
			(TypeInfo::Unknown, _) => *self.get_mut(a) = TypeInfo::Ref(b),
			(_, TypeInfo::Unknown) => *self.get_mut(b) = TypeInfo::Ref(a),
			(_, TypeInfo::Never) => {},
			(TypeInfo::Int, TypeInfo::Ty(x)) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Int(_))) => {
				*self.get_mut(a) = TypeInfo::Ref(b);
			},
			(TypeInfo::Ty(x), TypeInfo::Int) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Int(_))) => {
				*self.get_mut(b) = TypeInfo::Ref(a);
			},
			(TypeInfo::Float, TypeInfo::Ty(x)) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Float(_))) => {
				*self.get_mut(a) = TypeInfo::Ref(b);
			},
			(TypeInfo::Ty(x), TypeInfo::Float) if matches!(self.types[&x], Ty::Inbuilt(InbuiltType::Float(_))) => {
				*self.get_mut(b) = TypeInfo::Ref(a);
			},
			(
				TypeInfo::Ptr {
					mutable: false,
					to: to_a,
				},
				TypeInfo::Ptr { mutable: _, to: to_b },
			)
			| (
				TypeInfo::Ptr { mutable: _, to: to_a },
				TypeInfo::Ptr {
					mutable: false,
					to: to_b,
				},
			) => {
				let to_a = *to_a;
				let to_b = *to_b;

				*self.get_mut(a) = TypeInfo::Ptr {
					mutable: false,
					to: to_a,
				};
				*self.get_mut(b) = TypeInfo::Ptr {
					mutable: false,
					to: to_b,
				};
				self.unify(to_a, a_span, to_b, b_span, diagnostics);
			},
			(
				TypeInfo::Ptr {
					mutable: true,
					to: to_a,
				},
				TypeInfo::Ptr {
					mutable: true,
					to: to_b,
				},
			) => self.unify(*to_a, a_span, *to_b, b_span, diagnostics),
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

	pub fn reconstruct(&self, id: TypeId, span: Span, diagnostics: &mut Vec<Report<Span>>) -> Type {
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
			TypeInfo::Never => Type::Never,
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
				let ret = *ret;
				let args = args
					.iter()
					.map(|&x| self.reconstruct(x, span, diagnostics))
					.collect::<Vec<_>>();
				let ret = Box::new(self.reconstruct(ret, span, diagnostics));
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
			TypeInfo::Never => "!".to_string(),
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
