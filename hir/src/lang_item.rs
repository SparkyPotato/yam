use std::fmt::{Debug, Display, Formatter};

use diag::Diagnostics;
use id::Id;

use crate::{hir::ExprKind, Rodeo, Spur, ValDef, ValDefKind, ValRef};

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum LangItem {
	U8,
	U16,
	U32,
	U64,
	Usize,
	I8,
	I16,
	I32,
	I64,
	Isize,
	F32,
	F64,
	Bool,
	Void,
}

impl Id for LangItem {
	fn from_id(id: u32) -> Self { Self::all()[id as usize] }

	fn id(self) -> u32 { self as u32 }
}

impl LangItem {
	pub fn all() -> &'static [LangItem] {
		&[
			LangItem::U8,
			LangItem::U16,
			LangItem::U32,
			LangItem::U64,
			LangItem::Usize,
			LangItem::I8,
			LangItem::I16,
			LangItem::I32,
			LangItem::I64,
			LangItem::Isize,
			LangItem::F32,
			LangItem::F64,
			LangItem::Bool,
			LangItem::Void,
		]
	}

	pub fn verify_item(self, def: &ValDef, this: ValRef, diags: &mut Diagnostics) {
		match self {
			LangItem::U8
			| LangItem::U16
			| LangItem::U32
			| LangItem::U64
			| LangItem::Usize
			| LangItem::I8
			| LangItem::I16
			| LangItem::I32
			| LangItem::I64
			| LangItem::Isize
			| LangItem::F32
			| LangItem::F64
			| LangItem::Bool
			| LangItem::Void => match &def.kind {
				ValDefKind::Const(l) => {
					if let Some(ty) = l.ty_expr.as_ref() {
						if !matches!(ty.kind, ExprKind::Type) {
							diags.push(
								ty.span
									.error("inbuilt type lang items must have type `type`")
									.label(ty.span.label("change this to `type`")),
							);
						}
					}

					if !matches!(l.expr.kind, ExprKind::ValRef(v) if v == this) {
						diags.push(
							l.expr
								.span
								.error("inbuilt type lang items must be initialized by themselves")
								.label(l.expr.span.label(format!("change this to `{}`", self))),
						)
					}
				},
				_ => diags.push(
					def.span
						.error("inbuilt type lang items must be `const`s")
						.label(def.span.mark()),
				),
			},
		}
	}
}

impl Debug for LangItem {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl Display for LangItem {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				LangItem::U8 => "u8",
				LangItem::U16 => "u16",
				LangItem::U32 => "u32",
				LangItem::U64 => "u64",
				LangItem::Usize => "usize",
				LangItem::I8 => "i8",
				LangItem::I16 => "i16",
				LangItem::I32 => "i32",
				LangItem::I64 => "i64",
				LangItem::Isize => "isize",
				LangItem::F32 => "f32",
				LangItem::F64 => "f64",
				LangItem::Bool => "bool",
				LangItem::Void => "void",
			}
		)
	}
}

pub struct LangItemIdents {
	pub lang: Spur,
	pub u8: Spur,
	pub u16: Spur,
	pub u32: Spur,
	pub u64: Spur,
	pub usize: Spur,
	pub i8: Spur,
	pub i16: Spur,
	pub i32: Spur,
	pub i64: Spur,
	pub isize: Spur,
	pub f32: Spur,
	pub f64: Spur,
	pub bool: Spur,
	pub void: Spur,
}

impl LangItemIdents {
	pub fn new(rodeo: &mut Rodeo) -> Self {
		Self {
			lang: rodeo.get_or_intern("lang"),
			u8: rodeo.get_or_intern("u8"),
			u16: rodeo.get_or_intern("u16"),
			u32: rodeo.get_or_intern("u32"),
			u64: rodeo.get_or_intern("u64"),
			usize: rodeo.get_or_intern("usize"),
			i8: rodeo.get_or_intern("i8"),
			i16: rodeo.get_or_intern("i16"),
			i32: rodeo.get_or_intern("i32"),
			i64: rodeo.get_or_intern("i64"),
			isize: rodeo.get_or_intern("isize"),
			f32: rodeo.get_or_intern("f32"),
			f64: rodeo.get_or_intern("f64"),
			bool: rodeo.get_or_intern("bool"),
			void: rodeo.get_or_intern("void"),
		}
	}

	pub fn resolve_lang_item(&self, tok: Spur) -> Option<LangItem> {
		Some(match tok {
			x if x == self.u8 => LangItem::U8,
			x if x == self.u16 => LangItem::U16,
			x if x == self.u32 => LangItem::U32,
			x if x == self.u64 => LangItem::U64,
			x if x == self.usize => LangItem::Usize,
			x if x == self.i8 => LangItem::I8,
			x if x == self.i16 => LangItem::I16,
			x if x == self.i32 => LangItem::I32,
			x if x == self.i64 => LangItem::I64,
			x if x == self.isize => LangItem::Isize,
			x if x == self.f32 => LangItem::F32,
			x if x == self.f64 => LangItem::F64,
			x if x == self.bool => LangItem::Bool,
			x if x == self.void => LangItem::Void,
			_ => return None,
		})
	}
}
