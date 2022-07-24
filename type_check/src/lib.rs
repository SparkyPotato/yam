use std::collections::HashMap;

use diag::{ariadne::Report, Span};
use name_resolve::{resolved::Ctx, Rodeo};

pub mod typed;

type TypeId = u32;

#[derive(Debug, Clone)]
struct PtrInfo {
	pub mutability: bool,
	pub to: TypeId,
}

enum TypeInfo {
	Unknown,
	Ref(TypeId),
	Int,
	Uint,
	Float,
}

struct TypingEngine {
	next_id: u32,
	vars: HashMap<TypeId, TypeInfo>,
}

impl TypingEngine {
	fn reset(&mut self) {
		self.next_id = 0;
		self.vars.clear();
	}
}

pub fn type_check(ctx: Ctx, rodeo: &mut Rodeo, diagnostics: &mut Vec<Report<Span>>) -> typed::Ctx {}
