use crate::ValRef;

#[derive(Debug, Clone)]
pub enum Type {
	Void,
	Never,
	Type,
	Tuple(Vec<Type>),
	Fn { args: Vec<Type>, ret: Box<Type> },
	Ty(ValRef),
	Ptr { mutable: bool, to: Box<Type> },
	Unresolved(TypeId),
	Unknown,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TypeId(u32);

impl TypeId {
	pub fn new(id: u32) -> Self { TypeId(id) }

	pub fn id(self) -> u32 { self.0 }
}
