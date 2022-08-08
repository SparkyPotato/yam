use id::Id;

use crate::ValRef;

#[derive(Clone, Eq, PartialEq)]
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
	Err,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TypeId(u32);

impl Id for TypeId {
	fn from_id(id: u32) -> Self { Self(id) }

	fn id(self) -> u32 { self.0 }
}
