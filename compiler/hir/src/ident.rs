use text::Text;
use verde::{Id, Interned};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct PackageId(pub u32);

/// An absolute path.
#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub struct AbsPath {
	pub package: PackageId,
	/// May be `None` if this is the root module.
	pub path: Option<Id<InnerPath>>,
}

/// A relative path.
#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InnerPath {
	pub prec: Option<Id<Self>>,
	pub name: Text,
}
