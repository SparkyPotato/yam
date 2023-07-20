use text::Text;
use verde::{Id, Interned};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct PackageId(pub u32);

/// An absolute path.
#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AbsPath {
	Package(PackageId),
	Module { prec: Id<AbsPath>, name: Text },
}

impl From<PackageId> for AbsPath {
	fn from(x: PackageId) -> Self { Self::Package(x) }
}
