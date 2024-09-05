use std::fmt::Debug;

use text::Text;
use verde::{Db, Id, Interned};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct PackageId(pub u32);

/// An absolute path.
#[derive(Interned, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AbsPath {
	Package(PackageId),
	Name { prec: Id<AbsPath>, name: Text },
}

impl AbsPath {
	pub fn segments(db: &dyn Db, mut path: Id<Self>) -> (PackageId, Vec<Text>) {
		let mut names = Vec::new();
		loop {
			match *db.geti(path) {
				Self::Package(id) => return (id, names),
				Self::Name { prec, name } => {
					path = prec;
					names.push(name);
				},
			}
		}
	}
}

impl From<PackageId> for AbsPath {
	fn from(x: PackageId) -> Self { Self::Package(x) }
}

pub trait DebugAbsPath {
	type Output<'a>: Debug;

	fn debug<'a>(self, db: &'a dyn Db) -> Self::Output<'a>;
}

impl DebugAbsPath for Id<AbsPath> {
	type Output<'a> = DebugAbsPathStruct<'a>;

	fn debug<'a>(self, db: &'a dyn Db) -> Self::Output<'a> { DebugAbsPathStruct(self, db) }
}

pub struct DebugAbsPathStruct<'a>(Id<AbsPath>, &'a dyn Db);

impl<'a> Debug for DebugAbsPathStruct<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match *self.1.geti(self.0) {
			AbsPath::Package(x) => write!(f, "#{}", x.0),
			AbsPath::Name { prec, name } => write!(f, "{:?}.{}", prec.debug(self.1), name.as_str()),
		}
	}
}
