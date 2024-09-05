use hir::ident::{AbsPath, PackageId};
use rustc_hash::FxHashMap;
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Id, Tracked};

use crate::{
	index::{Declaration, ModuleTree, NameTy, PackageTree},
	resolve::{GlobalResolution, GlobalResolver},
	VisiblePackages,
};

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum PreludeItem {
	Module(Id<ModuleTree>),
	Item { path: Id<AbsPath>, ty: NameTy },
}

#[derive(Tracked, Eq, PartialEq)]
pub struct Prelude {
	#[id]
	id: (),
	pub prelude: FxHashMap<Text, PreludeItem>,
}

#[derive(Tracked, Eq, PartialEq)]
pub struct PackagePrelude {
	#[id]
	pkg: PackageId,
	prelude: FxHashMap<Text, PreludeItem>,
}

#[query]
pub fn get_prelude(ctx: &Ctx, tree: Id<PackageTree>) -> Prelude {
	let s = span!(Level::DEBUG, "generate prelude");
	let _e = s.enter();

	// TODO: sort based on visible packages
	let t = ctx.get(tree);

	let mut prelude = FxHashMap::default();
	for (&pkg, &t) in t.packages.iter() {
		let p = get_prelude_from_package(ctx, pkg, t);
		let p = ctx.get(p);
		prelude.extend(p.prelude.iter().map(|(&k, &v)| (k, v)));
	}

	Prelude { id: (), prelude }
}

#[query]
pub fn get_prelude_from_package(ctx: &Ctx, pkg: PackageId, t: Id<ModuleTree>) -> PackagePrelude {
	let s = span!(Level::DEBUG, "generate package prelude", pkg = ?pkg);
	let _e = s.enter();

	let tree = ctx.get(t);
	let Some(index) = tree.index.and_then(|x| {
		let i = ctx.get(x);
		i.has_prelude.then_some(i)
	}) else {
		return PackagePrelude {
			pkg,
			prelude: FxHashMap::default(),
		};
	};

	// The prelude is resolved using only the current package.
	let packages = VisiblePackages {
		package: PackageId(0),
		packages: {
			let mut m = FxHashMap::default();
			m.insert(Text::new("root"), PackageId(0));
			m
		},
	};
	let ptree = PackageTree {
		id: (),
		packages: {
			let mut m = FxHashMap::default();
			m.insert(PackageId(0), t);
			m
		},
	};
	let resolver = GlobalResolver::new(ctx, tree.path, None, None, &packages, &ptree);
	let mut prelude = FxHashMap::default();
	for (&name, decl) in index.decls.iter() {
		match decl {
			Declaration::Import { path, is_prelude, .. } if *is_prelude => {
				let item = match resolver.resolve_prelude(t, path.clone()) {
					GlobalResolution::Module(m) => PreludeItem::Module(m),
					GlobalResolution::Item { path, ty, .. } => PreludeItem::Item { path, ty },
					GlobalResolution::Error => continue,
				};
				prelude.insert(name, item);
			},
			_ => {},
		}
	}

	PackagePrelude { pkg, prelude }
}
