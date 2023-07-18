use hir::ident::AbsPath;
use rustc_hash::FxHashMap;
use syntax::ast;
use text::Text;
use verde::{query, Ctx, Db, Id, Tracked};

use crate::{
	index::Index,
	module::{Module, ModuleTree},
};

pub fn get_hir_sea(db: &dyn Db, modules: impl IntoIterator<Item = Id<LoweredModule>>) -> Vec<Id<hir::Item>> {
	let mut v = Vec::new();
	for module in modules {
		let module = db.get(module);
		for &item in module.items.iter() {
			v.push(item);
		}
	}
	v
}

#[derive(Tracked)]
pub struct LoweredModule {
	#[id]
	module: Id<AbsPath>,
	items: Vec<Id<hir::Item>>,
}

impl Eq for LoweredModule {}
impl PartialEq for LoweredModule {
	fn eq(&self, _: &Self) -> bool {
		// We don't care about actually comparing each item, since this struct is just a dummy struct to generate the
		// real HIR sea.
		false
	}
}

#[query]
pub fn lower_to_hir(ctx: &Ctx, module: Id<Module>, vis_map: Id<VisibilityMap>) -> LoweredModule {
	let module = ctx.get(module);
	let items = module
		.ast
		.items()
		.into_iter()
		.map(|x| {
			let item = lower_item(ctx, x);
			ctx.insert(item)
		})
		.collect();

	LoweredModule {
		module: module.path,
		items,
	}
}

fn lower_item(ctx: &Ctx, item: ast::Item) -> hir::Item { todo!() }

/// A map from paths visibile to a module to the indices that they point to.
#[derive(Tracked, Eq, PartialEq)]
pub struct VisibilityMap {
	#[id]
	module: Id<AbsPath>,
	modules: FxHashMap<Text, VisibleModule>,
}

#[derive(Eq, PartialEq)]
struct VisibleModule {
	index: Id<Index>,
	children: FxHashMap<Text, VisibleModule>,
}

impl VisibilityMap {
	pub fn new(ctx: &dyn Db, tree: &ModuleTree, module: Id<Module>) -> Id<Self> {
		for (pkg, tree) in tree.packages.iter() {}
		todo!()
	}
}
