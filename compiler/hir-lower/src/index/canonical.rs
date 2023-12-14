use std::collections::hash_map::Entry;

use diagnostics::Span;
use hir::ident::{AbsPath, PackageId};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::ast;
use text::Text;
use tracing::{span, Level};
use verde::{query, Ctx, Id, Tracked};

use crate::{
	index::{
		local,
		local::{PackageTree, RelPath, TempName},
		ErasedTempId,
		NameTy,
		TempId,
	},
	Packages,
	VisiblePackages,
};

#[query]
pub fn canonicalize_tree(ctx: &Ctx, tree: Id<PackageTree>, packages: Id<Packages>) -> CanonicalTree {
	let s = span!(Level::DEBUG, "canonicalize package tree");
	let _e = s.enter();

	let t = ctx.get(tree);
	let packages = ctx.get(packages);

	let mut sub = FxHashMap::default();
	let packages: FxHashMap<_, _> = t
		.packages
		.iter()
		.map(|(&pkg, &t)| {
			let t = canonicalize_module_tree(
				ctx,
				t,
				*packages.packages.get(&pkg).expect("Package not found in package list"),
				tree,
			);
			sub.insert(ctx.get(t).path, t);
			(pkg, t)
		})
		.collect();

	CanonicalTree {
		id: (),
		modules: packages
			.values()
			.flat_map(|&x| ctx.get(x).sub.clone())
			.chain(sub)
			.collect(),
		packages,
	}
}

#[query]
pub fn canonicalize_module_tree(
	ctx: &Ctx, tree_id: Id<local::ModuleTree>, visible: Id<VisiblePackages>, ptree: Id<PackageTree>,
) -> ModuleTree {
	let tree = ctx.get(tree_id);

	let mut public = CanonicalIndex::new(tree.path, true);
	let mut private = CanonicalIndex::new(tree.path, false);

	let mut sub = FxHashMap::default();
	for (&name, &tree) in tree.children.iter() {
		let tree = canonicalize_module_tree(ctx, tree, visible, ptree);
		private.names.insert(name, Declaration::Module(tree));
		sub.insert(ctx.get(tree).path, tree);
	}

	let (public, private) = if let Some(index) = tree.index {
		let index = ctx.get(index);
		let p = ctx.get(index.public);

		for (&name, decl) in index.decls.iter() {
			match decl {
				local::Declaration::Name { ty, id } => {
					let decl = Declaration::Name {
						path: ctx.add(AbsPath::Name { prec: index.path, name }),
						ty: *ty,
						id: *id,
					};

					match private.names.entry(name) {
						Entry::Vacant(v) => {
							v.insert(decl);
							if p.names.contains(&name) {
								public.names.insert(name, decl);
							}
						},
						Entry::Occupied(_) => {
							let span = id.erased();
							ctx.push(
								span.error("item name conflicts with submodule")
									.label(span.label(format!("conflicts with submodule `{}`", name.as_str()))),
							);
						},
					}
				},
				_ => {},
			}
		}

		let mut resolver = Resolver {
			ctx,
			us: tree_id,
			public,
			private,
			visible,
			ptree,
			error_set: FxHashSet::default(),
		};

		for (&name, decl) in index.decls.iter() {
			match decl {
				local::Declaration::Import { path, .. } => {
					let Some(decl) = resolver.resolve(path) else {
						continue;
					};

					resolver.private.names.insert(name, decl);
					if p.names.contains(&name) {
						resolver.public.names.insert(name, decl);
					}
				},
				_ => {},
			}
		}

		resolver.finish()
	} else {
		(public, private)
	};

	ModuleTree {
		path: tree.path,
		public: ctx.insert(public),
		private: ctx.insert(private),
		sub,
	}
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Declaration {
	Name {
		path: Id<AbsPath>,
		ty: NameTy,
		id: TempId<ast::Name>,
	},
	Module(Id<ModuleTree>),
}

#[derive(Tracked, Eq, PartialEq)]
pub struct CanonicalIndex {
	#[id]
	path: (Id<AbsPath>, bool),
	pub names: FxHashMap<Text, Declaration>,
}

impl CanonicalIndex {
	pub fn new(path: Id<AbsPath>, public: bool) -> Self {
		Self {
			path: (path, public),
			names: FxHashMap::default(),
		}
	}
}

#[derive(Tracked, Eq, PartialEq)]
pub struct CanonicalTree {
	#[id]
	id: (),
	pub packages: FxHashMap<PackageId, Id<ModuleTree>>,
	pub modules: FxHashMap<Id<AbsPath>, Id<ModuleTree>>,
}

#[derive(Tracked, Eq, PartialEq)]
pub struct ModuleTree {
	#[id]
	pub path: Id<AbsPath>,
	pub public: Id<CanonicalIndex>,
	pub private: Id<CanonicalIndex>,
	sub: FxHashMap<Id<AbsPath>, Id<ModuleTree>>,
}

// TODO: Contextually group unresolved import errors into a single error to avoid error spam.

struct Resolver<'a> {
	ctx: &'a Ctx<'a>,
	us: Id<local::ModuleTree>,
	public: CanonicalIndex,
	private: CanonicalIndex,
	visible: Id<VisiblePackages>,
	ptree: Id<PackageTree>,
	error_set: FxHashSet<ErasedTempId>,
}

impl Resolver<'_> {
	fn resolve(&mut self, path: &RelPath) -> Option<Declaration> {
		match path.dot {
			Some(_) => self.resolve_from_root(&path.names),
			None => self.resolve_from_current(&path.names),
		}
	}

	fn resolve_from_current(&mut self, path: &[TempName]) -> Option<Declaration> {
		let mut names = path.iter();
		let mut prev = names.next()?; // There's always atleast one name.

		let Some(&decl) = self.private.names.get(&prev.name) else {
			let span = prev.id.erased();
			if !self.error_set.contains(&span) {
				self.ctx.push(
					span.error("unknown submodule")
						.label(span.label("404: this submodule does not exist")),
				);
				self.error_set.insert(span);
			}
			return None;
		};

		let mut decl = decl;
		for name in names {
			match decl {
				Declaration::Module(tree) => {
					let tree = self.ctx.get(tree);
					let index = self.ctx.get(tree.public);
					decl = match index.names.get(&name.name) {
						Some(&decl) => decl,
						None => {
							let span = name.id.erased();
							if !self.error_set.contains(&span) {
								let private = self.ctx.get(tree.private);
								self.ctx.push(if private.names.contains_key(&name.name) {
									span.error("cannot import private name")
										.label(span.label("this is private"))
								} else {
									span.error("unknown import").label(
										span.label(format!("404: name does not exist in `{}`", prev.name.as_str())),
									)
								});
								self.error_set.insert(span);
							}
							return None;
						},
					};
				},
				Declaration::Name { id, .. } => {
					let span = prev.id.erased();
					if !self.error_set.contains(&span) {
						self.ctx.push(
							span.error("cannot import from item")
								.label(name.id.erased().label("tried importing from item here"))
								.label(span.label("this is an item"))
								.label(id.erased().label("item defined here")),
						);
						self.error_set.insert(span);
					}
					return None;
				},
			}

			prev = name;
		}

		Some(decl)
	}

	fn resolve_from_root(&mut self, path: &[TempName]) -> Option<Declaration> {
		let mut names = path.iter();
		let first = names.next()?; // There's always atleast one name.
		let span = first.id.erased();
		if !self.error_set.contains(&span) {
			self.ctx
				.push(span.error("root imports are unsupported").label(span.mark()));
			self.error_set.insert(span);
		}

		None

		// let visible = self.ctx.get(self.visible);
		// let Some(pkg) = visible.packages.get(&first.name) else {
		// 	let span = first.id.erased();
		// 	if !self.error_set.contains(&span) {
		// 		self.ctx.push(
		// 			span.error("unknown package")
		// 				.label(span.label("404: this package does not exist")),
		// 		);
		// 		self.error_set.insert(span);
		// 	}
		// 	return None;
		// };
		// let ptree = self.ctx.get(self.ptree);
		// let mut tree = *ptree
		// 	.packages
		// 	.get(pkg)
		// 	.expect("Invalid package ID in visibile packages");
		//
		// let mut prev = first.name;
		// while let Some(name) = names.next() {
		// 	if tree == self.us {
		// 		let span = name.id.erased();
		// 		if !self.error_set.contains(&span) {
		// 			// TODO: Allow this.
		// 			self.ctx.push(
		// 				span.error("cannot use root import through the current module")
		// 					.label(span.label(format!("try importing directly with `{}...`", name.name.as_str()))),
		// 			);
		// 			self.error_set.insert(span);
		// 		}
		// 		return None;
		// 	}
		//
		// 	let got = self.ctx.get(tree);
		// 	if let Some(id) = got.index {
		// 		let index = self.ctx.get(id);
		// 	}
		//
		// 	tree =
		// 		match got.children.get(&name.name) {
		// 			Some(&tree) => tree,
		// 			None => {
		// 				let span = name.id.erased();
		// 				if !self.error_set.contains(&span) {
		// 					self.ctx.push(span.error("unknown submodule").label(
		// 						span.label(format!("404: this submodule does not exist in `{}`", prev.as_str())),
		// 					));
		// 					self.error_set.insert(span);
		// 				}
		// 				return None;
		// 			},
		// 		};
		//
		// 	prev = name.name;
		// }
		//
		// return None;
	}

	fn finish(self) -> (CanonicalIndex, CanonicalIndex) { (self.public, self.private) }
}
