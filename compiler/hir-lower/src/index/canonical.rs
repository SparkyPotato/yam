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
	is_child_of,
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

	if let Some(index) = tree.index {
		let index = ctx.get(index);
		let p = ctx.get(index.public);

		for (&name, decl) in index.decls.iter() {
			match *decl {
				local::Declaration::Name { path, ty, id } => {
					let decl = Declaration::Name { path, ty, id };

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
			path: tree.path,
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

		(public, private) = resolver.finish();
	}

	ModuleTree {
		path: tree.path,
		public: ctx.insert(public),
		private: ctx.insert(private),
		sub,
	}
}

pub enum Declaration<M> {
	Name {
		path: Id<AbsPath>,
		ty: NameTy,
		id: TempId<ast::Name>,
	},
	Module(Id<M>),
}

impl<M> Copy for Declaration<M> {}
impl<M> Clone for Declaration<M> {
	fn clone(&self) -> Self { *self }
}
impl<M> PartialEq for Declaration<M> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(
				Declaration::Name { path, ty, id },
				Declaration::Name {
					path: p1,
					ty: ty1,
					id: id1,
				},
			) => path == p1 && ty == ty1 && id == id1,
			(Declaration::Module(m1), Declaration::Module(m2)) => m1 == m2,
			(..) => false,
		}
	}
}
impl<M> Eq for Declaration<M> {}

#[derive(Tracked, Eq, PartialEq)]
pub struct CanonicalIndex {
	#[id]
	path: (Id<AbsPath>, bool),
	pub names: FxHashMap<Text, Declaration<ModuleTree>>,
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
	path: Id<AbsPath>,
	public: CanonicalIndex,
	private: CanonicalIndex,
	visible: Id<VisiblePackages>,
	ptree: Id<PackageTree>,
	error_set: FxHashSet<ErasedTempId>,
}

impl Resolver<'_> {
	fn resolve(&mut self, path: &RelPath) -> Option<Declaration<ModuleTree>> {
		match path.dot {
			Some(_) => self.resolve_from_root(path.names.iter().copied()),
			None => self.resolve_from_current(path.names.iter().copied()),
		}
	}

	fn resolve_from_current(&mut self, path: impl IntoIterator<Item = TempName>) -> Option<Declaration<ModuleTree>> {
		let mut names = path.into_iter();
		let mut prev = names.next()?; // There's always atleast one name.

		let Some(&decl) = self.private.names.get(&prev.name) else {
			let span = prev.id.erased();
			if self.error_set.insert(span) {
				self.ctx.push(
					span.error("unknown submodule")
						.label(span.label("404: this submodule does not exist")),
				);
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
							if self.error_set.insert(span) {
								let private = self.ctx.get(tree.private);
								self.ctx.push(if private.names.contains_key(&name.name) {
									span.error("cannot import private name")
										.label(span.label("this is private"))
								} else {
									span.error("unknown import").label(
										span.label(format!("404: name does not exist in `{}`", prev.name.as_str())),
									)
								});
							}
							return None;
						},
					};
				},
				Declaration::Name { id, .. } => {
					let span = prev.id.erased();
					if self.error_set.insert(span) {
						self.ctx.push(
							span.error("cannot import from item")
								.label(name.id.erased().label("tried importing from item here"))
								.label(span.label("this is an item"))
								.label(id.erased().label("item defined here")),
						);
					}
					return None;
				},
			}

			prev = name;
		}

		Some(decl)
	}

	fn resolve_from_root(&mut self, path: impl IntoIterator<Item = TempName>) -> Option<Declaration<ModuleTree>> {
		let mut names = path.into_iter();
		let mut prev = names.next()?; // There's always atleast one name.

		let visible = self.ctx.get(self.visible);
		let Some(pkg) = visible.packages.get(&prev.name) else {
			let span = prev.id.erased();
			if self.error_set.insert(span) {
				self.ctx.push(
					span.error("unknown package")
						.label(span.label("404: this package does not exist")),
				);
			}
			return None;
		};
		let ptree = self.ctx.get(self.ptree);
		let tree = *ptree
			.packages
			.get(pkg)
			.expect("Invalid package ID in visibile packages");

		let mut decl = Declaration::Module(tree);
		while let Some(name) = names.next() {
			match decl {
				Declaration::Module(tree) => {
					if tree == self.us {
						return self.resolve_from_current(std::iter::once(name).chain(names));
					}

					let got = self.ctx.get(tree);
					if let Some(id) = got.index {
						let index = self.ctx.get(id);
						let can_access_priv = is_child_of(self.ctx, got.path, self.path);
						let is_public = (!can_access_priv)
							.then(|| self.ctx.get(index.public).names.contains(&name.name))
							.unwrap_or(false);
						let ldecl = index.decls.get(&name.name);
						if !can_access_priv && !is_public && ldecl.is_some() {
							let span = name.id.erased();
							if self.error_set.insert(span) {
								self.ctx.push(
									span.error("cannot import private name")
										.label(span.label("this is private")),
								);
							}
							return None;
						}
						decl = if let Some(decl) = ldecl {
							match *decl {
								local::Declaration::Name { path, ty, id } => Declaration::Name { path, ty, id },
								local::Declaration::Import { ref path, rename } => {
									let re_span = rename
										.map(|x| x.erased())
										.unwrap_or_else(|| path.names.last().unwrap().id.erased());
									let span = name.id.erased();
									if self.error_set.insert(span) {
										self.ctx.push(
											span.error("importing re-exports is not supported yet")
												.label(span.label("this is a re-export"))
												.label(re_span.label("re-exported here")),
										);
									}
									return None;
								},
							}
						} else {
							match got.children.get(&name.name) {
								Some(&tree) => Declaration::Module(tree),
								None => {
									let span = name.id.erased();
									if self.error_set.insert(span) {
										self.ctx.push(span.error("unknown name").label(span.label(format!(
											"404: this name does not exist in `{}`",
											prev.name.as_str()
										))));
									}
									return None;
								},
							}
						}
					}
				},
				Declaration::Name { id, .. } => {
					let span = prev.id.erased();
					if self.error_set.insert(span) {
						self.ctx.push(
							span.error("cannot import from item")
								.label(name.id.erased().label("tried importing from item here"))
								.label(span.label("this is an item"))
								.label(id.erased().label("item defined here")),
						);
					}
					return None;
				},
			}

			prev = name;
		}

		Some(match decl {
			Declaration::Name { path, ty, id } => Declaration::Name { path, ty, id },
			Declaration::Module(_) => unreachable!(),
		})
	}

	fn finish(self) -> (CanonicalIndex, CanonicalIndex) { (self.public, self.private) }
}

