use std::collections::HashMap;

pub use ast;
use ast::{Ast, ItemKind, Module};
use diag::Diagnostics;
use intern::Id;
pub use syntax;
use syntax::{builder::TreeBuilderContext, intern::TextIntern, SyntaxNode};
use tracing::{span, Level};

use crate::{api::Api, graph::FileGraph, parse::Parser};

mod api;
mod graph;
mod helpers;
mod parse;
#[cfg(test)]
mod tests;

pub trait FileResolver {
	fn push_module(&mut self, module: Id<str>, interner: &mut TextIntern);

	fn pop_module(&mut self);

	fn resolve_module(&mut self, module: Id<str>, interner: &mut TextIntern) -> Option<(Id<str>, String)>;

	fn done_with_data(&mut self, file: Id<str>, data: String, interner: &mut TextIntern);
}

pub struct TreeContext<T> {
	builder: TreeBuilderContext<'static>,
	files: HashMap<Id<str>, (Cst, Ast)>,
	file_graph: FileGraph,
	resolver: T,
}

impl<T: FileResolver> TreeContext<T> {
	pub fn new(resolver: T) -> Self {
		Self {
			builder: TreeBuilderContext::new(),
			files: HashMap::new(),
			file_graph: FileGraph::new(),
			resolver,
		}
	}

	pub fn interner(&mut self) -> &mut TextIntern { self.builder.interner() }

	pub fn resolver(&mut self) -> &mut T { &mut self.resolver }

	pub fn finalize(self) -> (TextIntern, T) { (self.builder.finalize(), self.resolver) }

	pub fn parse_file(&mut self, db: &mut dyn ast::Db, file: Id<str>, source: &str) -> Diagnostics {
		let mut diags = Diagnostics::new();

		self.file_graph.add_file(file);
		self.parse_recursive_inner(db, file, source, &mut diags);

		diags
	}

	fn parse_recursive_inner(&mut self, db: &mut dyn ast::Db, file: Id<str>, source: &str, diags: &mut Diagnostics) {
		let cst = self.parse_inner(db, file, source, diags);
		for item in cst.to_module().items() {
			match item.kind() {
				Some(ItemKind::Mod(m)) if m.module().is_none() => {
					if let Some(ident) = m.ident() {
						if let Some((f, data)) = self.resolver.resolve_module(ident.name(), self.builder.interner()) {
							if !self.file_graph.get_children(file).contains(&f) {
								self.resolver.push_module(ident.name(), self.builder.interner());
								self.parse_recursive_inner(db, f, &data, diags);
								self.file_graph.add_child(file, f);
								self.resolver.pop_module();
							}

							self.resolver.done_with_data(f, data, self.builder.interner());
						} else {
							diags.push(
								ident
									.span()
									.error("could not find file for module")
									.label(ident.span().mark()),
							);
						}
					}
				},
				_ => {},
			}
		}
		let ast = Ast::new(db, file, cst.to_module());
		self.files.insert(file, (cst, ast));
	}

	fn parse_inner(&mut self, db: &mut dyn ast::Db, file: Id<str>, source: &str, diags: &mut Diagnostics) -> Cst {
		let s = span!(Level::TRACE, "parse", file = self.builder.resolve(file));
		let _e = s.enter();

		let parser = Parser {
			api: Api::new(file, source, &mut self.builder),
			diags,
			silent: false,
		};

		let builder = parser.parse();

		let cst = Cst {
			node: SyntaxNode::new_root(builder.finish()),
			file,
		};
		let ast = Ast::new(db, file, cst.to_module());
		self.files.insert(file, (cst.clone(), ast));

		cst
	}
}

#[derive(Clone)]
pub struct Cst {
	node: SyntaxNode,
	file: Id<str>,
}

impl Cst {
	pub fn to_module(&self) -> Module { Module::new(self.node.clone(), self.file) }
}
