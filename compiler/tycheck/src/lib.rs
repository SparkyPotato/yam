use arena::{dense::DenseMap, Ix};
use diagnostics::Span;
use hir::{
	ast::ErasedAstId,
	ident::{AbsPath, DebugAbsPath},
	lang_item::LangItemMap,
	LangItem,
};
use rustc_hash::FxHashMap;
use tracing::{span, Level};
use verde::{query, storage, Ctx, Id};

use crate::{
	reader::HirReader,
	solver::{Partial, PartialType, TypeSolver},
};

pub mod decl;
mod reader;
mod solver;

#[storage]
pub struct Storage(decl::type_decl, type_check);

#[query]
pub fn type_check(
	ctx: &Ctx, item: Id<hir::Item>, decl: Id<thir::ItemDecl>, lang_items: Id<LangItemMap>,
	#[ignore] items: &FxHashMap<Id<AbsPath>, Id<hir::Item>>,
	#[ignore] decls: &FxHashMap<Id<AbsPath>, Id<thir::ItemDecl>>,
) -> thir::Item {
	let item = ctx.get(item);

	let s = span!(Level::DEBUG, "typecheck", path=?item.path.debug(ctx));
	let _e = s.enter();

	let d = ctx.get(decl);

	let lang_items = ctx.get(lang_items);
	let mut ck = Checker::new(ctx, &*item, &*d, &*lang_items, items, decls);

	match item.kind {
		hir::ItemKind::Fn(ref f) => {
			ck.fn_(f);
		},
		hir::ItemKind::Static(ref s) => {
			ck.static_(s);
		},
		_ => {},
	}
	let (exprs, locals) = ck.finish();

	thir::Item {
		path: item.path,
		decl,
		locals,
		exprs,
	}
}

struct Checker<'a> {
	ctx: &'a Ctx<'a>,
	decl: &'a thir::ItemDecl,
	solver: TypeSolver,
	reader: HirReader<'a>,
	decls: &'a FxHashMap<Id<AbsPath>, Id<thir::ItemDecl>>,
	exprs_out: DenseMap<hir::Expr, Ix<Partial>>,
	locals_out: DenseMap<hir::Local, Ix<Partial>>,
	void: Id<thir::Type>,
	bool: Id<thir::Type>,
	char: Id<thir::Type>,
}

impl<'a> Checker<'a> {
	fn new(
		ctx: &'a Ctx<'a>, item: &'a hir::Item, decl: &'a thir::ItemDecl, lang_items: &'a LangItemMap,
		items: &'a FxHashMap<Id<AbsPath>, Id<hir::Item>>, decls: &'a FxHashMap<Id<AbsPath>, Id<thir::ItemDecl>>,
	) -> Self {
		Self {
			ctx,
			decl,
			solver: TypeSolver::new(),
			reader: HirReader::new(&item, lang_items, items),
			decls,
			exprs_out: DenseMap::with_capacity(item.exprs.len()),
			locals_out: DenseMap::with_capacity(item.locals.len()),
			void: ctx.add(thir::Type::Void),
			bool: ctx.add(thir::Type::LangItem(LangItem::Bool)),
			char: ctx.add(thir::Type::LangItem(LangItem::Char)),
		}
	}

	fn finish(
		mut self,
	) -> (
		DenseMap<hir::Expr, Id<thir::Type>>,
		DenseMap<hir::Local, Id<thir::Type>>,
	) {
		self.solver.solve(self.ctx, self.reader.items, self.decls);
		let mut exprs = DenseMap::with_capacity(self.reader.exprs.len());
		for (id, &expr) in self.exprs_out.iter() {
			exprs.insert(id, self.solver.get(self.ctx, expr));
		}
		let mut locals = DenseMap::with_capacity(self.reader.locals.len());
		for (id, &local) in self.locals_out.iter() {
			locals.insert(id, self.solver.get(self.ctx, local));
		}
		(exprs, locals)
	}

	fn fn_(&mut self, f: &hir::Fn) {
		let (ret, span) = match self.decl.kind {
			thir::ItemDeclKind::Fn(ref d) => (d.ret, f.ret.map(|x| self.reader.types[x].id.erased())),
			_ => unreachable!("fn to non-fn"),
		};
		f.body.as_ref().map(|body| {
			let ty = self.block(body);
			let ret = self.solver.concrete(ret, span);
			self.solver.unify(ret, ty);
		});
	}

	fn static_(&mut self, s: &hir::Static) {
		let ty = match self.decl.kind {
			thir::ItemDeclKind::Static(ref s) => s.ty,
			_ => unreachable!("static to non-static"),
		};
		let span = self.reader.types[s.ty].id.erased();
		let req_ty = self.solver.concrete(ty, Some(span));
		let init_ty = self.expr(s.init);
		self.solver.unify(req_ty, init_ty);
	}

	fn type_(&mut self, ty: Ix<hir::Type>) -> Ix<Partial> {
		let t = &self.reader.types[ty];
		let span = Some(t.id.erased());
		match t.kind {
			hir::TypeKind::Array(ref a) => {
				let ty = self.type_(a.ty);
				let len = self.reader.array_len(self.ctx, a.len);
				self.solver.array(ty, len, span)
			},
			hir::TypeKind::Fn(ref f) => {
				let params = f.params.iter().map(|&ty| self.type_(ty)).collect();
				let ret = f
					.ret
					.map(|ty| self.type_(ty))
					.unwrap_or_else(|| self.solver.concrete(self.void, span));
				self.solver.fn_(params, ret, span)
			},
			hir::TypeKind::Infer => self.solver.infer(span),
			hir::TypeKind::Ptr(p) => {
				let ty = self.type_(p.ty);
				self.solver.ptr(p.mutable, ty, span)
			},
			hir::TypeKind::Struct(s) => self.solver.concrete(self.ctx.add(thir::Type::Struct(s)), span),
			hir::TypeKind::Enum(e) => self.solver.concrete(self.ctx.add(thir::Type::Enum(e)), span),
			hir::TypeKind::Alias(p) => self.solver.concrete(self.ctx.add(self.reader.type_alias(p)), span),
		}
	}

	fn block(&mut self, block: &hir::Block) -> Ix<Partial> {
		for &expr in block.discard.iter() {
			self.expr(expr);
		}

		if let Some(expr) = block.value {
			self.expr(expr)
		} else {
			self.solver.add(PartialType::Concrete(self.void), None)
		}
	}

	fn expr(&mut self, expr: Ix<hir::Expr>) -> Ix<Partial> {
		let e = &self.reader.exprs[expr];
		let span = e.id.map(|x| x.erased());
		let ty = match e.kind {
			hir::ExprKind::Continue => self.solver.concrete(self.void, span),
			hir::ExprKind::Array(ref a) => self.array(a, span),
			hir::ExprKind::Let(ref l) => self.let_(l, span),
			hir::ExprKind::Block(ref b) => self.block(b),
			hir::ExprKind::Infix(ref i) => self.infix(i, span),
			hir::ExprKind::Break(ref b) => todo!(),
			hir::ExprKind::Call(ref c) => self.call(c, span),
			hir::ExprKind::Cast(ref c) => self.cast(c, span),
			hir::ExprKind::Field(ref f) => self.field(f, span),
			hir::ExprKind::Index(ref i) => self.index(i, span),
			hir::ExprKind::Literal(ref l) => self.literal(l, span),
			hir::ExprKind::Loop(ref l) => self.loop_(l, span),
			hir::ExprKind::Match(ref m) => self.match_(m, span),
			hir::ExprKind::Fn(ref f) => self.fn_ref(f, span),
			hir::ExprKind::Static(ref s) => self.static_ref(s, span),
			hir::ExprKind::Struct(ref s) => self.struct_init(s, span),
			hir::ExprKind::Local(l) => {
				let x = self.locals_out[l];
				let y = self.solver.infer(span);
				self.solver.unify(x, y);
				y
			},
			hir::ExprKind::Param(p) => {
				let ty = match self.decl.kind {
					thir::ItemDeclKind::Fn(ref f) => f.params[p],
					_ => unreachable!("param to non-fn"),
				};
				self.solver.concrete(ty, span)
			},
			hir::ExprKind::EnumVariant(ref v) => self.enum_variant(v, span),
			hir::ExprKind::Ref(ref r) => self.ref_(r, span),
			hir::ExprKind::Prefix(ref p) => self.prefix_op(p, span),
			hir::ExprKind::Return(ref r) => todo!(),
		};
		self.exprs_out.insert(expr, ty);
		ty
	}

	fn array(&mut self, array: &hir::ArrayExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let ty;
		let len;
		if array.repeat {
			ty = self.expr(array.elems[0]);
			len = self.reader.array_len(self.ctx, array.elems[1]);
		} else {
			ty = self.solver.infer(span);
			for &elem in array.elems.iter() {
				let elem = self.expr(elem);
				self.solver.unify(ty, elem);
			}
			len = array.elems.len() as _;
		}
		self.solver.array(ty, len, span)
	}

	fn let_(&mut self, let_: &hir::LetExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let ty = let_
			.ty
			.map(|ty| self.type_(ty))
			.unwrap_or_else(|| self.solver.infer(None));
		let init = let_
			.init
			.map(|init| self.expr(init))
			.unwrap_or_else(|| self.solver.infer(None));
		self.solver.unify(ty, init);
		self.locals_out.insert(let_.local, ty);

		self.solver.concrete(self.void, span)
	}

	fn infix(&mut self, infix: &hir::InfixExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let lhs = self.expr(infix.lhs);
		let rhs = self.expr(infix.rhs);
		let result = self.solver.infer(span);
		self.solver.infix_op(lhs, rhs, infix.op, result);
		result
	}

	fn call(&mut self, call: &hir::CallExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let callee = self.expr(call.callee);
		let args = call.args.iter().map(|&arg| self.expr(arg)).collect();
		let result = self.solver.infer(span);
		self.solver.call(callee, args, result);
		result
	}

	fn struct_init(&mut self, init: &hir::StructExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let &i = self.decls.get(&init.struct_).unwrap();
		match self.ctx.get(i).kind {
			thir::ItemDeclKind::Struct(ref s) => {
				if s.fields.len() != init.args.len() {
					span.map(|x| {
						self.ctx.push(
							x.error(format!("expected {} fields", s.fields.len()))
								.label(x.label(format!("found {}", init.args.len()))),
						);
					});
				}

				let &i = self.reader.items.get(&init.struct_).unwrap();
				let si = self.ctx.get(i);
				let si = match si.kind {
					hir::ItemKind::Struct(ref s) => s,
					_ => unreachable!("struct_init to non-struct"),
				};

				for (((_, &ty), arg), field) in s.fields.iter().zip(init.args.iter()).zip(si.fields.iter()) {
					let arg = self.expr(*arg);

					let ty = self.solver.concrete(ty, Some(field.id.erased()));
					self.solver.unify(ty, arg);
				}

				self.solver.concrete(s.ty, span)
			},
			_ => unreachable!("struct_init to non-struct"),
		}
	}

	fn cast(&mut self, cast: &hir::CastExpr, _: Option<ErasedAstId>) -> Ix<Partial> {
		let expr = self.expr(cast.expr);
		let ty = self.type_(cast.ty);
		self.solver.cast(expr, ty);
		ty
	}

	fn field(&mut self, field: &hir::FieldExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let expr = self.expr(field.expr);
		let result = self.solver.infer(span);
		self.solver.field(expr, field.field, result);
		result
	}

	fn index(&mut self, index: &hir::IndexExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let expr = self.expr(index.expr);
		let index = self.expr(index.index);
		let result = self.solver.infer(span);
		self.solver.index(expr, index, result);
		result
	}

	fn literal(&mut self, literal: &hir::Literal, span: Option<ErasedAstId>) -> Ix<Partial> {
		match literal.kind {
			hir::LiteralKind::Bool => self.solver.concrete(self.bool, span),
			hir::LiteralKind::Char => self.solver.concrete(self.char, span),
			hir::LiteralKind::Float => {
				let ty = self.solver.infer(span);
				self.solver.float(ty);
				ty
			},
			hir::LiteralKind::Int => {
				let ty = self.solver.infer(span);
				self.solver.int(ty);
				ty
			},
			hir::LiteralKind::String => {
				let char = self.solver.concrete(self.char, None);
				self.solver.ptr(false, char, span)
			},
		}
	}

	fn loop_(&mut self, loop_: &hir::LoopExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let body = self.block(&loop_.body);
		let ty = self.solver.concrete(self.void, span);
		self.solver.unify(body, ty);
		ty
	}

	fn match_(&mut self, match_: &hir::MatchExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let expr = self.expr(match_.expr);
		let result = self.solver.infer(span);
		for arm in match_.arms.iter() {
			let value = self.expr(arm.value);
			self.solver.unify(expr, value);
			let then = self.expr(arm.then);
			self.solver.unify(result, then);
		}
		result
	}

	fn fn_ref(&mut self, f: &Id<AbsPath>, span: Option<ErasedAstId>) -> Ix<Partial> {
		let &f = self.decls.get(f).unwrap();
		match self.ctx.get(f).kind {
			thir::ItemDeclKind::Fn(ref f) => self.solver.concrete(f.ty, span),
			_ => unreachable!("fn_ref to non-fn"),
		}
	}

	fn static_ref(&mut self, s: &Id<AbsPath>, span: Option<ErasedAstId>) -> Ix<Partial> {
		let &s = self.decls.get(s).unwrap();
		match self.ctx.get(s).kind {
			thir::ItemDeclKind::Static(ref s) => self.solver.concrete(s.ty, span),
			_ => unreachable!("static_ref to non-static"),
		}
	}

	fn enum_variant(&mut self, v: &hir::VariantExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let &i = self.reader.items.get(&v.path).unwrap();
		match self.ctx.get(i).kind {
			hir::ItemKind::Enum(ref e) => {
				let mut found = false;
				for &hir::Variant(v) in e.variants.iter() {
					if v.name == v.name {
						found = true;
						break;
					}
				}
				if !found {
					span.map(|x| {
						self.ctx.push(
							x.error(format!("unknown variant of enum `{}`", e.name.name.as_str()))
								.label(x.label(format!("variant `{}` doesn't exit", v.variant.name.as_str()))),
						);
					});
				}
			},
			_ => unreachable!("enum_variant to non-enum"),
		}

		let &i = self.decls.get(&v.path).unwrap();
		match self.ctx.get(i).kind {
			thir::ItemDeclKind::Enum(ref e) => self.solver.concrete(e.ty, span),
			_ => unreachable!("enum_variant to non-enum"),
		}
	}

	fn ref_(&mut self, r: &hir::RefExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let ty = self.expr(r.expr);
		self.solver.ptr(r.mutable, ty, span)
	}

	fn prefix_op(&mut self, p: &hir::PrefixExpr, span: Option<ErasedAstId>) -> Ix<Partial> {
		let expr = self.expr(p.expr);
		let result = self.solver.infer(span);
		self.solver.prefix(p.op, expr, result);
		result
	}
}
