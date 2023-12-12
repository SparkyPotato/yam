use arena::{dense::DenseMap, Arena, Ix};
use diagnostics::Span;
use hir::{ast::ErasedAstId, ident::AbsPath, lang_item::LangItemMap, LangItem};
use rustc_hash::FxHashMap;
use verde::{query, storage, Ctx, Id};

use crate::{
	reader::HirReader,
	solver::{Partial, PartialType, TypeSolver},
};

mod reader;
mod solver;

#[storage]
pub struct Storage(type_check);

#[query]
pub fn type_check(
	ctx: &Ctx, item: Id<hir::Item>, lang_items: Id<LangItemMap>,
	#[ignore] items: &FxHashMap<Id<AbsPath>, Id<hir::Item>>,
) -> thir::Item {
	let item = ctx.get(item);
	let lang_items = ctx.get(lang_items);
	let mut ck = Checker::new(ctx, &*item, &*lang_items, items);

	let kind = match item.kind {
		hir::ItemKind::Struct(ref s) => thir::ItemKind::Struct(ck.struct_(s)),
		hir::ItemKind::Enum(ref e) => thir::ItemKind::Enum(ck.enum_(e)),
		hir::ItemKind::Fn(ref f) => thir::ItemKind::Fn(ck.fn_(f)),
		hir::ItemKind::TypeAlias(ref t) => thir::ItemKind::TypeAlias(ck.type_alias(t)),
		hir::ItemKind::Static(ref s) => thir::ItemKind::Static(ck.static_(s)),
	};
	let (exprs, locals) = ck.finish();

	thir::Item {
		path: item.path,
		locals,
		exprs,
		kind,
	}
}

struct Checker<'a> {
	ctx: &'a Ctx<'a>,
	solver: TypeSolver,
	reader: HirReader<'a>,
	params: DenseMap<hir::Param, Id<thir::Type>>,
	exprs_out: DenseMap<hir::Expr, Ix<Partial>>,
	locals_out: DenseMap<hir::Local, Ix<Partial>>,
	void: Id<thir::Type>,
	bool: Id<thir::Type>,
	char: Id<thir::Type>,
}

impl<'a> Checker<'a> {
	fn new(
		ctx: &'a Ctx<'a>, item: &'a hir::Item, lang_items: &'a LangItemMap,
		items: &'a FxHashMap<Id<AbsPath>, Id<hir::Item>>,
	) -> Self {
		Self {
			ctx,
			solver: TypeSolver::new(),
			reader: HirReader::new(&item.types, &item.exprs, &item.locals, lang_items, items),
			params: DenseMap::new(),
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
		self.solver.solve(self.ctx, &self.reader);
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

	fn struct_(&mut self, s: &hir::Struct) -> thir::Struct {
		thir::Struct {
			fields: self.params(&s.fields),
		}
	}

	fn enum_(&mut self, e: &hir::Enum) -> thir::Enum {
		let var = e.variants.len();
		let variants = if var == 0 { 1 } else { var.ilog2() + 1 };
		let bits = ((variants + 7) / 8) * 8;
		thir::Enum {
			repr: match bits {
				8 => LangItem::U8,
				16 => LangItem::U16,
				32 => LangItem::U32,
				64 => LangItem::U64,
				_ => unreachable!(),
			},
		}
	}

	fn fn_(&mut self, f: &hir::Fn) -> thir::Fn {
		let (ret, ret_ty) = f
			.ret
			.map(|ty| {
				let span = self.reader.types[ty].id.erased();
				let ty = self.reader.req_type(self.ctx, ty, true);
				(ty, self.solver.concrete(ty, Some(span)))
			})
			.unwrap_or_else(|| (self.void, self.solver.concrete(self.void, None)));
		self.params = self.params(&f.params);
		f.body.as_ref().map(|body| {
			let ty = self.block(body);
			self.solver.unify(ret_ty, ty);
		});

		thir::Fn {
			params: std::mem::replace(&mut self.params, DenseMap::default()),
			ret,
		}
	}

	fn type_alias(&mut self, t: &hir::TypeAlias) -> thir::TypeAlias {
		thir::TypeAlias {
			ty: self.reader.req_type(self.ctx, t.ty, true),
		}
	}

	fn static_(&mut self, s: &hir::Static) -> thir::Static {
		let span = self.reader.types[s.ty].id.erased();
		let ty = self.reader.req_type(self.ctx, s.ty, true);
		let req_ty = self.solver.concrete(ty, Some(span));
		let init_ty = self.expr(s.init);
		self.solver.unify(req_ty, init_ty);

		thir::Static { ty }
	}

	fn params(&mut self, params: &Arena<hir::Param>) -> DenseMap<hir::Param, Id<thir::Type>> {
		let mut map = DenseMap::with_capacity(params.len());
		for (id, param) in params.ids_iter() {
			map.insert(id, self.reader.req_type(self.ctx, param.ty, true));
		}
		map
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
			_ => {
				let ty = self.reader.req_type(self.ctx, ty, false);
				return self.solver.concrete(ty, span);
			},
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
			hir::ExprKind::Param(p) => self.solver.concrete(self.params[p], span),
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
		let &i = self.reader.items.get(&init.struct_).unwrap();
		let item = self.ctx.get(i);
		match item.kind {
			hir::ItemKind::Struct(ref s) => {
				if s.fields.len() != init.args.len() {
					span.map(|x| {
						self.ctx.push(
							x.error(format!("expected {} fields", s.fields.len()))
								.label(x.label(format!("found {}", init.args.len()))),
						);
					});
				}
				for (field, arg) in s.fields.iter().zip(init.args.iter()) {
					let arg = self.expr(*arg);

					let reader = HirReader::new(
						&item.types,
						&item.exprs,
						&item.locals,
						self.reader.lang_items,
						self.reader.items,
					);
					let ty = reader.req_type(self.ctx, field.ty, false);

					let ty = self.solver.concrete(ty, Some(field.id.erased()));
					self.solver.unify(ty, arg);
				}
			},
			_ => unreachable!("struct_init to non-struct"),
		}

		let ty = self.ctx.add(thir::Type::Struct(init.struct_));
		self.solver.concrete(ty, span)
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
		let &f = self.reader.items.get(f).unwrap();
		let item = self.ctx.get(f);
		match item.kind {
			hir::ItemKind::Fn(ref f) => {
				let reader = HirReader::new(
					&item.types,
					&item.exprs,
					&item.locals,
					self.reader.lang_items,
					self.reader.items,
				);
				let params = f
					.params
					.iter()
					.map(|x| reader.req_type(self.ctx, x.ty, false))
					.collect();
				let ret = f
					.ret
					.map(|x| reader.req_type(self.ctx, x, false))
					.unwrap_or_else(|| self.void);

				let ty = self.ctx.add(thir::Type::Fn(thir::FnType { params, ret }));
				self.solver.concrete(ty, span)
			},
			_ => unreachable!("fn_ref to non-fn"),
		}
	}

	fn static_ref(&mut self, s: &Id<AbsPath>, span: Option<ErasedAstId>) -> Ix<Partial> {
		let &s = self.reader.items.get(s).unwrap();
		let item = self.ctx.get(s);
		match item.kind {
			hir::ItemKind::Static(ref s) => {
				let reader = HirReader::new(
					&item.types,
					&item.exprs,
					&item.locals,
					self.reader.lang_items,
					self.reader.items,
				);
				let ty = reader.req_type(self.ctx, s.ty, false);
				self.solver.concrete(ty, span)
			},
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

		let ty = self.ctx.add(thir::Type::Enum(v.path));
		self.solver.concrete(ty, span)
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
