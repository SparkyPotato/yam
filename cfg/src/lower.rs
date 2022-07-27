use std::collections::HashMap;

use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use name_resolve::{
	resolved,
	resolved::{InbuiltType, Lit, LocalRef, Spur, ValExprKind},
};

use crate::{
	types,
	types::{ExprKind, TypeEngine, TypeInfo},
	Arg,
	ArgId,
	BasicBlock,
	BasicBlockId,
	BinOp,
	Ctx,
	Field,
	Fn,
	Instr,
	InstrId,
	InstrKind,
	Rodeo,
	Struct,
	Ty,
	TyRef,
	Type,
	TypeId,
	UnOp,
	Val,
	ValRef,
};

pub fn lower_to_cfg(ctx: resolved::Ctx, rodeo: &Rodeo, diagnostics: &mut Vec<Report<Span>>) -> Ctx {
	let mut lowerer = CfgLower {
		engine: TypeEngine::new(&ctx.types, &ctx.inbuilt_types, rodeo),
		types: &ctx.types,
		globals: &ctx.globals,
		inbuilts: &ctx.inbuilt_types,
		rodeo,
		diagnostics,
		local_types: HashMap::new(),
		local_instrs: HashMap::new(),
		fn_ret: None,
	};

	Ctx {
		types: ctx.types.iter().map(|(r, ty)| (*r, lowerer.lower_ty(ty))).collect(),
		globals: ctx
			.globals
			.iter()
			.map(|(r, val)| (*r, lowerer.lower_val(val)))
			.collect(),
		inbuilt_types: ctx.inbuilt_types,
	}
}

struct CfgLower<'a> {
	engine: TypeEngine<'a>,
	types: &'a HashMap<TyRef, resolved::Ty>,
	globals: &'a HashMap<ValRef, resolved::Val>,
	inbuilts: &'a HashMap<InbuiltType, TyRef>,
	rodeo: &'a Rodeo,
	diagnostics: &'a mut Vec<Report<Span>>,
	local_types: HashMap<LocalRef, TypeId>,
	local_instrs: HashMap<LocalRef, InstrId>,
	fn_ret: Option<(TypeId, Span)>,
}

impl CfgLower<'_> {
	fn lower_ty(&mut self, ty: &resolved::Ty) -> Ty {
		match ty {
			resolved::Ty::Inbuilt(i) => Ty::Inbuilt(*i),
			resolved::Ty::Struct(s) => Ty::Struct(Struct {
				path: s.path.clone(),
				fields: s
					.fields
					.iter()
					.map(|x| Field {
						name: x.name,
						ty: self.lower_ty_expr(&x.ty),
						span: x.span,
					})
					.collect(),
				span: s.span,
			}),
		}
	}

	fn lower_val(&mut self, val: &resolved::Val) -> Val {
		match val {
			resolved::Val::Const(_) => unreachable!("consts are not supported"),
			resolved::Val::Static(_) => unreachable!("statics are not supported"),
			resolved::Val::Fn(f) => Val::Fn(self.lower_fn(f)),
		}
	}

	fn lower_fn(&mut self, f: &resolved::Fn) -> Fn {
		let mut curr = BasicBlock::default();

		let ret = f.ret.as_ref().map(|x| self.lower_ty_expr(&x)).unwrap_or(Type::Void);
		let args: Vec<_> = f
			.args
			.iter()
			.enumerate()
			.map(|(i, x)| match &x.pat.node {
				resolved::PatKind::Binding(binding) => {
					let ty = self.lower_ty_expr(&x.ty);

					self.local_instrs.insert(
						binding.binding,
						curr.instr(Instr {
							kind: InstrKind::Arg(ArgId(i as _)),
							ty: ty.clone(),
							span: x.span,
						}),
					);
					(Arg { ident: x.span, ty }, binding.binding)
				},
			})
			.collect();

		let block = self.infer_fn(
			&args,
			&ret,
			f.ret.as_ref().map(|x| x.span).unwrap_or(Self::useless_span()),
			&f.block,
		);

		curr.args = args.into_iter().map(|x| x.0).collect();

		let mut blocks = vec![curr];

		let mut curr = 0;
		let id = self.lower_block(block, &mut curr, &mut blocks);
		blocks[curr].instr(Instr {
			kind: InstrKind::Ret(Some(id)),
			ty: ret.clone(),
			span: f.block.span,
		});

		self.engine.reset();
		self.local_types.clear();

		Fn {
			path: f.path.clone(),
			ret,
			blocks,
			span: f.span,
		}
	}

	fn lower_ty_expr(&mut self, ty: &resolved::TyExpr) -> Type {
		match &ty.node {
			resolved::TyExprKind::Err => Type::Err,
			resolved::TyExprKind::TyRef(r) => Type::TyRef(*r),
			resolved::TyExprKind::Type => unreachable!("`type` is not supported"),
			resolved::TyExprKind::TypeOf(_) => unreachable!("typeof is not supported"),
			resolved::TyExprKind::Ptr(ptr) => Type::Ptr {
				mutable: ptr.mutability,
				to: Box::new(self.lower_ty_expr(&ptr.to)),
			},
			resolved::TyExprKind::Tuple(_) => unreachable!("tuples are not supported"),
		}
	}

	fn lower_block(&mut self, block: types::Block, curr: &mut usize, blocks: &mut Vec<BasicBlock>) -> InstrId {
		let ret = self.engine.reconstruct(block.ty, block.span, &mut self.diagnostics);
		for expr in block.exprs {
			self.lower_expr(expr, curr, blocks);
		}

		if matches!(ret, Type::Void) {
			blocks[*curr].instr(Instr {
				kind: InstrKind::Ret(None),
				ty: ret,
				span: block.span,
			})
		} else {
			InstrId(blocks[*curr].instrs.len() as u32 - 1)
		}
	}

	fn lower_expr(&mut self, expr: types::Expr, curr: &mut usize, blocks: &mut Vec<BasicBlock>) -> InstrId {
		let ty = self.engine.reconstruct(expr.ty, expr.span, &mut self.diagnostics);

		let kind = match expr.kind {
			ExprKind::Lit(lit) => InstrKind::Literal(lit),
			ExprKind::Block(b) => return self.lower_block(b, curr, blocks),
			ExprKind::ValRef(v) => InstrKind::Global(v),
			ExprKind::LocalRef(l) => return self.local_instrs[&l],
			ExprKind::Let(l) => {
				let expr = self.lower_expr(*l.expr.unwrap(), curr, blocks);
				match l.pat.node {
					resolved::PatKind::Binding(binding) => {
						self.local_instrs.insert(binding.binding, expr);
					},
				}
				return blocks[*curr].instr(Instr {
					kind: InstrKind::Void,
					ty,
					span: l.span,
				});
			},
			ExprKind::Cast(c) => InstrKind::Cast(self.lower_expr(*c.expr, curr, blocks)),
			ExprKind::Call(c) => InstrKind::Call {
				target: self.lower_expr(*c.target, curr, blocks),
				args: c.args.into_iter().map(|x| self.lower_expr(x, curr, blocks)).collect(),
			},
			ExprKind::Unary(u) => InstrKind::Unary {
				op: u.op,
				value: self.lower_expr(*u.expr, curr, blocks),
			},
			ExprKind::Binary(b) => match b.op {
				BinOp::Add
				| BinOp::Sub
				| BinOp::Mul
				| BinOp::Div
				| BinOp::Rem
				| BinOp::Shl
				| BinOp::Shr
				| BinOp::Lt
				| BinOp::Gt
				| BinOp::Leq
				| BinOp::Geq
				| BinOp::Eq
				| BinOp::Neq
				| BinOp::BitAnd
				| BinOp::BitOr
				| BinOp::BitXor
				| BinOp::And
				| BinOp::Or => InstrKind::Binary {
					left: self.lower_expr(*b.lhs, curr, blocks),
					op: b.op,
					right: self.lower_expr(*b.rhs, curr, blocks),
				},
				BinOp::Assign => {
					if let Some(lhs) = self.assign_lhs(*b.lhs) {
						let rhs = self.lower_expr(*b.rhs, curr, blocks);
						self.local_instrs.insert(lhs, rhs);
					}

					InstrKind::Void
				},
				BinOp::AddAssign
				| BinOp::SubAssign
				| BinOp::MulAssign
				| BinOp::DivAssign
				| BinOp::RemAssign
				| BinOp::BitAndAssign
				| BinOp::BitOrAssign
				| BinOp::BitXorAssign
				| BinOp::ShlAssign
				| BinOp::ShrAssign => {
					if let Some(lhs) = self.assign_lhs(*b.lhs) {
						let rhs = self.lower_expr(*b.rhs, curr, blocks);
						let lhs = self
							.local_instrs
							.insert(lhs, InstrId(blocks[*curr].instrs.len() as _))
							.unwrap();
						InstrKind::Binary {
							left: lhs,
							op: Self::binop_assign_to_op(b.op),
							right: rhs,
						}
					} else {
						InstrKind::Void
					}
				},
				BinOp::PlaceConstruct => unreachable!("place construct is not supported"),
			},
			ExprKind::Break(_) => unreachable!(),
			ExprKind::Continue(_) => unreachable!(),
			ExprKind::Return(r) => InstrKind::Ret(r.map(|x| self.lower_expr(*x, curr, blocks))),
			ExprKind::If(if_) => {
				let mut cond_block = *curr;
				let cond = self.lower_expr(*if_.cond, &mut cond_block, blocks);

				let cond_jmp_id = blocks[cond_block].instr(Instr {
					kind: InstrKind::Void,
					ty: Type::Void,
					span: expr.span,
				});

				let else_span = if_.else_.as_ref().map(|x| x.span).unwrap_or(expr.span);
				let else_ = if_.else_.map(|x| self.lower_expr(*x, &mut cond_block, blocks));
				let else_ = else_.unwrap_or_else(|| {
					blocks[cond_block].instr(Instr {
						kind: InstrKind::Void,
						ty: Type::Void,
						span: else_span,
					})
				});

				let if_block = blocks.len();
				blocks.push(BasicBlock::default());
				let mut if_end = if_block;
				let then = self.lower_block(if_.then, &mut if_end, blocks);

				blocks[cond_block].instrs[cond_jmp_id.0 as usize] = Instr {
					kind: InstrKind::CondJmp {
						if_: cond,
						to: BasicBlockId(if_block as _),
						args: Vec::new(),
					},
					ty: Type::Void,
					span: expr.span,
				};

				*curr = blocks.len();
				blocks.push(BasicBlock {
					instrs: vec![],
					args: vec![Arg {
						ident: expr.span,
						ty: ty.clone(),
					}],
				});
				blocks[cond_block].instr(Instr {
					kind: InstrKind::Jmp {
						to: BasicBlockId(*curr as _),
						args: vec![else_],
					},
					ty: Type::Void,
					span: expr.span,
				});
				blocks[if_end].instr(Instr {
					kind: InstrKind::Jmp {
						to: BasicBlockId(*curr as _),
						args: vec![then],
					},
					ty: Type::Void,
					span: expr.span,
				});

				InstrKind::Arg(ArgId(0))
			},
			ExprKind::Loop(l) => {
				let loop_start = *curr;
				*curr = blocks.len();
				blocks.push(BasicBlock::default());
				self.lower_block(l.block, curr, blocks);
				InstrKind::Jmp {
					to: BasicBlockId(loop_start as _),
					args: vec![],
				}
			},
			ExprKind::While(w) => {
				let cond = self.lower_expr(*w.cond, curr, blocks);
				let loop_start = *curr;

				let to = BasicBlockId(blocks.len() as _);
				blocks[*curr].instr(Instr {
					kind: InstrKind::CondJmp {
						if_: cond,
						to,
						args: Vec::new(),
					},
					ty: Type::Void,
					span: expr.span,
				});
				let early_end_block = *curr;
				let early_end_instr = blocks[*curr].instr(Instr {
					kind: InstrKind::Void,
					ty: Type::Void,
					span: expr.span,
				});

				*curr = blocks.len();
				blocks.push(BasicBlock::default());
				self.lower_block(w.block, curr, blocks);
				let to = BasicBlockId(blocks.len() as _);
				blocks[*curr].instr(Instr {
					kind: InstrKind::CondJmp {
						if_: cond,
						to: BasicBlockId(loop_start as u32 + 1),
						args: Vec::new(),
					},
					ty: Type::Void,
					span: expr.span,
				});
				blocks[*curr].instr(Instr {
					kind: InstrKind::Jmp { to, args: Vec::new() },
					ty: Type::Void,
					span: expr.span,
				});

				blocks[early_end_block].instrs[early_end_instr.0 as usize] = Instr {
					kind: InstrKind::Jmp { to, args: Vec::new() },
					ty: Type::Void,
					span: expr.span,
				};
				*curr = blocks.len();
				blocks.push(BasicBlock::default());
				InstrKind::Void
			},
			ExprKind::For(_) => unreachable!(),
			ExprKind::Err => unreachable!(),
		};

		blocks[*curr].instr(Instr {
			kind,
			ty,
			span: expr.span,
		})
	}

	fn infer_fn(
		&mut self, args: &[(Arg, LocalRef)], ret: &Type, ret_span: Span, block: &resolved::Block,
	) -> types::Block {
		for (arg, r) in args {
			let id = self.insert_ty(&arg.ty);
			self.local_types.insert(*r, id);
		}

		let ret = self.insert_ty(ret);
		self.fn_ret = Some((ret, ret_span));

		let (block, span) = self.infer_block(block);

		self.engine.unify(ret, ret_span, block.ty, span, &mut self.diagnostics);

		block
	}

	fn infer_block(&mut self, block: &resolved::Block) -> (types::Block, Span) {
		let mut exprs = Vec::with_capacity(block.stmts.len());
		let mut ty = None;
		let mut last_span = Self::useless_span();

		let last_expr = block.stmts.len().wrapping_sub(1);
		for (i, stmt) in block.stmts.iter().enumerate() {
			let (expr, last) = match &stmt.node {
				resolved::StmtKind::Expr(expr) => {
					if i != last_expr
						&& !matches!(
							expr,
							ValExprKind::Block(_)
								| ValExprKind::If(_) | ValExprKind::While(_)
								| ValExprKind::Loop(_) | ValExprKind::For(_)
						) {
						self.diagnostics.push(
							stmt.span
								.report(ReportKind::Error)
								.with_message("expected `;` after statement")
								.with_label(Label::new(stmt.span).with_message("consider putting a `;` after this"))
								.finish(),
						);

						(expr, false)
					} else {
						(expr, true)
					}
				},
				resolved::StmtKind::Semi(expr) => (expr, false),
				resolved::StmtKind::Err => continue,
			};

			let expr = self.infer_expr(expr, stmt.span);

			if last {
				ty = Some(expr.ty);
				last_span = stmt.span;
			}

			exprs.push(expr);
		}

		(
			types::Block {
				exprs,
				ty: ty.unwrap_or_else(|| self.engine.insert(TypeInfo::Void)),
				span: block.span,
			},
			last_span,
		)
	}

	fn infer_expr(&mut self, expr: &ValExprKind, span: Span) -> types::Expr {
		let (kind, ty) = match expr {
			ValExprKind::Lit(lit) => (types::ExprKind::Lit(*lit), self.infer_lit(lit)),
			ValExprKind::Block(block) => {
				let (block, _) = self.infer_block(block);
				let ty = block.ty;
				(ExprKind::Block(block), ty)
			},
			ValExprKind::ValRef(val) => (types::ExprKind::ValRef(*val), self.infer_val(val)),
			ValExprKind::LocalRef(local) => (types::ExprKind::LocalRef(*local), self.local_types[local]),
			ValExprKind::Let(l) => {
				let init_ty = l.expr.as_ref().map(|x| (self.infer_expr(&x.node, x.span).ty, x.span));
				let given_ty = l.ty.as_ref().map(|x| {
					let ty = self.lower_ty_expr(&x);
					(self.insert_ty(&ty), x.span)
				});

				let ty = match (init_ty, given_ty) {
					(Some((rhs, rhs_span)), Some((lhs, lhs_span))) => {
						self.engine.unify(lhs, lhs_span, rhs, rhs_span, &mut self.diagnostics);
						lhs
					},
					(Some((ty, _)), None) | (None, Some((ty, _))) => ty,
					(None, None) => self.engine.insert(TypeInfo::Unknown),
				};

				match l.pat.node {
					resolved::PatKind::Binding(binding) => self.local_types.insert(binding.binding, ty),
				};

				(
					ExprKind::Let(types::Let {
						pat: l.pat,
						expr: l.expr.as_ref().map(|x| Box::new(self.infer_expr(&x.node, x.span))),
						span: l.span,
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::List(_) => unreachable!("arrays are not supported"),
			ValExprKind::Array(_) => unreachable!("arrays are not supported"),
			ValExprKind::Cast(cast) => {
				let ty = self.lower_ty_expr(&cast.ty);
				let ty = self.insert_ty(&ty);
				(
					ExprKind::Cast(types::Cast {
						expr: Box::new(self.infer_expr(&cast.expr.node, cast.expr.span)),
						ty,
					}),
					ty,
				)
			},
			ValExprKind::Fn(_) => unreachable!("closures are not supported"),
			ValExprKind::MacroRef(_) => unreachable!("macros are not supported"),
			ValExprKind::Call(call) => {
				let target = self.infer_expr(&call.target.node, call.target.span);
				let ty = target.ty;
				(
					ExprKind::Call(types::Call {
						target: Box::new(target),
						args: call
							.args
							.iter()
							.map(|x| self.infer_expr(Self::expr_to_val(&x.node), x.span))
							.collect(),
					}),
					self.engine.insert(TypeInfo::FnRet(ty)),
				)
			},
			ValExprKind::Index(_) => unreachable!("indexing is not supported"),
			ValExprKind::Access(_) => unreachable!("field access is not supported"),
			ValExprKind::Unary(unary) => {
				let expr = self.infer_expr(&unary.expr.node, unary.expr.span);
				let ty = match unary.op {
					UnOp::Not => self.engine.insert(TypeInfo::Ref(expr.ty)),
					UnOp::Neg => self.engine.insert(TypeInfo::Ref(expr.ty)),
					UnOp::Addr => self.engine.insert(TypeInfo::Ptr {
						mutable: false,
						to: expr.ty,
					}),
					UnOp::DoubleAddr => {
						let to = self.engine.insert(TypeInfo::Ptr {
							mutable: false,
							to: expr.ty,
						});
						self.engine.insert(TypeInfo::Ptr { mutable: false, to })
					},
					UnOp::AddrMut => self.engine.insert(TypeInfo::Ptr {
						mutable: true,
						to: expr.ty,
					}),
					UnOp::DoubleAddrMut => {
						let to = self.engine.insert(TypeInfo::Ptr {
							mutable: true,
							to: expr.ty,
						});
						self.engine.insert(TypeInfo::Ptr { mutable: false, to })
					},
					UnOp::Deref => self.engine.insert(TypeInfo::Deref(expr.ty)),
				};
				(
					ExprKind::Unary(types::Unary {
						op: unary.op,
						expr: Box::new(expr),
					}),
					ty,
				)
			},
			ValExprKind::Binary(binary) => {
				let lhs = self.infer_expr(&binary.lhs.node, binary.lhs.span);
				let rhs = self.infer_expr(&binary.rhs.node, binary.rhs.span);
				let ty = match binary.op {
					BinOp::Add
					| BinOp::Sub
					| BinOp::Mul
					| BinOp::Div
					| BinOp::Rem
					| BinOp::Shl
					| BinOp::Shr
					| BinOp::BitAnd
					| BinOp::BitOr
					| BinOp::BitXor => self.engine.insert(TypeInfo::Ref(lhs.ty)),
					BinOp::Lt
					| BinOp::Gt
					| BinOp::Leq
					| BinOp::Geq
					| BinOp::Eq
					| BinOp::Neq
					| BinOp::And
					| BinOp::Or => self.engine.insert(TypeInfo::Ty(self.inbuilts[&InbuiltType::Bool])),
					BinOp::Assign
					| BinOp::AddAssign
					| BinOp::SubAssign
					| BinOp::MulAssign
					| BinOp::DivAssign
					| BinOp::RemAssign
					| BinOp::BitAndAssign
					| BinOp::BitOrAssign
					| BinOp::BitXorAssign
					| BinOp::ShlAssign
					| BinOp::ShrAssign => {
						self.engine
							.unify(lhs.ty, lhs.span, rhs.ty, rhs.span, &mut self.diagnostics);
						self.engine.insert(TypeInfo::Void)
					},
					BinOp::PlaceConstruct => unreachable!("placement construction is not supported"),
				};
				(
					ExprKind::Binary(types::Binary {
						op: binary.op,
						lhs: Box::new(lhs),
						rhs: Box::new(rhs),
					}),
					ty,
				)
			},
			ValExprKind::Break(br) => (
				ExprKind::Break(
					br.as_ref()
						.map(|x| Box::new(self.infer_expr(Self::expr_to_val(&x.node), x.span))),
				),
				self.engine.insert(TypeInfo::Never),
			),
			ValExprKind::Continue(c) => (
				ExprKind::Continue(
					c.as_ref()
						.map(|x| Box::new(self.infer_expr(Self::expr_to_val(&x.node), x.span))),
				),
				self.engine.insert(TypeInfo::Never),
			),
			ValExprKind::Return(ret) => {
				let expr = ret
					.as_ref()
					.map(|x| Box::new(self.infer_expr(Self::expr_to_val(&x.node), x.span)));

				let ty = expr
					.as_ref()
					.map(|x| x.ty)
					.unwrap_or(self.engine.insert(TypeInfo::Void));
				let (ret, ret_span) = self.fn_ret.unwrap();
				self.engine.unify(
					ret,
					ret_span,
					ty,
					expr.as_ref().map(|x| x.span).unwrap_or(span),
					&mut self.diagnostics,
				);

				(ExprKind::Return(expr), self.engine.insert(TypeInfo::Never))
			},
			ValExprKind::If(if_) => {
				let cond = Box::new(self.infer_expr(&if_.cond.node, if_.cond.span));
				let (block, span) = self.infer_block(&if_.then);
				let els = if_
					.else_
					.as_ref()
					.map(|x| self.infer_expr(Self::expr_to_val(&x.node), x.span));

				let ty = if let Some(els) = &els {
					self.engine
						.unify(block.ty, span, els.ty, els.span, &mut self.diagnostics);
					block.ty
				} else {
					self.engine.insert(TypeInfo::Void)
				};

				(
					ExprKind::If(types::If {
						cond,
						then: block,
						else_: els.map(Box::new),
					}),
					ty,
				)
			},
			ValExprKind::Loop(loop_) => {
				let (block, _) = self.infer_block(&loop_.block);
				(
					ExprKind::Loop(types::Loop {
						block,
						while_: loop_
							.while_
							.as_ref()
							.map(|x| Box::new(self.infer_expr(&x.node, x.span))),
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::While(w) => {
				let (block, _) = self.infer_block(&w.block);
				(
					ExprKind::While(types::While {
						cond: Box::new(self.infer_expr(&w.cond.node, w.cond.span)),
						block,
					}),
					self.engine.insert(TypeInfo::Void),
				)
			},
			ValExprKind::For(_) => unreachable!("for loops are not supported"),
			ValExprKind::Err => unreachable!("already filtered out"),
		};

		types::Expr { kind, ty, span }
	}

	fn infer_lit(&mut self, lit: &Lit) -> TypeId {
		let ty = match lit {
			Lit::Int(_) => TypeInfo::Int,
			Lit::Float(_) => TypeInfo::Float,
			Lit::Bool(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Bool]),
			Lit::Char(_) => TypeInfo::Ty(self.inbuilts[&InbuiltType::Uint(32)]),
			Lit::String(_) => {
				let r = self.inbuilts[&InbuiltType::Uint(8)];
				TypeInfo::Ptr {
					mutable: false,
					to: self.engine.insert(TypeInfo::Ty(r)),
				}
			},
		};
		self.engine.insert(ty)
	}

	fn infer_val(&mut self, val: &ValRef) -> TypeId {
		match &self.globals[val] {
			resolved::Val::Fn(f) => {
				let args = f
					.args
					.iter()
					.map(|x| {
						let ty = self.lower_ty_expr(&x.ty);
						self.insert_ty(&ty)
					})
					.collect();
				let ret = f
					.ret
					.as_ref()
					.map(|x| {
						let ty = self.lower_ty_expr(&x);
						self.insert_ty(&ty)
					})
					.unwrap_or(self.engine.insert(TypeInfo::Void));
				self.engine.insert(TypeInfo::Fn { args, ret })
			},
			resolved::Val::Const(_) => unreachable!("consts are not supported"),
			resolved::Val::Static(_) => unreachable!("statics are not supported"),
		}
	}

	fn insert_ty(&mut self, ty: &Type) -> TypeId {
		let ty = match ty {
			Type::Void => TypeInfo::Void,
			Type::Never => TypeInfo::Never,
			Type::Fn { args, ret } => TypeInfo::Fn {
				args: args.iter().map(|x| self.insert_ty(x)).collect(),
				ret: self.insert_ty(&ret),
			},
			Type::TyRef(r) => TypeInfo::Ty(*r),
			Type::Ptr { mutable, to } => TypeInfo::Ptr {
				mutable: *mutable,
				to: self.insert_ty(&to),
			},
			Type::Err => TypeInfo::Unknown,
		};
		self.engine.insert(ty)
	}

	fn assign_lhs(&mut self, expr: types::Expr) -> Option<LocalRef> {
		match expr.kind {
			ExprKind::LocalRef(lhs) => Some(lhs),
			_ => {
				self.diagnostics.push(
					expr.span
						.report(ReportKind::Error)
						.with_message("cannot assign")
						.with_label(Label::new(expr.span).with_message("cannot assign to this expression"))
						.finish(),
				);

				None
			},
		}
	}

	fn expr_to_val(expr: &resolved::ExprKind) -> &ValExprKind {
		match expr {
			resolved::ExprKind::Val(val) => val,
			resolved::ExprKind::Err => &resolved::ValExprKind::Err,
			_ => unreachable!("type exprs are not supported in this position"),
		}
	}

	fn binop_assign_to_op(op: BinOp) -> BinOp {
		match op {
			BinOp::Add
			| BinOp::Sub
			| BinOp::Mul
			| BinOp::Div
			| BinOp::Rem
			| BinOp::Shl
			| BinOp::Shr
			| BinOp::Lt
			| BinOp::Gt
			| BinOp::Leq
			| BinOp::Geq
			| BinOp::Eq
			| BinOp::Neq
			| BinOp::BitAnd
			| BinOp::BitOr
			| BinOp::BitXor
			| BinOp::And
			| BinOp::Or
			| BinOp::Assign
			| BinOp::PlaceConstruct => unreachable!(),
			BinOp::AddAssign => BinOp::Add,
			BinOp::SubAssign => BinOp::Sub,
			BinOp::MulAssign => BinOp::Mul,
			BinOp::DivAssign => BinOp::Div,
			BinOp::RemAssign => BinOp::Rem,
			BinOp::BitAndAssign => BinOp::BitAnd,
			BinOp::BitOrAssign => BinOp::BitOr,
			BinOp::BitXorAssign => BinOp::BitXor,
			BinOp::ShlAssign => BinOp::Shl,
			BinOp::ShrAssign => BinOp::Shr,
		}
	}

	fn useless_span() -> Span {
		Span {
			start: 0,
			end: 0,
			file: Spur::default(),
		}
	}
}
