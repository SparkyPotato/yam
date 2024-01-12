use arena::{dense::DenseMap, Ix};
use hir::{ident::AbsPath, LangItem};
use pretty::RcDoc;
use verde::{Db, Id};

pub fn pretty_print(db: &dyn Db, hir: Id<hir::Item>, thir: Id<thir::Item>) -> String {
	inner(db, hir, thir).pretty(80).to_string()
}

fn inner(db: &dyn Db, hir: Id<hir::Item>, thir: Id<thir::Item>) -> RcDoc<'static> {
	let hir = db.get(hir);
	let thir = db.get(thir);
	let decl = db.get(thir.decl);

	let mut printer = PrettyPrinter {
		db,
		hir: &*hir,
		thir: &*thir,
		params: DenseMap::new(),
	};

	match (&hir.kind, &decl.kind) {
		(hir::ItemKind::Struct(hir), thir::ItemDeclKind::Struct(thir)) => printer.struct_(hir, thir),
		(hir::ItemKind::Enum(hir), thir::ItemDeclKind::Enum(thir)) => printer.enum_(hir, thir),
		(hir::ItemKind::Fn(hir), thir::ItemDeclKind::Fn(thir)) => printer.fn_(hir, thir),
		(hir::ItemKind::TypeAlias(hir), thir::ItemDeclKind::TypeAlias(thir)) => printer.type_alias(hir, thir),
		(hir::ItemKind::Static(hir), thir::ItemDeclKind::Static(thir)) => printer.static_(hir, thir),
		_ => unreachable!(),
	}
}

struct PrettyPrinter<'a> {
	db: &'a dyn Db,
	hir: &'a hir::Item,
	thir: &'a thir::Item,
	params: DenseMap<hir::Param, hir::Name>,
}

impl PrettyPrinter<'_> {
	fn struct_(&self, hir: &hir::Struct, thir: &thir::StructDecl) -> RcDoc<'static> {
		RcDoc::text("struct")
			.append(RcDoc::space())
			.append(self.path(self.hir.path))
			.append(RcDoc::space())
			.append(RcDoc::text("{"))
			.append(
				RcDoc::line()
					.append(RcDoc::concat(hir.fields.iter().zip(thir.fields.iter()).map(
						|(p, (_, &ty))| self.param(p, ty).append(RcDoc::text(",")).append(RcDoc::line()),
					)))
					.nest(2),
			)
			.append(RcDoc::text("}"))
	}

	fn enum_(&self, hir: &hir::Enum, thir: &thir::EnumDecl) -> RcDoc<'static> {
		RcDoc::text("enum")
			.append(RcDoc::space())
			.append(self.path(self.hir.path))
			.append(RcDoc::text(":"))
			.append(RcDoc::space())
			.append(self.lang_item(thir.repr))
			.append(RcDoc::space())
			.append(RcDoc::text("{"))
			.append(
				RcDoc::line()
					.append(RcDoc::concat(hir.variants.iter().enumerate().map(|(i, x)| {
						RcDoc::text(x.0.name.as_str())
							.append(RcDoc::space())
							.append(RcDoc::text("="))
							.append(RcDoc::space())
							.append(RcDoc::as_string(i))
							.append(RcDoc::text(","))
							.append(RcDoc::line())
					})))
					.nest(2),
			)
			.append(RcDoc::text("}"))
	}

	fn fn_(&mut self, hir: &hir::Fn, thir: &thir::FnDecl) -> RcDoc<'static> {
		let abi = match hir.abi {
			Some(ref a) => {
				let ty = match a.abi {
					Some(ref abi) => RcDoc::<()>::text("\"")
						.append(RcDoc::text(abi.abi))
						.append(RcDoc::text("\""))
						.append(RcDoc::space()),
					None => RcDoc::nil(),
				};

				RcDoc::text("extern").append(RcDoc::space()).append(ty)
			},
			None => RcDoc::nil(),
		};
		let params = RcDoc::intersperse(
			hir.params.ids_iter().map(|(i, p)| {
				self.params.insert(i, p.name);
				self.param(p, thir.params[i])
			}),
			RcDoc::text(", "),
		);
		let ret = self.type_(thir.ret);
		let body = match hir.body {
			Some(ref b) => RcDoc::space().append(self.block(b)),
			None => RcDoc::text(";"),
		};

		abi.append(RcDoc::text("fn"))
			.append(RcDoc::space())
			.append(self.path(self.hir.path))
			.append(RcDoc::text("("))
			.append(params)
			.append(RcDoc::text(") -> "))
			.append(ret)
			.append(body)
	}

	fn type_alias(&self, _: &hir::TypeAlias, thir: &thir::TypeAliasDecl) -> RcDoc<'static> {
		RcDoc::text("type")
			.append(RcDoc::space())
			.append(self.path(self.hir.path))
			.append(RcDoc::space())
			.append(RcDoc::text("="))
			.append(RcDoc::space())
			.append(self.type_(thir.ty))
			.append(RcDoc::text(";"))
	}

	fn static_(&self, hir: &hir::Static, thir: &thir::StaticDecl) -> RcDoc<'static> {
		RcDoc::text("static")
			.append(RcDoc::space())
			.append(self.path(self.hir.path))
			.append(RcDoc::text(":"))
			.append(RcDoc::space())
			.append(self.type_(thir.ty))
			.append(RcDoc::space())
			.append(RcDoc::text("="))
			.append(RcDoc::space())
			.append(self.expr(hir.init))
			.append(RcDoc::text(";"))
	}

	fn param(&self, p: &hir::Param, ty: Id<thir::Type>) -> RcDoc<'static> {
		RcDoc::text(p.name.name.as_str())
			.append(RcDoc::text(":"))
			.append(RcDoc::space())
			.append(self.type_(ty))
	}

	fn type_(&self, ty: Id<thir::Type>) -> RcDoc<'static> {
		match *self.db.geti(ty) {
			thir::Type::Array(ref a) => {
				let ty = self.type_(a.ty);
				RcDoc::text("[")
					.append(ty)
					.append(RcDoc::text("; "))
					.append(RcDoc::as_string(a.len))
					.append(RcDoc::text("]"))
			},
			thir::Type::Fn(ref f) => {
				let params = RcDoc::intersperse(f.params.iter().map(|p| self.type_(*p)), RcDoc::text(", "));
				let ret = self.type_(f.ret);
				RcDoc::text("fn(")
					.append(params)
					.append(RcDoc::text(") -> "))
					.append(ret)
			},
			thir::Type::Struct(p) | thir::Type::Enum(p) => self.path(p),
			thir::Type::Ptr(ref p) => {
				let d = RcDoc::text("*");
				if p.mutable { d.append(RcDoc::text("mut ")) } else { d }.append(self.type_(p.ty))
			},
			thir::Type::LangItem(l) => self.lang_item(l),
			thir::Type::Error => RcDoc::text("!"),
			thir::Type::Void => RcDoc::text("void"),
		}
	}

	fn block(&self, hir: &hir::Block) -> RcDoc<'static> {
		RcDoc::text("{")
			.append({
				let expr = match hir.value {
					Some(e) => self.expr(e).append(RcDoc::line()),
					None => RcDoc::nil(),
				};
				RcDoc::line()
					.append(RcDoc::concat(
						hir.discard
							.iter()
							.map(|&e| self.expr(e).append(RcDoc::text(";")).append(RcDoc::line())),
					))
					.append(expr)
					.nest(2)
			})
			.append(RcDoc::text("}"))
	}

	fn expr(&self, e: Ix<hir::Expr>) -> RcDoc<'static> {
		let hir = &self.hir.exprs[e];
		let thir = self.thir.exprs[e];
		let mut cast = false;
		let mut needs_group = false;

		let expr = match hir.kind {
			hir::ExprKind::Continue => RcDoc::text("continue"),
			hir::ExprKind::Array(ref a) => RcDoc::text("[")
				.append(if a.repeat {
					self.expr(a.elems[0])
						.append(RcDoc::text("; "))
						.append(RcDoc::as_string(a.elems.len()))
				} else {
					RcDoc::intersperse(a.elems.iter().map(|&e| self.expr(e)), RcDoc::text(", "))
				})
				.append(RcDoc::text("]")),
			hir::ExprKind::Let(ref l) => {
				needs_group = true;
				RcDoc::text("let")
					.append(RcDoc::space())
					.append(RcDoc::text(l.name.name.as_str()))
					.append(RcDoc::text(":"))
					.append(RcDoc::space())
					.append(self.type_(self.thir.locals[l.local]))
					.append(l.init.map(|e| {
						RcDoc::space()
							.append(RcDoc::text("="))
							.append(RcDoc::space())
							.append(self.expr(e))
					}))
			},
			hir::ExprKind::Block(ref b) => self.block(b),
			hir::ExprKind::Infix(i) => {
				needs_group = true;
				let op = RcDoc::text(match i.op {
					hir::InfixOp::Or => "||",
					hir::InfixOp::And => "&&",
					hir::InfixOp::Eq => "==",
					hir::InfixOp::NotEq => "!=",
					hir::InfixOp::Lt => "<",
					hir::InfixOp::Leq => "<=",
					hir::InfixOp::Gt => ">",
					hir::InfixOp::Geq => ">=",
					hir::InfixOp::Add => "+",
					hir::InfixOp::Sub => "-",
					hir::InfixOp::Mul => "*",
					hir::InfixOp::Div => "/",
					hir::InfixOp::Mod => "%",
					hir::InfixOp::Shl => "<<",
					hir::InfixOp::Shr => ">>",
					hir::InfixOp::Xor => "^",
					hir::InfixOp::BitOr => "|",
					hir::InfixOp::BitAnd => "&",
					hir::InfixOp::Assign => "=",
					hir::InfixOp::AddAssign => "+=",
					hir::InfixOp::SubAssign => "-=",
					hir::InfixOp::MulAssign => "*=",
					hir::InfixOp::DivAssign => "/=",
					hir::InfixOp::ModAssign => "%=",
					hir::InfixOp::ShlAssign => "<<=",
					hir::InfixOp::ShrAssign => ">>=",
					hir::InfixOp::XorAssign => "^=",
					hir::InfixOp::BitOrAssign => "|=",
					hir::InfixOp::BitAndAssign => "&=",
				});
				self.expr(i.lhs)
					.append(RcDoc::space())
					.append(op)
					.append(RcDoc::space())
					.append(self.expr(i.rhs))
			},
			hir::ExprKind::Break(b) => {
				needs_group = true;
				RcDoc::text("break").append(b.map(|e| RcDoc::space().append(self.expr(e))))
			},
			hir::ExprKind::Call(ref c) => self
				.expr(c.callee)
				.append(RcDoc::text("("))
				.append(RcDoc::intersperse(
					c.args.iter().map(|&e| self.expr(e)),
					RcDoc::text(", "),
				))
				.append(RcDoc::text(")")),
			hir::ExprKind::Struct(ref s) => self
				.path(s.struct_)
				.append(RcDoc::text("("))
				.append(RcDoc::intersperse(
					s.args.iter().map(|&e| self.expr(e)),
					RcDoc::text(", "),
				))
				.append(RcDoc::text(")")),
			hir::ExprKind::Cast(ref c) => {
				cast = true;
				self.expr(c.expr).append(RcDoc::text(" as ")).append(self.type_(thir))
			},
			hir::ExprKind::Field(ref f) => self
				.expr(f.expr)
				.append(RcDoc::text(".").append(RcDoc::text(f.field.name.as_str()))),
			hir::ExprKind::Index(ref i) => self
				.expr(i.expr)
				.append(RcDoc::text("[").append(self.expr(i.index)).append(RcDoc::text("]"))),
			hir::ExprKind::Literal(l) => match l {
				hir::Literal::Bool(b) => RcDoc::text(b.to_string()),
				hir::Literal::Char(c) => RcDoc::text(format!("'{}'", c as char)),
				hir::Literal::Float(f) => RcDoc::text(f.to_string()),
				hir::Literal::Int(i) => RcDoc::text(i.to_string()),
				hir::Literal::String(s) => RcDoc::text(format!("\"{}\"", s)),
			},
			hir::ExprKind::Loop(ref l) => RcDoc::text("loop").append(RcDoc::space()).append(self.block(&l.body)),
			hir::ExprKind::Match(ref m) => RcDoc::text("match")
				.append(RcDoc::space())
				.append(self.expr(m.expr))
				.append(RcDoc::space())
				.append(RcDoc::text("{"))
				.append(
					RcDoc::line()
						.append(RcDoc::concat(m.arms.iter().map(|a| {
							self.expr(a.value)
								.append(RcDoc::space())
								.append(RcDoc::text("=>"))
								.append(RcDoc::space())
								.append(self.expr(a.then))
								.append(RcDoc::text(","))
								.append(RcDoc::line())
						})))
						.nest(2),
				)
				.append(RcDoc::line())
				.append(RcDoc::text("}")),
			hir::ExprKind::Fn(f) => self.path(f),
			hir::ExprKind::Static(s) => self.path(s),
			hir::ExprKind::Local(l) => RcDoc::text(self.hir.locals[l].decl.name.as_str()),
			hir::ExprKind::Param(p) => RcDoc::text(self.params[p].name.as_str()),
			hir::ExprKind::EnumVariant(ref v) => self
				.path(v.path)
				.append(RcDoc::text("."))
				.append(RcDoc::text(v.variant.name.as_str())),
			hir::ExprKind::Ref(ref r) => {
				needs_group = true;
				RcDoc::text("&")
					.append(if r.mutable { RcDoc::text("mut ") } else { RcDoc::nil() })
					.append(self.expr(r.expr))
			},
			hir::ExprKind::Prefix(ref p) => {
				needs_group = true;
				let op = RcDoc::text(match p.op {
					hir::PrefixOp::Not => "!",
					hir::PrefixOp::Neg => "-",
					hir::PrefixOp::Deref => "*",
				});
				op.append(self.expr(p.expr))
			},
			hir::ExprKind::Return(r) => {
				needs_group = true;
				RcDoc::text("return").append(r.map(|e| RcDoc::space().append(self.expr(e))))
			},
		};

		RcDoc::text("(")
			.append(if needs_group {
				RcDoc::text("(").append(expr).append(RcDoc::text(")"))
			} else {
				expr
			})
			.append(if !cast {
				RcDoc::text(":").append(RcDoc::space()).append(self.type_(thir))
			} else {
				RcDoc::nil()
			})
			.append(RcDoc::text(")"))
	}

	fn path(&self, p: Id<AbsPath>) -> RcDoc<'static> {
		match *self.db.geti(p) {
			AbsPath::Package(ref p) => RcDoc::text(format!("#{}", p.0)),
			AbsPath::Name { prec, name } => self
				.path(prec)
				.append(RcDoc::text("."))
				.append(RcDoc::text(name.as_str())),
		}
	}

	fn lang_item(&self, l: LangItem) -> RcDoc<'static> {
		RcDoc::text(match l {
			LangItem::U8 => "@u8",
			LangItem::U16 => "@u16",
			LangItem::U32 => "@u32",
			LangItem::U64 => "@u64",
			LangItem::U128 => "@u128",
			LangItem::I8 => "@i8",
			LangItem::I16 => "@i16",
			LangItem::I32 => "@i32",
			LangItem::I64 => "@i64",
			LangItem::I128 => "@i128",
			LangItem::Bool => "@bool",
			LangItem::Char => "@char",
			LangItem::F32 => "@f32",
			LangItem::F64 => "@f64",
		})
	}
}
