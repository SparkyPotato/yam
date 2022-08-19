use std::fmt::{Result, Write};

use id::Id;

use crate::{ctx::Hir, hir::*, types::Type};

pub struct HirWriter<'a, T: Write> {
	hir: &'a Hir,
	w: &'a mut T,
	indent: usize,
}

impl<'a, T: Write> HirWriter<'a, T> {
	pub fn new(hir: &'a Hir, w: &'a mut T) -> Self { Self { hir, w, indent: 0 } }
}

impl<T: Write> HirWriter<'_, T> {
	pub fn write(&mut self) -> Result {
		for (id, val) in self.hir.globals.iter() {
			if let Some(lang) = self.hir.lang_item_of(id) {
				writeln!(self.w, "@lang({})", lang)?;
			}

			self.global(val)?;
			writeln!(self.w, "\n")?;
		}

		Ok(())
	}

	pub fn global(&mut self, val: &ValDef) -> Result {
		match &val.kind {
			ValDefKind::Static(s) => {
				write!(self.w, "static ")?;
				self.path(&val.path)?;
				self.global_let(s)?;
			},
			ValDefKind::Const(c) => {
				write!(self.w, "const ")?;
				self.path(&val.path)?;
				self.global_let(c)?;
			},
			ValDefKind::Fn(f) => self.fn_(&val.path, f)?,
			ValDefKind::FnDecl(sig) => {
				self.abi(&sig.abi)?;
				write!(self.w, "fn ")?;
				self.path(&val.path)?;
				self.sig(&sig)?;
			},
			ValDefKind::Struct(s) => self.struct_(&val.path, s)?,
		}

		Ok(())
	}

	pub fn global_let(&mut self, g: &GlobalLet) -> Result {
		write!(self.w, ": ")?;
		self.ty(&g.ty)?;
		write!(self.w, " = ")?;
		self.expr(&g.expr.node)?;

		Ok(())
	}

	pub fn fn_(&mut self, path: &Path, f: &Fn) -> Result {
		self.abi(&f.sig.abi)?;
		write!(self.w, "fn ")?;
		self.path(&path)?;
		self.sig(&f.sig)?;
		write!(self.w, " ")?;
		self.block(&f.block)?;
		Ok(())
	}

	pub fn abi(&mut self, abi: &Abi) -> Result {
		match abi {
			Abi::Abi(s) => write!(self.w, "extern \"{}\" ", self.hir.resolve_intern(s.node))?,
			Abi::Extern => write!(self.w, "extern ")?,
			Abi::None => {},
		}

		Ok(())
	}

	pub fn sig(&mut self, sig: &FnSignature) -> Result {
		write!(self.w, "(")?;
		for (i, arg) in sig.args.iter().enumerate() {
			if i > 0 {
				write!(self.w, ", ")?;
			}
			self.ty(&arg.ty.node.ty)?;
		}
		write!(self.w, ") -> ")?;
		self.ty(&sig.ret)?;

		Ok(())
	}

	pub fn struct_(&mut self, path: &Path, s: &Struct) -> Result {
		write!(self.w, "struct ")?;
		self.path(&path)?;
		write!(self.w, " {{")?;
		for (i, field) in s.fields.iter().enumerate() {
			if i > 0 {
				write!(self.w, ", ")?;
			}

			write!(self.w, "{}: ", self.hir.resolve_intern(field.name.node))?;
			self.ty(&field.ty.node.ty)?;
			writeln!(self.w)?;
		}
		write!(self.w, "}}")?;
		Ok(())
	}

	pub fn block(&mut self, block: &Block) -> Result {
		writeln!(self.w, "{{")?;
		self.indent += 2;
		for stmt in block.stmts.iter() {
			write!(self.w, "{:indent$}", "", indent = self.indent)?;

			match &stmt.node {
				StmtKind::Expr(expr) => self.expr(expr)?,
				StmtKind::Semi(expr) => {
					self.expr(expr)?;
					write!(self.w, ";")?;
				},
				StmtKind::Err => {},
			}

			writeln!(self.w)?;
		}
		self.indent -= 2;
		write!(self.w, "{:indent$}}}", "", indent = self.indent)?;
		Ok(())
	}

	pub fn expr(&mut self, expr: &ExprData) -> Result {
		write!(self.w, "[")?;

		match &expr.kind {
			ExprKind::ValRef(r) => self.path(&self.hir.globals[*r].path)?,
			ExprKind::LocalRef(l) => {
				write!(self.w, "l{}", l.id())?;
			},
			ExprKind::Never => write!(self.w, "!")?,
			ExprKind::Type => write!(self.w, "type")?,
			ExprKind::TypeOf(x) => {
				write!(self.w, "type(")?;
				self.expr(&x.node)?;
				write!(self.w, ")")?;
			},
			ExprKind::Ptr(p) => {
				write!(self.w, "*{} ", if p.mutability { "mut" } else { "const" })?;
				self.expr(&p.to.node)?;
			},
			ExprKind::Tuple(vals) => {
				write!(self.w, "(")?;
				for (i, v) in vals.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.expr(&v.node)?;
				}
				write!(self.w, ")")?;
			},
			ExprKind::Lit(l) => match l {
				Lit::Bool(b) => write!(self.w, "{}", b)?,
				Lit::Char(c) => write!(self.w, "'{}'", c)?,
				Lit::Float(f) => write!(self.w, "{}", f)?,
				Lit::Int(i) => write!(self.w, "{}", i)?,
				Lit::String(s) => write!(self.w, "\"{}\"", self.hir.resolve_intern(*s))?,
			},
			ExprKind::Block(b) => self.block(b)?,
			ExprKind::Let(l) => {
				match &l.pat.node {
					PatKind::Binding(b) => {
						write!(self.w, "let l{}: ", b.binding.id())?;
					},
				}

				self.ty(&l.ty)?;

				if let Some(expr) = &l.expr {
					write!(self.w, " = ")?;
					self.expr(&expr.node)?;
				}
			},
			ExprKind::List(_) => unreachable!(),
			ExprKind::Array(_) => unreachable!(),
			ExprKind::Cast(c) => {
				self.expr(&c.expr.node)?;
				write!(self.w, " as ")?;
				self.ty(&c.ty.node.ty)?;
			},
			ExprKind::Fn(_) => unreachable!(),
			ExprKind::Call(c) => {
				self.expr(&c.target.node)?;
				write!(self.w, "(")?;
				for (i, arg) in c.args.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.expr(&arg.node)?;
				}
				write!(self.w, ")")?;
			},
			ExprKind::Index(_) => unreachable!(),
			ExprKind::Access(a) => {
				self.expr(&a.target.node)?;
				write!(self.w, ".{}", self.hir.resolve_intern(a.field.node))?;
			},
			ExprKind::Unary(u) => {
				write!(self.w, "{}", u.op)?;
				self.expr(&u.expr.node)?;
			},
			ExprKind::Binary(b) => {
				self.expr(&b.lhs.node)?;
				write!(self.w, " {} ", b.op)?;
				self.expr(&b.rhs.node)?;
			},
			ExprKind::Break(b) => {
				write!(self.w, "break")?;
				if let Some(val) = &b {
					write!(self.w, " ")?;
					self.expr(&val.node)?;
				}
			},
			ExprKind::Continue => write!(self.w, "continue")?,
			ExprKind::Return(r) => {
				write!(self.w, "return")?;
				if let Some(val) = &r {
					write!(self.w, " ")?;
					self.expr(&val.node)?;
				}
			},
			ExprKind::If(if_) => {
				write!(self.w, "if ")?;
				self.expr(&if_.cond.node)?;
				write!(self.w, " ")?;
				self.block(&if_.then)?;
				if let Some(else_) = &if_.else_ {
					write!(self.w, " else ")?;
					self.expr(&else_.node)?;
				}
			},
			ExprKind::Loop(loop_) => {
				write!(self.w, "loop ")?;
				self.block(&loop_.block)?;
				if let Some(cond) = &loop_.while_ {
					write!(self.w, " while ")?;
					self.expr(&cond.node)?;
				}
			},
			ExprKind::While(while_) => {
				write!(self.w, "while ")?;
				self.expr(&while_.cond.node)?;
				write!(self.w, " ")?;
				self.block(&while_.block)?;
			},
			ExprKind::For(_) => unreachable!(),
			ExprKind::Infer => {},
			ExprKind::Err => {},
		}

		write!(self.w, ": ")?;
		self.ty(&expr.ty)?;
		write!(self.w, "]")?;

		Ok(())
	}

	pub fn path(&mut self, path: &Path) -> Result {
		for (i, ident) in path.iter().enumerate() {
			if i > 0 {
				write!(self.w, ".")?;
			}
			write!(self.w, "{}", self.hir.resolve_intern(ident.node))?;
		}
		Ok(())
	}

	pub fn ty(&mut self, ty: &Type) -> Result {
		match ty {
			Type::Void => write!(self.w, "void")?,
			Type::Never => write!(self.w, "!")?,
			Type::Type => write!(self.w, "type")?,
			Type::Tuple(vals) => {
				write!(self.w, "(")?;
				for (i, v) in vals.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.ty(v)?;
				}
				write!(self.w, ")")?;
			},
			Type::Fn { args, ret } => {
				write!(self.w, "fn(")?;
				for (i, v) in args.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.ty(v)?;
				}
				write!(self.w, ") -> ")?;
				self.ty(ret)?;
			},
			Type::Ty(r) => self.path(&self.hir.globals[*r].path)?,
			Type::Ptr { mutable, to } => {
				write!(self.w, "*{} ", if *mutable { "mut" } else { "const" })?;
				self.ty(to)?;
			},
			Type::Unresolved(_) => write!(self.w, "unresolved")?,
			Type::Unknown => write!(self.w, "unknown")?,
			Type::Err => write!(self.w, "err")?,
		}

		Ok(())
	}
}
