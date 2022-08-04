use std::fmt::{Result, Write};

use hir::{
	hir::{Abi, FnSignature, Lit, Path, Struct},
	types::Type,
};
use id::Id;

use crate::ssir::*;

pub struct SsirWriter<'a, T> {
	ssir: &'a Ssir,
	w: &'a mut T,
}

impl<'a, T: Write> SsirWriter<'a, T> {
	pub fn new(ssir: &'a Ssir, w: &'a mut T) -> Self { Self { ssir, w } }
}

impl<T: Write> SsirWriter<'_, T> {
	pub fn write(&mut self) -> Result {
		for (_, ty) in self.ssir.tys.iter() {}

		for (_, global) in self.ssir.values.iter() {
			self.global(global)?;
			writeln!(self.w)?;
		}

		Ok(())
	}

	pub fn ty_def(&mut self, ty: &TyDef) -> Result {
		match &ty.kind {
			TyDefKind::Struct(s) => self.struct_(&ty.path, s)?,
			TyDefKind::LangItem(_) => {},
		}

		Ok(())
	}

	pub fn global(&mut self, val: &ValDef) -> Result {
		match &val.kind {
			ValDefKind::FnDecl(sig) => self.sig(&val.path, sig),
			ValDefKind::Fn(f) => self.fn_(&val.path, f),
		}
	}

	pub fn abi(&mut self, abi: &Abi) -> Result {
		match abi {
			Abi::Abi(s) => write!(self.w, "extern \"{}\" ", self.ssir.resolve_intern(s.node))?,
			Abi::Extern => write!(self.w, "extern ")?,
			Abi::None => {},
		}

		Ok(())
	}

	pub fn sig(&mut self, path: &Path, sig: &FnSignature) -> Result {
		self.abi(&sig.abi)?;

		write!(self.w, "fn ")?;
		self.path(path)?;
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

	pub fn fn_(&mut self, path: &Path, f: &Fn) -> Result {
		self.abi(&f.abi)?;
		write!(self.w, "fn ")?;
		self.path(&path)?;
		write!(self.w, " -> ")?;
		self.ty(&f.ret)?;
		writeln!(self.w, ":")?;

		for (id, block) in f.blocks.iter() {
			self.block(id, block)?;
		}

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

			write!(self.w, "{}: ", self.ssir.resolve_intern(field.name.node))?;
			self.ty(&field.ty.node.ty)?;
			writeln!(self.w)?;
		}
		write!(self.w, "}}")?;
		Ok(())
	}

	pub fn block(&mut self, id: Block, block: &BasicBlock) -> Result {
		write!(self.w, "  b{}(", id.id())?;
		for (i, arg) in block.args().enumerate() {
			if i > 0 {
				write!(self.w, ", ")?;
			}

			self.value(arg.0)?;
			write!(self.w, ": ")?;
			self.ty(&arg.1)?;
		}
		writeln!(self.w, "):")?;

		for (id, instr) in block.instrs() {
			write!(self.w, "#{:>3}", id.id())?;

			match instr {
				Instr::Value { value, instr, ty } => {
					self.value(*value)?;
					write!(self.w, " = ")?;
					self.val_instr(&instr.kind)?;
					write!(self.w, ": ")?;
					self.ty(&instr.ty)?;
					writeln!(self.w)?;
				},
				Instr::NonValue(_) => {},
			}
		}

		Ok(())
	}

	pub fn val_instr(&mut self, instr: &ValueInstr) -> Result {
		match instr {
			ValueInstr::Literal(l) => match l {
				Lit::Bool(b) => write!(self.w, "{}", b)?,
				Lit::Int(i) => write!(self.w, "{}", i)?,
				Lit::Float(f) => write!(self.w, "{}", f)?,
				Lit::Char(c) => write!(self.w, "'{}'", c)?,
				Lit::String(s) => write!(self.w, "\"{}\"", self.ssir.resolve_intern(*s))?,
			},
			ValueInstr::Global(g) => {
				write!(self.w, "load ")?;
				self.path(&self.ssir.values[*g].path)?;
			},
			ValueInstr::Call { target, args } => {
				write!(self.w, "call ")?;
				self.value(*target)?;
				write!(self.w, "(")?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.value(*arg)?;
				}
				write!(self.w, ")")?;
			},
			ValueInstr::Cast(v) => {
				write!(self.w, "cast ")?;
				self.value(*v)?;
			},
			ValueInstr::Unary { op, value } => {
				write!(self.w, "{} ", op)?;
				self.value(*value)?;
			},
			ValueInstr::Binary { left, op, right } => {
				self.value(*left)?;
				write!(self.w, " {} ", op)?;
				self.value(*right)?;
			},
		}

		Ok(())
	}

	pub fn non_val_instr(&mut self, instr: &NonValueInstr) -> Result {
		match instr {
			NonValueInstr::Jump { to, args } => {
				write!(self.w, "jmp b{}(", to.id())?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.value(*arg)?;
				}
				write!(self.w, ")")?;
			},
			NonValueInstr::JumpIf { cond, to, args } => {
				write!(self.w, "jmp if ")?;
				self.value(*cond)?;
				write!(self.w, " b{}(", to.id())?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(self.w, ", ")?;
					}
					self.value(*arg)?;
				}
				write!(self.w, ")")?;
			},
			NonValueInstr::Ret(ret) => {
				write!(self.w, "ret ")?;
				if let Some(ret) = ret {
					self.value(*ret)?;
				}
			},
		}

		Ok(())
	}

	pub fn path(&mut self, path: &Path) -> Result {
		for (i, ident) in path.iter().enumerate() {
			if i > 0 {
				write!(self.w, ".")?;
			}
			write!(self.w, "{}", self.ssir.resolve_intern(ident.node))?;
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
			Type::Ty(r) => self.path(&self.ssir.tys[*r].path)?,
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

	pub fn value(&mut self, value: Value) -> Result {
		if value.id() >= Value::UNKNOWN.id() {
			write!(self.w, "unknown")
		} else {
			write!(self.w, "%{}", value.id())
		}
	}
}
