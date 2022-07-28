use std::collections::HashMap;

use name_resolve::resolved::{InbuiltType, Lit, Path, TyRef, ValRef};

use crate::{BasicBlock, BinOp, Ctx, Fn, Instr, InstrKind, Rodeo, Ty, Type, UnOp, Val};

pub fn pretty_print(rodeo: &Rodeo, ctx: &Ctx) {
	let printer = PrettyPrinter {
		rodeo,
		globals: &ctx.globals,
		types: &ctx.types,
	};

	for (_, val_type) in &ctx.globals {
		printer.print_val(val_type);
	}
}

pub fn pretty_print_fn(rodeo: &Rodeo, globals: &HashMap<ValRef, Val>, types: &HashMap<TyRef, Ty>, f: &Fn) {
	let printer = PrettyPrinter { rodeo, globals, types };

	printer.print_fn(f);
}

struct PrettyPrinter<'a> {
	rodeo: &'a Rodeo,
	globals: &'a HashMap<ValRef, Val>,
	types: &'a HashMap<TyRef, Ty>,
}

impl PrettyPrinter<'_> {
	pub fn print_val(&self, val: &Val) {
		match val {
			Val::Fn(f) => self.print_fn(f),
		}
	}

	pub fn print_fn(&self, f: &Fn) {
		print!("fn ");
		self.print_path(&f.path);
		print!(" -> ");
		self.print_ty(&f.ret);
		println!(":");
		for (id, block) in f.blocks.iter().enumerate() {
			self.print_block(id, block);
		}
		println!();
	}

	pub fn print_block(&self, id: usize, block: &BasicBlock) {
		print!("  b{}(", id);
		for (i, arg) in block.args.iter().enumerate() {
			print!("{}: ", i);
			self.print_ty(&arg.ty);
			if i != block.args.len() - 1 {
				print!(", ");
			}
		}
		println!("):");

		for (id, instr) in block.instrs.iter().enumerate() {
			self.print_instr(id, instr);
		}
	}

	pub fn print_instr(&self, id: usize, instr: &Instr) {
		print!("    %{} = ", id);
		self.print_instr_kind(&instr.kind);
		print!(" : ");
		self.print_ty(&instr.ty);
		println!();
	}

	pub fn print_instr_kind(&self, kind: &InstrKind) {
		match kind {
			InstrKind::Void => print!("void"),
			InstrKind::Literal(lit) => match lit {
				Lit::Bool(val) => print!("{}", val),
				Lit::Char(c) => print!("'{}'", c),
				Lit::Float(f) => print!("{}", f),
				Lit::Int(i) => print!("{}", i),
				Lit::String(s) => print!("\"{}\"", self.rodeo.resolve(s)),
			},
			InstrKind::Global(g) => match &self.globals[g] {
				Val::Fn(f) => {
					print!("fn ");
					self.print_path(&f.path);
				},
			},
			InstrKind::Arg(arg) => print!("arg {}", arg.0),
			InstrKind::Call { target, args } => print!(
				"call %{} ({})",
				target.0,
				args.iter().map(|x| format!("%{}", x.0)).collect::<Vec<_>>().join(", ")
			),
			InstrKind::Cast(val) => print!("cast {}", val.0),
			InstrKind::Unary { op, value } => print!(
				"{} %{}",
				match op {
					UnOp::Not => "!",
					UnOp::Neg => "-",
					UnOp::Addr => "&",
					UnOp::DoubleAddr => "&&",
					UnOp::AddrMut => "&mut",
					UnOp::DoubleAddrMut => "&&mut",
					UnOp::Deref => "*",
				},
				value.0
			),
			InstrKind::Binary { left, op, right } => print!(
				"%{} {} %{}",
				left.0,
				match op {
					BinOp::Add => "+",
					BinOp::Sub => "-",
					BinOp::Mul => "*",
					BinOp::Div => "/",
					BinOp::Rem => "%",
					BinOp::Shl => "<<",
					BinOp::Shr => ">>",
					BinOp::Lt => "<",
					BinOp::Gt => ">",
					BinOp::Leq => "<=",
					BinOp::Geq => ">=",
					BinOp::Eq => "==",
					BinOp::Neq => "!=",
					BinOp::BitAnd => "&",
					BinOp::BitOr => "|",
					BinOp::BitXor => "^",
					BinOp::And => "&&",
					BinOp::Or => "||",
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
					| BinOp::ShrAssign
					| BinOp::PlaceConstruct => unreachable!("shouldn't have been lowered to"),
				},
				right.0
			),
			InstrKind::Jmp { to, args } => print!(
				"jmp b{} ({})",
				to.0,
				args.iter().map(|x| format!("%{}", x.0)).collect::<Vec<_>>().join(", ")
			),
			InstrKind::CondJmp { if_, to, args } => print!(
				"jmp if %{} b{} ({})",
				if_.0,
				to.0,
				args.iter().map(|x| format!("%{}", x.0)).collect::<Vec<_>>().join(", ")
			),
			InstrKind::Ret(id) => match id {
				Some(id) => print!("ret %{}", id.0),
				None => print!("ret"),
			},
		}
	}

	pub fn print_ty(&self, ty: &Type) {
		match ty {
			Type::Void => print!("void"),
			Type::Never => print!("!"),
			Type::Fn { args, ret } => {
				print!("fn(");
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						print!(", ");
					}
					self.print_ty(arg);
				}
				print!(") -> ");
				self.print_ty(ret);
			},
			Type::TyRef(r) => match &self.types[r] {
				Ty::Inbuilt(i) => match i {
					InbuiltType::Bool => print!("bool"),
					InbuiltType::Float(v) => print!("f{}", v),
					InbuiltType::Int(v) => {
						if *v != 0 {
							print!("i{}", v);
						} else {
							print!("isize");
						}
					},
					InbuiltType::Uint(v) => {
						if *v != 0 {
							print!("u{}", v);
						} else {
							print!("usize");
						}
					},
				},
				Ty::Struct(s) => self.print_path(&s.path),
			},
			Type::Ptr { mutable, to } => {
				print!("*{}", if *mutable { "m " } else { "c " });
				self.print_ty(to);
			},
			Type::Err => print!("err"),
		}
	}

	pub fn print_path(&self, path: &Path) {
		print!(
			"{}",
			path.iter()
				.map(|x| self.rodeo.resolve(&x.node))
				.collect::<Vec<_>>()
				.join(".")
		);
	}
}
