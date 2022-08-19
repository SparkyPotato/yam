use diag::Span;
use intern::{Id, Resolver};
use syntax::{intern::spur_to_id, kind::SyntaxKind, SyntaxElementRef, SyntaxNode, SyntaxToken, TextRange};

pub struct Module(SyntaxNode, Id<str>);

impl Module {
	pub fn new(node: SyntaxNode, file: Id<str>) -> Self {
		assert!(matches!(node.kind(), SyntaxKind::File | SyntaxKind::Mod));
		Module(node, file)
	}

	pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Item))
			.cloned()
			.map(|x| Item(x, self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Item(SyntaxNode, Id<str>);

impl Item {
	pub fn attrs(&self) -> impl Iterator<Item = Attr> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Attribute))
			.cloned()
			.map(|x| Attr(x, self.1))
	}

	pub fn visibility(&self) -> Option<Visibility> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Visibility))
			.map(|x| Visibility(x.clone(), self.1))
	}

	pub fn kind(&self) -> Option<ItemKind> {
		self.0
			.children()
			.find(|x| {
				matches!(
					x.kind(),
					SyntaxKind::Fn
						| SyntaxKind::Struct | SyntaxKind::Trait
						| SyntaxKind::Enum | SyntaxKind::TypeAlias
						| SyntaxKind::Impl
				)
			})
			.and_then(|x| match x.kind() {
				SyntaxKind::Fn => Some(ItemKind::Fn(Fn(x.clone(), self.1))),
				SyntaxKind::Struct => Some(ItemKind::Struct(Struct(x.clone(), self.1))),
				SyntaxKind::Trait => Some(ItemKind::Trait(Trait(x.clone(), self.1))),
				SyntaxKind::Enum => Some(ItemKind::Enum(Enum(x.clone(), self.1))),
				SyntaxKind::TypeAlias => Some(ItemKind::TypeAlias(TypeAlias(x.clone(), self.1))),
				SyntaxKind::Impl => Some(ItemKind::Impl(Impl(x.clone(), self.1))),
				_ => None,
			})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum ItemKind {
	Struct(Struct),
	Enum(Enum),
	Trait(Trait),
	TypeAlias(TypeAlias),
	Fn(Fn),
	Impl(Impl),
}

impl ItemKind {
	pub fn span(&self) -> Span {
		match self {
			ItemKind::Struct(x) => x.span(),
			ItemKind::Enum(x) => x.span(),
			ItemKind::Trait(x) => x.span(),
			ItemKind::TypeAlias(x) => x.span(),
			ItemKind::Fn(x) => x.span(),
			ItemKind::Impl(x) => x.span(),
		}
	}
}

pub struct Struct(SyntaxNode, Id<str>);

impl Struct {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|x| Ident::new(x.into_token()?, self.1))
			.next()
	}

	pub fn generics(&self) -> Option<GenericsDecl> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Generics))
			.map(|x| GenericsDecl(x.clone(), self.1))
	}

	pub fn where_bound(&self) -> Option<Where> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Where))
			.map(|x| Where(x.clone(), self.1))
	}

	pub fn data(&self) -> Option<Variant> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::EnumVariant))
			.map(|x| Variant(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Enum(SyntaxNode, Id<str>);

impl Enum {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|x| Ident::new(x.into_token()?, self.1))
			.next()
	}

	pub fn generics(&self) -> Option<GenericsDecl> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Generics))
			.map(|x| GenericsDecl(x.clone(), self.1))
	}

	pub fn where_bound(&self) -> Option<Where> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Where))
			.map(|x| Where(x.clone(), self.1))
	}

	pub fn variants(&self) -> impl Iterator<Item = Variant> + '_ {
		self.0
			.children()
			.filter(|x| matches!(x.kind(), SyntaxKind::EnumVariant))
			.map(|x| Variant(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Trait(SyntaxNode, Id<str>);

impl Trait {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|x| Ident::new(x.into_token()?, self.1))
			.next()
	}

	pub fn generics(&self) -> Option<GenericsDecl> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Generics))
			.map(|x| GenericsDecl(x.clone(), self.1))
	}

	pub fn where_bound(&self) -> Option<Where> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Where))
			.map(|x| Where(x.clone(), self.1))
	}

	pub fn items(&self) -> Option<Module> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Mod))
			.map(|x| Module::new(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct TypeAlias(SyntaxNode, Id<str>);

impl TypeAlias {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|x| Ident::new(x.into_token()?, self.1))
			.next()
	}

	pub fn generics(&self) -> Option<GenericsDecl> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Generics))
			.map(|x| GenericsDecl(x.clone(), self.1))
	}

	pub fn where_bound(&self) -> Option<Where> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Where))
			.map(|x| Where(x.clone(), self.1))
	}

	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Fn(SyntaxNode, Id<str>);

impl Fn {
	pub fn is_extern(&self) -> bool { self.0.children_with_tokens().any(|x| x.kind() == SyntaxKind::ExternKw) }

	pub fn abi(&self) -> Option<Abi> {
		self.0
			.children_with_tokens()
			.filter_map(|x| match x.kind() {
				SyntaxKind::StringLit => Some(Abi(x.into_token()?.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|x| Ident::new(x.into_token()?, self.1))
			.next()
	}

	pub fn generics(&self) -> Option<GenericsDecl> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Generics))
			.map(|x| GenericsDecl(x.clone(), self.1))
	}

	pub fn where_bound(&self) -> Option<Where> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Where))
			.map(|x| Where(x.clone(), self.1))
	}

	pub fn args(&self) -> Option<Args> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Args))
			.map(|x| Args(x.clone(), self.1))
	}

	pub fn ret(&self) -> Option<Type> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn block(&self) -> Option<Block> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Block))
			.map(|x| Block(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Impl(SyntaxNode, Id<str>);

impl Impl {
	pub fn generics(&self) -> Option<GenericsDecl> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Generics))
			.map(|x| GenericsDecl(x.clone(), self.1))
	}

	pub fn what(&self) -> Option<Type> {
		self.0
			.children()
			.filter_map(|x| match x.kind() {
				SyntaxKind::Type => Some(Type(x.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn on(&self) -> Option<Type> {
		self.0
			.children()
			.filter_map(|x| match x.kind() {
				SyntaxKind::Type => Some(Type(x.clone(), self.1)),
				_ => None,
			})
			.nth(1)
	}

	pub fn where_bound(&self) -> Option<Where> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Where))
			.map(|x| Where(x.clone(), self.1))
	}

	pub fn items(&self) -> Option<Module> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Mod))
			.map(|x| Module::new(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct GenericsDecl(SyntaxNode, Id<str>);

impl GenericsDecl {
	pub fn generics(&self) -> impl Iterator<Item = GenericDecl> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Generic))
			.cloned()
			.map(|x| GenericDecl(x, self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct GenericDecl(SyntaxNode, Id<str>);

impl GenericDecl {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|x| Ident::new(x.into_token()?, self.1))
			.next()
	}

	pub fn bounds(&self) -> Option<Type> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Type | SyntaxKind::SumType))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Type(SyntaxNode, Id<str>);

impl Type {
	pub fn kind(&self) -> Option<TypeKind> {
		self.0
			.children_with_tokens()
			.find(|x| {
				matches!(
					x.kind(),
					SyntaxKind::Path | SyntaxKind::TypeOf | SyntaxKind::Ptr | SyntaxKind::Tuple | SyntaxKind::SumType
				)
			})
			.and_then(|x| match x.kind() {
				SyntaxKind::Underscore => Some(TypeKind::Infer(Infer(x.into_token()?.clone(), self.1))),
				SyntaxKind::Path => Some(TypeKind::Path(TypePath(x.into_node()?.clone(), self.1))),
				SyntaxKind::TypeOf => Some(TypeKind::TypeOf(TypeOf(x.into_node()?.clone(), self.1))),
				SyntaxKind::Ptr => Some(TypeKind::Ptr(Ptr(x.into_node()?.clone(), self.1))),
				SyntaxKind::Tuple => Some(TypeKind::Tuple(TupleType(x.into_node()?.clone(), self.1))),
				SyntaxKind::SumType => Some(TypeKind::Sum(SumType(x.into_node()?.clone(), self.1))),
				_ => None,
			})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum TypeKind {
	Path(TypePath),
	Infer(Infer),
	TypeOf(TypeOf),
	Ptr(Ptr),
	Tuple(TupleType),
	Sum(SumType),
}

pub struct TypePath(SyntaxNode, Id<str>);

impl TypePath {
	pub fn segments(&self) -> impl Iterator<Item = PathSegment> + '_ {
		self.0
			.children_with_tokens()
			.filter(|n| matches!(n.kind(), SyntaxKind::Ident | SyntaxKind::Generics))
			.filter_map(|x| match x.kind() {
				SyntaxKind::Ident => Ident::new(x.into_token()?, self.1).map(PathSegment::Ident),
				SyntaxKind::Generics => Some(PathSegment::Generics(Generics(x.into_node()?.clone(), self.1))),
				_ => None,
			})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum PathSegment {
	Ident(Ident),
	Generics(Generics),
}

pub struct Generics(SyntaxNode, Id<str>);

impl Generics {
	pub fn tys(&self) -> impl Iterator<Item = Type> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Type))
			.cloned()
			.map(|x| Type(x, self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Infer(SyntaxToken, Id<str>);

impl Infer {
	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct TypeOf(SyntaxNode, Id<str>);

impl TypeOf {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Expr))
			.map(|x| Expr(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Ptr(SyntaxNode, Id<str>);

impl Ptr {
	pub fn mutable(&self) -> Option<bool> {
		self.0
			.children_with_tokens()
			.find(|x| matches!(x.kind(), SyntaxKind::ConstKw | SyntaxKind::MutKw))
			.map(|x| x.kind() == SyntaxKind::MutKw)
	}

	pub fn to(&self) -> Option<Type> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct TupleType(SyntaxNode, Id<str>);

impl TupleType {
	pub fn elems(&self) -> impl Iterator<Item = Type> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Type))
			.cloned()
			.map(|x| Type(x, self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct SumType(SyntaxNode, Id<str>);

impl SumType {
	pub fn elems(&self) -> impl Iterator<Item = Type> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Type))
			.cloned()
			.map(|x| Type(x, self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Where(SyntaxNode, Id<str>);

impl Where {
	pub fn clauses(&self) -> impl Iterator<Item = WhereClause> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::WhereClause))
			.map(|x| WhereClause(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct WhereClause(SyntaxNode, Id<str>);

impl WhereClause {
	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn bound(&self) -> Option<Bound> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Bound))
			.map(|x| Bound(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Bound(SyntaxNode, Id<str>);

impl Bound {
	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Variant(SyntaxNode, Id<str>);

impl Variant {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.find(|x| matches!(x.kind(), SyntaxKind::Ident))
			.map(|x| Ident::new(x.into_token()?, self.1))
			.flatten()
	}

	pub fn kind(&self) -> VariantKind {
		self.0
			.children()
			.filter_map(|x| match x.kind() {
				SyntaxKind::Fields => Some(VariantKind::Fields(Fields(x.clone(), self.1))),
				SyntaxKind::Tuple => Some(VariantKind::Tuple(TupleType(x.clone(), self.1))),
				_ => unreachable!(),
			})
			.next()
			.unwrap_or(VariantKind::Empty)
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum VariantKind {
	Empty,
	Fields(Fields),
	Tuple(TupleType),
}

pub struct Fields(SyntaxNode, Id<str>);

impl Fields {
	pub fn fields(&self) -> impl Iterator<Item = Field> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Field))
			.map(|x| Field(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Field(SyntaxNode, Id<str>);

impl Field {
	pub fn ident(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.find(|x| matches!(x.kind(), SyntaxKind::Ident))
			.map(|x| Ident::new(x.into_token()?, self.1))
			.flatten()
	}

	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.find(|n| matches!(n.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Args(SyntaxNode, Id<str>);

impl Args {
	pub fn args(&self) -> impl Iterator<Item = Arg> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Arg))
			.cloned()
			.map(|x| Arg(x, self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Arg(SyntaxNode, Id<str>);

impl Arg {
	pub fn pat(&self) -> Option<Pat> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Pat))
			.map(|x| Pat(x.clone(), self.1))
	}

	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Type))
			.map(|x| Type(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Block(SyntaxNode, Id<str>);

impl Block {
	pub fn stmts(&self) -> impl Iterator<Item = StmtKind> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Expr | SyntaxKind::SemiExpr))
			.cloned()
			.filter_map(|x| match x.kind() {
				SyntaxKind::Expr => Some(StmtKind::Expr(Expr(x.clone(), self.1))),
				SyntaxKind::SemiExpr => Some(StmtKind::Semi(SemiExpr(x.clone(), self.1))),
				_ => None,
			})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum StmtKind {
	Expr(Expr),
	Semi(SemiExpr),
}

pub struct Expr(SyntaxNode, Id<str>);

impl Expr {
	pub fn kind(&self) -> Option<ExprKind> {
		self.0
			.children_with_tokens()
			.filter_map(|x| match x.kind() {
				SyntaxKind::Expr => Self(x.into_node()?.clone(), self.1).kind(),
				SyntaxKind::Tuple => Some(ExprKind::Tuple(TupleExpr(x.into_node()?.clone(), self.1))),
				SyntaxKind::Block => Some(ExprKind::Block(Block(x.into_node()?.clone(), self.1))),
				SyntaxKind::BoolLit => Some(ExprKind::BoolLit(BoolLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::CharLit => Some(ExprKind::CharLit(CharLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::FloatLit => Some(ExprKind::FloatLit(FloatLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::IntLit => Some(ExprKind::IntLit(IntLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::StringLit => Some(ExprKind::StringLit(StringLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::Ident => Some(ExprKind::Ident(Ident::new(x.into_token()?, self.1)?)),
				SyntaxKind::Break => Some(ExprKind::Break(Break(x.into_node()?.clone(), self.1))),
				SyntaxKind::Continue => Some(ExprKind::Continue(Continue(x.into_node()?.clone(), self.1))),
				SyntaxKind::Return => Some(ExprKind::Return(Return(x.into_node()?.clone(), self.1))),
				SyntaxKind::Loop => Some(ExprKind::Loop(Loop(x.into_node()?.clone(), self.1))),
				SyntaxKind::While => Some(ExprKind::While(While(x.into_node()?.clone(), self.1))),
				SyntaxKind::For => Some(ExprKind::For(For(x.into_node()?.clone(), self.1))),
				SyntaxKind::If => Some(ExprKind::If(If(x.into_node()?.clone(), self.1))),
				SyntaxKind::Let => Some(ExprKind::Let(Let(x.into_node()?.clone(), self.1))),
				SyntaxKind::Match => Some(ExprKind::Match(Match(x.into_node()?.clone(), self.1))),
				SyntaxKind::Prefix => Some(ExprKind::Prefix(Prefix(x.into_node()?.clone(), self.1))),
				SyntaxKind::Infix => Some(ExprKind::Infix(Infix(x.into_node()?.clone(), self.1))),
				SyntaxKind::Cast => Some(ExprKind::Cast(Cast(x.into_node()?.clone(), self.1))),
				SyntaxKind::Call => Some(ExprKind::Call(Call(x.into_node()?.clone(), self.1))),
				SyntaxKind::Index => Some(ExprKind::Index(Index(x.into_node()?.clone(), self.1))),
				SyntaxKind::Access => Some(ExprKind::Access(Access(x.into_node()?.clone(), self.1))),
				SyntaxKind::StructLit => Some(ExprKind::StructLit(StructLit(x.into_node()?.clone(), self.1))),
				SyntaxKind::Ascript => Some(ExprKind::Ascript(Ascript(x.into_node()?.clone(), self.1))),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum ExprKind {
	Tuple(TupleExpr),
	Block(Block),
	BoolLit(BoolLit),
	CharLit(CharLit),
	FloatLit(FloatLit),
	IntLit(IntLit),
	StringLit(StringLit),
	Ident(Ident),
	Break(Break),
	Continue(Continue),
	Return(Return),
	Loop(Loop),
	While(While),
	For(For),
	If(If),
	Let(Let),
	Match(Match),
	Prefix(Prefix),
	Infix(Infix),
	Cast(Cast),
	Call(Call),
	Index(Index),
	Access(Access),
	StructLit(StructLit),
	Ascript(Ascript),
}

pub struct TupleExpr(SyntaxNode, Id<str>);

impl TupleExpr {
	pub fn elems(&self) -> impl Iterator<Item = Expr> + '_ {
		self.0
			.children()
			.filter(|n| matches!(n.kind(), SyntaxKind::Expr))
			.map(|x| Expr(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct BoolLit(SyntaxToken, Id<str>);

impl BoolLit {
	pub fn value(&self) -> Id<str> { spur_to_id(self.0.text_key()) }

	pub fn resolve(&self, resolver: &impl Resolver<str>) -> bool { resolver.resolve(self.value()) == "true" }

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct CharLit(SyntaxToken, Id<str>);

impl CharLit {
	pub fn value(&self) -> Id<str> { spur_to_id(self.0.text_key()) }

	pub fn resolve(&self, resolver: &impl Resolver<str>) -> char {
		resolver
			.resolve(self.value())
			.chars()
			.nth(1)
			.expect("char literal parsing broke?")
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct FloatLit(SyntaxToken, Id<str>);

impl FloatLit {
	pub fn value(&self) -> Id<str> { spur_to_id(self.0.text_key()) }

	pub fn resolve(&self, resolver: &impl Resolver<str>) -> f64 {
		resolver
			.resolve(self.value())
			.parse()
			.expect("float literal parsing broke?")
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct IntLit(SyntaxToken, Id<str>);

impl IntLit {
	pub fn value(&self) -> Id<str> { spur_to_id(self.0.text_key()) }

	pub fn resolve(&self, resolver: &impl Resolver<str>) -> i64 {
		resolver
			.resolve(self.value())
			.parse()
			.expect("int literal parsing broke?")
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct StringLit(SyntaxToken, Id<str>);

impl StringLit {
	pub fn value(&self) -> Id<str> { spur_to_id(self.0.text_key()) }

	pub fn resolve<'a>(&self, resolver: &'a impl Resolver<str>) -> &'a str {
		let res = resolver.resolve(self.value());
		&res[1..res.len() - 1]
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Break(SyntaxNode, Id<str>);

impl Break {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Continue(SyntaxNode, Id<str>);

impl Continue {
	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Return(SyntaxNode, Id<str>);

impl Return {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Loop(SyntaxNode, Id<str>);

impl Loop {
	pub fn block(&self) -> Option<Block> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Block => Some(Block(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn while_(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct While(SyntaxNode, Id<str>);

impl While {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn block(&self) -> Option<Block> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Block => Some(Block(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct For(SyntaxNode, Id<str>);

impl For {
	pub fn pat(&self) -> Option<Pat> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Pat => Some(Pat(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn block(&self) -> Option<Block> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Block => Some(Block(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct If(SyntaxNode, Id<str>);

impl If {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn block(&self) -> Option<Block> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Block => Some(Block(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn else_(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Let(SyntaxNode, Id<str>);

impl Let {
	pub fn pat(&self) -> Option<Pat> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Pat => Some(Pat(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Type => Some(Type(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Match(SyntaxNode, Id<str>);

impl Match {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn arms(&self) -> Option<MatchArms> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::MatchArms => Some(MatchArms(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct MatchArms(SyntaxNode, Id<str>);

impl MatchArms {
	pub fn arms(&self) -> impl Iterator<Item = MatchArm> + '_ {
		self.0.children().filter_map(|n| match n.kind() {
			SyntaxKind::MatchArm => Some(MatchArm(n.clone(), self.1)),
			_ => None,
		})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct MatchArm(SyntaxNode, Id<str>);

impl MatchArm {
	pub fn pat(&self) -> Option<Pat> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Pat => Some(Pat(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn guard(&self) -> Option<MatchGuard> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::MatchGuard => Some(MatchGuard(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct MatchGuard(SyntaxNode, Id<str>);

impl MatchGuard {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Prefix(SyntaxNode, Id<str>);

impl Prefix {
	pub fn op(&self) -> Option<PrefixOp> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Op => Some(PrefixOp(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct PrefixOp(SyntaxNode, Id<str>);

impl PrefixOp {
	pub fn op(&self) -> Option<PrefixOpKind> {
		let mut iter = self
			.0
			.children_with_tokens()
			.filter(|x| matches!(x, SyntaxElementRef::Token(_)));
		let first = iter.next()?;
		let second = iter.next();
		match (first.kind(), second.map(|x| x.kind())) {
			(SyntaxKind::Minus, None) => Some(PrefixOpKind::Minus),
			(SyntaxKind::Exclaim, None) => Some(PrefixOpKind::Not),
			(SyntaxKind::Star, None) => Some(PrefixOpKind::Deref),
			(SyntaxKind::And, None) => Some(PrefixOpKind::Ref),
			(SyntaxKind::And, Some(SyntaxKind::MutKw)) => Some(PrefixOpKind::RefMut),
			_ => None,
		}
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum PrefixOpKind {
	Minus,
	Not,
	Deref,
	Ref,
	RefMut,
}

pub struct Infix(SyntaxNode, Id<str>);

impl Infix {
	pub fn lhs(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn rhs(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.nth(1)
	}

	pub fn op(&self) -> Option<InfixOp> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Op => Some(InfixOp(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct InfixOp(SyntaxNode, Id<str>);

impl InfixOp {
	pub fn op(&self) -> Option<InfixOpKind> {
		let mut iter = self
			.0
			.children_with_tokens()
			.filter(|x| matches!(x, SyntaxElementRef::Token(_)));
		let first = iter.next()?;
		let second = iter.next();
		match (first.kind(), second.map(|x| x.kind())) {
			(SyntaxKind::Eq, None) => Some(InfixOpKind::Assign),
			(SyntaxKind::Eq, Some(SyntaxKind::Eq)) => Some(InfixOpKind::Eq),
			(SyntaxKind::Exclaim, Some(SyntaxKind::Eq)) => Some(InfixOpKind::Ne),
			(SyntaxKind::Lt, None) => Some(InfixOpKind::Lt),
			(SyntaxKind::Lt, Some(SyntaxKind::Eq)) => Some(InfixOpKind::Le),
			(SyntaxKind::Lt, Some(SyntaxKind::Lt)) => Some(InfixOpKind::Shl),
			(SyntaxKind::Gt, None) => Some(InfixOpKind::Gt),
			(SyntaxKind::Gt, Some(SyntaxKind::Eq)) => Some(InfixOpKind::Ge),
			(SyntaxKind::Gt, Some(SyntaxKind::Gt)) => Some(InfixOpKind::Shr),
			(SyntaxKind::And, None) => Some(InfixOpKind::BitAnd),
			(SyntaxKind::And, Some(SyntaxKind::And)) => Some(InfixOpKind::And),
			(SyntaxKind::And, Some(SyntaxKind::Eq)) => Some(InfixOpKind::BitAndAssign),
			(SyntaxKind::Caret, None) => Some(InfixOpKind::BitXor),
			(SyntaxKind::Caret, Some(SyntaxKind::Eq)) => Some(InfixOpKind::BitXorAssign),
			(SyntaxKind::Pipe, None) => Some(InfixOpKind::BitOr),
			(SyntaxKind::Pipe, Some(SyntaxKind::Pipe)) => Some(InfixOpKind::Or),
			(SyntaxKind::Pipe, Some(SyntaxKind::Eq)) => Some(InfixOpKind::BitOrAssign),
			(SyntaxKind::Plus, None) => Some(InfixOpKind::Add),
			(SyntaxKind::Plus, Some(SyntaxKind::Eq)) => Some(InfixOpKind::AddAssign),
			(SyntaxKind::Minus, None) => Some(InfixOpKind::Sub),
			(SyntaxKind::Minus, Some(SyntaxKind::Eq)) => Some(InfixOpKind::SubAssign),
			(SyntaxKind::Star, None) => Some(InfixOpKind::Mul),
			(SyntaxKind::Star, Some(SyntaxKind::Eq)) => Some(InfixOpKind::MulAssign),
			(SyntaxKind::Slash, None) => Some(InfixOpKind::Div),
			(SyntaxKind::Slash, Some(SyntaxKind::Eq)) => Some(InfixOpKind::DivAssign),
			(SyntaxKind::Percent, None) => Some(InfixOpKind::Rem),
			(SyntaxKind::Percent, Some(SyntaxKind::Eq)) => Some(InfixOpKind::RemAssign),
			_ => None,
		}
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum InfixOpKind {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
	And,
	Or,
	BitAnd,
	BitOr,
	BitXor,
	Shl,
	Shr,
	Assign,
	BitAndAssign,
	BitOrAssign,
	BitXorAssign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
	RemAssign,
}

pub struct Cast(SyntaxNode, Id<str>);

impl Cast {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Type => Some(Type(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Call(SyntaxNode, Id<str>);

impl Call {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn args(&self) -> Option<CallArgs> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Args => Some(CallArgs(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct CallArgs(SyntaxNode, Id<str>);

impl CallArgs {
	pub fn args(&self) -> impl Iterator<Item = Expr> + '_ {
		self.0.children().filter_map(|n| match n.kind() {
			SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
			_ => None,
		})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Index(SyntaxNode, Id<str>);

impl Index {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn index(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.nth(1)
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Access(SyntaxNode, Id<str>);

impl Access {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn segment(&self) -> Option<PathSegment> {
		self.0
			.children_with_tokens()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Ident => Ident::new(n.into_token()?, self.1).map(PathSegment::Ident),
				SyntaxKind::Generics => Some(PathSegment::Generics(Generics(n.into_node()?.clone(), self.1))),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct StructLit(SyntaxNode, Id<str>);

impl StructLit {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn fields(&self) -> impl Iterator<Item = FieldInit> + '_ {
		self.0.children().filter_map(|n| match n.kind() {
			SyntaxKind::Field => Some(FieldInit(n.clone(), self.1)),
			_ => None,
		})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct FieldInit(SyntaxNode, Id<str>);

impl FieldInit {
	pub fn field(&self) -> Option<Ident> {
		self.0
			.children_with_tokens()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Ident => Ident::new(n.into_token()?, self.1),
				_ => None,
			})
			.next()
	}

	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Ascript(SyntaxNode, Id<str>);

impl Ascript {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Expr => Some(Expr(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn ty(&self) -> Option<Type> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Type => Some(Type(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct SemiExpr(SyntaxNode, Id<str>);

impl SemiExpr {
	pub fn expr(&self) -> Option<Expr> {
		self.0
			.children()
			.find(|x| matches!(x.kind(), SyntaxKind::Expr))
			.map(|x| Expr(x.clone(), self.1))
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Pat(SyntaxNode, Id<str>);

impl Pat {
	pub fn kind(&self) -> Option<PatKind> {
		self.0
			.children_with_tokens()
			.filter_map(|x| match x.kind() {
				SyntaxKind::Underscore => Some(PatKind::IgnoreOne),
				SyntaxKind::Dot => Some(PatKind::IgnoreMany),
				SyntaxKind::IntLit => Some(PatKind::IntLit(IntLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::CharLit => Some(PatKind::CharLit(CharLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::StringLit => Some(PatKind::StringLit(StringLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::BoolLit => Some(PatKind::BoolLit(BoolLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::FloatLit => Some(PatKind::FloatLit(FloatLit(x.into_token()?.clone(), self.1))),
				SyntaxKind::EnumVariant => Some(PatKind::Variant {
					pat: VariantPat(x.into_node()?.clone(), self.1),
					mutable: self.0.children_with_tokens().any(|x| x.kind() == SyntaxKind::MutKw),
				}),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub enum PatKind {
	IgnoreOne,
	IgnoreMany,
	IntLit(IntLit),
	CharLit(CharLit),
	StringLit(StringLit),
	BoolLit(BoolLit),
	FloatLit(FloatLit),
	Variant { pat: VariantPat, mutable: bool },
}

pub struct VariantPat(SyntaxNode, Id<str>);

impl VariantPat {
	pub fn path(&self) -> Option<TypePath> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Path => Some(TypePath(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn tuple(&self) -> Option<TuplePat> {
		self.0
			.children()
			.filter_map(|n| match n.kind() {
				SyntaxKind::Tuple => Some(TuplePat(n.clone(), self.1)),
				_ => None,
			})
			.next()
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct TuplePat(SyntaxNode, Id<str>);

impl TuplePat {
	pub fn pats(&self) -> impl Iterator<Item = Pat> + '_ {
		self.0.children().filter_map(|n| match n.kind() {
			SyntaxKind::Pat => Some(Pat(n.clone(), self.1)),
			_ => None,
		})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Abi(SyntaxToken, Id<str>);

impl Abi {
	pub fn value(&self) -> Id<str> { spur_to_id(self.0.text_key()) }

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Attr(SyntaxNode, Id<str>);

impl Attr {
	pub fn ident(&self) -> Option<Ident> { self.0.first_token().and_then(|x| Ident::new(x, self.1)) }

	pub fn tt(&self) -> Option<TokenTree> { self.0.first_child().and_then(|x| TokenTree::new(x, self.1)) }

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Ident {
	name: Id<str>,
	span: Span,
}

impl Ident {
	pub fn new(x: &SyntaxToken, file: Id<str>) -> Option<Self> {
		(x.kind() == SyntaxKind::Ident).then_some(Self {
			name: spur_to_id(x.text_key()),
			span: make_span(x.text_range(), file),
		})
	}

	pub fn name(&self) -> Id<str> { self.name }

	pub fn span(&self) -> Span { self.span }
}

pub struct TokenTree(SyntaxNode, Id<str>);

impl TokenTree {
	pub fn new(x: &SyntaxNode, file: Id<str>) -> Option<Self> {
		(x.kind() == SyntaxKind::Ident).then_some(Self(x.clone(), file))
	}

	pub fn toks(&self) -> impl Iterator<Item = Token> + '_ {
		self.0.children_with_tokens().filter_map(|x| match x {
			SyntaxElementRef::Token(t) => Some(Token::new(t, self.1)),
			SyntaxElementRef::Node(_) => None,
		})
	}

	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

pub struct Token {
	value: Id<str>,
	kind: SyntaxKind,
	span: Span,
}

impl Token {
	pub fn new(x: &SyntaxToken, file: Id<str>) -> Self {
		Self {
			value: spur_to_id(x.text_key()),
			kind: x.kind(),
			span: make_span(x.text_range(), file),
		}
	}

	pub fn value(&self) -> Id<str> { self.value }

	pub fn kind(&self) -> SyntaxKind { self.kind }

	pub fn span(&self) -> Span { self.span }
}

pub struct Visibility(SyntaxNode, Id<str>);

impl Visibility {
	pub fn span(&self) -> Span { make_span(self.0.text_range(), self.1) }
}

fn make_span(range: TextRange, file: Id<str>) -> Span {
	Span {
		start: range.start().into(),
		end: range.end().into(),
		file,
	}
}
