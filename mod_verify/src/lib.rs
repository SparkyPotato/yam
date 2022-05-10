use diag::{
	ariadne::{Label, Report, ReportKind},
	Span,
};
use parse::ast::{ExprKind, Module, StmtKind};

pub fn verify(module: &Module, diags: &mut Vec<Report<Span>>) {
	for stmt in &module.stmts {
		diags.push(match &stmt.node {
			StmtKind::Expr(e) | StmtKind::Semi(e) => match e {
				ExprKind::Lit(_) => expr_not_allowed_in_context(stmt.span, "literals"),
				ExprKind::Block(_) => expr_not_allowed_in_context(stmt.span, "blocks"),
				ExprKind::Ident(_) => expr_not_allowed_in_context(stmt.span, "identifiers"),
				ExprKind::Let(_) => expr_not_allowed_in_context(stmt.span, "let bindings"),
				ExprKind::List(_) | ExprKind::Array(_) => expr_not_allowed_in_context(stmt.span, "arrays"),
				ExprKind::Type | ExprKind::TypeOf(_) | ExprKind::Ptr(_) | ExprKind::Struct(_) => {
					expr_not_allowed_in_context(stmt.span, "type expressions")
				},
				ExprKind::MacroRef(_) => expr_not_allowed_in_context(stmt.span, "macros"),
				ExprKind::Call(_) => expr_not_allowed_in_context(stmt.span, "calls"),
				ExprKind::Index(_) => expr_not_allowed_in_context(stmt.span, "indexes"),
				ExprKind::Access(_) => expr_not_allowed_in_context(stmt.span, "field accesses"),
				ExprKind::Unary(_) => expr_not_allowed_in_context(stmt.span, "unary expressions"),
				ExprKind::Binary(_) => expr_not_allowed_in_context(stmt.span, "binary expressions"),
				ExprKind::Cast(_) => expr_not_allowed_in_context(stmt.span, "casts"),
				_ => continue,
			},
			_ => continue,
		})
	}
}

fn expr_not_allowed_in_context(span: Span, expr: &str) -> Report<Span> {
	span.report(ReportKind::Error)
		.with_message(format!("{} are not allowed in this context", expr))
		.with_label(Label::new(span))
		.finish()
}
