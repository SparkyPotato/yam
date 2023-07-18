use std::path::{Path, PathBuf};

use clap::Parser;
use diagnostics::{quick_diagnostic, DiagKind, FilePath};
use driver::{CompileInput, Database, SourceFile};
use tracing_subscriber::{fmt, fmt::format::FmtSpan, EnvFilter};
use walkdir::WalkDir;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Options {
	/// The root file of the package
	path: PathBuf,
}

fn main() {
	let _ = tracing::subscriber::set_global_default(
		fmt()
			.pretty()
			.with_span_events(FmtSpan::ACTIVE)
			.with_env_filter(EnvFilter::from_env("YAMLOG"))
			.finish(),
	);

	let options = Options::parse();

	let Some(root_dir) = options.path.parent() else {
		quick_diagnostic(DiagKind::Error, format!("expected file: {}", options.path.display()));
		return;
	};
	let files: Vec<_> = load_file(&options.path)
		.into_iter()
		.chain(
			WalkDir::new(root_dir)
				.into_iter()
				.filter_map(|x| x.ok())
				.filter_map(|x| {
					let path = x.path();
					let is_yam = path.extension().map(|x| x == "yam").unwrap_or(false);
					let is_root = path == options.path;
					(is_yam && !is_root).then(|| path.to_owned())
				})
				.filter_map(|path| load_file(&path)),
		)
		.collect();

	if files.is_empty() {
		return;
	}

	driver::compile(CompileInput {
		db: Database::default(),
		files,
	});
}

fn load_file(path: &Path) -> Option<SourceFile> {
	let source = std::fs::read_to_string(path)
		.map_err(|err| {
			quick_diagnostic(
				DiagKind::Error,
				format!("failed to read file `{}`: {}", path.display(), err),
			);
		})
		.ok()?;
	let path = FilePath::new(&path.to_string_lossy());
	Some(SourceFile { path, source })
}
