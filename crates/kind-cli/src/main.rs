

use std::path::PathBuf;
use std::time::Instant;

use clap::{Parser, Subcommand};
use driver::resolution::ResolutionError;
use kind_driver::session::Session;

use kind_report::data::{Diagnostic, Log};

use kind_report::RenderConfig;

use kind_driver as driver;

use crate::io_write::render_to_stderr;

mod watch;
mod io_write;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[clap(propagate_version = true)]
pub struct Cli {
    /// Configuration file to change information about
    /// pretty printing or project root.
    #[arg(short, long, value_name = "FILE")]
    pub config: Option<PathBuf>,

    /// Turn on the debugging information generated
    /// by the compiler.
    #[arg(short, long)]
    pub debug: bool,

    /// Show warning messages
    #[arg(short, long)]
    pub warning: bool,

    /// Disable colors in error messages
    #[arg(short, long)]
    pub no_color: bool,

    /// How much concurrency in HVM
    #[arg(long)]
    pub tids: Option<usize>,

    /// Prints all of the functions and their evaluation
    #[arg(short, long)]
    pub trace: bool,

    /// Only ascii characters in error messages
    #[arg(short, long)]
    pub ascii: bool,

    /// Entrypoint of the file that makes the erasure checker
    /// not remove the entry.
    #[arg(short, long)]
    entrypoint: Option<String>,

    #[arg(short, long, value_name = "FILE")]
    pub root: Option<PathBuf>,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Check a file
    #[clap(aliases = &["c"])]
    Check { file: String },

    /// Evaluates Main on Kind2
    #[clap(aliases = &["er"])]
    Eval { file: String },

    #[clap(aliases = &["k"])]
    ToKindCore { file: String },

    #[clap(aliases = &["e"])]
    Erase { file: String },

    #[clap(aliases = &["w"])]
    Watch { file: String },

    /// Runs Main on the HVM
    #[clap(aliases = &["r"])]
    Run { file: String },

    /// Generates a checker (.hvm) for a file
    #[clap(aliases = &["gc"])]
    GenChecker { file: String },

    /// Stringifies a file
    #[clap(aliases = &["s"])]
    Show { file: String },

    /// Compiles a file to Kindelia (.kdl)
    #[clap(aliases = &["kdl"])]
    ToKDL {
        file: String,
        /// If given, a namespace that goes before each compiled name. Can be at most 10 charaters long.
        #[clap(long, aliases = &["ns"])]
        namespace: Option<String>,
    },

    /// Compiles a file to HVM (.hvm)
    #[clap(aliases = &["hvm"])]
    ToHVM { file: String },
}

pub fn compile_in_session<T>(
    render_config: &RenderConfig,
    root: PathBuf,
    file: String,
    compiled: bool,
    fun: &mut dyn FnMut(&mut Session) -> anyhow::Result<T>,
) -> anyhow::Result<T> {
    let (rx, tx) = std::sync::mpsc::channel();

    let mut session = Session::new(root, rx);

    eprintln!();

    render_to_stderr(
        &render_config,
        &session,
        &Log::Checking(format!("The file '{}'", file)),
    );

    let start = Instant::now();

    let res = fun(&mut session);

    let diagnostics = tx.try_iter().collect::<Vec<Box<dyn Diagnostic>>>();

    if diagnostics.is_empty() {

        render_to_stderr(
            &render_config,
            &session,
            &if compiled {
                Log::Compiled(start.elapsed())
            } else {
                Log::Checked(start.elapsed())
            },
        );

        eprintln!();

        res
    } else {
        render_to_stderr(&render_config, &session, &Log::Failed(start.elapsed()));
        eprintln!();

        for diagnostic in diagnostics {
            render_to_stderr(&render_config, &session, &diagnostic)
        }

        match res {
            Ok(_) => Err(ResolutionError.into()),
            Err(res) => Err(res)
        }
    }
}

pub async fn run_cli(config: Cli) -> anyhow::Result<()> {

    kind_report::check_if_colors_are_supported(config.no_color);

    let render_config = kind_report::check_if_utf8_is_supported(config.ascii, 2);
    let root = config.root.unwrap_or_else(|| PathBuf::from("."));

    let mut entrypoints = vec!["Main".to_string()];

    if let Some(res) = &config.entrypoint {
        entrypoints.push(res.clone())
    }

    match config.command {
        Command::Check { file } => {
            compile_in_session(&render_config, root, file.clone(), false, &mut |session| {
                let (_, rewrites) = driver::type_check_book(session, &PathBuf::from(file.clone()), entrypoints.clone(), config.tids)?;

                render_to_stderr(
                    &render_config,
                    session,
                    &Log::Rewrites(rewrites));

                Ok(())
            })?;
        }
        Command::ToHVM { file } => {
            let result = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                let book =
                    driver::erase_book(session, &PathBuf::from(file.clone()), entrypoints.clone())?;
                Ok(driver::compile_book_to_hvm(book, config.trace))
            })?;

            println!("{}", result);
        }
        Command::Run { file } => {
            let res = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                let path = PathBuf::from(file.clone());
                let book = driver::erase_book(session, &path, entrypoints.clone())?;
                driver::check_main_entry(session, &book)?;
                let book = driver::compile_book_to_hvm(book, config.trace);
                let (result, rewrites) = driver::execute_file(&book.to_string(), config.tids)?;

                render_to_stderr(
                    &render_config,
                    session,
                    &Log::Rewrites(rewrites));

                Ok(result)
            })?;
            println!("{}", res);
        }
        Command::Show { file } => {
            compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                driver::to_book(session, &PathBuf::from(file.clone()))
            })
            .map(|res| {
                print!("{}", res);
                res
            })?;
        }
        Command::ToKindCore { file } => {
            let res = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                driver::desugar_book(session, &PathBuf::from(file.clone()))
            })?;
            print!("{}", res);
        }
        Command::Erase { file } => {
            let res = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                driver::erase_book(session, &PathBuf::from(file.clone()), entrypoints.clone())
            })?;
            print!("{}", res);
        }
        Command::GenChecker { file } => {
            let res = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                driver::check_erasure_book(session, &PathBuf::from(file.clone()))
            })?;
            print!("{}", driver::generate_checker(&res));
        }
        Command::Eval { file } => {
            let res = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                let book = driver::desugar_book(session, &PathBuf::from(file.clone()))?;
                driver::check_main_desugared_entry(session, &book)?;
                let (res, rewrites) = driver::eval_in_checker(&book);

                render_to_stderr(
                    &render_config,
                    session,
                    &Log::Rewrites(rewrites));

                Ok(res)
            })?;
            println!("{}", res);
        }
        Command::ToKDL { file, namespace } => {
            let res = compile_in_session(&render_config, root, file.clone(), true, &mut |session| {
                driver::compile_book_to_kdl(
                    &PathBuf::from(file.clone()),
                    session,
                    &namespace.clone().unwrap_or("".to_string()),
                    entrypoints.clone(),
                )
            })?;
            println!("{}", res);
        },
        Command::Watch { file } => {
            watch::run(&root, &PathBuf::from(file)).await;
        }
    }

    Ok(())
}

#[tokio::main]
async fn main() {
    match run_cli(Cli::parse()).await {
        Ok(_) => std::process::exit(0),
        Err(_) => std::process::exit(1),
    }
}