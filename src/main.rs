use ansi_term::Color;
use clap::{App, Arg};
use regex::{Captures, Match, Regex};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::process;
use unicode_width::UnicodeWidthStr;

fn main() {
    let matches = App::new("reverse")
        .version("0.1.0")
        .author("Max Sharnoff <reverse@max.sharnoff.org>")
        .about("A fine-grained intra-project version-tracking tool")
        .arg(Arg::with_name("INPUT").multiple(true))
        .arg(Arg::with_name("config").long("config").takes_value(true))
        .arg(
            Arg::with_name("list-defs")
                .long("list-defs")
                .conflicts_with("show-matches"),
        )
        .arg(
            Arg::with_name("show-matches")
                .long("show-matches")
                .conflicts_with("list-defs"),
        )
        .get_matches();

    let (config_path, is_default_cfg_path) = match matches.value_of("config") {
        Some(path) => (path, false),
        None => ("reverse.toml", true),
    };

    // Note: `parse_config` will use `process::exit` if it encounters an error
    let config = parse_config(config_path, is_default_cfg_path);

    let input_paths: Vec<&Path> = match matches.values_of("INPUT") {
        None => vec![".".as_ref()],
        Some(vs) => vs.map(|s| s.as_ref()).collect::<Vec<_>>(),
    };

    let list_defs = matches.is_present("list-defs");
    let show_matches = matches.is_present("show-matches");
    run(config, input_paths, list_defs, show_matches);
}

// The config, as it's parsed from a user's file
#[derive(Debug, Deserialize)]
struct Config {
    def_match: String,
    def_name_group: usize,
    def_version_group: usize,
    req_match: String,
    req_name_group: usize,
    req_version_group: usize,
}

// Parses a config from the file
fn parse_config(config_path: &str, is_default_cfg_path: bool) -> Config {
    let str_res = fs::canonicalize(config_path).and_then(|path| {
        let s = fs::read_to_string(&path)?;
        Ok((path, s))
    });

    let (abs_path, file_str) = match str_res {
        Ok((path, s)) => (path, s),
        Err(e) => {
            match is_default_cfg_path {
                true => eprintln!("Failed to read config file '{}': {}", config_path, e),
                false => eprintln!("Failed to read config file: {}", e),
            }

            process::exit(1)
        }
    };

    match toml::de::from_str(&file_str) {
        Ok(cfg) => cfg,
        Err(e) => {
            match is_default_cfg_path {
                true => eprintln!(
                    "Config parse error from '{}': {}",
                    abs_path.to_string_lossy(),
                    e
                ),
                false => eprintln!("Config parse error: {}", e),
            }

            process::exit(1);
        }
    }
}

// The actual execution of the program, with minimal handling of user input
//
// If this function encounters a critical error, it uses `process::exit` to terminate the program
fn run(config: Config, input_paths: Vec<&Path>, list_defs: bool, show_matches: bool) {
    // Step 1: Validate regexes

    let def_matcher = match Regex::new(&config.def_match) {
        Ok(re) => re,
        Err(e) => {
            eprintln!(
                "Failed to compile `def_match` {:?}: {}",
                &config.def_match, e
            );
            process::exit(1);
        }
    };

    let req_matcher = match Regex::new(&config.req_match) {
        Ok(re) => re,
        Err(e) => {
            eprintln!(
                "Failed to compile `req_match` {:?}: {}",
                &config.def_match, e
            );
            process::exit(1);
        }
    };

    // Step 2: Set up file queue(s)
    //
    // The first main thing we'll do is to set up our queues for tracking which files we still need
    // to process
    //
    // We'll use a helper macro for this because it makes some of the code simpler later on.
    let mut path_queue = Vec::new();
    let mut paths_in_progress = HashSet::new();

    macro_rules! add_path {
        ($path:expr) => {{
            let path = $path;

            match fs::canonicalize(&path) {
                Ok(p) => {
                    if paths_in_progress.insert(p.clone()) {
                        path_queue.push(p);
                    }
                }
                Err(e) => {
                    // If there was an error, we'll print it and do nothing with the path
                    eprintln!("Failed to canonicalize path {:?}: {}", path, e);
                }
            }
        }};
    }

    for p in input_paths {
        add_path!(p);
    }

    // Step 3: Process files
    //
    // After setting up the queue, we'll start going through all of the files, assembling the list
    // of definitions and requirements.
    //
    // If one of our regexes fails to give the groups we want, we'll exit.
    let mut defs = HashMap::new();
    let mut reqs = HashMap::new();

    while let Some(p) = path_queue.pop() {
        // First, we'll check what kind of filesystem object we have here:
        let meta = match fs::metadata(&p) {
            Ok(m) => m,
            Err(e) => {
                // If we couldn't get anything about it, we'll return
                eprintln!("Could not read file {:?}: {}", p, e);
                continue;
            }
        };

        // If it's a directory, we'll add all of the children to the queue
        if meta.is_dir() {
            let entries = match fs::read_dir(&p) {
                Ok(iter) => iter,
                Err(e) => {
                    eprintln!("Could not read directory {:?}: {}", p, e);
                    continue;
                }
            };

            for res in entries {
                if let Ok(entry) = res {
                    add_path!(entry.path());
                }
            }
        } else if meta.is_file() {
            process_file(
                &config,
                &def_matcher,
                &req_matcher,
                p,
                &mut defs,
                &mut reqs,
                show_matches,
            );
        }
    }

    // Step 3.1:
    //
    // Show definitions or matches:
    // The matches will have already been done in `process_file`, but we'll show the definitions
    if list_defs {
        for def in defs.values().flatten() {
            def.pretty_print();
        }
    }

    // If we listed the definitions or showed matches, we'll exit without checking for any errors.
    //
    // There's a little bit of work here in order to produce a correct info message:
    if list_defs || show_matches {
        let flag_str = match (list_defs, show_matches) {
            (true, true) => "flags '--list-defs' and '--show-matches' were enabled",
            (true, false) => "flag '--list-defs' was enabled",
            (false, true) => "flag '--show-matches' was enabled",
            (false, false) => unreachable!(),
        };

        eprintln!("Warning: No errors checked because {}", flag_str);
        process::exit(0);
    }

    // Step 4: Check for conflicts and emit errors if there are any
    //
    // First, we'll check we don't have two definitions for the same name
    let mut exit = false;
    let defs = defs
        .into_iter()
        .filter_map(|(name, mut def_list)| match def_list.len() {
            1 => Some((name, def_list.remove(0))),
            _ => {
                exit = true;
                Definition::print_conflict(&name, &def_list);
                None
            }
        })
        .collect::<HashMap<_, _>>();

    if exit {
        process::exit(1);
    }

    for req in reqs.values().flatten() {
        match defs.get(&req.name) {
            None => req.print_unknown_name(),
            Some(def) => {
                if &def.version != &req.version {
                    req.print_version_mismatch(def);
                }
            }
        }
    }
}

struct Source {
    file: PathBuf,
    line_no: usize,
    line: String,
    // The range of the line, in bytes
    line_range: Range<usize>,
}

struct Definition {
    src: Source,
    name: String,
    version: String,
}

struct Requirement {
    src: Source,
    name: String,
    version: String,
}

// Reads a file, adding it to the list
fn process_file(
    config: &Config,
    def_matcher: &Regex,
    req_matcher: &Regex,
    path: PathBuf,
    defs: &mut HashMap<String, Vec<Definition>>,
    reqs: &mut HashMap<String, Vec<Requirement>>,
    show_matches: bool,
) {
    // First, we'll attempt to read the file:
    let file_str = match fs::read(&path).map(String::from_utf8) {
        Ok(Ok(s)) => s,
        // If it wasn't utf8, we aren't interested
        Ok(Err(_)) => return,
        Err(e) => {
            eprintln!("Could not read file {:?}: {}", path, e);
            return;
        }
    };

    let source = |line: &str, line_no, re_match: Match| Source {
        file: path.clone(),
        line_no,
        line: String::from(line),
        line_range: re_match.range(),
    };

    // A helper function to exit if our regex fails
    fn no_group(group_num: usize, match_ty: &str, captures: &Captures) -> ! {
        eprintln!(
            "No match group {} for {} match {:?}",
            group_num, match_ty, &captures[0]
        );
        eprintln!("Note: here are the groups by id:");
        let indent = (captures.len() + 1).to_string().len();
        for (i, c) in captures.iter().enumerate() {
            eprintln!(
                " {:<width$}: {:?}",
                i,
                c.as_ref().map(Match::as_str),
                width = indent
            );
        }

        process::exit(1)
    }

    // And then we'll process each match in the file
    for (i, line) in file_str.lines().enumerate() {
        let line_no = i + 1;

        if let Some(cs) = def_matcher.captures(line) {
            if show_matches {
                pretty_print_match(&cs, &path, line_no, line, "definition");
            }

            // We're okay to grab index 0 because it's always guaranteed to be the whole match
            let src = source(line, line_no, cs.get(0).unwrap());
            let name = cs
                .get(config.def_name_group)
                .unwrap_or_else(|| no_group(config.def_name_group, "definition", &cs))
                .as_str();

            let version = cs
                .get(config.def_version_group)
                .unwrap_or_else(|| no_group(config.def_version_group, "definition", &cs))
                .as_str();

            let mut def_list = defs.remove(name).unwrap_or(Vec::new());
            def_list.push(Definition {
                src,
                name: String::from(name),
                version: String::from(version),
            });
            defs.insert(name.into(), def_list);
        }

        // And once more for the requirements
        if let Some(cs) = req_matcher.captures(line) {
            if show_matches {
                pretty_print_match(&cs, &path, line_no, line, "requirement");
            }

            let src = source(line, line_no, cs.get(0).unwrap());
            let name = cs
                .get(config.req_name_group)
                .unwrap_or_else(|| no_group(config.req_name_group, "requirement", &cs))
                .as_str();

            let version = cs
                .get(config.req_version_group)
                .unwrap_or_else(|| no_group(config.req_version_group, "requirement", &cs))
                .as_str();

            let mut req_list = reqs.remove(name).unwrap_or(Vec::new());
            req_list.push(Requirement {
                src,
                name: String::from(name),
                version: String::from(version),
            });
            reqs.insert(name.into(), req_list);
        }
    }
}

fn pretty_print_match(
    cs: &Captures,
    path: &Path,
    line_no: usize,
    line_str: &str,
    match_type: &str,
) {
    // The way we'll pretty print a match is by giving the file, line number, and then the full
    // line, with the region of the match highlighted in red.
    let match_range = cs.get(0).unwrap().range();

    let pre_str = &line_str[..match_range.start];
    let match_str = Color::Red.paint(&line_str[match_range.clone()]);
    let post_str = &line_str[match_range.end..];

    eprintln!(
        "{}:{}:{}: {}{}{}",
        path.to_string_lossy(),
        line_no,
        match_type,
        pre_str,
        match_str,
        post_str,
    );
}

// @require foo v0.1
const ERR_COLOR: Color = Color::Red;
const CTX_COLOR: Color = Color::Blue;

// @def foo v0.2
impl Definition {
    fn print_conflict(name: &str, defs: &[Definition]) {
        assert!(!defs.is_empty());

        let indent = defs
            .iter()
            .map(|def| def.src.required_indent())
            .max()
            .unwrap();

        let mut s = format!(
            "{} Multiple definitions for {:?} found\n",
            ERR_COLOR.paint("[error]"),
            name
        );
        for def in defs {
            writeln!(&mut s, "{}", def.src.display_with_indent(indent)).unwrap();
        }

        eprintln!("{}", s);
    }

    fn pretty_print(&self) {
        eprintln!(
            "Definition {{ name: {}, version: {} }}",
            self.name, self.version
        );
    }
}

// @def bar v3
impl Requirement {
    fn print_unknown_name(&self) {
        eprintln!(
            "{} Unknown requirement name {:?}\n{}\n",
            ERR_COLOR.paint("[error]"),
            self.name,
            self.src,
        );
    }

    fn print_version_mismatch(&self, def: &Definition) {
        let indent = self.src.required_indent().max(def.src.required_indent());

        let indent_str = " ".repeat(indent);

        eprintln!(
            concat!(
                "{} Version mismatch for '{}'; expected '{}', found '{}'\n",
                "{}\n",
                "{} {} the definition for '{}' is given here:\n",
                "{}\n",
            ),
            // 1st line
            ERR_COLOR.paint("[error]"),
            self.name,
            def.version,
            self.version,
            // 2nd line(s)
            self.src.display_with_indent(indent),
            // 3rd line
            indent_str,
            CTX_COLOR.paint("= note:"),
            def.name,
            // 4th line(s)
            def.src.display_with_indent(indent),
        );
    }
}

// @def bar v0
impl Source {
    fn required_indent(&self) -> usize {
        self.line_no.to_string().len()
    }

    fn display_with_indent<'a>(&'a self, indent: usize) -> impl 'a + Display {
        struct SourceDisplay<'b> {
            line: &'b str,
            line_no_str: String,
            file_name: &'b Path,
            indent: String,
            col: usize,
            highlight_start_width: usize,
            highlight_end_width: usize,
        }

        impl<'b> Display for SourceDisplay<'b> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let highlight_pre = " ".repeat(self.highlight_start_width);
                let highlight = "^".repeat(self.highlight_end_width - self.highlight_start_width);

                write!(
                    f,
                    concat!(
                        "{}{} {}:{}:{}\n",
                        "{} {}\n",
                        "{:<width$} {} {}\n",
                        "{} {} {}{}",
                    ),
                    // 1st line
                    self.indent,
                    CTX_COLOR.paint("-->"),
                    self.file_name.to_string_lossy(),
                    self.line_no_str,
                    self.col,
                    // 2nd line
                    self.indent,
                    CTX_COLOR.paint("|"),
                    // 3rd line
                    self.line_no_str,
                    CTX_COLOR.paint("|"),
                    self.line,
                    // 4th line
                    self.indent,
                    CTX_COLOR.paint("|"),
                    highlight_pre,
                    ERR_COLOR.paint(highlight),
                    // 3nd line width
                    width = self.indent.len(),
                )
            }
        }

        let line_no_str = self.line_no.to_string();
        let indent = " ".repeat(indent);
        let col = self.line[..self.line_range.start].chars().count();
        let highlight_start_width = self.line[..self.line_range.start].width();
        let highlight_end_width = self.line[..self.line_range.end].width();

        SourceDisplay {
            line: &self.line,
            line_no_str,
            file_name: &self.file,
            indent,
            col,
            highlight_start_width,
            highlight_end_width,
        }
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display_with_indent(self.required_indent()).fmt(f)
    }
}
