use ansi_term::Color;
use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};
use glob::Pattern as GlobPattern;
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
        // @req "reverse description" v1
        .about("A fine-grained intra-project tool for maintaining internal consistency")
        .arg(
            Arg::with_name("INPUT")
                .multiple(true)
                .help("Provides the input files to search")
                .long_help(concat!(
                    "Provides the input files to search. Searching is recursive, and\n",
                    "excludes any files matching the exclude list from the configuration.\n",
                    "If no input paths are given, the default path is the current directory.\n",
                )),
        )
        .arg(
            Arg::with_name("config")
                .long("config")
                .takes_value(true)
                .help("Optionally sets the configuration file to use")
                .long_help(concat!(
                    "Optionally sets the configuration file to use. If not provided,\n",
                    "this defaults to 'reverse.toml' in the current directory",
                )),
        )
        .arg(
            Arg::with_name("quiet")
                .long("quiet")
                .help("suppresses output, exiting with 1 to indicate failure")
                .long_help(concat!(
                    "Suppresses output, exiting with 1 to indicate failure.\n",
                    "This may be particularly useful part of a git pre-commit hook, for\n",
                    "example so that internal version matching is guaranteed",
                )),
        )
        .arg(
            Arg::with_name("list-defs")
                .long("list-defs")
                .help("lists all of the matches for definitions in the searched files")
                .long_help(concat!(
                    "lists all of the matches for definitions in the searched files.\n",
                    "See also: `--show-matches`",
                ))
                .conflicts_with_all(&["quiet", "show-matches"]),
        )
        .arg(
            Arg::with_name("show-matches")
                .long("show-matches")
                .help("displays every instance of a match for any pattern")
                .conflicts_with_all(&["quiet", "list-defs"]),
        )
        .arg(
            Arg::with_name("strict")
                .long("strict")
                .short("s")
                .help("exits with an error code if a warning is matched")
        )
        // The 'init' subcommand shouldn't take other arguments from above
        .setting(AppSettings::ArgsNegateSubcommands)
        .subcommand(
            SubCommand::with_name("init")
                .about("Initializes a 'reverse.toml' for a project")
                .arg(
                    Arg::with_name("DIR")
                        .takes_value(true)
                        .help("Provides a directory to initialze into")
                )
                .arg(
                    Arg::with_name("filename")
                        .short("f")
                        .long("filename")
                        .takes_value(true)
                        .help("Sets the name of the file to write to, within the directory")
                        .long_help(concat!(
                            "Sets the name of the file to write to, within the directory. Defaults\n",
                            "to 'reverse.toml'."
                        ))
                )
                .arg(
                    Arg::with_name("lang")
                        .long("lang")
                        .takes_value(true)
                        .help("Load the configuration from a language preset"),
                )
        )
        .after_help(concat!(
            "Information about configuration:\n",
            "\n",
            "By default, `reverse` searches in the current directory for a configuration file,\n",
            "specifially at 'reverse.toml'. This can be set with the `--config` flag. As might be\n",
            "evident by the name, this is expected to be a TOML file, with a few different fields.\n",
            "If you're new to this tool, don't worry yet about what these fields correspond to.\n",
            "\n",
            // @req "config field names" v1
            "     | field name        | type     | purpose                                    \n",
            "     |-------------------|----------|--------------------------------------------\n",
            "     | def_match         | string   | The regex used to match on definitions     \n",
            "     | def_name_group    | integer  | The group index of matches from `def_match`\n",
            "     |                   |          | to use for the unique definition name.     \n",
            "     | def_version_group | integer  | The group index from `def_match` to provide\n",
            "     |                   |          | the version of the definition, as a string \n",
            "     | req_match         | string   | Same as `def_match`, but for requirements  \n",
            "     | req_name_group    | integer  | Same as `def_name_group`, but for          \n",
            "     |                   |          | requirements                               \n",
            "     | req_version_group | integer  | Same as `def_version_group`, but for       \n",
            "     |                   |          | requirements                               \n",
            "     | warn_match        | string   | An optional regex to warn on if neither    \n",
            "     |                   |          | 'def' nor 'req' match.                     \n",
            "     | exclude           | [string] | This optional list of patterns gives a set \n",
            "     |                   |          | of paths to exclude from searching.        \n",
            "\n",
            "This tool is fairly simple; it recursively searches all of the files/directories\n",
            "supplied to it (defaulting to the current directory), and finds all occurences of\n",
            "'definitions' and 'requirements'. These are both simply `(name, version)` pairs,\n",
            "where all requirements must the same version as the original definition.\n",
            "\n",
            "The fields described above essentially give the instructions for finding these\n",
            "definitions and requirements, alongside their version names.\n",
            "\n",
            "\n",
            "Sample configuration:\n",
            "\n",
            "For Rust projects, I find a configuration like the following works well:\n",
            "\n",
            "    def_match         = '''@def\\s*([^\\s]+|\"[^\"]*\")\\s*(v[\\d\\.]+)'''\n",
            "    def_name_group    = 1\n",
            "    def_version_group = 2\n",
            "    req_match         = '''@req\\s*([^\\s]+|\"[^\"]*\")\\s*(v[\\d\\.]+)'''\n",
            "    req_name_group    = 1\n",
            "    req_version_group = 2\n",
            "    warn_match        = '''@(req|def)'''\n",
            "    exclude           = [\"target\", \".git\", \"reverse.toml\"]\n",
            "\n",
            "where definitions are given by (in a code comment):\n",
            // This trips up normal 'reverse' runs on the repo, so we'll add a little '@req' just
            // to keep it happy:
            //     @req foo v0.0.1
            "   // @def foo v0.0.1\n",
            "or:\n",
            "   // @def \"longer foo bar\" v0.0.1\n",
            "and requirements are given exactly the same way, except with `@req` replacing\n",
            "`@def`.\n",
            "\n",
            "\n",
            "Sample usage in a git pre-commit hook:\n",
            "\n",
            "Because it's mentioned above in the description for `--quiet`, it's worth showing\n",
            "how `reverse` can be used as part of a pre-commit hook. Here's one, for example:\n",
            "\n",
            "    #!/bin/sh\n",
            "\n",
            "    dir=$(git rev-parse --show-toplevel)\n",
            "\n",
            "    # Check that there's no internal version mismatches\n",
            "    reverse --quiet --strict --config=\"$dir/reverse.toml\"\n",
            "    if [ \"$?\" -ne \"0\" ]; then\n",
            "        exit 1\n",
            "    fi\n",
            "\n",
            "    # Otherwise, continue on with the hook script...\n",
        ))
        .get_matches();

    if let Some(init_matches) = matches.subcommand_matches("init") {
        run_init(init_matches);
        return;
    }

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
    let quiet = matches.is_present("quiet");
    let strict = matches.is_present("strict");
    run(config, input_paths, quiet, list_defs, show_matches, strict);
}

fn run_init(matches: &ArgMatches) {
    macro_rules! config_fmt {
        () => {
            concat! {
                // @req "reverse repo url" v0
                "# Configuration for `reverse`: https://github.com/sharnoff/reverse\n",
                "def_match         = '''@def\\s*([^\\s]+|\"[^\"]*\")\\s*(v[\\d\\.]+)'''\n",
                "def_name_group    = 1\n",
                "def_version_group = 2\n",
                "req_match         = '''@req\\s*([^\\s]+|\"[^\"]*\")\\s*(v[\\d\\.]+)'''\n",
                "req_name_group    = 1\n",
                "req_version_group = 2\n",
                "warn_match        = '''@(req|def)'''\n",
                "exclude           = [\".git\", \"reverse.toml\"{}]\n",
            }
        };
    }

    #[rustfmt::skip]
    static LANG_CONFIGS: &[(&str, &[&str], &str)] = &[
        ("Rust", &["rs", "rust"], ", \"target\""),
        ("Python", &["py", "python"], ", \".__pycache__\""),
    ];

    let path = Path::new(matches.value_of("DIR").unwrap_or("."))
        .join(matches.value_of("filename").unwrap_or("reverse.toml"));

    let extra_exclude = match matches.value_of("lang") {
        None => "",
        Some(lang) => {
            // Search for the language with the given name/abbreviation
            let matched_lang = LANG_CONFIGS
                .iter()
                .find(|(_, abs, _)| abs.iter().any(|&a| a == lang));

            match matched_lang {
                Some((_, _, exclude)) => exclude,
                None => {
                    // Print an error message with descriptive information about available
                    // languages, alongside suggestions for adding a new one.
                    eprintln!(
                        "No language found for name '{}'. The available options are listed below:",
                        lang
                    );

                    for (lang, aliases, _) in LANG_CONFIGS.iter() {
                        let mut alias_str = aliases[0].to_owned();
                        aliases[1..].iter().for_each(|a| {
                            alias_str.push_str(", ");
                            alias_str.push_str(a);
                        });

                        eprintln!("  {}: {}", lang, alias_str);
                    }

                    eprintln!(concat!(
                        "If you'd like to add a language, just open an issue on the repository at:",
                        // @req "reverse repo url" v0
                        "https://github.com/sharnoff/reverse"
                    ));

                    process::exit(1);
                }
            }
        }
    };

    match std::fs::write(&path, format!(config_fmt!(), extra_exclude)) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("failed to write config to {:?}: {}", path, e);
            process::exit(1);
        }
    }
}

// The config, as it's parsed from a user's file
// @def "config field names" v1
#[derive(Debug, Deserialize)]
struct Config {
    def_match: String,
    def_name_group: usize,
    def_version_group: usize,
    req_match: String,
    req_name_group: usize,
    req_version_group: usize,
    warn_match: Option<String>,

    #[serde(default)]
    exclude: Vec<String>,
}

// Parses a config from the file
fn parse_config(config_path: &str, is_default_cfg_path: bool) -> Config {
    let str_res = fs::canonicalize(config_path).and_then(|path| {
        let s = fs::read_to_string(&path)?;
        Ok((path, s))
    });

    let (abs_config_path, file_str) = match str_res {
        Ok((path, s)) => (path, s),
        Err(e) => {
            match is_default_cfg_path {
                true => eprintln!("Failed to read config file '{}': {}", config_path, e),
                false => eprintln!("Failed to read config file: {}", e),
            }

            process::exit(1)
        }
    };

    let mut cfg: Config = match toml::de::from_str(&file_str) {
        Ok(cfg) => cfg,
        Err(e) => {
            match is_default_cfg_path {
                true => eprintln!(
                    "Config parse error from '{}': {}",
                    abs_config_path.to_string_lossy(),
                    e
                ),
                false => eprintln!("Config parse error: {}", e),
            }

            process::exit(1);
        }
    };

    // We'll localize all of the excludes to the directory that the config was in
    //
    // we can unwrap here because '/' will never be a file
    let config_parent = abs_config_path.parent().unwrap();

    for ex_path in cfg.exclude.iter_mut() {
        let p: &Path = ex_path.as_ref();
        if !p.is_absolute() {
            *ex_path = config_parent.join(&ex_path).to_string_lossy().into();
        }
    }

    cfg
}

// The actual execution of the program, with minimal handling of user input
//
// If this function encounters a critical error, it uses `process::exit` to terminate the program
fn run(
    config: Config,
    input_paths: Vec<&Path>,
    quiet: bool,
    list_defs: bool,
    show_matches: bool,
    strict: bool,
) {
    // Step 1: Validate regexes and produce exclusion globs

    fn make_regex(string: &str, field: &str) -> Regex {
        match Regex::new(string) {
            Ok(re) => re,
            Err(e) => {
                eprintln!("Failed to compile `{}` {:?}: {}", field, string, e);
                process::exit(1);
            }
        }
    }

    let def_matcher = make_regex(&config.def_match, "def_match");
    let req_matcher = make_regex(&config.req_match, "req_match");
    let warn_matcher = config
        .warn_match
        .as_ref()
        .map(|s| make_regex(s.as_str(), "warn_match"));

    let mut excludes_failed = false;
    let mut excludes = Vec::new();
    for abs_path in &config.exclude {
        match GlobPattern::new(abs_path) {
            Ok(g) => excludes.push(g),
            Err(e) => {
                eprintln!("Invalid glob {:?}: {}", abs_path, e);
                excludes_failed = true;
            }
        }
    }

    if excludes_failed {
        process::exit(1);
    }

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
        // Skip this path if it matches an exclude glob:
        if excludes.iter().any(|g| g.matches(&p.to_string_lossy())) {
            continue;
        }

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
                warn_matcher.as_ref(),
                p,
                &mut defs,
                &mut reqs,
                show_matches,
                quiet,
                strict,
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
        if list_defs && defs.is_empty() {
            eprintln!("no definitions found");
        } else if show_matches && (defs.is_empty() && reqs.is_empty()) {
            eprintln!("no matches found");
        }
        process::exit(0);
    }

    if !quiet && defs.is_empty() && reqs.is_empty() {
        eprintln!("no matches found");
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

    // We'll group the definitions with the requirements that use them, so that we can give better
    // formatting when displaying
    let mut sets = defs
        .into_iter()
        .map(|(name, def)| (name, (def, Vec::new())))
        .collect::<HashMap<_, _>>();

    for req in reqs.values().flatten() {
        match sets.get_mut(&req.name) {
            None => {
                req.print_unknown_name();
                exit = true;
            }
            Some((_, ref mut reqs)) => reqs.push(req),
        }
    }

    if exit {
        process::exit(1);
    }

    for (def, reqs) in sets.values() {
        if !quiet {
            let n_passed = reqs.iter().filter(|r| r.version == def.version).count();
            let color = match () {
                () if reqs.is_empty() => WARN_COLOR,
                () if n_passed == reqs.len() => PASS_COLOR,
                () => {
                    exit = true;
                    ERR_COLOR
                }
            };

            eprintln!(
                "{} -> {}",
                def.name,
                color.paint(format!("{}/{}", n_passed, reqs.len()))
            );
        }

        reqs.iter().for_each(|req| {
            if req.version != def.version {
                req.print_version_mismatch(def);
                exit = true;
            }
        });
    }

    if exit {
        process::exit(1);
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
// TODO: This should use a `RegexSet` instead of multiple, individual regexes.
fn process_file(
    config: &Config,
    def_matcher: &Regex,
    req_matcher: &Regex,
    warn_matcher: Option<&Regex>,
    path: PathBuf,
    defs: &mut HashMap<String, Vec<Definition>>,
    reqs: &mut HashMap<String, Vec<Requirement>>,
    show_matches: bool,
    quiet: bool,
    strict: bool,
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

        let mut matched_def = false;
        let mut matched_req = false;

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
            matched_def = true;
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
            matched_req = true;
        }

        if let Some(m) = warn_matcher.and_then(|m| m.find(line)) {
            if !matched_req && !matched_def {
                if !quiet || strict {
                    let src = source(line, line_no, m);
                    let indent = src.required_indent();
                    let color = if strict { ERR_COLOR } else { WARN_COLOR };
                    eprintln!("{}", src.display_with_indent(indent, color));
                }

                if strict {
                    process::exit(1);
                }
            }
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

const ERR_COLOR: Color = Color::Red;
const CTX_COLOR: Color = Color::Blue;
const WARN_COLOR: Color = Color::Yellow;
const PASS_COLOR: Color = Color::Green;

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
            writeln!(&mut s, "{}", def.src.display_with_indent(indent, ERR_COLOR)).unwrap();
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
            self.src.display_with_indent(indent, ERR_COLOR),
            // 3rd line
            indent_str,
            CTX_COLOR.paint("= note:"),
            def.name,
            // 4th line(s)
            def.src.display_with_indent(indent, ERR_COLOR),
        );
    }
}

impl Source {
    fn required_indent(&self) -> usize {
        self.line_no.to_string().len()
    }

    fn display_with_indent<'a>(
        &'a self,
        indent: usize,
        highlight_color: Color,
    ) -> impl 'a + Display {
        struct SourceDisplay<'b> {
            line: &'b str,
            line_no_str: String,
            file_name: &'b Path,
            indent: String,
            col: usize,
            highlight_start_width: usize,
            highlight_end_width: usize,
            highlight_color: Color,
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
                    self.highlight_color.paint(highlight),
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
            highlight_color,
        }
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.display_with_indent(self.required_indent(), ERR_COLOR)
            .fmt(f)
    }
}
