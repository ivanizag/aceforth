use std::fs;
use std::io;

use clap::Arg;
use clap::App as ClapApp;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use aceforthlib::Runner;
use aceforthlib::GRAPH_CHARS;

// Welcome message
const WELCOME: &str =
"aceforth: Jupiter Ace Forth https://github.com/ivanizag/aceforth";

const INTRO: &str =
"Enter Jupiter Ace Forth commands. Type $HELP for help, control-C to exit.";

const METACOMMAND_PREFIX: &str = "$";
const METACOMMANDS_HELP: &str = r##"
Type the same command you would type in the Jupiter ACE prompt.

Additional metacommands are available starting with {prefix}:
  {prefix}HELP: Shows this help
  {prefix}QUIT: Exits the emulator
  {prefix}SCREENSHOT: Shows the current screen contents
  {prefix}SCREEN: Toggles screen dumping
  {prefix}TRACE: Toggles ROM tracing
  {previx}IMAGE [filename]: Saves a PNG image of the screen
  {prefix}SAVE [filename]: Saves a snapshot to a file
  {prefix}LOAD [filename]: Loads a snapshot from a file
  {prefix}GRAPHS: Show the Jupiter Ace graphical characters for easy copy-pasting.
  {prefix}VIS: Toggles invisible mode, Overwrites the VIS and INVIS words
  {prefix}BUILDSNAP: (internal) Prepare a new base snapshot
"##;

fn main() {
    // Parse arguments
    let matches = ClapApp::new(WELCOME)
        .arg(Arg::with_name("cpu_trace")
            .short("c")
            .long("cpu-trace")
            .help("Traces CPU instructions execuions"))
        .arg(Arg::with_name("io_trace")
            .short("i")
            .long("io-trace")
            .help("Traces ports IN and OUT"))
        .arg(Arg::with_name("rom_trace")
            .short("r")
            .long("rom-trace")
            .help("Traces calls to the ROM entrypoints"))
        .get_matches();

    let trace_cpu = matches.is_present("cpu_trace");
    let trace_io = matches.is_present("io_trace");
    let trace_rom = matches.is_present("rom_trace");

    println!("{}", WELCOME);
    println!("{}", INTRO);
    println!();

    let mut app = App::new(trace_cpu, trace_io, trace_rom);

    let mut editor: DefaultEditor;
    match DefaultEditor::new() {
        Ok(e) => editor = e,
        Err(err) => {
            println!("Error creating the readline editor: {}", err);
            return;
        }
    }
    let _ = editor.load_history("history.txt");

    while !app.quit {
        let readline = match app.pending_input {
            Some(ref input) => editor.readline_with_initial("aceforth? ", (input, "")),
            None => editor.readline("aceforth> ")
        };

        match readline {
            Ok(line) => {
                let _ = editor.add_history_entry(line.as_str());
                app.execute(&line);

                if app.dump_screen {
                    app.print_screen();
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    let _ = editor.save_history("history.txt");
}

struct App {
    runner: Runner,
    pending_input: Option<String>,
    dump_screen: bool,
    quit: bool,
}

impl App {
    fn new(trace_cpu: bool, trace_io: bool, trace_rom: bool) -> App {
        let mut app = App {
            runner: Runner::new(trace_cpu, trace_io, trace_rom),
            pending_input: None,
            dump_screen: false,
            quit: false,
        };
        app.runner.prepare();
        app
    }

    fn execute(&mut self, commands: &str) {
        let mut lines = commands.lines().collect::<Vec<_>>();
        if lines.is_empty() {
            // At least one line is considered
            lines.push("");
        }

        for line in lines {
            if let Some(command) = line.strip_prefix(METACOMMAND_PREFIX) {
                if !command.starts_with(" ") {
                    self.execute_metacommand(command);
                    continue;
                }  
            }

            let response = self.runner.execute_command(line, 1_000_000_000);

            println!("{}", response.output);

            if let Some(error_code) = response.error_code {
                println!(">>> Error code {}: {}", error_code, Runner::error_message(error_code));
            }
            self.pending_input = response.pending_input;
            if self.quit
                    || response.error_code.is_some()
                    || self.pending_input.is_some() {
                return;
            }
        }
    }

    fn execute_metacommand(&mut self, command: &str) {
        let parts = command.split_whitespace().collect::<Vec<_>>();
        let command = parts[0].to_uppercase();

        match command.as_str() {
            "HELP" => {
                println!("{}", METACOMMANDS_HELP.replace("{prefix}", METACOMMAND_PREFIX));
            },
            "QUIT" => {
                self.quit = true;
            },
            "SCREENSHOT" => {
                self.print_screen();
            },
            "SCREEN" => {
                self.dump_screen = !self.dump_screen;
                println!("Screen dumping is now {}", if self.dump_screen { "enabled" } else { "disabled" });
            },
            "TRACE" => {
                let trace_rom = self.runner.toggle_trace_rom();
                println!("ROM tracing is now {}", if trace_rom { "enabled" } else { "disabled" });
            },
            "SAVE" => {
                let filename = parts.get(1).unwrap_or(&"snapshot.sav");
                match self.save_snapshot(filename) {
                    Ok(_) => println!("Snapshot saved to {}", filename),
                    Err(e) => println!("Error saving snapshot: {}", e),
                }
            },
            "LOAD" => {
                let filename = parts.get(1).unwrap_or(&"snapshot.sav");
                match self.load_snapshot(filename) {
                    Ok(_) => println!("Snapshot loaded from {}", filename),
                    Err(e) => println!("Error loading snapshot: {}", e),
                }
            },
            "GRAPHS" => {
                println!("Graph characters: {}", GRAPH_CHARS);
            },
            "VIS" => {
                let force_invis = self.runner.toggle_invis();
                println!("Invisible mode is now {}", if force_invis { "forced" } else { "not forced" });
            },
            "IMAGE" => {
                let filename = parts.get(1).unwrap_or(&"screen.png");
                match self.save_screen_image(filename) {
                    Ok(_) => println!("Screen image saved to {}", filename),
                    Err(e) => println!("Error saving screen image: {}", e),
                }
            },
            "BUILDSNAP" => {
                self.save_snapshot_internal();
            },
            _ => {
                println!("Unknown metacommand: {}", command);
            }
        }
    }

    fn print_screen(&self) {
        println!("╔════════════════════════════════╗");
        for line in self.runner.get_screen_as_text().lines() {
            println!("║{}║", line);
        }
        println!("╚════════════════════════════════╝");

    }

    pub fn save_snapshot(&self, filename: &str) -> io::Result<()> {
        let state = self.runner.save_snapshot();
        fs::write(filename, state)
    }

    pub fn load_snapshot(&mut self, filename: &str) -> io::Result<()> {
        let data = fs::read(filename)?;
        self.runner.load_snapshot(&data)
    }

    pub fn save_snapshot_internal(&mut self) {
        let state = self.runner.save_snapshot_internal();
        fs::write("initialstate.sav", state).unwrap();
        println!("Snapshot saved to initialstate.sav");
    }
    pub fn save_screen_image(&self, filename: &str) -> io::Result<()> {
        let image = self.runner.save_screen_image();
        fs::write(filename, image)
    }
}
