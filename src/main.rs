use clap::{Arg, App};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;


mod ace_machine;
mod runner;

// Welcome message
const WELCOME: &str =
"jaceforth: Jupiter Ace Forth https://github.com/ivanizag/jaceforth";

const INTRO: &str =
"Enter Jupiter Ace Forth commands. Type control-C to exit.";


fn main() {
    // Parse arguments
    let matches = App::new(WELCOME)
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

    let mut runner = runner::Runner::new(trace_cpu, trace_io, trace_rom);
    runner.prepare();

    let mut editor: DefaultEditor;
    match DefaultEditor::new() {
        Ok(e) => editor = e,
        Err(err) => {
            println!("Error creating the readline editor: {}", err);
            return;
        }
    }
    let _ = editor.load_history("history.txt");

    let mut pending_input: Option<String> = None;
    loop {
        let readline = match pending_input {
            Some(input) => editor.readline_with_initial("jaceforth? ", (&input, "")),
            None => editor.readline("jaceforth> ")
        };

        match readline {
            Ok(line) => {
                let _ = editor.add_history_entry(line.as_str());
                let response = runner.execute(&line);

                println!("{}", response.output);
                pending_input = response.pending_input;
                if let Some(error_code) = response.error_code {
                    println!("Error code: {}", error_code);
                }
                if let Some(screen) = response.screen {
                    println!("----------------------------------");
                    for line in screen.lines() {
                        println!("|{}|", line);
                    }
                    println!("----------------------------------");
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
