use clap::{Arg, App};

mod ace_machine;
mod runner;

// Welcome message
const WELCOME: &str =
"jaceforth: Jupiter Ace Emulator https://github.com/ivanizag/jaceforth";

fn main() {
    // Parse arguments
    let matches = App::new(WELCOME)
        .arg(Arg::with_name("CMD")
            .help("The binary image to run, usually a .COM file")
            .required(false)
            .index(1))
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
    let command = match matches.value_of("CMD") {
        Some(c) => c,
        None => "",
    };

    let mut runner = runner::Runner::new(trace_cpu, trace_io, trace_rom);
    println!("Command: {}", command);
    runner.prepare();
    let (response, error_code) = runner.execute_command(command);
    print!("Response: {}", response);
    println!("Error code: {}", error_code);
}
