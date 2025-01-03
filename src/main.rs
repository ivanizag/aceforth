use clap::{Arg, App};
use iz80::*;

mod ace_machine;

use self::ace_machine::AceMachine;

// Welcome message
const WELCOME: &str =
"izace: Jupiter Ace Emulator https://github.com/ivanizag/izace";


const ROM_EMIT_CHAR_ADDRESS: u16 = 0x0008;
const WAIT_FOR_SYNC_HALT_ADDRESS: u16 = 0x0679;
const QUERY_LOOP_ADDRESS: u16 = 0x059b;

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
        None => "1 2 3 4 5 + .",
    };

    // Init device
    let mut machine = AceMachine::new(trace_io);
    let mut cpu = Cpu::new_z80();
    cpu.set_trace(trace_cpu);

    // Start the cpu
    println!("{}", WELCOME);


    // Go to the query state
    while cpu.registers().pc() != QUERY_LOOP_ADDRESS {
        cpu.execute_instruction(&mut machine);
    }

    // Set command
    println!("Command: {}", command);
    machine.inject_command(command);

    // Run up to the query loop again
    let mut response = String::new();
    cpu.execute_instruction(&mut machine);
    while cpu.registers().pc() != QUERY_LOOP_ADDRESS {
        cpu.execute_instruction(&mut machine);

        if cpu.is_halted() {
            println!("HALT instruction that will never be interrupted");
            break;
        }

        let mut pc = cpu.registers().pc();

        // Hooks on the ROM code
        match pc {
            ROM_EMIT_CHAR_ADDRESS => {
                response.push(cpu.registers().a() as char);
            },
            WAIT_FOR_SYNC_HALT_ADDRESS => {
                // Skip the HALT instruction
                pc += 1;
                cpu.registers().set_pc(pc);
            }
            _ => {}
        }

        // Tracing the ROM calls
        if trace_rom {
            match pc {
                0x0000 => println!("ROM RESET"),
                0x0008 => println!("ROM EMIT CHAR {}-{}", cpu.registers().a() as char, cpu.registers().a()),
                //0x0010 => println!("ROM PUSH DE"),
                //0x0018 => println!("ROM POP DE"),
                0x0022 => println!("ROM ERROR {}", cpu.registers().a()),
                //0x0028 => println!("ROM FIND RAM END"),
                0x0038 => println!("ROM VSYNC INT"),
                _ => {}
            }
        }
    }

    response = response.replace("\r", "\n");

    println!("Response: {}", response);
}
