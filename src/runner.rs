use iz80::*;

use super::ace_machine::AceMachine;

pub struct Runner {
    cpu: Cpu,
    machine: AceMachine,
    trace_rom: bool,
}

const ROM_EMIT_CHAR_ADDRESS: u16 = 0x0008;
const ROM_RAISE_ERROR_ADDRESS: u16 = 0x0022;
const WAIT_FOR_SYNC_HALT_ADDRESS: u16 = 0x0679;
const QUERY_LOOP_ADDRESS: u16 = 0x059b;

impl Runner {
    pub fn new(trace_cpu: bool, trace_io: bool, trace_rom: bool) -> Runner {
        let machine = AceMachine::new(trace_io);
        let mut cpu = Cpu::new_z80();
        cpu.set_trace(trace_cpu);

        Runner {
            cpu,
            machine,
            trace_rom,
        }
    }

    pub fn prepare(&mut self) {
        // Go to the query state
        while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);
        }
    }

    pub fn execute_command(&mut self, command: &str) -> (String, u8) {

        // Set command
        self.machine.inject_command(command);

        let mut response = String::new();
        let mut error_code = 0;

        // Run up to the query loop again
        if self.cpu.registers().pc() == QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);
        }

        while self.cpu.registers().pc() != QUERY_LOOP_ADDRESS {
            self.cpu.execute_instruction(&mut self.machine);

            if self.cpu.is_halted() {
                println!("HALT instruction that will never be interrupted");
                break;
            }

            let mut pc = self.cpu.registers().pc();

            // Hooks on the ROM code
            match pc {
                ROM_EMIT_CHAR_ADDRESS => {
                    response.push(self.cpu.registers().a() as char);
                },

                ROM_RAISE_ERROR_ADDRESS => {
                    error_code = self.cpu.registers().a();
                },

                WAIT_FOR_SYNC_HALT_ADDRESS => {
                    // Skip the HALT instruction used to slow down listings
                    pc += 1;
                    self.cpu.registers().set_pc(pc);
                }
                _ => {}
            }

            // Tracing the ROM calls
            if self.trace_rom {
                match pc {
                    0x0000 => println!("ROM RESET"),
                    0x0008 => println!("ROM EMIT CHAR {}-{}", self.cpu.registers().a() as char, self.cpu.registers().a()),
                    //0x0010 => println!("ROM PUSH DE"),
                    //0x0018 => println!("ROM POP DE"),
                    0x0022 => println!("ROM ERROR {}", self.cpu.registers().a()),
                    //0x0028 => println!("ROM FIND RAM END"),
                    0x0038 => println!("ROM VSYNC INT"),
                    _ => {}
                }
            }
        }

        response = response.replace("\r", "\n");
        (response, error_code)
    }
}
