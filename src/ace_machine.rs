use iz80::Machine;

static ROM: &[u8] = include_bytes!("../resources/jupiter ace.rom");


const INSCRN_ADDRESS: u16 = 0x3C1E; // Start of the input buffer
const CURSOR_ADDRESS: u16 = 0x3C20; // Cursor position
const ENDBUF_ADDRESS: u16 = 0x3C22; // End of the input buffer
const LHALF_ADDRESS:  u16 = 0x3C24; // End of the output buffer
const ADDRESS_STATIN: u16 = 0x3c28;


const START_OF_SCREEN: u16 = 0x2400;
const MINIMAL_END_OF_OUTPUT_BUFFER: u16 = 0x2440;
const INITIAL_START_OF_INPUT_BUFFER: u16 = 0x26e0;
const END_OF_SCREEN: u16 = 0x2700;
pub const MAX_INPUT_BUFFER_SIZE: u16 = END_OF_SCREEN - MINIMAL_END_OF_OUTPUT_BUFFER - 2;

const COLUMNS: u16 = 32;

const STATIN_ENTER_MASK: u8 = 0b0010_0000;

pub struct AceMachine {
    ram: [u8; 65536],
    trace_io: bool,
}

impl AceMachine {
    pub fn new(trace_io: bool) -> AceMachine {
        AceMachine {
            ram: [0; 65536],
            trace_io,
        }
    }

    pub fn inject_command(&mut self, command: &str) {
        for ch in command.chars() {
            self.inject_char(ch);
        }

        let mut statin = self.peek(ADDRESS_STATIN);
        statin |= STATIN_ENTER_MASK;
        self.poke(ADDRESS_STATIN, statin);
    }

    fn inject_char(&mut self, c: char) -> bool {
        // The char is inserted at the end of the buffer
        // Assumptions:
        //    - The cursor is at the last line of the screen
        //    - The cursor is at the end of the buffer
        let mut cursor = self.peek16(CURSOR_ADDRESS);

        if cursor == END_OF_SCREEN - 1 {
            // The cursor is at the end of the screen
            // We need to scroll the screen up if the output buffer
            // is bigger that two lines
            let mut end_of_output = self.peek16(LHALF_ADDRESS);
            if end_of_output <= MINIMAL_END_OF_OUTPUT_BUFFER {
                // We can't scroll the screen anymore
                return false;
            }

            // Scroll the screen up one line
            for i in START_OF_SCREEN..(END_OF_SCREEN - COLUMNS) {
                self.poke(i, self.peek(i+32));
            }
            for i in (END_OF_SCREEN-32)..END_OF_SCREEN {
                self.poke(i, 0x20);
            }

            let mut start_of_input = self.peek16(INSCRN_ADDRESS);
            start_of_input += 32;
            self.poke16(INSCRN_ADDRESS, start_of_input);

            end_of_output -= 32;
            self.poke16(LHALF_ADDRESS, end_of_output);

            cursor -= 32;
        }

        self.poke(cursor, c as u8);
        cursor += 1;
        self.poke16(CURSOR_ADDRESS, cursor);
        self.poke16(ENDBUF_ADDRESS, cursor + 1);
        true
    }

    pub fn extract_pending_input(&mut self) -> Option<String> {
        let mut cursor = self.peek16(CURSOR_ADDRESS);
        let endbuf = self.peek16(ENDBUF_ADDRESS);
        let len = endbuf - cursor - 1;
        if len == 0 {
            return None;
        }

        // Extract the info
        let mut command = String::new();
        cursor += 1; // Skip the cursor symbol
        let mut line = Vec::new();
        while cursor < END_OF_SCREEN {
            let mut c = self.peek(cursor);
            if c == 0 {
                c = 0x20;
            }
            line.push(c as char);

            cursor += 1;
            if cursor % COLUMNS == 0 {
                let text = line.iter().collect::<String>();
                command.push_str(text.trim_end());
                line.clear();
            }
        }

        // Restore the buffers
        let buffer_start = self.peek16(INSCRN_ADDRESS);
        for i in buffer_start..END_OF_SCREEN {
            self.poke(i, 0x20);
        }
        self.poke16(INSCRN_ADDRESS, INITIAL_START_OF_INPUT_BUFFER);
        self.poke16(CURSOR_ADDRESS, INITIAL_START_OF_INPUT_BUFFER+1);
        self.poke16(ENDBUF_ADDRESS, INITIAL_START_OF_INPUT_BUFFER+2);
        self.poke16(LHALF_ADDRESS, INITIAL_START_OF_INPUT_BUFFER);
        self.poke(INITIAL_START_OF_INPUT_BUFFER, 0x00);
        self.poke(INITIAL_START_OF_INPUT_BUFFER+1, 0x97); // Cursor symbol

        Some(command.to_string())
    }

    pub fn get_screen_as_text(&self) -> String {
        let mut screen = String::new();
        for i in 0..(24*32) {
            let c = self.ram[i + START_OF_SCREEN as usize];
            screen.push_str(&ace_to_printable(c));
            if i % 32 == 31 {
                screen.push('\n');
            }
        }
        screen
    }

    pub fn serialize(&self) -> Vec<u8> {
        self.ram.to_vec()
    }

    pub fn deserialize(&mut self, data: &[u8]) {
        self.ram.copy_from_slice(data);
    }
}

/*
Memory map (from https://k1.spdns.de/Vintage/Sinclair/80/Jupiter%20Ace/ROMs/memory.txt):
    0000-2000   8 kB ROM: ACE OS and Forth Interpreter/Compiler
    2000-2800   1 kB video RAM. actually 2 mirrored blocks 2000-2400-2800.
        2000-2300 Video memory, 768 bytes. 32 x 24 Display (used by the video system)
        2300-2400 Cassette Header Information (256 bytes)  
        2400-2700 Video memory, 768 bytes. 32 x 24 Display (used by the CPU)
            Bits 6-0 address character in the Character Definitions Table
            Bit 7 indicates inverse (black on white) text.
        2700-2800 PAD. (work space used by Forth)
    2800-3000   Character Definition Table. actually 2 mirrored blocks 2800-2C00-3000.
        2800-2C00 1 kB Character Definition Table, 128 x 8 bytes (used by video system) 
        2C00-3000 1 kB Character Definition Table, 128 x 8 bytes (used by the CPU)
    3000-4000   User Program. actually 4 mirrored blocks 3000-3400-3800-3C00-4000.
        3000-3400 Unused.
        3400-3800 Unused.
        3800-3C00 Unused.
        3C00-4000 User Program.
    4000-10000  48 kB available for RAM expansion.
*/
fn physical_address(address: u16) -> u16 {
    match address {
        0x2000..0x2300 => address + 0x400, // First copy of video RAM

        0x2800..0x2c00 => address + 0x400, // First copy of character definition table

        0x3000..0x3400 => address + 0xc00, // First copy of user program
        0x3400..0x3800 => address + 0x800, // Second copy of user program
        0x3800..0x3c00 => address + 0x400, // Third copy of user program
        _ => address
    }
}
impl Machine for AceMachine {
    fn peek(&self, address: u16) -> u8 {
        if address < ROM.len() as u16 {
            return ROM[address as usize]
        }
        let physical_address = physical_address(address);
        self.ram[physical_address as usize]
    }

    fn poke(&mut self, address: u16, value: u8) {
        let physical_address = physical_address(address);
        self.ram[physical_address as usize] = value;
    }

    /*
    Ports
     */
    fn port_out(&mut self, address: u16, value: u8) {
        if self.trace_io {
            println!("OUT(0x{:04x}, 0x{:02x}): ", address, value);
        }
    }

    fn port_in(&mut self, address: u16) -> u8 {
        if self.trace_io {
            println!("IN(0x{:04x})", address);
        }

        match address {
            0xfefe => 0xff, //self.keyboard.get_keyport(0),
            0xfdfe => 0xff, //self.keyboard.get_keyport(1),
            0xfbfe => 0xff, //self.keyboard.get_keyport(2),
            0xf7fe => 0xff, //self.keyboard.get_keyport(3),
            0xeffe => 0xff, //self.keyboard.get_keyport(4),
            0xdffe => 0xff, //self.keyboard.get_keyport(5),
            0xbffe => 0xff, //self.keyboard.get_keyport(6),
            0x7ffe => 0xff, //self.keyboard.get_keyport(7),
            _ => 0xff,
        }
    }
}

pub fn ace_to_unicode(code: u8) -> char {
    let code = code & 0x7f;

    match code {
        0x00 => ' ',
        0x01..=0x0f => char::from_u32(0x2400 + code as u32).unwrap(),
        0x10 => '■',
        0x11 => '▝',
        0x12 => '▘',
        0x13 => '▀',
        0x14 => '▗',
        0x15 => '▐',
        0x16 => '▚',
        0x17 => '▜',
        0x18..=0x1f => char::from_u32(0x2400 + code as u32).unwrap(),
        0x60 => '£',
        0x7f => '©',
        _ => code as char,
    }
}

pub fn ace_to_printable(code: u8) -> String {
    match code {
        0x90 => " ".to_string(),
        0x91 => "▙".to_string(),
        0x92 => "▟".to_string(),
        0x93 => "▄".to_string(),
        0x94 => "▛".to_string(),
        0x95 => "▌".to_string(),
        0x96 => "▞".to_string(),
        0x97 => "▖".to_string(),
        _ => {
            if code & 0x80 != 0 {
                format!("\x1b[7m{}\x1b[0m", ace_to_unicode(code))
            } else {
                ace_to_unicode(code).to_string()
            }
        }
    }
}