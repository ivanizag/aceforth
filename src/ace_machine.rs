use iz80::Machine;

static ROM: &[u8] = include_bytes!("../resources/jupiter ace.rom");


const INSCRN_ADDRESS: u16 = 0x3C1E;
const CURSOR_ADDRESS: u16 = 0x3C20;
const ENDBUF_ADDRESS: u16 = 0x3C22;
const ADDRESS_STATIN: u16 = 0x3c28;

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

    /*
    pub fn dump_buffer_info(&self) {
        let inscrn = self.peek16(INSCRN_ADDRESS);
        let cursor = self.peek16(CURSOR_ADDRESS);
        let endbuf = self.peek16(ENDBUF_ADDRESS);
        println!("INSRSCRN: 0x{:04x}, ", inscrn);
        println!("CURSOR: 0x{:04x}, ", cursor);
        println!("ENDBUF: 0x{:04x}", endbuf);
        let inscrn_char = self.peek(inscrn);
        println!("Char at inscrn: {}, 0x{:02x}", (inscrn_char & 0x7f) as char, inscrn_char);
        let cursor_char = self.peek(cursor);
        println!("Char at cursor: {}, 0x{:02x}", (cursor_char & 0x7f) as char, cursor_char);

        for i in inscrn..=(endbuf+5) {
            let c = self.peek(i);
            println!("at {:04x} {}, 0x{:02x}", i, (c&0x7f) as char, c);
        }
    }
    */

    pub fn inject_command(&mut self, command: &str) {
        let mut cursor = self.peek16(INSCRN_ADDRESS) + 1;
        for c in command.chars() {
            cursor += 1;
            self.poke(cursor, c as u8);
        }
        cursor += 1;
        self.poke(cursor, 0x00);
        self.poke16(CURSOR_ADDRESS, self.peek16(INSCRN_ADDRESS) + 1);
        self.poke16(ENDBUF_ADDRESS, cursor);

        let mut statin = self.peek(ADDRESS_STATIN);
        statin |= STATIN_ENTER_MASK;
        self.poke(ADDRESS_STATIN, statin);
    }

    pub fn extract_pending_input(&self) -> Option<String> {
        let cursor = self.peek16(CURSOR_ADDRESS);
        let endbuf = self.peek16(ENDBUF_ADDRESS);
        let len = endbuf - cursor - 1;
        if len == 0 {
            return None;
        }

        assert!(self.peek(cursor) == '?' as u8 | 0x80); // Inverse question mark

        let mut command = String::new();
        for i in (cursor+1)..endbuf {
            let c = self.peek(i);
            if c == 0 {
                break;
            }
            command.push(c as char);
        }
        Some(command)
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



/*
0000:           LHALF           EQU     03C24H  ; output field end 
0000:           
0000:           KEYCOD          EQU     03C26H  ; pressed key
0000:           KEYCNT          EQU     03C27H  ; timer
0000:           STATIN          EQU     03C28H  ; bit 0 release input
0000:                                           ; bit 1 caps lock
0000:                                           ; bit 2 graphics
0000:                                           ; bit 3 inverse
0000:                                           ; bit 5 "ENTER" pressed


0000:           SCRPOS          EQU     03C1CH  ; output field cursor
0000:           INSCRN          EQU     03C1EH  ; input field start 
0000:           CURSOR          EQU     03C20H  ; input field cursor
0000:           ENDBUF          EQU     03C22H  ; input field end



Key processing in VSYNC interrupt:
    if caps lock and a..=z AND 0x5F
    if graphics: AND 0x9f (0b1001_1111)
    if inverse: OR 0x80

    if Control -> DOCTRL(A) + DCSETCUR
    else       -> DCDCNORM(A)

    DCDNORM stores the command in INSCRN?

Input processing in 0x594: QSTART
    Releases input: SET 0, STATIN
                    RES 5, STATIN
    Waits for enter: BIT 5, STATIN
    Goes to DCCURDEL (delete char at cursor (the return?))
        Reads char from CURSOR
            DCGETCIN ( BC = ENDBUF-CURSOR+1 )
        if not a 0 or at ENDBUF

    GETSTRING (0x5DF) reads words from INSCRN

*/