# aceforth: Jupiter Ace Forth Interpreter

aceforth is a Forth interactive interpreter. It runs the Forth implementation used by the Jupiter Ace computer. It does so by emulating the Z80 cpu and the Jupiter ACE harfware. It executes the original Jupiter ACE Rom injecting commands on the input buffer and reading the output buffer.

aceforth runs on a terminal with the typical readline capabilites, including perstistent history and command editing.

aceforth is not a complete emulator of the Jupiter Ace it does just what is needed to run the Forth implementation. There are many emulators available, I have been using César Hernández [ZEsarUX](https://github.com/chernandezba/zesarux).
## Usage

Run aceforth:

```
$ ./aceforth
aceforth: Jupiter Ace Forth https://github.com/ivanizag/aceforth
Enter Jupiter Ace Forth commands. Type $HELP for help, control-C to exit.

aceforth> 1 2 + .
3 
aceforth> vlist

FORTH UFLOAT INT FNEGATE F/ F* F+ F- LOAD BVERIFY VERIFY BLOAD BSAVE SAVE LIST EDIT FORGET REDEFINE EXIT ." ( [ +LOOP LOOP DO UNTIL REPEAT BEGIN THEN ELSE WHILE IF ] LEAVE J I' I DEFINITIONS VOCABULARY IMMEDIATE RUNS> DOES> COMPILER CALL DEFINER ASCII LITERAL CONSTANT VARIABLE ALLOT C, , CREATE : DECIMAL MIN MAX XOR AND OR 2- 1- 2+ 1+ D+ - + DNEGATE NEGATE U/MOD */ * MOD / */MOD /MOD U* D< U< < > = 0> 0< 0= ABS OUT IN INKEY BEEP PLOT AT F. EMIT CR SPACES SPACE HOLD CLS # #S U. . SIGN #> <# TYPE ROLL PICK OVER ROT ?DUP R> >R ! @ C! C@ SWAP DROP DUP SLOW FAST INVIS VIS CONVERT NUMBER EXECUTE FIND VLIST WORD RETYPE QUERY LINE ; PAD BASE CURRENT CONTEXT HERE ABORT QUIT 
aceforth> 
```

## Commands

The same commands as the original Jupiter Ace Forth are available. See the [Jupiter Ace Manual](https://archive.org/details/Jupiter_Ace_Users_Manual_Issue_2_1982_Jupiter_Cantab) for a complete list of commands.

Additional metacommands are available starting with $:

  - $HELP: Shows this help
  - $QUIT: Exits the emulator
  - $SCREENSHOT: Shows the current screen contents
  - $SCREEN: Toggles screen dumping
  - $TRACE: Toggles ROM tracing
  - $SAVE [filename]: Saves a snapshot to a file
  - $LOAD [filename]: Loads a snapshot from a file
  - $GRAPHS: Show the Jupiter Ace graphical characters for easy copy-pasting.
  - $VIS: Toggles invisible mode, Overwrites the VIS and INVIS words

As seen here:

```
aceforth> $HELP

Type the same command you would type in the Jupiter Ace prompt.

Additional metacommands are available starting with $:
  $HELP: Shows this help
  $QUIT: Exits the emulator
  $SCREENSHOT: Shows the current screen contents
  $SCREEN: Toggles screen dumping
  $TRACE: Toggles ROM tracing
  $SAVE [filename]: Saves a snapshot to a file
  $LOAD [filename]: Loads a snapshot from a file
  $GRAPHS: Show the Jupiter Ace graphical characters for easy copy-pasting.
  $VIS: Toggles invisible mode, Overwrites the VIS and INVIS words


aceforth> $VIS
Invisible mode is now not forced
aceforth> 4 DUP 4 * .
4 DUP 4 * . 16  OK  

aceforth> $GRAPHS
Graph characters: ■ ▝ ▘ ▀ ▗ ▐ ▚ ▜ ▙ ▟ ▄ ▛ ▌ ▞ ▖
aceforth> : box cr ." ▛▜" cr ." ▙▟" ;
: box cr ." ▛▜"cr ." ▙▟";  OK  

aceforth> box
box 
▛▜
▙▟ OK  

aceforth> $SCREENSHOT

╔════════════════════════════════╗
║4 DUP 4 * . 16  OK              ║
║: box cr ." ▛▜"cr ." ▙▟";  OK   ║
║box                             ║
║▛▜                              ║
║▙▟ OK                           ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║                                ║
║ ▖                              ║
╚════════════════════════════════╝
aceforth> 
```


## More information on the Jupiter Ace

- The main source is the manual: [Jupiter Ace Manual](https://archive.org/details/Jupiter_Ace_Users_Manual_Issue_2_1982_Jupiter_Cantab)
- The Jupiter Ace Archive: [jupiter-ace.co.uk/](https://www.jupiter-ace.co.uk/]())
- The Mercury Ace clone that was the actual reason for doing this project: [Mercury Ace](https://codeberg.org/wilco2009/Mercury_Ace)