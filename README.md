# Alius 6502

Alius 6502 is a simple computer based on a 6502 CPU.

## Installation

The code is written in 6502 assembly and it designed to compile with vbcc http://www.compilers.de/vbcc

```bash
vasm6502_oldstyle -Fbin -dotdir file.s
```

## Usage
monitor_rom.s - This is the main ROM of the project.

header.inc - This is a header file to include in your own projects and gives easy access to ROM code.


test_ROM/blink.s - This just cycles all the I/O lines and is good for initial testing of the compute board.

demos/* - This is a small and growing collection of example code you can run from the SDcard.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License
GNU GPLv3
