# Alius 6502

Alius 6502 is a simple computer based on a 6502 CPU.

## Installation

The code is written in 6502 assembly and it's designed to compile with vasm/vbcc: http://www.compilers.de/vasm.html

### **Windows**
&nbsp;&nbsp;To get the correct executable, click "Last Release Binaries", and then click on the "vasm6502_oldstyle_Win64.zip".

&nbsp;&nbsp;Unzip it, and then move the folder "vasm6502_oldstyle_Win64" to your Desktop for easy acess.

&nbsp;&nbsp;Next, open the "vasm6502_oldstyle_Win64" folder on your Desktop, and in the address bar, type in "cmd".

&nbsp;&nbsp;This will open the command prompt in that location.

&nbsp;&nbsp;Finally, download or craete your own program using Notepad/Notepad++

### **Linux**
Use these commands to download and unzip vasm.
```bash
sudo apt install unzip 
wget http://www.ibaug.de/vbcc/vbcc6502_r3p1.zip
unzip vbcc6502_r3p1.zip
cd vbcc6502/vbcc6502_linux/vbcc/bin
```
At this point, you should move vasm6502_oldstyle to /usr/bin to put it in your PATH variable.

To do this use the following command: 
```bash
sudo mv vasm6502_oldstyle /usr/bin
```
You won't need the "vbcc6502_r3p1.zip" file and "vbcc6502" directory anymore, so assuming the files are in your home directory, here is how to delete them
```bash
cd
rm -rf vbcc6502 vbcc6502_r3p1.zip 
```

And that's it! vasm should be installed on your system.

## Compiling
Once in CMD/Terminal, use this command to compile the source code into machine code.

Linux:
```bash
vasm6502_oldstyle -Fbin -dotdir file.s
```
Windows:
```bash
vasm6502_oldstyle.exe -Fbin -dotdir file.s
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
