# Alius 6502

Alius 6502 is a simple computer based on a 6502 CPU.

## Installation

The code is written in 6502 assembly and it's designed to compile with vasm http://sun.hasenbraten.de/vasm/.

### **Windows**
&nbsp;&nbsp;To get the correct executable, go to http://sun.hasenbraten.de/vasm, click "Last Release Binaries", and then click on the "vasm6502_oldstyle_Win64.zip".

&nbsp;&nbsp;Unzip it, and then move the folder "vasm6502_oldstyle_Win64" to your Desktop for easy acess.

&nbsp;&nbsp;Next, open the "vasm6502_oldstyle_Win64" folder on your Desktop, and in the address bar, type in "cmd".

&nbsp;&nbsp;This will open the command prompt in that location.

&nbsp;&nbsp;Finally, download or craete your own program using Notepad/Notepad++
And that's it! vasm is now installed on your system.

### **Linux**
Use these commands to download vasm, and unzip it.
 ```bash
wget http://www.ibaug.de/vbcc/vbcc_linux_x64.tar.gz
tar xvzf vbcc_linux_x64.tar.gz
cd vbcc/bin
 ```
In this folder, you will find many files. Ignore them all and just move the file named "vasm6502_oldstyle" to /usr/bin.This will put the binary in your PATH variable, so instead of calling it by ./vasm6502_oldstyle, you can call it just by vasm6502_oldstyle. You may also rename the file to be easily typable. I am NOT renaming the file in this example.
```bash
mv vasm6502_oldstyle /usr/bin

```
After this, you should remove the "vbcc" dirctory and the "vbcc_linux_x64.tar.gz" file to clean up.
To do so, use this command.
```bash
cd ../..
rm -rf vbcc vbcc_linux_x64.tar.gz
```
And that's it! vasm is now installed on your system.
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
