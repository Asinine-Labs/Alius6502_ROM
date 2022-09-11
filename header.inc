;-----------------------------------------------------------------------------------------
;   
; Headers - ROM CODE for Alius 6502 board V1.0
;   
;-----------------------------------------------------------------------------------------

; I/O Chip
PORTB = $8010 
PORTA = $8011 
DDRB = $8012
DDRA = $8013

; Content and display mask for the 7 segment display
DisplayMask = $D3              ; D3,D4,D5,D6,D7,D8 
DisplayData = $D0              ; D0,D1,D2 

; Random number generator.
RandByte = $0200               ; Random byte from random function
RandSeed = $0201               ; 16 bit seed value for random function, Also $0202

; Used during read of keypad
KeyCode = $0205                ; Code of key pressed.

; Used for the return of error code.
ErrorCode = $0206              ; Assorted error codes return. 00 = good.

; Used for loading of files form SDcard.
FileNumber = $0250             ; File Number to create FileName
FileName = $0251               ; File Name to load
FileSize = $025D               ; File Size, 2 bytes including $025D(LSB) ,$025E(MSB)
SectorCount = $025F            ; Number of whole sectors needed to load
LoadAddrPTR = $0260            ; Address pointer - Where to load file, $0260(LSB),$0261(MSB)

; Hardware IRQ calls code based on the pointer at IRQ_Vec
IRQ_Vec = $02FE                ; IRQ vector in RAM, $02FE and $02FF


; Rom jump table, fixed locations across ROM versions
MonitorROM = $FF00             ; Main Monitor ROM
UpdateDisplay = $FF10          ; Update the 7segmanet display
ReadKeypad = $FF14             ; Reads the keypad, with debounce
ScanKeypad = $FF18             ; Checks for key press, no debounce
GetRandomByte = $FF20          ; Random byte returned in A 
Sleep_Long = $FF30             ; Sleep about 1 second
Sleep_Short = $FF34            ; Sleep about 100 milliseconds
SPI_Write_Byte = $FF40         ; Writes the byte in A register to SPI bus.
SPI_Read_Byte = $FF44          ; Reads a byte in A register from SPI bus.
SPI_Select_SDcard = $FF50      ; Changes CS_ to low for CDcard (Active low)
SPI_Unselect_SDcard = $FF54    ; Changes CS_ to high for SDcard (Active low)
SPI_Select_7seg = $FF58        ; Changes CS_ to low for 7 segment display (Active low)
SPI_Unselect_7seg = $FF5C      ; Chanegs CS_ to high for 7 segment display (Active low)
Init_SD_card = $FF60           ; Init the SDcard to SPI mode
SD_Card_Mount = $FF64          ; Reads Bootsector and patition table to setup card.
SD_Card_Read_Sector = $FF68    ; Reads a sector from the SDcard 
SD_Card_Write_Sector = $FF6C   ; Writes a sector to SDard (NOT implemented)
CreateFileName = $FF70         ; Convert a filenumber to a filename
FindFile= $FF74                ; Read FAT sector
LoadFile = $FF78               ; Loads file to memory
LBA_Addr = $FF7C               ; Calculates LBA sector from cluster number.
GetNextSector = $FF80          ; Increments sector, goes to FAT for next cluster if needed.
FindNextCluster = $FF84        ; Find next cluster from FAT
BootStrap = $FF90              ; Load file and run it.

