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

;-----------------------------------------------------------------------------------------
; Zero Page Storage - Used for performace related code
;-----------------------------------------------------------------------------------------
; Tempory storage, counters, and pointers
ZP_Temp = $FF                  ; Temp byte
ZP_Counter = $FE               ; 8 bit counter
ZP_Counter16 = $FC             ; 16 bit counter, 2 bytes including $FC,$FD
; FAT32 pointers and counters
ZP_SectorBufPTR = $FA          ; 2 bytes including FA,FB
ZP_DIRRecordPTR = $F8          ; 2 bytes including F8,F9
ZP_SectorSize = $F6            ; 2 bytes including F6,F7
; Content and display mask for the 7 segment display
ZP_DisplayMask = $D3           ; D3,D4,D5,D6,D7,D8
ZP_Display = $D0               ; D0,D1,D2


;-----------------------------------------------------------------------------------------
; $0200 Page - Used for FAT32 and other data
;-----------------------------------------------------------------------------------------
; Random number generator.
RandByte = $0200               ; Random byte from random function
RandSeed = $0201               ; 16 bit seed value for random function, Also $0202
; Used during read of keypad
KeyRow = $0203                 ; Row of key press
KeyCol = $0204                 ; Col of key press
KeyCode = $0205                ; Code of key pressed
; Used for the return of error code.
ErrorCode = $0206              ; Assorted error codes return. 00 = good.
; Used as part of monitor rom input
InputPosition = $0207          ; Which char of input address or data
MonitorMode = $0208            ; Current mode of monitor
; FAT32 support data
CurrentSector = $0209          ; 4 bytes including 0209,020A,020B,020C
ClusterBeginLBA = $020D        ; 4 bytes including 020D,020E,020F,0210
SecPerCluster = $0211          ; How many sectors per cluster.
SectorIndex = $0212            ; Which sector of the current cluster
RootDirCluster = $0213         ; 4 bytes including 0213,0214,0215,0216
ClusterNum = $0217             ; 4 bytes including 0217,0218,0219,021A
SectorTemp  = $021B            ; 4 bytes including 021B,021C,021D,021E
SDHCFlag = $021F               ; Flag to show if we have an SDHC card
ClusterTemp = $0220            ; 4 bytes including 0220,0221,0222,0223
FAT1BeginLBA = $0224           ; 4 bytes including 0224,0225,0226,0227
FAT2BeginLBA = $0228           ; 4 bytes including 0228,0229,022A,022B
FATsize = $022C                ; 4 bytes including 022C,022D,022E,022F
TempByte = $0230               ; Temp byte
CurrentFATSector     = $0231      ; Which FAT sector is in memory
RequestedFATSector   = $0232      ; Which FAT sector is requested
BytesToWrite       = $0233     ; 4 bytes including 0233,0234,0235,0236
SectorCount        = $0237     ; Number of whole sectors needed to load 0237,0238,0239,023A



; Used for loading of files from SDcard.
FileNumber         = $0250     ; File Number to create FileName
FileName           = $0251     ; File Name to load
FileAddrPTR        = $0260     ; Address pointer - file to load or save, $0260(LSB),$0261(MSB)
FileFirstCluster   = $0262     ; 4 bytes including 0262,0263,0264,0265
FileSize           = $0266     ; File Size, 4 bytes including $0266(LSB), $0267, 0268, $0269(MSB)


; Debug CPU status.
AREG = $0270                   ; Value of A register
XREG = $0271                   ; Value of X register
YREG = $0272                   ; Value of Y register
Status = $0273                 ; Value of Status register
PC_Low = $0274                 ; Value of Program Count Low byte
PC_High = $0275                ; Value of Program Count High Byte



; Hardware IRQ calls code based on the pointer at NMI_Vec & IRQ_Vec
NMI_Vec = $02FC                ; MNI vector in RAM, $02FC and $02FD
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
GetNextReadSector = $FF80      ; Increments sector, goes to FAT for next cluster if needed.
GetNextReadClustert = $FF84    ; Find next cluster from FAT
WriteFile = $FF88              ; Find next cluster from FAT
UpdateDir = $FF8C              ; Update the Directory with FileName,FileSize,FirstCluster
BootStrap = $FF90              ; Load file and run it.
WriteFAT = $FF94               ; Write Current FAT to disk
FindNextWriteSector = $FF98    ; Finds the next free Sector on disk
FindFreeCluster = $FF9C        ; Finds the next free Cluster on disk
LoadFAT = $FFA0                ; Read zero-th FAT Sector to $0500

