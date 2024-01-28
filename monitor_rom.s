;-----------------------------------------------------------------------------------------
;
;   ROM CODE for Alius 6502 board V1.0
;
;   Memory Map
;   $0000 -> $00FF - Zero page, some bytes used for fast access data, counters, pointers
;   $0100 -> $01FF - Stack
;   $0200 -> $02FF - Used by system for assorted FAT32 info, counters, pointers
;   $0300 -> $04FF - Used by FAT32 code, buffer for sectors during Directory and Cluster search
;   $0500 -> $06FF - FAT data used by FAT32 code
;   $0700 -> $7FFF - User space RAM
;   $8000 -> $9FFF - I/O
;   $A000 -> $FEFF - ROM Code
;   $FF00 -> $FFFB - ROM Jump table
;   $FFFC -> $FFFF - Reset and IRQ Vectors
;
;-----------------------------------------------------------------------------------------
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
ZP_BlobBufPTR = $F4            ; Read/Write Buffer  $F4,$F5
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
CurrentFATSector     = $0231   ; Which FAT sector is in memory
RequestedFATSector   = $0232   ; Which FAT sector is requested
BytesToWrite         = $0233   ; 4 bytes including 0233,0234,0235,0236
SectorCount          = $0237   ; 4 bytes - Number of whole sectors needed to load 0237,0238,0239,023A
ChunkNum             = $0240   ; 2 Bytes - Chunk number to load/save  0240,0241
ChunkTemp            = $0242   ; 2 Bytes - Chunk page number to load/save 0242,0243
FATDirty             = $0244   ; Dirty flag for FAT cache.
BlobFirstCluster     = $0245   ; 4 bytes including 0245,0246,0247,0248


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



;-----------------------------------------------------------------------------------------
; Main code of Monitor ROM
;-----------------------------------------------------------------------------------------
  .org $8000                  ; We don't user first 16K of the 32K ROM chip
  .org $A000                  ; Real start of code is at $A000



Reset:
  ldx #$FF
  txs                          ; Setup stack
  lda #<IRQ_Exit               ; Low byte of IRQ_Exit
  sta IRQ_Vec                  ; Set default IRQ handler
  lda #>IRQ_Exit               ; High byte of IRQ_Exit
  sta IRQ_Vec+1                ; Set default IRQ handler
  lda #<IRQ_Exit               ; Low byte of IRQ_Exit
  sta NMI_Vec                  ; Set default NMI handler
  lda #>IRQ_Exit               ; High byte of IRQ_Exit
  sta NMI_Vec+1                ; Set default NMI handler
  lda #$FF
  sta DDRB                     ; Set 6522 Port B to all output
  lda #$00
  sta DDRA                     ; Set 6522 Port A to all input
  lda #$10                     ; Set T2 mode to free running
  sta $801B                    ; Set T2 mode to free running
  lda #$00                     ; Set T2 counter to 0 for fastest speed
  sta $8018                    ; Set T2 counter to 0 for fastest speed
  lda #$00                     ; Set output byte to 00, for max brightness
  sta $801A                    ; Set output byte to 00, for max brightness
  lda #$00                     ; Set display to 00
  sta ZP_Display               ; Init display byte right
  sta ZP_Display+1             ; Init display byte middle
  sta ZP_Display+2             ; Init display byte left
  lda #$00
  sta InputPosition            ; Init input position.
  sta MonitorMode              ; 00-Idle, 01-Read, 02-Write-Addr, 03-Write-Data, 04-Execute.
  lda #0b01111111              ; Display mask, default to digits without the decimal point.
  sta ZP_DisplayMask
  sta ZP_DisplayMask+1
  sta ZP_DisplayMask+2
  sta ZP_DisplayMask+3
  sta ZP_DisplayMask+4
  sta ZP_DisplayMask+5
  lda #0b00001100              ; Config SPI bus, set both CS_ set high, clock and data low.
  sta PORTB
  cli                          ; Turn on interrupts to support BRK debug.
  lda #$00                     ; Setup to read "00.BIN"
  sta FileNumber               ; Set filenumner ot 00
  jmp BootStrap                ; Check for SDcard and loads file or return to monitor ROM.



MonitorROM:
  ldy #$00                     ; Zero offset for the load.
  lda (ZP_Display+1),y         ; Load data from address in zero page
  sta ZP_Display               ; Set data to display "buffer"
  jsr UpdateDisplay            ; Update the 7 segment display
  jsr ReadKeypad               ; Read the keypad
  cmp #$FF                     ; If no key pressed then
  beq MonitorROM               ; Jump back to mainloop
CheckReadKey:
  cmp #$FA                     ; Check if read button pressed
  bne CheckWriteKey            ; If not Read, then check Write key
  ldx #$01                     ; Set mode to read
  stx MonitorMode              ; Set mode to read
  ldx #$00                     ; Set input position to 0
  stx InputPosition            ; Reset input position
  jmp MonitorROM
CheckWriteKey:
  cmp #$FB                     ; Check if write button pressed
  bne CheckExecKey             ; If not Write, then check Exec key
  ldx #$02                     ; Set Mode to write
  stx MonitorMode              ; Set Mode to write
  ldx #$00                     ; Reset address position
  stx InputPosition            ; Reset address position
  jmp MonitorROM
CheckExecKey:
  cmp #$FC                     ; Check if exec button pressed
  bne CheckInputMode           ; If not Exec, then deal with other input
  ldx #$04                     ; Set Mode to exec
  stx MonitorMode              ; Set Mode to exec
  ldx #$00                     ; Reset address position
  stx InputPosition            ; Reset address position
  jmp MonitorROM
CheckInputMode:
  lda MonitorMode              ; Get current Mode
CheckIdle:
  cmp #$00                     ; Check Mode with 00/idle
  bne CheckRead                ; If not idle, then check if Mode is Read
  jmp Idle                     ; Mode is Idle
CheckRead:
  cmp #$01                     ; Check Mode with 01/Read
  bne CheckWriteAddr           ; If not Read, then check if Mode is WriteAddress
  jmp ReadMode                 ; mode is Read
CheckWriteAddr:
  cmp #$02                     ; Check Mode with 02/WriteAddress
  bne CheckWriteData           ; If not WriteAddress, then check if Mode is WriteData
  jmp WriteAddr                ; Mode if Write Address
CheckWriteData:
  cmp #$03                     ; Check Mode with 03/WriteData
  bne CheckExec                ; If not WriteData, then check if Mode is Execute
  jmp WriteData                ; Mode is WriteData
CheckExec:
  cmp #$04                     ; Check Mode with 04/Exec
  bne Idle                     ; If not exec, then we are done
  jmp Exec                     ; Mode is exec
Idle:
  jmp MonitorROM

ReadMode:
  lda KeyCode                  ; Get key pressed
  cmp #$FD                     ; If key is ent
  beq IncAddress               ; Increment the read address
  jmp AddressInput             ; If not ENT, then assume its an address
IncAddress:
  lda #$00                     ; Reest address position to 00
  sta InputPosition            ; Reset address position
  inc ZP_Display+1             ; Increment address low byte
  bne ReadModeExit
  inc ZP_Display+2             ; Increment address high byte
ReadModeExit:
  jmp MonitorROM

WriteAddr:
  lda KeyCode                  ; Get key pressed
  cmp #$FD                     ; If key is ENT, then check mode to WriteData
  bne AddressInput             ; If Some other key pressed, then process input address
  lda #$00                     ; Reset input position to 00 ready for input data
  sta InputPosition            ; Reset inout position to 00 ready for input data
  lda #$03                     ; Change Mode to 03/WriteData
  sta MonitorMode              ; Store new Mode
  jmp MonitorROM

WriteData:
  lda KeyCode                  ; Get key pressed
  cmp #$FD                     ; If key is ENT
  bne WriteDataInput           ; If some other key pressed then inout the data
  lda #$00                     ; Reset input position to 00
  sta InputPosition            ; Reset input position
  inc ZP_Display+1             ; Increment address low byte
  bne WriteDataExit
  inc ZP_Display+2             ; Increment address high byte
WriteDataExit:
  jmp MonitorROM

WriteDataInput:
  lda InputPosition            ; Get Data position
  cmp #$00                     ; If Data position is 0
  beq DataPosition0            ; Data position is 0
  jmp DataPosition1            ; Else data position 1
DataPosition0:
  lda #$0F                     ; Mask off top half of byte, keep lower half
  and ZP_Display               ; Mask off top half of byte, keep lower half
  sta ZP_Display               ; Mask off top half of byte, keep lower half
  lda KeyCode                  ; Get the key pressed
  asl                          ; Shift left 4 times to get key to upper half of byte
  asl                          ; Shift left 4 times to get key to upper half of byte
  asl                          ; Shift left 4 times to get key to upper half of byte
  asl                          ; Shift left 4 times to get key to upper half of byte
  ora ZP_Display               ; Join new top half with old lower half
  sta ZP_Display               ; Store new Byte
  inc InputPosition            ; Move Input position to next half byte
  jmp StoreDataToMemory
DataPosition1:
  lda #$F0                     ; Mask off bottom half of byte, keep upper half
  and ZP_Display               ; Mask off bottom half of byte, Keep upper half
  sta ZP_Display               ; Store Byte
  lda KeyCode                  ; Get key pressed
  ora ZP_Display               ; Join new bottom half with old top half
  sta ZP_Display               ; Store new byte
  lda #$00                     ; Reset Input position to 00
  sta InputPosition            ; Store Input position
StoreDataToMemory:
  lda ZP_Display               ; Get the new Data byte to store.
  ldy #$00                     ; Set zero offset.
  sta (ZP_Display+1),y         ; Store Data to address in zero page
  jmp MonitorROM

AddressInput:
  lda InputPosition            ; Get input position
  cmp #$00                     ; If input position is 0
  beq AddressPosition0         ;
  cmp #$01                     ; If input position is 1
  beq AddressPosition1         ;
  cmp #$02                     ; If input position is 2
  beq AddressPosition2         ;
  cmp #$03                     ; If input position is 3
  beq AddressPosition3         ;
  jmp MonitorROM               ; This should never happen!
AddressPosition0:
  lda #$0F                     ; Mask off top half of address byte, keep lower half
  and ZP_Display+2             ; Mask off top half of address byte, keep lower half
  sta ZP_Display+2             ; Store half byte
  lda KeyCode                  ; Get key pressed
  asl                          ; Shift left 4 time to get top half byte
  asl
  asl
  asl
  ora ZP_Display+2             ; Join with lower half byte
  sta ZP_Display+2             ; Store new address byte
  inc InputPosition            ; Increment input position to get next half byte
  jmp MonitorROM
AddressPosition1:
  lda #$F0                     ; Mask off lower half byte, keep upper half
  and ZP_Display+2             ; Mask off lower half byte, keep upper half
  sta ZP_Display+2             ; Store half byte
  lda KeyCode                  ; Get key pressed
  ora ZP_Display+2             ; Join new half byte with old half byte
  sta ZP_Display+2             ; Store new Address byte
  inc InputPosition            ; Increment input position to get next half byte
  jmp MonitorROM
AddressPosition2:
  lda #$0F                     ; Mask off top half of address byte, keep lower half
  and ZP_Display+1             ; Mask off top half of address byte, keep lower half
  sta ZP_Display+1             ; Store half byte
  lda KeyCode                  ; Get key pressed
  asl                          ; Shift left 4 time to get top half byte
  asl
  asl
  asl
  ora ZP_Display+1             ; Join with lower half byte
  sta ZP_Display+1             ; Store new address byte
  inc InputPosition            ; Increment input position to get next half byte
  jmp MonitorROM
AddressPosition3:
  lda #$F0                     ; Mask off lower half byte, keep upper half
  and ZP_Display+1             ; Mask off lower half byte, keep upper half
  sta ZP_Display+1             ; Store half byte
  lda KeyCode                  ; Get key pressed
  ora ZP_Display+1             ; Join new half byte with old half byte
  sta ZP_Display+1             ; Store new Address byte
  lda #$00                     ; Reset input position to 00
  sta InputPosition            ; Reset input position to 00
  jmp MonitorROM

Exec:
  lda KeyCode                  ; Get key pressed
  cmp #$FD                     ; If key is ENT
  beq ExecEnt                  ; Go and do exec
  jmp AddressInput             ; If some other key pressed
ExecEnt:
  jmp (ZP_Display+1)           ; Jump to address pointed ot by Display+1 and Display+2



ReadKeypad:                    ; Reads keypad with debounce (blocking)
  jsr ScanKeypad               ; Check what is pressed right now
  sta KeyCode                  ; Store which key was pressed
  cmp #$FF                     ; $FF means nothing pressed
  beq ReadKeypadExit           ; Nothing pressed, so return
WaitKeyRelease:
  jsr Sleep_Short              ; Sleeps about 0.1 of a second
  jsr ScanKeypad               ; Check what is press right now
  cmp #$FF                     ; $FF means nothings pressed
  bne WaitKeyRelease           ; Jump back to wait loop
  lda KeyCode                  ; Load which key was pressed
ReadKeypadExit:
  rts



ScanKeypad:                    ; Checks for key press, no debounce.
  ldx #$04                     ; Count down the 4 rows
  lda #0b10000000              ; Start at Row 1
  sta ZP_Temp                  ; Store row
ScanRow:
  lda PORTB                    ; Load current state
  and #0b00001111              ; Mask off low bit to keep SPI CS_ state.
  ora ZP_Temp                  ; Set Row to scan
  sta PORTB                    ; Active Row
  lda PORTA                    ; load Col
  and #0b00011111              ; Mask off keypad only, we dont want SPI data
  cmp #$00                     ; Check if any key pressed
  bne Row_Found                ; Found a key pressed
  dex                          ; Count row down number
  lda ZP_Temp                  ; Load Row bit
  lsr                          ; Move row bit left
  sta ZP_Temp                  ; Store Row bit
  cmp #0b00001000              ; Check if we got to the end of rows
  bne ScanRow                  ; If not done then go around again
  lda #$FF                     ; Return $FF whcih mean no key pressed
  rts                          ; return
Row_Found:                     ; Once we found a key, Row*Col math
  stx KeyRow                   ; Store row
  ldy #$FF                     ; Set counter to $FF, it rolls to $00 at start of loop
FindCol:
  iny                          ; Increment the counter of col's
  lsr                          ; Shift the bit
  bcc FindCol                  ; Check if this it the bit we are looking for
  tya                          ; Move Y counter to A so we can do math
  asl                          ; A*2
  asl                          ; A*2 again get A*4
  clc                          ; Clear carry before add
  adc KeyRow                   ; Add row
  tax                          ; Move A to X so we can use X as index to array
  lda KeypadArray,x            ; Lookup key code
  rts



Sleep_Long:                    ; Sleep for about 0.5 second
  ldx #$FF
  ldy #$FF
Sleep_loop_Y:
  dey
Sleep_loop_X:
  dex
  cpx #$FF
  bne Sleep_loop_X
  cpy #$FF
  bne Sleep_loop_Y
  rts



Sleep_Short:                   ; Sleep for about 50mS
  ldx #$FF
Sleep_Short_loop:
  dex
  cpx #$FF
  bne Sleep_Short_loop
  rts



UpdateDisplay:
  jsr SPI_Select_7seg          ; Set CS_ for 7 segment display
  jsr Update_7seg              ; Send relevent bytes to the display
  jsr SPI_Unselect_7seg        ; Clear CS_ for 7 segment display
  rts



Update_7seg:                   ; Displays three bytes as 6 HEX digits
  lda ZP_Display               ; Get first byte to display
  tay                          ; Store byte in Y for later
  and #$0F                     ; Mask off upper half, keep lower half
  tax                          ; Use half byte as X index to array
  lda Array_7seg,x             ; Use X index to get segment patten for HEX digit
  and ZP_DisplayMask           ; Mask off any segments not to display, good for supressing decimal point
  jsr SPI_Write_Byte           ; Send bit patten to SPI bus
  tya                          ; Return byte to display from Y
  and #$F0                     ; Mask off lower half, keep upper half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  tax                          ; Use half byte as X index to array
  lda Array_7seg,x             ; Use X index to get segment patten for HEX digit
  and ZP_DisplayMask+1         ; Mask off any segments not to display, good for supressing decimal point
  jsr SPI_Write_Byte           ; Send bit patten to SPI bus
  lda ZP_Display+1             ; Get second byte to display
  tay                          ; Store byte in Y for later
  and #$0F                     ; Mask off upper half, keep lower half
  tax                          ; Use half byte as X index to array
  lda Array_7seg,x             ; Use X index to get segment patten for HEX digit
  and ZP_DisplayMask+2         ; Mask off any segments not to display, good for supressing decimal point
  jsr SPI_Write_Byte           ; Send bit patten to SPI bus
  tya                          ; Return byte to display from Y
  and #$F0                     ; Mask off lower half, keep upper half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  tax                          ; Use half byte as X index to array
  lda Array_7seg,x             ; Use X index to get segment patten for HEX digit
  and ZP_DisplayMask+3         ; Mask off any segments not to display, good for supressing decimal point
  jsr SPI_Write_Byte           ; Send bit patten to SPI bus
  lda ZP_Display+2             ; Get third byte to display
  tay                          ; Store byte in Y for later
  and #$0F                     ; Mask off upper half, keep lower half
  tax                          ; Use half byte as X index to array
  lda Array_7seg,x             ; Use X index to get segment patten for HEX digit
  and ZP_DisplayMask+4         ; Mask off any segments not to display, good for supressing decimal point
  jsr SPI_Write_Byte           ; Send bit patten to SPI bus
  tya                          ; Return byte to display from Y
  and #$F0                     ; Mask off lower half, keep upper half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  lsr                          ; Shift right to move top half byte to lower half
  tax                          ; Use half byte as X index to array
  lda Array_7seg,x             ; Use X index to get segment patten for HEX digit
  and ZP_DisplayMask+5         ; Mask off any segments not to display, good for supressing decimal point
  jsr SPI_Write_Byte           ; Send bit patten to SPI bus
  rts



SPI_Select_SDcard:
  lda PORTB                    ; Load current state of PORTB
  and #0b11110111              ; Turn off bit #3 for CS_ (Active Low) of SDCard
  sta PORTB                    ; Store new state to PORTB
  rts



SPI_Unselect_SDcard:
  lda PORTB                    ; Load current state of PORTB
  ora #0b00001000              ; Turn on bit #3 for CS_ (Active Low) of SDcard
  sta PORTB                    ; Store new state of PORTB
  rts



SPI_Select_7seg:
  lda PORTB                    ; Load current state of PORTB
  and #0b11111011              ; Turn off bit #2 for CS_ (Active Low) of 7 segment display
  sta PORTB                    ; Store new state of PORTB
  rts



SPI_Unselect_7seg:
  lda PORTB                    ; Load current state of PORTB
  ora #0b00000100              ; turn on bit #2 for CS_ (Active Low) of 7 segment display
  sta PORTB                    ; Store new state of PORTB
  rts



SPI_Write_Byte:                ; Writes the byte in A register to SPI bus.
  sta ZP_Temp                  ; Store Byte in Temp
  ldx #$08                     ; Setup counter for 8 bits
SPI_loop:
  asl ZP_Temp                  ; Bit shift byte in temp, top bit goes into carry
  bcc SPI_out_low_bit          ; Check bit in carry
SPI_out_high_bit:
  lda PORTB                    ; Load current PORTB
  and #0b11111100              ; Mask off lower bits to be changed
  ora #0b00000010              ; Set data high & clock low
  sta PORTB                    ; Store Data bit to PORTB
  inc PORTB                    ; Cycle clock high
  dec PORTB                    ; Cycle clock low
  jmp SPI_bit_done             ; Done with this bit
SPI_out_low_bit:
  lda PORTB                    ; Load current PORTB
  and #0b11111100              ; Mask off lower bits to be changed
  sta PORTB                    ; Store Data bit to PORTB
  inc PORTB                    ; Cycle clock high
  dec PORTB                    ; Cycle clock low
SPI_bit_done:
  dex                          ; Decrement bit counter
  bne SPI_loop                 ; If bit counter not 0 then go around again
  rts



SPI_Read_Byte:                 ; Reads a byte from SPI bus stores in A register.
  ldx #$08                     ; Setup X as counter for 8 bits
  lda PORTB                    ; Load current PORTB
  ora #0b00000010              ; Ensure data bit high, always output a 1 during a read
  and #0b11111110              ; Ensure clock low
  sta PORTB                    ; Store Clock/Data to PORTB
SPI_Read_Bit:
  inc PORTB                    ; Set clock high
  lda PORTA                    ; Get data from PORTA
  dec PORTB                    ; Set clock low
  rol                          ; Store bit in carry.
  tya                          ; Get part byte from Y to A.
  rol                          ; Move bit from carry into A.
  tay                          ; Store part byte back in Y for later.
  dex                          ; Count down the 8 bits
  bne SPI_Read_Bit             ; If we havent done all 8 bits then go around again
  rts



Init_SD_card:
  jsr Sleep_Short              ; Sleep about 100ms to ensure card is powered up.
  lda #0b00001110              ; SD_CS_ high, 7Seg_CS_ high,  data high, clock low
  sta PORTB                    ; Store SPI CS_,Data,Clock to PORTB
  lda #20                      ; Cycle the SPI clock 160 times by sending 20 bytes with CS_ high
  sta ZP_Counter               ; Store counter
Init_SD_loop:
  lda #$FF                     ; Send $FF Byte
  jsr SPI_Write_Byte           ; Send $FF Byte
  dec ZP_Counter               ; Decrement counter
  bne Init_SD_loop             ; If Counter is not zero then go around again
  jsr SPI_Select_SDcard        ; Set CS_ low for SDcard
  lda #$FF                     ; Send $FF byte after CS_ change
  jsr SPI_Write_Byte           ; Send $FF byte after CS_ change
CMD0:
  lda #$40                     ; Setup CMD0
  jsr SPI_Write_Byte           ; Send CMD0 Byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$95                     ; Setup CRC byte
  jsr SPI_Write_Byte           ; Send CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$01                     ; Check R1 result code
  beq CMD8                     ; If R1 result code is 1 then move to CMD8
  jmp SD_Card_Error            ; If R1 result code is not 1 then return error
CMD8:
  lda #$FF                     ; Setup $FF
  jsr SPI_Write_Byte           ; Send $FF before CMD
  lda #$48                     ; Setup CMD8
  jsr SPI_Write_Byte           ; Send CMD8 byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$01                     ; Voltage select byte
  jsr SPI_Write_Byte           ; Send data byte
  lda #$AA                     ; Check patten
  jsr SPI_Write_Byte           ; Send data byte
  lda #$87                     ; Setup CRC byte
  jsr SPI_Write_Byte           ; Send CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$01                     ; Check if R1 result code is 1
  beq CMD8_OK                  ; If R1 result code is 1 then check R7 result code
  jmp SD_Card_Error            ; If R1 result code is not 1 then return an error
CMD8_OK:
  jsr SPI_Read_Byte            ; Read R7 result code byte 2
  jsr SPI_Read_Byte            ; Read R7 result code byte 3
  jsr SPI_Read_Byte            ; Read R7 result code byte 4
  jsr SPI_Read_Byte            ; Read R7 result code byte 5
  cmp #$AA                     ; R7 byte 5 result code should be $AA
  beq CMD55                    ; If R7 byte 5 is $AA then move on to CMD55
  jmp SD_Card_Error            ; If R7 byte 5 is not $AA then return error
CMD55:
  lda #$40                     ; Only go around 64 times
  sta ZP_Counter16             ; Setup counter
CMD55_Loop:
  dec ZP_Counter16             ; Decremnet counter
  bne SendCMD55_ACMD41         ; If Counter is not zero then send CMD55
  jmp SD_Card_Error            ; After 32 time around we stop with error
SendCMD55_ACMD41:
  lda #$FF                     ; Setup $FF byte
  jsr SPI_Write_Byte           ; Send $FF before CMD
  lda #$77                     ; Setup CMD55
  jsr SPI_Write_Byte           ; Send CMD55 byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup fake CRC, CRC is not needed at this point
  jsr SPI_Write_Byte           ; Send fake CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$01                     ; Check if R1 resutl code is 1
  beq ACMD41                   ; If R1 is 1 then move to CMD41
  jmp SD_Card_Error
ACMD41:
  lda #$FF                     ; Setup $FF byte
  jsr SPI_Write_Byte           ; Send $FF byte before CMD
  lda #$69                     ; Setup ACMD41
  jsr SPI_Write_Byte           ; Send ACMD41
  lda #$40                     ; Setup $40 - support high capacity cards
  jsr SPI_Write_Byte           ; Send data byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$00                     ; Setup null byte
  jsr SPI_Write_Byte           ; Send null byte
  lda #$FF                     ; Setup fake CRC, CRC not needed at theis point
  jsr SPI_Write_Byte           ; Send CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$01                     ; Check if R1 result code is 1
  beq CMD55_Loop               ; If R1 is 1 then go around again
  cmp #$00                     ; Check if R1 result code is 00
  beq CMD58                    ; If R1 result code is 00 then move to CMD58
  jmp SD_Card_Error            ; Return error
CMD58:
  lda #$FF                     ; Setup $FF byte
  jsr SPI_Write_Byte           ; Send $FF byte before CMD
  lda #$7A                     ; Setup CMD58
  jsr SPI_Write_Byte           ; Send CMD58
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup stuff byte
  jsr SPI_Write_Byte           ; Send stuff byte
  lda #$FF                     ; Setup fake CRC, CRC byte is not needed at this point
  jsr SPI_Write_Byte           ; Send CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$00                     ; Check R1 result code is 00
  beq SD_Card_Ready            ; If R1 result code is 00 then card is ready
  jmp SD_Card_Error            ; If R1 result code is not 1 then return error
SD_Card_Ready:
  jsr SPI_Read_Byte            ; Read R3 result code byte 1
  and #0b01000000              ; Mask off must the SDHC bit.
  sta SDHCFlag                 ; Store a flag to show if we have a SDcard or an SDHC card
  jsr SPI_Read_Byte            ; Read R3 result code byte 2
  jsr SPI_Read_Byte            ; Read R3 result code byte 3
  jsr SPI_Read_Byte            ; Read R3 result code byte 4
SetBlockLength:
  lda #$FF                     ; Setup $FF byte
  jsr SPI_Write_Byte           ; Send $FF byte before CMD16
  lda #$50                     ; Setup CMD16
  jsr SPI_Write_Byte           ; Send byte CMD16
  lda #$00                     ; bits 24-31
  jsr SPI_Write_Byte           ; Send byte
  lda #$00                     ; bits 16-23
  jsr SPI_Write_Byte           ; Send byte
  lda #$02                     ; bits 8-15    $0200 = 512 bytes
  jsr SPI_Write_Byte           ; Send byte
  lda #$00                     ; bits 0-7
  jsr SPI_Write_Byte           ; Send byte
  lda #$FF                     ; Setup fake CRC
  jsr SPI_Write_Byte           ; Send CRC byte before CMD
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$00                     ; Check R1 result code is 00
  beq EndSDInit                ; If R1 result code is 00 then init finished
  jmp SD_Card_Error            ; If R1 result code is not 1 then return error
EndSDInit:
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send $FF junk byte before CS_ change
  jsr SPI_Unselect_SDcard      ; Change CS_ to high to unslect SDcard
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send junk byte after CS_ change
  lda #$00                     ; Return no error
  sta ErrorCode                ; store error code
  rts



SD_Card_Read_Sector:           ; Reads a sector to location pointed to by ZP_SectorBufPTR
  jsr Sector_Math
  lda ZP_SectorSize            ; Get bytes to read - LSB
  cmp #$00                     ; Check if size to load is 00
  bne CMD17_Setup              ; If not 00 then go read sector
  lda ZP_SectorSize+1          ; Get bytes to read - LSB
  cmp #$00                     ; Check if size to load is 00
  bne CMD17_Setup              ; If not 00 then go read sector
  jmp CMD17_Cleanup            ; Nothing to read, so exit
CMD17_Setup:
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send $FF junk before CS_ change
  jsr SPI_Select_SDcard        ; Select the SD card
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send junk after CS_ change
CMD17:
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send $FF junk before CMD
  lda #$51                     ; Setup CMD17
  jsr SPI_Write_Byte           ; Send CMD17 byte
  lda SectorTemp               ; Get LBA Sector byte 0
  jsr SPI_Write_Byte           ; Send LBA Sector byte 0
  lda SectorTemp+1             ; Get LBA Sector byte 1
  jsr SPI_Write_Byte           ; Send LBA Sector byte 1
  lda SectorTemp+2             ; Get LBA Sector byte 2
  jsr SPI_Write_Byte           ; Send address byte 2
  lda SectorTemp+3             ; Get LBA Sector byte 3
  jsr SPI_Write_Byte           ; send address byte 3
  lda #$FF                     ; Setup fake CRC, CRC is not needed at this point
  jsr SPI_Write_Byte           ; Send CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  and #$FE                     ; keep only to bits
  cmp #$00                     ; check that we have no error
  beq CMD17_StartToken         ; If no error then wait for start token
  jmp SD_Card_Error            ; if error then exit read sector
CMD17_StartToken:
  jsr SPI_Read_Byte            ; Read Start Token byte
  cmp #$FE                     ; Check if start token $FE has been read
  bne CMD17_StartToken         ; Loop until start token is read
  lda #$00                     ; Set counter to 00
  sta ZP_Counter16             ; Setup 16 bit counter
  sta ZP_Counter16+1           ; Setup 16 bit counter
CMD17_ReadBytes:
  jsr SPI_Read_Byte            ; Read a byte of sector
  ldy #$00                     ; set $00 offset for the pointer
  sta (ZP_SectorBufPTR),y      ; Store the byte
IncSectorBuffer:
  inc ZP_SectorBufPTR          ; Increment buffer pointer low byte
  bne IncBytesRead             ; If not zero then increment read bytes counter
  inc ZP_SectorBufPTR+1        ; Increment buffer pointer high byte
IncBytesRead:
  inc ZP_Counter16             ; Increment counter low byte
  bne CheckForSize             ; If not zero then go check if we are at end of size to read
  inc ZP_Counter16+1           ; Increment counter high byte
CheckForSize:                  ; Check if we are at end of buffer
  lda ZP_SectorSize            ; Get bytes to read - low byte
  cmp ZP_Counter16             ; Check bytes read - low byte
  bne CMD17_ReadBytes          ; If not at end then go read more bytes
  lda ZP_SectorSize+1          ; Get bytes to read - high byte
  cmp ZP_Counter16+1           ; Check bytes read - high byte
  bne CMD17_ReadBytes          ; If not at end then go read more bytes
ReadJunkLoop:                  ; Reads any extra bytes if this was a partial sector read
  lda #$00                     ; Check if we have read $0200 bytes
  cmp ZP_Counter16             ; Check bytes read - low byte
  bne ReadJunk                 ; Not at 512, so go read junk bytes
  lda #$02                     ; Check high byte of read byte counter
  cmp ZP_Counter16+1           ; Check bytes read - high byte
  beq CMD17_Cleanup            ; Read all 512 bytes
ReadJunk:                      ; Read a junk byte
  jsr SPI_Read_Byte            ; Read a byte
  inc ZP_Counter16             ; Increment counter low byte
  bne ReadJunkLoop             ; If not zero then go read junk bytes
  inc ZP_Counter16+1           ; Increment counter high byte
  jmp ReadJunkLoop             ; Go read more junk bytes
CMD17_Cleanup:
  jsr SPI_Read_Byte            ; Read Sector CRC byte, but don't use it
  lda #$FF                     ; Setup junk byte
  jsr SPI_Write_Byte           ; Send junk after read finished
  lda #$FF                     ; Setup junk byte
  jsr SPI_Write_Byte           ; Send junk before CS_ change
  jsr SPI_Unselect_SDcard      ; Unselect the SD card.
  lda #$FF                     ; Setup junk byte
  jsr SPI_Write_Byte           ; Send junk after CS_ change
  rts



SD_Card_Write_Sector:
  jsr Sector_Math              ; Go calculate Sector for SDHC vs SD
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send $FF junk before CS_ change
  jsr SPI_Select_SDcard        ; Select the SD card
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send junk after CS_ change
CMD24:
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send $FF junk before CMD
  lda #$58                     ; Setup CMD24
  jsr SPI_Write_Byte           ; Send CMD24 byte
  lda SectorTemp               ; Get LBA Sector byte 0
  jsr SPI_Write_Byte           ; Send LBA Sector byte 0
  lda SectorTemp+1             ; Get LBA Sector byte 1
  jsr SPI_Write_Byte           ; Send LBA Sector byte 1
  lda SectorTemp+2             ; Get LBA Sector byte 2
  jsr SPI_Write_Byte           ; Send address byte 2
  lda SectorTemp+3             ; Get LBA Sector byte 3
  jsr SPI_Write_Byte           ; send address byte 3
  lda #$FF                     ; Fake CRC FF
  jsr SPI_Write_Byte           ; Send Fake CRC byte
  jsr SD_Card_Result           ; Wait for R1 result code
  and #$FE                     ; keep only to bits
  cmp #$00                     ; check that we have no error
  beq WriteSector
  jmp SD_Card_Error            ; if error then exit read sector
WriteSector:
  lda #$FE                     ; Start token
  jsr SPI_Write_Byte           ; Send Start token byte
  lda #$00                     ; Set counter to 00
  sta ZP_Counter16             ; Setup 16 bit counter
  sta ZP_Counter16+1           ; Setup 16 bit counter
CMD24_WriteBytes:
  ldy #$00                     ; set $00 offset for the pointer
  lda (ZP_SectorBufPTR),y      ; get byte from memory
  jsr SPI_Write_Byte           ; Send byte of  data
  inc ZP_SectorBufPTR          ; Increment buffer pointer low byte
  bne IncBytesWritten          ; If not zero then increment bytes counter
  inc ZP_SectorBufPTR+1        ; Increment buffer pointer high byte
IncBytesWritten:
  inc ZP_Counter16             ; Increment counter low byte
  bne CheckWriteSize           ; If not zero then check if write is done.
  inc ZP_Counter16+1           ; Incremnet counter high byte
CheckWriteSize:
  lda #$00                     ; Check if we have written $0200 bytes
  cmp ZP_Counter16             ; Check LSB of counter
  bne CMD24_WriteBytes         ; Go write more bytes
  lda #$02                     ; Check if we have written $0200 bytes
  cmp ZP_Counter16+1           ; Check MSB of counter
  bne CMD24_WriteBytes         ; go write more bytes
CMD24_CheckWriteValid:
  jsr SD_Card_Result           ; Wait for R1 result code
  and #$1F                     ; We only check lower bits
  cmp #$05                     ; Check for status 05 for accepted write data
  beq CMD24_Cleanup            ; If write accepted then cleanup
  jmp SD_Card_Error            ; If error then exit read sector
CMD24_Cleanup:
  jsr SD_Card_Result           ; Wait for R1 result code
  cmp #$00                     ; Check if result is 00 (busy)
  beq CMD24_Cleanup            ; If result is zero then we wait/check again
  lda #$FF                     ; Setup junk byte
  jsr SPI_Write_Byte           ; Send junk before CS_ change
  jsr SPI_Unselect_SDcard      ; Unselect the SD card.
  lda #$FF                     ; Setup junk byte
  jsr SPI_Write_Byte           ; Send junk after CS_ change
  rts



SD_Card_Mount:                 ; Reads Boot Sector and Patition table to setup SDcard.
  lda #$00                     ; Setup to read sector 00
  sta CurrentSector            ; MSB of sector
  sta CurrentSector+1
  sta CurrentSector+2
  sta CurrentSector+3          ; LSB of Sector
  lda #$00                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR          ; Set buffer start pointer Low byte
  lda #$03                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR+1        ; Set buffer start pointer High byte
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Set buffer end pointer Low byte
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Set buffer end pointer High byte
  jsr SD_Card_Read_Sector      ; Read sector 00
  lda $04FE                    ; Check Boot Sector marker at $01FE of sector (loaded at $0300)
  cmp #$55                     ; Check Boot Sector marker of $55
  beq CheckBootSec             ; Continue to check Boot Sector marker
  jmp BootSecError             ; No match means return error
CheckBootSec:
  lda $04FF                    ; Check Boot Sector marker at $01FF of Sector (loaded at $0300)
  cmp #$AA                     ; Check boot sector marker of $AA
  beq CheckPatitionType        ; Continue to check Patition type
  jmp BootSecError             ; No match means return error
CheckPatitionType:
  lda $04C2                    ; Get patition type code from $01C2 of Sector (Loaded at $0300)
  cmp #$0B                     ; Check for type $0B
  beq ReadPartition            ; Matching is good.
  cmp #$0C                     ; Check for type $0C
  beq ReadPartition            ; Matching is good.
  jmp BootSecError             ; Type not matching, return error
ReadPartition:
  lda $04C9                    ; Get LBA start (partition table), note reverse order of bytes
  sta CurrentSector            ; Store LBA Sector number of partition table
  lda $04C8                    ; Get LBA start (partition table), note reverse order of bytes
  sta CurrentSector+1          ; Store LBA Sector number of partition table
  lda $04C7                    ; Get LBA start (partition table), note reverse order of bytes
  sta CurrentSector+2          ; Store LBA Sector number of partition table
  lda $04C6                    ; Get LBA start (partition table), note reverse order of bytes
  sta CurrentSector+3          ; Store LBA Sector number of partition table
  lda #$00                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR          ; Store Low byte of start buffer pointer
  lda #$03                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR+1        ; Store High byte of start buffer pointer
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Store Low byte of end of buffer pointer
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Store High byte of end of buffer pointer
  jsr SD_Card_Read_Sector      ; Read patition table
  clc                          ; Clear carry before add
  lda CurrentSector+3          ; Load partition table sector LSB
  adc $030E                    ; Add reserved sectors to LBA begin to get FAT begin
  sta FAT1BeginLBA+3           ; Store the begining of FAT (begining of partition + reserved sectors)
  lda CurrentSector+2          ; Load partition table sector next byte
  adc $030F                    ; Add reserved sectors to LBA begin to get FAT begin
  sta FAT1BeginLBA+2           ; Store the begining of FAT (begining of partition + reserved sectors)
  lda CurrentSector+1          ; Load partition table sector next byte
  adc #$0                      ; Add reserved sectors to LBA begin to get FAT begin.
  sta FAT1BeginLBA+1           ; Store the begining of FAT (begining of partition + reserved sectors)
  lda CurrentSector            ; Load partition table sector MSB
  adc #$00                     ; Add reserved sectors to LBA begin to get FAT begin.
  sta FAT1BeginLBA             ; Store the begining of FAT (begining of partition + reserved sectors)
  lda $0324                    ; Get sectors per FAT
  sta FATsize+3                ; Store Size of FAT
  lda $0325                    ; Get sectors per FAT
  sta FATsize+2                ; Store Size of FAT
  lda $0326                    ; Get sectors per FAT
  sta FATsize+1                ; Store Size of FAT
  lda $0327                    ; Get sectors per FAT
  sta FATsize                  ; Store Size of FAT
  clc                          ; clear carry before add
  lda FAT1BeginLBA+3           ; get FAT1 LSB
  adc FATsize+3                ; add sectors per FAT
  sta FAT2BeginLBA+3           ; Store to FAT2 begin
  lda FAT1BeginLBA+2           ; get FAT1 LSB
  adc FATsize+2                ; add sectors per FAT
  sta FAT2BeginLBA+2           ; Store to FAT2 begin
  lda FAT1BeginLBA+1           ; get FAT1 LSB
  adc FATsize+1                ; add sectors per FAT
  sta FAT2BeginLBA+1           ; Store to FAT2 begin
  lda FAT1BeginLBA             ; get FAT1 LSB
  adc FATsize                  ; add sectors per FAT
  sta FAT2BeginLBA             ; Store to FAT2 begin
ClusterBegin:                  ; Get Sectors per FAT, then * 2, then add FAT bagin to get ClusterBegin.
  lda $0327                    ; Get Sectors per FAT
  sta ClusterBeginLBA          ; Store Sectors per FAT, using ClusterBegiaLBA as temp location
  lda $0326                    ; Get sectors per FAT
  sta ClusterBeginLBA+1        ; Store Sectors per FAT, using ClusterBegiaLBA as temp location
  lda $0325                    ; Get sectors per FAT
  sta ClusterBeginLBA+2        ; Store Sectors per FAT, using ClusterBegiaLBA as temp location
  lda $0324                    ; Get sectors per FAT
  sta ClusterBeginLBA+3        ; Store Sectors per FAT, using ClusterBegiaLBA as temp location
  clc
  rol ClusterBeginLBA+3        ; Sectors per FAT * 2 (LSB)
  rol ClusterBeginLBA+2        ; Sectors per FAT * 2
  rol ClusterBeginLBA+1        ; Sectors per FAT * 2
  rol ClusterBeginLBA          ; Sectors per FAT * 2 (MSB)
  clc                          ; Clear carry before adding FAT begin and size of FAT
  lda FAT1BeginLBA+3           ; Get FAT begin
  adc ClusterBeginLBA+3        ; Add Size of FAT (Sectors per FAT * 2)
  sta ClusterBeginLBA+3        ; Store cluster begin
  lda FAT1BeginLBA+2           ; Get FAT begin
  adc ClusterBeginLBA+2        ; Add Size of FAT (Sectors per FAT * 2)
  sta ClusterBeginLBA+2        ; Store cluster begin
  lda FAT1BeginLBA+1           ; Get FAT begin
  adc ClusterBeginLBA+1        ; Add Size of FAT (Sectors per FAT * 2)
  sta ClusterBeginLBA+1        ; Store cluster begin
  lda FAT1BeginLBA             ; Get FAT begin
  adc ClusterBeginLBA          ; Add Size of FAT (Sectors per FAT * 2)
  sta ClusterBeginLBA          ; Store cluster begin
  lda $030D                    ; Get sectors per cluster from partition table
  sta SecPerCluster            ; Store sectors per cluster
  lda $032F                    ; Get Director first cluster MSB
  sta RootDirCluster           ; Store Directory First Cluster
  lda $032E                    ; Get Directory first cluster
  sta RootDirCluster+1         ; Store Directory First Cluster
  lda $032D                    ; Get Directory first cluster
  sta RootDirCluster+2         ; Store Directory First Cluster
  lda $032C                    ; Get Directory first cluster LSB
  sta RootDirCluster+3         ; Store directory First Cluster
  jsr LoadFAT                  ; Load first sector of FAT
  lda #$00                     ; Set error to no error
  sta ErrorCode                ; Return no error
  rts                          ; Return
BootSecError:
  lda #$FF                     ; Return an Error
  sta ErrorCode                ; Return an Error
  rts                          ; Return



SD_Card_Error:                 ; Clean up SPI/SDcard things and return an error
  lda #$FF                     ; Set error
  sta ErrorCode                ; Return an error
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send junk before CS_ change
  jsr SPI_Unselect_SDcard      ; Ensure SDcard is not slected
  lda #$FF                     ; Setup $FF junk byte
  jsr SPI_Write_Byte           ; Send junk after CS_ change
  rts                          ; Return an error



SD_Card_Result:                ; Wait for SDcard to return R1, SDcard sends $FF at idle.
  lda $FF                      ; Setup counter to only loop $FF times
  sta ZP_Counter               ; Setup counter so we can't loop forever
WaitLoop:
  jsr SPI_Read_Byte            ; Read single byte
  cmp #$FF                     ; Check if it's $FF
  bne WaitExit                 ; If byte is NOT $FF then data is returned and we exit
  dec ZP_Counter               ; Check if we have looped too many times
  bne WaitLoop                 ; Still waiting for data, go around again.
WaitExit:
  rts                          ; Return R1 or $FF



FindFile:
  lda RootDirCluster           ; Get first dir cluster
  sta ClusterNum               ; Setup to read first dir cluster
  lda RootDirCluster+1         ; Get first dir cluster
  sta ClusterNum+1             ; Setup to read first dir cluster
  lda RootDirCluster+2         ; Get first dir cluster
  sta ClusterNum+2             ; Setup to read first dir cluster
  lda RootDirCluster+3         ; Get first dir cluster
  sta ClusterNum+3             ; Setup to read first dir cluster
  jsr LBA_Addr                 ; Convert cluster number to sector number
  lda #$00                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR          ; Setup to read sector to $0300
  lda #$03                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR+1        ; Setup to read sector to $0300
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Setup to read $200 bytes
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Setup to read $200 bytes
  jsr SD_Card_Read_Sector      ; Read the DIR sector to $0300
CheckSector:                   ; Search whole sector
  lda #$00                     ; Save ptr to DIR $0300
  sta ZP_DIRRecordPTR          ; DIRRecordPTR points to buffer to search
  lda #$03                     ; Save ptr to DIR $0300
  sta ZP_DIRRecordPTR+1        ; DIRRecordPTR points to buffer to search
  ldx #$10                     ; Check 16 records in the sector
CheckNextRecord:
  ldy #$00                     ; Search from byte 00
CheckAnotherByte:
  lda FileName,y               ; load char at Filename+Y
  cmp (ZP_DIRRecordPTR),y      ; Check against Buffer+Y
  bne NextRecord               ; No match on this record
  iny                          ; Increment to next char
  cpy #$0B                     ; 11 bytes
  bne CheckAnotherByte         ; Check all 11 bytes.
FileFound:                     ; Grab cluster number from DIR record
  ldy #$14                     ; $14 is offset to cluster number with directory record
  lda (ZP_DIRRecordPTR),y      ; Get Cluster number from directory record
  sta ClusterNum+1             ; Store cluster number of file
  iny                          ; Increment offset into directory record
  lda (ZP_DIRRecordPTR),y      ; Get Cluster number from directory record
  sta ClusterNum               ; Store cluster number of file
  ldy #$1A                     ; Next part of cluster number is at offset $1A
  lda (ZP_DIRRecordPTR),y      ; Get Cluster number from directory record
  sta ClusterNum+3             ; Store cluster number of file
  iny                          ; Increment offset into directory record
  lda (ZP_DIRRecordPTR),y      ; Get Cluster number from directory record
  sta ClusterNum+2             ; Store cluster number of file
  ldy #$1C                     ; FileSize is at offset $1C
  lda (ZP_DIRRecordPTR),y      ; Get FileSize from directory record
  sta FileSize                 ; Store FileSize byte
  iny                          ; move to next byte in record for file size
  lda (ZP_DIRRecordPTR),y      ; Get FileSize from directory record
  sta FileSize+1               ; Store FileSize byte
  lda #$00                     ; No error as we found the file
  sta ErrorCode                ; Return with no error
  rts                          ; Exit because we found file
NextRecord:
  clc                          ; Clear the carry before we add.
  lda ZP_DIRRecordPTR          ; Get pointer to Directory record
  adc #$20                     ; Add $20 to increment Pointer to next record.
  sta ZP_DIRRecordPTR          ; Store new directory record pointer low byte
  lda #$00                     ; Deal with overflow
  adc ZP_DIRRecordPTR+1        ; Increment high byte via carry
  sta ZP_DIRRecordPTR+1        ; Store new directory record pointer high byte
  dex                          ; Decrement the record counter
  bne CheckNextRecord          ; Check the next record
  jsr GetNextReadSector        ; No more records in this sector, so get next sector
  lda #$00                     ; Check error is 00
  cmp ErrorCode                ; Check Error is 00
  bne FileNotFound             ; If error not 00 then no more sectors and so file not found
  lda #$00                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR          ; Setup to read sector to $0300
  lda #$03                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR+1        ; Setup to read sector to $0300
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Setup to read $200 bytes
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Setup to read $200 bytes
  jsr SD_Card_Read_Sector      ; Read the DIR sector to $0300
  lda #$00                     ; Check error after reading sector
  cmp ErrorCode                ; If no more sectors, then file not found
  beq CheckSector              ; Go search new sector
FileNotFound:
  lda #$02                     ; Return error
  sta ErrorCode                ; Return error
  rts



GetNextReadSector:
  lda SectorIndex              ; Get which sector of this cluster
  cmp SecPerCluster            ; Check against number of sectors per cluster
  bne .IncrementSector         ; If more sectors in this cluster then just increment sector
  jsr GetNextReadCluster       ; If last sector in this cluster, then go find next cluster in chain
  lda ErrorCode                ; Get error code from the find next cluster
  cmp #$00                     ; Check is error is 00
  beq .GotNextSector           ; If no error then we have next sector
  rts                          ; Exit with error as we have no more clusters
.IncrementSector
  inc SectorIndex              ; Increment sector index
  clc                          ; Clear carry before add
  lda #$01                     ; Add 01 to Sector
  adc CurrentSector+3          ; Add to lower byte of sector number
  sta CurrentSector+3          ; store new sector number
  lda #$00                     ; Load 00 for add to deal with carry
  adc CurrentSector+2          ; Add to next byte
  sta CurrentSector+2          ; Store next byte
  lda #$00                     ; Load 00 for add to deal with carry
  adc CurrentSector+1          ; Add to next byte
  sta CurrentSector+1          ; Store next byte
  lda #$00                     ; Load 00 for add to deal with carry
  adc CurrentSector            ; Add to next byte
  sta CurrentSector            ; Store next byte
  lda #$00                     ; Set error to 00
  sta ErrorCode                ; Return no error
.GotNextSector
  rts



GetNextReadCluster:
  lda ClusterNum+3             ; Get second byte of Cluster num
  rol                          ; Rotate upper bit to carry
  lda ClusterNum+2             ; Get next byte
  rol                          ; Rotate carry into lower bit.
  sta RequestedFATSector       ; Requested fat sector is Cluster num / 128
  jsr RequestFATSector         ; Request FAT Sector
  lda #$00                     ; Zero out offset into FAT
  sta ZP_DIRRecordPTR          ; Zero out offset into FAT
  sta ZP_DIRRecordPTR+1        ; Zero out offset into FAT
  lda ClusterNum+3             ; Keep lower 7 bits of cluster number for later
  and #$7F                     ; Mask off top bit to keep lower 7 bits for later
  sta ZP_DIRRecordPTR          ; Store the lower 7 bits to the pointer
  clc                          ; Clear carry for rotate
  rol ZP_DIRRecordPTR          ; Mul*2
  rol ZP_DIRRecordPTR          ; Mul*2 again (4 bytes per record in the FAT)
  rol ZP_DIRRecordPTR+1        ; Deal with overflow of mul
  clc                          ; Clear carry before add
  lda ZP_DIRRecordPTR+1        ; Add $0500 to offset into FAT.
  adc #$05                     ; add $0500 to offset into FAT.
  sta ZP_DIRRecordPTR+1        ; add $0500 to offset into FAT.
  lda #$00                     ; add $0500 to offset into FAT.
  adc ZP_DIRRecordPTR          ; add $0500 to offset into FAT.
  sta ZP_DIRRecordPTR          ; add $0500 to offset into FAT.
  ldy #$00                     ; lookup new cluster number from FAT
  lda (ZP_DIRRecordPTR),y      ; lookup new cluster number from FAT
  sta ClusterNum+3             ; lookup new cluster number from FAT
  iny                          ; lookup new cluster number from FAT
  lda (ZP_DIRRecordPTR),y      ; lookup new cluster number from FAT
  sta ClusterNum+2             ; lookup new cluster number from FAT
  iny                          ; lookup new cluster number from FAT
  lda (ZP_DIRRecordPTR),y      ; lookup new cluster number from FAT
  sta ClusterNum+1             ; lookup new cluster number from FAT
  iny                          ; lookup new cluster number from FAT
  lda (ZP_DIRRecordPTR),y      ; lookup new cluster number from FAT
  sta ClusterNum               ; lookup new cluster number from FAT
.CheckClusterEndMarker
  lda ClusterNum+3             ; Check for end cluster marker
  and #$F8                     ; Check for end cluster marker
  cmp #$F8                     ; Check for end cluster marker
  bne .GoodCluster             ; Have NOT Found end marker
  lda #$ff                     ; Check for end cluster marker
  cmp ClusterNum+2             ; Check for end cluster marker
  bne .GoodCluster             ; Have NOT found end marker
  cmp ClusterNum+1             ; Check for end cluster marker
  bne .GoodCluster             ; Have NOT found end marker
  lda ClusterNum               ; Check for end cluster marker
  and #$0F                     ; Check for end cluster marker
  cmp #$0F                     ; Check for end cluster marker
  bne .GoodCluster             ; Have NOT found end marker
  jmp .EndCluster              ; All bytes match end marker
.GoodCluster:
  jsr LBA_Addr                 ; Convert cluster to sector
  lda #$01                     ; Reset cluster index
  sta SectorIndex              ; Reset cluster index
  lda #$00                     ; Set no error
  sta ErrorCode                ; Set no error
  rts                          ; Return with no error
.EndCluster:
  lda #$FF                     ; Error no more clusters.
  sta ErrorCode                ; Set error
  rts                          ; Return with error



Sector_Math:
  lda CurrentSector            ; Move current sector to a temp locaton to preserve it.
  sta SectorTemp               ; Move current sector to a temp locaton to preserve it.
  lda CurrentSector+1          ; Move current sector to a temp locaton to preserve it.
  sta SectorTemp+1             ; Move current sector to a temp locaton to preserve it.
  lda CurrentSector+2          ; Move current sector to a temp locaton to preserve it.
  sta SectorTemp+2             ; Move current sector to a temp locaton to preserve it.
  lda CurrentSector+3          ; Move current sector to a temp locaton to preserve it.
  sta SectorTemp+3             ; Move current sector to a temp locaton to preserve it.
Check_SDHC:
  lda #$00                     ; Standard SD card will have a Flag of 00, SDHC will be $40
  cmp SDHCFlag                 ; Check if we have a SDHC card or not.
  bne Sector_Math_Done         ; SDHC card means we just go with sector as in temp
SD_Math:                       ; Standard SD card need sector converted to bytes.
  lda CurrentSector+3          ; Move sector by one whole byte to get * 256
  sta SectorTemp+2             ; Move sector by one whole byte to get * 256
  lda CurrentSector+2          ; Move sector by one whole byte to get * 256
  sta SectorTemp+1             ; Move sector by one whole byte to get * 256
  lda CurrentSector+1          ; Move sector by one whole byte to get * 256
  sta SectorTemp               ; Move sector by one whole byte to get * 256
  lda #$00                     ; Move sector by one whole byte to get * 256
  sta SectorTemp+3             ; Move sector by one whole byte to get * 256
  clc                          ; Clear carry before the rotate
  rol SectorTemp+3             ; Rotate left to get a mul * 2
  rol SectorTemp+2             ; Rotate left to get a mul * 2
  rol SectorTemp+1             ; Rotate left to get a mul * 2
  rol SectorTemp               ; Rotate left to get a mul * 2
Sector_Math_Done:
  rts



LBA_Addr:                      ; Calculates LBA sector from cluster address.
  sec                          ; Set carry before subtract
  lda ClusterNum+3             ; LSB
  sbc #$02                     ; Subtract 2 from cluster number because rules.
  sta CurrentSector+3          ; Store Sector LSB
  lda ClusterNum+2             ; Get next byte of cluster
  sbc #$00                     ; Subtract 00 to deal with borrow
  sta CurrentSector+2          ; Store Sector byte
  lda ClusterNum+1             ; Get next byte of cluster
  sbc #$00                     ; Subtract 00 to deal with borrow`
  sta CurrentSector+1          ; Store Sector byte
  lda ClusterNum               ; Get next byte of cluster
  sbc #$00                     ; Subtract 00 to deal with borrow
  sta CurrentSector            ; Store sector number
  lda SecPerCluster            ; Get how many sectors per cluster, always a multiple of 2
  cmp #$00                     ; Ensure we have a valid Sectors per cluster
  bne SectorMultiplyLoop       ; If we have a valid sectors per cluster then do the math
  jmp LBAError                 ; Return an error
SectorMultiplyLoop:            ; Multiply number of sectors per cluster by cluster
  lsr                          ; Shift Sectors per cluster to get carry
  bcs SectorMultiplyDone       ; If carry set then we have gone around enough times
  asl CurrentSector+3          ; Shift left to multiply by 2
  rol CurrentSector+2          ; Rotate right to multiply by 2 with carry
  rol CurrentSector+1          ; Rotate right to multiply by 2 with carry
  rol CurrentSector            ; Rotate right to multiply by 2 with carry
  jmp SectorMultiplyLoop       ; Go around again.
SectorMultiplyDone:
  clc                          ; Clear carry before add
  lda CurrentSector+3          ; Get Sector LSB
  adc ClusterBeginLBA+3        ; Add ClusterBegin
  sta CurrentSector+3          ; Store Sector number
  lda CurrentSector+2          ; Get Sector next byte
  adc ClusterBeginLBA+2        ; Add ClusterBegin next byte
  sta CurrentSector+2          ; Store Sector number
  lda CurrentSector+1          ; Get Sector next byte
  adc ClusterBeginLBA+1        ; Add ClusterBegin next byte
  sta CurrentSector+1          ; Store sector number
  lda CurrentSector            ; Get Sector MSB
  adc ClusterBeginLBA          ; Add ClusterBegin MSB
  sta CurrentSector            ; Store Sector MSB
  lda #$01                     ; Reset index to 01
  sta SectorIndex              ; Reset sector index to 01
  rts

LBAError:
  lda #$FF                     ; Error LBA invalid.
  sta ErrorCode                ; Set error
  rts                          ; Return with error.



GetRandomByte:
  ldy #$08                     ; Get 8 bits
GetRandBit:
  clc                          ; Clear carry before rotate
  ror RandSeed+1               ; Rotate internal state
  ror RandSeed                 ; Rotate internal state
  bcc RandDone                 ; If carry set then take that bit
  lda RandSeed+1               ; Get internal state
  eor #$AC                     ; Exclusive OR with $AC
  sta RandSeed+1               ; Store new interanl state
RandDone:
  ror RandByte                 ; Put random bit from carry in to final byte
  dey                          ; Count donw for the 8 bits
  bne GetRandBit               ; Get another bit
  lda RandByte                 ; Return byte via A
  rts



LoadFile:                      ; Loads file to address at FileAddrPTR
  jsr FindFile                 ; Find File, gets file Cluster and FileSize
  lda #$00                     ; Check for error, if file not found
  cmp ErrorCode                ; Check if no file to load
  bne LoadError                ; If no file found then loda error
  jsr LBA_Addr                 ; Calculates LBA sector from Cluster number.
LoadFileLoop:                  ; Top of loop that read file one sector at a time.
  lda FileSize+1               ; Calculate number of whole sectors needed.
  clc                          ; Clear carry before rotate right
  ror                          ; Rotate Right MSB of file size
  sta SectorCount              ; Store Number of sectors needed
  lda FileAddrPTR              ; Setup Sector pointer based on Load Address (LSB)
  sta ZP_SectorBufPTR          ; Setup Sector pointer based on Load Address (LSB)
  lda FileAddrPTR+1            ; Setup Sector pointer based on Load Address (MSB)
  sta ZP_SectorBufPTR+1        ; Setup Sector pointer based on Load Address (MSB)
  lda #$00                     ; Check if one or more whole sectors to load
  cmp SectorCount              ; Check number of sectors to load
  beq LoadPartSector           ; Number of whole sectors is zero so load part sector
LoadWholeSector:               ; If whole sector to read then read whole sector
  lda #$00                     ; Setup to read whole $0200 sector
  sta ZP_SectorSize            ; Setup Read Size low byte
  lda #$02                     ; Setup to read whole $0200 sector
  sta ZP_SectorSize+1          ; Setup Read Size high byte
  jsr SD_Card_Read_Sector      ; Read Sector
  lda FileSize+1               ; Subtract $0200 from file size
  sec                          ; Set carry before subtract
  sbc #$02                     ; We only need to subtract from MSB of filesize
  sta FileSize+1               ; Store FileSize MSB again
  lda FileAddrPTR+1            ; Add $0200 to Load Address (MSB)
  clc                          ; Clear carry before add
  adc #$02                     ; Add $0200 to file load pointer (MSB)
  sta FileAddrPTR+1            ; Store Load addrfess MSB
  jsr GetNextReadSector        ; Find the next sector to load
  jmp LoadFileLoop             ; Go around again
LoadPartSector:                ; Load remainder of sector
  lda FileSize                 ; Setup to read whole $0200 sector
  sta ZP_SectorSize            ; Setup Read Size low byte
  lda FileSize+1               ; Setup to read whole $0200 sector
  sta ZP_SectorSize+1          ; Setup Read Size high byte
  jsr SD_Card_Read_Sector      ; Read Sector
FileLoaded:
  lda #$00                     ; File Loaded
  sta ErrorCode                ; Return No Error
  rts                          ; Return with no error
LoadError:
  lda #$FF                     ; File Not Loaded
  sta ErrorCode                ; Return Error
  rts                          ; Return Error



WriteFile:
  jsr FindFile                 ; Check if we have file with this name already
  lda #$02                     ; 02 is File not found, so we are good to save file
  cmp ErrorCode                ; Check if our error code is 02
  beq .SetupWrite              ; Go write the file.
  jmp WriteFileError           ; Dont write file, return error
.SetupWrite
  lda FileSize                 ; Move file size to Bytes to write counter
  sta BytesToWrite             ; Move file size to Bytes to write counter
  lda FileSize+1               ; Move file size to Bytes to write counter
  sta BytesToWrite+1           ; Move file size to Bytes to write counter
  jsr LoadFAT                  ; Load a copy of the FAT tp $0500
  lda #$FF                     ; Set marker to show we have no previous cluster.
  sta ClusterNum               ; Store $FF in MSB of ClusterNum to show we are first in chain.
  jsr FindFreeCluster          ; Find our first free cluster
  jsr LBA_Addr                 ; Get Sector number for the first cluster
  lda ClusterNum               ; Copy the cluster number to First cluster so we have it for the dir entry
  sta FileFirstCluster         ; Copy the cluster number to First cluster so we have it for the dir entry
  lda ClusterNum+1             ; Copy the cluster number to First cluster so we have it for the dir entry
  sta FileFirstCluster+1       ; Copy the cluster number to First cluster so we have it for the dir entry
  lda ClusterNum+2             ; Copy the cluster number to First cluster so we have it for the dir entry
  sta FileFirstCluster+2       ; Copy the cluster number to First cluster so we have it for the dir entry
  lda ClusterNum+3             ; Copy the cluster number to First cluster so we have it for the dir entry
  sta FileFirstCluster+3       ; Copy the cluster number to First cluster so we have it for the dir entry
WriteFileLoop:                 ; Write the file to SDcard one sector at a time
  lda BytesToWrite+1           ; Calculate number of whole sectors needed
  clc                          ; Clear carry before the rotate right
  ror                          ; Rotate Right MSB of file size to get number of whole clusters needed
  sta SectorCount              ; Store the number of whole sectors needed
  lda FileAddrPTR              ; Setup Sector buffer pointer based on Load Address (LSB)
  sta ZP_SectorBufPTR          ; Setup Sector buffer pointer based on Load Address (LSB)
  lda FileAddrPTR+1            ; Setup Sector buffer pointer based on Load Address (MSB)
  sta ZP_SectorBufPTR+1        ; Setup Sector buffer pointer based on Load Address (MSB)
  lda #$00                     ; Check if one or more whole sectors to write
  cmp SectorCount              ; Check number of sectors to load
  beq WriteLastSector          ; Number of whole sectors is zero so Write last sector
WriteFileSector:               ; Write a whole sector
  jsr SD_Card_Write_Sector     ; Write data to SDcard
  lda BytesToWrite+1           ; Subtract $0200 from file size
  sec                          ; Set carry before subtract
  sbc #$02                     ; We only need to subtract from MSB of filesize
  sta BytesToWrite+1           ; Store FileSize MSB again
  lda FileAddrPTR+1            ; Add $0200 to Load Address (MSB)
  clc                          ; Clear carry before add
  adc #$02                     ; Add $0200 to file load pointer (MSB)
  sta FileAddrPTR+1            ; Store Load address MSB
  lda BytesToWrite+3           ; Check if we have more bytes to write
  ora BytesToWrite+2           ; Check if we have more bytes to write
  ora BytesToWrite+1           ; Check if we have more bytes to write
  ora BytesToWrite             ; Check if we have more bytes to write
  cmp #$00                     ; Check if byte to write is $00
  beq WriteFileCleanup         ; If nothing more to write then do cleanup
  jsr FindNextWriteSector      ; Find the next sector to Write to
  jmp WriteFileLoop            ; Go around again and write more of the file
WriteLastSector:               ; Write the last sector
  jsr SD_Card_Write_Sector     ; Write data to SDcard
WriteFileCleanup:
  jsr WriteFAT                 ; Write the FAT back the the SDcard.
  jsr UpdateDir                ; Add the file to the directory structure.
DoneFileWrite:
  rts



WriteFileError:
  lda #$05                     ; Error 5 - I/O error
  sta ErrorCode                ; Store error code and return
  rts



FindNextWriteSector:           ; Find the next sector to write to
  lda SectorIndex              ; Get which sector of this cluster
  cmp SecPerCluster            ; Check against number of sectors per cluster
  bne .IncrementSector         ; If we have more sectors in this cluster then just increment sector
  jsr FindFreeCluster          ; If we are at last sector in this cluster, then go find next cluster
  jsr LBA_Addr                 ; Calculate the sector address from the cluster number
  jmp .FoundNextSector         ; Return after finding a new sector in the new cluster
.IncrementSector               ; Increment the sector number within the current cluster
  inc SectorIndex              ; Increment sector index, which sector with in the cluster
  clc                          ; Clear carry before add
  lda #$01                     ; Add 01 to Sector
  adc CurrentSector+3          ; Add to lower byte of sector number
  sta CurrentSector+3          ; store new sector number
  lda #$00                     ; Load 00 for add to deal with carry
  adc CurrentSector+2          ; Add to next byte
  sta CurrentSector+2          ; Store next byte
  lda #$00                     ; Load 00 for add to deal with carry
  adc CurrentSector+1          ; Add to next byte
  sta CurrentSector+1          ; Store next byte
  lda #$00                     ; Load 00 for add to deal with carry
  adc CurrentSector            ; Add to next byte
  sta CurrentSector            ; Store next byte
  lda #$00                     ; Set error to 00
  sta ErrorCode                ; Return no error
.FoundNextSector
  rts



FindFreeCluster:               ; Returns free cluster number to ClusterNum
  lda ClusterNum               ; Backup current cluster number to clustertemp
  sta ClusterTemp              ; Backup current cluster number to clustertemp
  lda ClusterNum+1             ; Backup current cluster number to clustertemp
  sta ClusterTemp+1            ; Backup current cluster number to clustertemp
  lda ClusterNum+2             ; Backup current cluster number to clustertemp
  sta ClusterTemp+2            ; Backup current cluster number to clustertemp
  lda ClusterNum+3             ; Backup current cluster number to clustertemp
  sta ClusterTemp+3            ; Backup current cluster number to clustertemp
  lda #$ff                     ; check if no current cluster
  cmp ClusterNum
  bne .SearchFromCurrent
.SearchFromZero
  lda #00                      ; Start search at cluster 00
  sta ClusterNum               ; Set Cluster to 00
  sta ClusterNum+1             ; Set Cluster to 00
  sta ClusterNum+2             ; Set Cluster to 00
  sta ClusterNum+3             ; Set Cluster to 00
.SearchFromCurrent
  lda ClusterNum+3             ; Get second byte of Cluster num
  rol                          ; rotate upper bit to carry
  lda ClusterNum+2             ; get next byte
  rol                          ; rotate carry into lower bit.
  sta RequestedFATSector       ; requested fat sector is Cluster num / 128
  jsr RequestFATSector         ; load correct FAT sector
  lda ClusterNum+3             ; mask off lower bits of clusternumber
  and #$80                     ; so we can search from sector start
  sta ClusterNum+3             ; store clusternum
  lda #$00                     ; Set pointer to $0500
  sta ZP_DIRRecordPTR          ; Set pointer to $0500
  lda #$05                     ; Set pointer to $0500
  sta ZP_DIRRecordPTR+1        ; Set pointer to $0500
  ldy #$00                     ; Checking the zeroth byte
.CheckClusterFree              ; Loop over clusters looking for one that is all zeros (free)
  lda (ZP_DIRRecordPTR),y      ; Load the byte to check
  cmp #$00                     ; Compare to zero - zero is free
  bne .IncClusterNum            ; If not zero then cluster is inuse and we can move to next cluster
  iny                          ; Increment the byte to check
  cpy #$04                     ; We check 4 bytes
  bne .CheckClusterFree         ; Check the next byte in the cluster
  jmp .FoundFreeCluster         ; Current cluster must be free
.IncClusterNum                 ; Increment the pointer and the cluster number
  clc                          ; Clear carry before add
  lda ClusterNum+3             ; Load low byte of ClusterNum
  adc #$01                     ; Add 01
  sta ClusterNum+3             ; Store new low byte of cluster number
  lda ClusterNum+2             ; Add 00 to next bytes to deal with any carry
  adc #$00                     ; Add 00 to next bytes to deal with any carry
  sta ClusterNum+2             ; Add 00 to next bytes to deal with any carry
  lda ClusterNum+1             ; Add 00 to next bytes to deal with any carry
  adc #$00                     ; Add 00 to next bytes to deal with any carry
  sta ClusterNum+1             ; Add 00 to next bytes to deal with any carry
  lda ClusterNum               ; Add 00 to next bytes to deal with any carry
  adc #$00                     ; Add 00 to next bytes to deal with any carry
  sta ClusterNum               ; Add 00 to next bytes to deal with any carry
  clc                          ; Clear carry before add
  lda ZP_DIRRecordPTR          ; Load low byte of pointer
  adc #$04                     ; Add 4 as we have 4 bytes per record
  sta ZP_DIRRecordPTR          ; Store new low byte
  lda ZP_DIRRecordPTR+1        ; load high byte of pointer
  adc #$00                     ; Add 00, this deals with any carry
  sta ZP_DIRRecordPTR+1        ; Store new high byte
  lda #$07
  cmp ZP_DIRRecordPTR+1
  bne .GoSearchAgain
  lda ClusterNum+3             ; Get second byte of Cluster num
  rol                          ; rotate upper bit to carry
  lda ClusterNum+2             ; get next byte
  rol                          ; rotate carry into lower bit.
  sta RequestedFATSector       ; requested fat sector is Cluster num / 128
  jsr RequestFATSector         ; loda correct FAT sector
  lda #$00                     ; Set pointer to $0500
  sta ZP_DIRRecordPTR          ; Set pointer to $0500
  lda #$05                     ; Set pointer to $0500
  sta ZP_DIRRecordPTR+1        ; Set pointer to $0500
.GoSearchAgain
  ldy #$00                     ; reset Y pointer to 00Y pointer to 00
  jmp .CheckClusterFree         ; Go check the next cluster
.FoundFreeCluster              ; We have found a free cluster
  ldy #$00                     ; We now mark it as end of the cluster chain with $FF
  lda #$FF                     ; We now mark it as end of the cluster chain with $FF
  sta (ZP_DIRRecordPTR),y      ; Mark new cluster as end of chain.
  iny                          ; Mark all 4 bytes as $FF
  sta (ZP_DIRRecordPTR),y      ; Mark new cluster as end of chain.
  iny                          ; Mark all 4 bytes as $FF
  sta (ZP_DIRRecordPTR),y      ; Mark new cluster as end of chain.
  iny                          ; Mark all 4 bytes as $FF
  sta (ZP_DIRRecordPTR),y      ; Mark new cluster as end of chain.
  lda #$01                     ; Set FAT Dirty flag.
  sta FATDirty                 ; Set FAT Dirty flag.
  lda #$FF                     ; Check if High byte of Cluster Temp is $FF
  cmp ClusterTemp              ; If high byte is $FF then this is the first cluster and we dont need to link it
  bne LinkClusterChain         ; If it is not $FF then we go and link clustertemp (previous cluster) to clusternum
  rts



LinkClusterChain:
  lda #$01                     ; Set FAT Dirty flag.
  sta FATDirty                 ; Set FAT Dirty flag.
  lda ClusterTemp+3            ; Get second byte of Cluster num
  rol                          ; rotate upper bit to carry
  lda ClusterTemp+2            ; get next byte
  rol                          ; rotate carry into lower bit.
  sta RequestedFATSector       ; requested fat sector is Cluster num / 128
  jsr RequestFATSector         ; go load correct page of FAT
  lda #$00                     ; Zero out offset into FAT
  sta ZP_DIRRecordPTR          ; Zero out offset into FAT
  sta ZP_DIRRecordPTR+1        ; Zero out offset into FAT
  lda ClusterTemp+3            ; Get LSB of old cluster number
  and #$7f
  sta ZP_DIRRecordPTR          ; Store LSB of old cluster number to the pointer
  clc                          ; Clear carry before rotate
  rol ZP_DIRRecordPTR          ; Mul*2
  rol ZP_DIRRecordPTR+1        ; Deal with overflow of mul
  rol ZP_DIRRecordPTR          ; Mul*2 again (4 bytes per record in the FAT)
  rol ZP_DIRRecordPTR+1        ; Deal with overflow of mul
  clc                          ; Clear carry before add
  lda ZP_DIRRecordPTR+1        ; Add $0500 to offset into FAT.
  adc #$05                     ; add $0500 to offset into FAT.
  sta ZP_DIRRecordPTR+1        ; add $0500 to offset into FAT.
  lda #$00                     ; add $0500 to offset into FAT.
  adc ZP_DIRRecordPTR          ; add $0500 to offset into FAT.
  sta ZP_DIRRecordPTR          ; add $0500 to offset into FAT.
  ldy #$00                     ; Set byte index to zero
  lda ClusterNum+3             ; Get LSB of new cluster
  sta (ZP_DIRRecordPTR),y      ; Store to record of old cluster
  iny                          ; Increment byte index
  lda ClusterNum+2             ; Get next byte of new cluster
  sta (ZP_DIRRecordPTR),y      ; Store to record of new cluster
  iny                          ; Increment byte index
  lda ClusterNum+1             ; Get next byte of new cluster
  sta (ZP_DIRRecordPTR),y      ; Store to record of new cluster
  iny                          ; Increment byte index
  lda ClusterNum               ; Get next byte of new cluster
  sta (ZP_DIRRecordPTR),y      ; Store to record of new cluster
  lda #$01                     ; Set FAT Dirty flag.
  sta FATDirty                 ; Set FAT Dirty flag.
  rts



UpdateDir:
  lda RootDirCluster           ; Get first directory cluster
  sta ClusterNum               ; Setup to read first directory cluster
  lda RootDirCluster+1         ; Get first directory cluster
  sta ClusterNum+1             ; Setup to read first directory cluster
  lda RootDirCluster+2         ; Get first directory cluster
  sta ClusterNum+2             ; Setup to read first directory cluster
  lda RootDirCluster+3         ; Get first directory cluster
  sta ClusterNum+3             ; Setup to read first directory cluster
  jsr LBA_Addr                 ; Convert cluster number to sector number
  lda #$00                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR          ; Setup to read sector to $0300
  lda #$03                     ; Setup to read sector to $0300
  sta ZP_SectorBufPTR+1        ; Setup to read sector to $0300
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Setup to read $200 bytes
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Setup to read $200 bytes
  jsr SD_Card_Read_Sector      ; Read the DIR sector to $0300
  lda #$00                     ; Setup pointer to DIR $0300
  sta ZP_DIRRecordPTR          ; DIRRecordPTR points to buffer to search
  lda #$03                     ; Setup pointer to DIR $0300
  sta ZP_DIRRecordPTR+1        ; DIRRecordPTR points to buffer to search
.SearchDirFree                 ; Search the directory for a free entry
  ldy #$00                     ; Clear the index
  lda (ZP_DIRRecordPTR),y      ; Load the first byte of the directory entry
  cmp #$00                     ; Check if its zero
  beq .FoundFreeDirEntry       ; If zero hen its free
  cmp #$E5                     ; check if its $E5 as thsat also marks as free
  beq .FoundFreeDirEntry       ; If $E5 then its free
.SearchNextRecord              ; Move up 32 bytes to the next record
  clc                          ; Clear the carry before we add.
  lda ZP_DIRRecordPTR          ; Get pointer to Directory record
  adc #$20                     ; Add $20 to increment Pointer to next record.
  sta ZP_DIRRecordPTR          ; Store new directory record pointer low byte
  lda ZP_DIRRecordPTR+1        ; Get high byte of pointer
  adc #$00                     ; Add zero to deal with overflow
  sta ZP_DIRRecordPTR+1        ; Store new directory record pointer high byte
  jmp .SearchDirFree           ; Go back and search more.
.FoundFreeDirEntry             ; Found a free directory entry, lets update it.
  ldy #$00                     ; Index to bytes of file name
.UpdateAnotherByte             ; Update the file name one byte at a time
  lda FileName,y               ; load char at Filename+Y
  sta (ZP_DIRRecordPTR),y      ; Store it to Buffer+Y
  iny                          ; Increment to next byte
  cpy #$0B                     ; 11 bytes
  bne .UpdateAnotherByte       ; Update all 11 bytes.
  ldy #$0B                     ; Set index to $0B (File Attribute byte)
  lda #$00                     ; Attribute byte will be zero
  sta (ZP_DIRRecordPTR),y      ; Write file attribute byte
  ldy #$14                     ; Set index to $14 (Cluster high bytes)
  lda FileFirstCluster+1       ; Get cluster high byte
  sta (ZP_DIRRecordPTR),y      ; Set cluster high byte in buffer
  ldy #$15                     ; Set index to $15 (Cluster high bytes)
  lda FileFirstCluster         ; Get cluster high byte
  sta (ZP_DIRRecordPTR),y      ; Set cluster high byte in buffer
  ldy #$1A                     ; Set index to $1A (Cluster low bytes)
  lda FileFirstCluster+3       ; Get cluster low byte
  sta (ZP_DIRRecordPTR),y      ; Set cluster low byte in buffer
  ldy #$1B                     ; Set index to $1B (Cluster low bytes)
  lda FileFirstCluster+2       ; Get cluster low byte
  sta (ZP_DIRRecordPTR),y      ; Set cluster low byte in buffer
  ldy #$1C                     ; Set index to $1C (file size)
  lda FileSize                 ; get file size
  sta (ZP_DIRRecordPTR),y      ; Set file size in buffer
  ldy #$1D                     ; Set index to $1D (file size)
  lda FileSize+1               ; Get file size
  sta (ZP_DIRRecordPTR),y      ; Set file size in buffer
  ldy #$1E                     ; Set index to $1D (file size)
  lda FileSize+2               ; Get file size
  sta (ZP_DIRRecordPTR),y      ; Set file size in buffer
  ldy #$1F                     ; Set index to $1D (file size)
  lda FileSize+3               ; Get file size
  sta (ZP_DIRRecordPTR),y      ; Set file size in buffer
  lda #$00                     ; Setup to write $0300 back to SDcard
  sta ZP_SectorBufPTR          ; Setup to write $0300 back to SDcard
  lda #$03                     ; Setup to write $0300 back to SDcard
  sta ZP_SectorBufPTR+1        ; Setup to write $0300 back to SDcard
  lda #$00                     ; Setup to write $200 bytes
  sta ZP_SectorSize            ; Setup to write $200 bytes
  lda #$02                     ; Setup to write $200 bytes
  sta ZP_SectorSize+1          ; Setup to write $200 bytes
  jsr SD_Card_Write_Sector     ; Write Directory sector back to SDcard
  rts



LoadFAT:                       ; Just load zero-th sector of the FAT to $0500
  lda FAT1BeginLBA             ; Copy FAT begin to current sector
  sta CurrentSector            ; Copy FAT begin to current sector
  lda FAT1BeginLBA+1           ; Copy FAT begin to current sector
  sta CurrentSector+1          ; Copy FAT begin to current sector
  lda FAT1BeginLBA+2           ; Copy FAT begin to current sector
  sta CurrentSector+2          ; Copy FAT begin to current sector
  lda FAT1BeginLBA+3           ; Copy FAT begin to current sector
  sta CurrentSector+3          ; Copy FAT begin to current sector
  lda #$00                     ; Setup to read FAT to $0500
  sta ZP_SectorBufPTR          ; Setup to read FAT to $0500
  lda #$05                     ; Setup to read FAT to $0500
  sta ZP_SectorBufPTR+1        ; Setup to read FAT to $0500
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Setup to read $200 bytes
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Setup to read $200 bytes
  jsr SD_Card_Read_Sector      ; Read FAT to $0500
  lda #$00                     ; current FAT Sector is 00
  sta CurrentFATSector         ; current FAT Sector is 00
  lda #$00                     ; Clear FAT Dirty flag.
  sta FATDirty                 ; Clear FAT Dirty flag.
  rts



WriteFAT:                      ; Writes current secotr of FAT back to disk
  lda FAT1BeginLBA             ; Copy FAT begin to current sector
  sta CurrentSector            ; Copy FAT begin to current sector
  lda FAT1BeginLBA+1           ; Copy FAT begin to current sector
  sta CurrentSector+1          ; Copy FAT begin to current sector
  lda FAT1BeginLBA+2           ; Copy FAT begin to current sector
  sta CurrentSector+2          ; Copy FAT begin to current sector
  lda FAT1BeginLBA+3           ; Copy FAT begin to current sector
  sta CurrentSector+3          ; Copy FAT begin to current sector
  clc                          ; Clear carry before add.
  lda CurrentSector+3          ; Add current FAT sector to FAT1Begin.
  adc CurrentFATSector         ; Add current FAT sector to FAT1Begin.
  sta CurrentSector+3          ; Add current FAT sector to FAT1Begin.
  lda CurrentSector+2          ; Add current FAT sector to FAT1Begin.
  adc #$00                     ; Add current FAT sector to FAT1Begin.
  sta CurrentSector+2          ; Add current FAT sector to FAT1Begin.
  lda CurrentSector+1          ; Add current FAT sector to FAT1Begin.
  adc #$00                     ; Add current FAT sector to FAT1Begin.
  sta CurrentSector+1          ; Add current FAT sector to FAT1Begin.
  lda CurrentSector            ; Add current FAT sector to FAT1Begin.
  adc #$00                     ; Add current FAT sector to FAT1Begin.
  sta CurrentSector            ; Add current FAT sector to FAT1Begin.
  lda #$00                     ; Setup to write FAT from $0500
  sta ZP_SectorBufPTR          ; Setup to write FAT from $0500
  lda #$05                     ; Setup to write FAT from $0500
  sta ZP_SectorBufPTR+1        ; Setup to write FAT from $0500
  lda #$00                     ; Setup to write $200 bytes
  sta ZP_SectorSize            ; Setup to write $200 bytes
  lda #$02                     ; Setup to write $200 bytes
  sta ZP_SectorSize+1          ; Setup to write $200 bytes
  jsr SD_Card_Write_Sector     ; Write FAT1 from $0500
  clc                          ; clear carry before add.
  lda CurrentSector            ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize                  ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector            ; Adding size of FAT to current sector to get to FAT2Begin
  lda CurrentSector+1          ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize+1                ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector+1          ; Adding size of FAT to current sector to get to FAT2Begin
  lda CurrentSector+2          ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize+2                ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector+2          ; Adding size of FAT to current sector to get to FAT2Begin
  lda CurrentSector+3          ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize+3                ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector+3          ; Adding size of FAT to current sector to get to FAT2Begin
  lda #$00                     ; Setup to write FAT from $0500
  sta ZP_SectorBufPTR          ; Setup to write FAT from $0500
  lda #$05                     ; Setup to write FAT from $0500
  sta ZP_SectorBufPTR+1        ; Setup to write FAT from $0500
  lda #$00                     ; Setup to write $200 bytes
  sta ZP_SectorSize            ; Setup to write $200 bytes
  lda #$02                     ; Setup to write $200 bytes
  sta ZP_SectorSize+1          ; Setup to write $200 bytes
  jsr SD_Card_Write_Sector     ; Write FAT2 from $0500
  rts



RequestFATSector:               ; Loads a requested Sector of the FAT
  lda RequestedFATSector        ; Which sector is requested?
  cmp CurrentFATSector          ; Check against current sector
  bne .CheckDirtyFlag           ; Load requested sector.
  jmp .LoadFATDone              ; If requested FAT sector is loaded then we are done.
.CheckDirtyFlag                 ; Check if we need to flush before load.
  lda #$00                      ; $00 is a clean flag
  cmp FATDirty                  ; Check value of FATDirty
  bne .FlushCurrentFATSector    ; FAT Dirty, so flush old FAT
  jmp .LoadRequestedFATSector   ; FAT Clean, so go read new FAT
.FlushCurrentFATSector          ; Writes buffer at $0500 back to disk
  lda FAT1BeginLBA              ; Copy FAT begin to current sector
  sta CurrentSector             ; Copy FAT begin to current sector
  lda FAT1BeginLBA+1            ; Copy FAT begin to current sector
  sta CurrentSector+1           ; Copy FAT begin to current sector
  lda FAT1BeginLBA+2            ; Copy FAT begin to current sector
  sta CurrentSector+2           ; Copy FAT begin to current sector
  lda FAT1BeginLBA+3            ; Copy FAT begin to current sector
  sta CurrentSector+3           ; Copy FAT begin to current sector
  clc                           ; Clear carry before add.
  lda CurrentSector+3           ; Add current FAT sector to FAT1Begin.
  adc CurrentFATSector          ; Add current FAT sector to FAT1Begin.
  sta CurrentSector+3           ; Add current FAT sector to FAT1Begin.
  lda CurrentSector+2           ; Add current FAT sector to FAT1Begin.
  adc #$00                      ; Add current FAT sector to FAT1Begin.
  sta CurrentSector+2           ; Add current FAT sector to FAT1Begin.
  lda CurrentSector+1           ; Add current FAT sector to FAT1Begin.
  adc #$00                      ; Add current FAT sector to FAT1Begin.
  sta CurrentSector+1           ; Add current FAT sector to FAT1Begin.
  lda CurrentSector             ; Add current FAT sector to FAT1Begin.
  adc #$00                      ; Add current FAT sector to FAT1Begin.
  sta CurrentSector             ; Add current FAT sector to FAT1Begin.
  lda #$00                      ; Setup to write FATsector from $0500
  sta ZP_SectorBufPTR           ; Setup to write FATsector from $0500
  lda #$05                      ; Setup to write FATsector from $0500
  sta ZP_SectorBufPTR+1         ; Setup to write FATsector from $0500
  lda #$00                      ; Setup to write $200 bytes
  sta ZP_SectorSize             ; Setup to write $200 bytes
  lda #$02                      ; Setup to write $200 bytes
  sta ZP_SectorSize+1           ; Setup to write $200 bytes
  jsr SD_Card_Write_Sector      ; write the FATsector from $0500
  clc                           ; clear carry before add.
  lda CurrentSector+3           ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize+3                 ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector+3           ; Adding size of FAT to current sector to get to FAT2Begin
  lda CurrentSector+2           ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize+2                 ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector+2           ; Adding size of FAT to current sector to get to FAT2Begin
  lda CurrentSector+1           ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize+1                 ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector+1           ; Adding size of FAT to current sector to get to FAT2Begin
  lda CurrentSector             ; Adding size of FAT to current sector to get to FAT2Begin
  adc FATsize                   ; Adding size of FAT to current sector to get to FAT2Begin
  sta CurrentSector             ; Adding size of FAT to current sector to get to FAT2Begin
  lda #$00                      ; Setup to write FAT from $0500
  sta ZP_SectorBufPTR           ; Setup to write FAT from $0500
  lda #$05                      ; Setup to write FAT from $0500
  sta ZP_SectorBufPTR+1         ; Setup to write FAT from $0500
  lda #$00                      ; Setup to write $200 bytes
  sta ZP_SectorSize             ; Setup to write $200 bytes
  lda #$02                      ; Setup to write $200 bytes
  sta ZP_SectorSize+1           ; Setup to write $200 bytes
  jsr SD_Card_Write_Sector      ; Write FAT2 from $0500
.LoadRequestedFATSector
  lda FAT1BeginLBA              ; Copy FAT begin to current sector
  sta CurrentSector             ; Copy FAT begin to current sector
  lda FAT1BeginLBA+1            ; Copy FAT begin to current sector
  sta CurrentSector+1           ; Copy FAT begin to current sector
  lda FAT1BeginLBA+2            ; Copy FAT begin to current sector
  sta CurrentSector+2           ; Copy FAT begin to current sector
  lda FAT1BeginLBA+3            ; Copy FAT begin to current sector
  sta CurrentSector+3           ; Copy FAT begin to current sector
  clc                           ; Clear carry before add.
  lda CurrentSector+3           ; Add sector requested to FAT1Begin.
  adc RequestedFATSector        ; Add sector requested to FAT1Begin.
  sta CurrentSector+3           ; Add sector requested to FAT1Begin.
  lda CurrentSector+2           ; Add sector requested to FAT1Begin.
  adc #$00                      ; Add sector requested to FAT1Begin.
  sta CurrentSector+2           ; Add sector requested to FAT1Begin.
  lda CurrentSector+1           ; Add sector requested to FAT1Begin.
  adc #$00                      ; Add sector requested to FAT1Begin.
  sta CurrentSector+1           ; Add sector requested to FAT1Begin.
  lda CurrentSector             ; Add sector requested to FAT1Begin.
  adc #$00                      ; Add sector requested to FAT1Begin.
  sta CurrentSector             ; Add sector requested to FAT1Begin.
  lda #$00                      ; Setup to read FATsector to $0500
  sta ZP_SectorBufPTR           ; Setup to read FATsector to $0500
  lda #$05                      ; Setup to read FATsector to $0500
  sta ZP_SectorBufPTR+1         ; Setup to read FATsector to $0500
  lda #$00                      ; Setup to read $200 bytes
  sta ZP_SectorSize             ; Setup to read $200 bytes
  lda #$02                      ; Setup to read $200 bytes
  sta ZP_SectorSize+1           ; Setup to read $200 bytes
  jsr SD_Card_Read_Sector       ; Read the FATsector to $0500
  lda RequestedFATSector        ; Update CurrentFATSector to match RequestedFATSector
  sta CurrentFATSector          ; Update CurrentFATSector to match RequestedFATSector
  lda #$00                      ; Clear FAT Dirty flag.
  sta FATDirty                  ; Clear FAT Dirty flag.
.LoadFATDone
  rts



InitBlob:
  sta TempByte                 ; If A is $00 then allocate file but dont write, $FF == zero fill file.
.ClearMemory
  lda #$00                     ; setup blob buffer at $0800
  sta ZP_BlobBufPTR            ; setup blob buffer at $0800
  lda #$08                     ; setup blob buffer at $0800
  sta ZP_BlobBufPTR+1          ; setup blob buffer at $0800
  ldy #$00                     ; clear memory from $0800 to $09FF
  ldx #$02                     ; clear memory from $0800 to $09FF
  lda #$00                     ; clear memory with $00
.ClearMemLoop
  sta (ZP_BlobBufPTR),y        ; clear byte of memory
  iny                          ; move to next byte
  bne .ClearMemLoop            ; go around again
  clc                          ; clear carry before add.
  lda ZP_BlobBufPTR+1          ; add one to MSB of buffer address
  adc #$01                     ; add one to MSB of buffer address
  sta ZP_BlobBufPTR+1          ; add one to MSB of buffer address
  lda #$00                     ; reset A to $00 after adding
  dex                          ; move counter to next page and see if we are done.
  bne .ClearMemLoop            ; not done, so go clear more memory
.CreateBlobFile
  jsr FindFile                 ; Check if we have file with this name already
  lda #$02                     ; 02 is File not found, so we are good to save file
  cmp ErrorCode                ; Check if our error code is 02
  beq .GoWriteBlobFile         ; File Not found, so we create it.
  lda ClusterNum               ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster         ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster         ; Keep First Cluster for Blob access
  lda ClusterNum+1             ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster+1       ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster+1       ; Keep First Cluster for Blob access
  lda ClusterNum+2             ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster+2       ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster+2       ; Keep First Cluster for Blob access
  lda ClusterNum+3             ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster+3       ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster+3       ; Keep First Cluster for Blob access
  jmp .CreateBlobFileDone      ; Dont write file, return error
.GoWriteBlobFile
  lda FileSize                 ; Move file size to Bytes to write counter
  sta BytesToWrite             ; Move file size to Bytes to write counter
  lda FileSize+1               ; Move file size to Bytes to write counter
  sta BytesToWrite+1           ; Move file size to Bytes to write counter
  lda FileSize+2               ; Move file size to Bytes to write counter
  sta BytesToWrite+2           ; Move file size to Bytes to write counter
  lda FileSize+3               ; Move file size to Bytes to write counter
  sta BytesToWrite+3           ; Move file size to Bytes to write counter
  jsr LoadFAT                  ; Load a copy of the FAT to $0500
  lda #$FF                     ; Set marker to show we have no previous cluster.
  sta ClusterNum               ; Store $FF in MSB of ClusterNum to show we are first in chain.
  jsr FindFreeCluster          ; Find our first free cluster
  jsr LBA_Addr                 ; Get Sector number for the first cluster
  lda ClusterNum               ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster         ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster         ; Keep First Cluster for Blob access
  lda ClusterNum+1             ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster+1       ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster+1       ; Keep First Cluster for Blob access
  lda ClusterNum+2             ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster+2       ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster+2       ; Keep First Cluster for Blob access
  lda ClusterNum+3             ; Get the First cluster number so we can keep it for later
  sta FileFirstCluster+3       ; Keep First Cluster so we have it for the dir entry
  sta BlobFirstCluster+3       ; Keep First Cluster for Blob access
.WriteBlobFileLoop             ; Write the file to SDcard one sector at a time
  lda BytesToWrite+1           ; Calculate number of whole sectors needed
  sta SectorCount              ; Store the number of whole sectors needed
  lda BytesToWrite+2           ; Calculate number of whole sectors needed
  sta SectorCount+1            ; Store the number of whole sectors needed
  lda BytesToWrite+3           ; Calculate number of whole sectors needed
  sta SectorCount+2            ; Store the number of whole sectors needed
  lda #$00
  sta SectorCount+3
  clc
  ror SectorCount+3
  ror SectorCount+2
  ror SectorCount+1
  ror SectorCount
  lda #$00                     ; Setup Address (LSB)
  sta ZP_SectorBufPTR          ; Setup Address (LSB)
  lda #$08                     ; Setup Address (MSB)
  sta ZP_SectorBufPTR+1        ; Setup Address (MSB)
  lda SectorCount+3
  ora SectorCount+2
  ora SectorCount+1
  ora SectorCount
  sta SectorCount
  lda #$00                     ; Check if one or more whole sectors to write
  cmp SectorCount              ; Check number of sectors to load
  beq .WriteBlobLastSector     ; Number of whole sectors is zero so Write last sector
.WriteBlobFileSector           ; Write a whole sector
  lda #$00                     ; Setup Address (LSB)
  sta ZP_SectorBufPTR          ; Setup Address (LSB)
  lda #$08                     ; Setup Address (MSB)
  sta ZP_SectorBufPTR+1        ; Setup Address (MSB)
  lda #$00                     ; Setup to write $200 bytes
  sta ZP_SectorSize            ; Setup to write $200 bytes
  lda #$02                     ; Setup to write $200 bytes
  sta ZP_SectorSize+1          ; Setup to write $200 bytes
  lda #$FF                     ; Check the flag for if we write the sector
  cmp TempByte                 ; TempByte is the flag for empty file or not.
  bne .WriteDone               ; Branch over the sector write
  jsr SD_Card_Write_Sector     ; Write data to SDcard
.WriteDone
  lda BytesToWrite+1           ; Subtract $0200 from file size
  sec                          ; Set carry before subtract
  sbc #$02                     ; We only need to subtract from MSB of filesize
  sta BytesToWrite+1           ; Store FileSize MSB again
  lda BytesToWrite+2
  sbc #$00                     ; subtract 00 to deal with borrow
  sta BytesToWrite+2           ; Store FileSize again
  lda BytesToWrite+3
  sbc #$00                     ; subtract 00 to deal with borrow
  sta BytesToWrite+3           ; Store FileSize again
  lda BytesToWrite+3           ; Check if we have more bytes to write
  ora BytesToWrite+2           ; Check if we have more bytes to write
  ora BytesToWrite+1           ; Check if we have more bytes to write
  ora BytesToWrite             ; Check if we have more bytes to write
  cmp #$00                     ; Check if bytes to write is $00
  beq .CreateBlobCleanup       ; If nothing more to write then do cleanup
  jsr FindNextWriteSector      ; Find the next sector to Write to
  jmp .WriteBlobFileLoop       ; Go around again and write more of the file
.WriteBlobLastSector           ; Write the last sector
  lda #$00                     ; Setup Address (LSB)
  sta ZP_SectorBufPTR          ; Setup Address (LSB)
  lda #$08                     ; Setup Address (MSB)
  sta ZP_SectorBufPTR+1        ; Setup Address (MSB)
  lda #$00                     ; Setup to write $200 bytes
  sta ZP_SectorSize            ; Setup to write $200 bytes
  lda #$02                     ; Setup to write $200 bytes
  sta ZP_SectorSize+1          ; Setup to write $200 bytes
  jsr SD_Card_Write_Sector     ; Write data to SDcard
.CreateBlobCleanup
  jsr WriteFAT                 ; Write the FAT back the the SDcard.
  jsr UpdateDir                ; Add the file to the directory structure.
.CreateBlobFileDone
  rts



Read_Chunk:                    ; read a sector of a file.
  lda #$00                     ; setup blob buffer at $0800
  sta ZP_BlobBufPTR            ; setup blob buffer at $0800
  lda #$08                     ; setup blob buffer at $0800
  sta ZP_BlobBufPTR+1          ; setup blob buffer at $0800
  lda ChunkNum
  sta ChunkTemp                ; store chunk number
  lda ChunkNum+1
  sta ChunkTemp+1              ; store chunk number
  lda BlobFirstCluster         ; Get First Cluster of Blob for searching
  sta ClusterNum               ; Set Cluster Number for start of search
  lda BlobFirstCluster+1       ; Get First Cluster of Blob for searching
  sta ClusterNum+1             ; Set Cluster Number for start of search
  lda BlobFirstCluster+2       ; Get First Cluster of Blob for searching
  sta ClusterNum+2             ; Set Cluster Number for start of search
  lda BlobFirstCluster+3       ; Get First Cluster of Blob for searching
  sta ClusterNum+3             ; Set Cluster Number for start of search
  jsr LBA_Addr                 ; Convert Cluster num to sector num
  lda #$01                     ; Reset Sector index to 01
  sta SectorIndex              ; Reset Sector index to 01
.CountChunks                   ; loop to move through the file sector chain
  lda ChunkTemp                ; loop to move through the file sector chain
  ora ChunkTemp+1
  cmp #$00                     ; loop to move through the file sector chain
  beq .LoadChunk               ; loop to move through the file sector chain
  jsr GetNextReadSector        ; loop to move through the file sector chain
  sec                          ; set carry before subtract
  lda ChunkTemp
  sbc #$01
  sta ChunkTemp
  lda ChunkTemp+1
  sbc #$00
  sta ChunkTemp+1
  jmp .CountChunks             ; loop to move through the file sector chain
.LoadChunk
  lda ZP_BlobBufPTR            ; Setup to read sector to ZP_BlobBufPTR
  sta ZP_SectorBufPTR          ; Setup to read sector to ZP_BlobBufPTR
  lda ZP_BlobBufPTR+1          ; Setup to read sector to ZP_BlobBufPTR
  sta ZP_SectorBufPTR+1        ; Setup to read sector to ZP_BlobBufPTR
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Setup to read $200 bytes
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Setup to read $200 bytes
  jsr SD_Card_Read_Sector      ; Read the sector to ZP_BlobBufPTR
  rts



Write_Chunk:                   ; Write a sector of a file.
  lda #$00                     ; setup blob buffer at $0800
  sta ZP_BlobBufPTR            ; setup blob buffer at $0800
  lda #$08                     ; setup blob buffer at $0800
  sta ZP_BlobBufPTR+1          ; setup blob buffer at $0800
  lda ChunkNum
  sta ChunkTemp                ; store chunk number
  lda ChunkNum+1
  sta ChunkTemp+1              ; store chunk number
  lda BlobFirstCluster         ; Get First Cluster of Blob for searching
  sta ClusterNum               ; Set Cluster Number for start of search
  lda BlobFirstCluster+1       ; Get First Cluster of Blob for searching
  sta ClusterNum+1             ; Set Cluster Number for start of search
  lda BlobFirstCluster+2       ; Get First Cluster of Blob for searching
  sta ClusterNum+2             ; Set Cluster Number for start of search
  lda BlobFirstCluster+3       ; Get First Cluster of Blob for searching
  sta ClusterNum+3             ; Set Cluster Number for start of search
  jsr LBA_Addr                 ; Convert Cluster num to sector num
  lda #$01                     ; Reset Sector index to 01
  sta SectorIndex              ; Reset Sector index to 01
.CountChunks                   ; loop to move through the file sector chain
  lda ChunkTemp                ; loop to move through the file sector chain
  ora ChunkTemp+1
  cmp #$00                     ; loop to move through the file sector chain
  beq .WriteChunk              ; loop to move through the file sector chain
  jsr GetNextReadSector        ; loop to move through the file sector chain
  sec                          ; set carry before subtract
  lda ChunkTemp
  sbc #$01
  sta ChunkTemp
  lda ChunkTemp+1
  sbc #$00
  sta ChunkTemp+1
  jmp .CountChunks             ; loop to move through the file sector chain
.WriteChunk
  lda ZP_BlobBufPTR            ; Setup to read sector to ZP_BlobBufPTR
  sta ZP_SectorBufPTR          ; Setup to read sector to ZP_BlobBufPTR
  lda ZP_BlobBufPTR+1          ; Setup to read sector to ZP_BlobBufPTR
  sta ZP_SectorBufPTR+1        ; Setup to read sector to ZP_BlobBufPTR
  lda #$00                     ; Setup to read $200 bytes
  sta ZP_SectorSize            ; Setup to read $200 bytes
  lda #$02                     ; Setup to read $200 bytes
  sta ZP_SectorSize+1          ; Setup to read $200 bytes
  jsr SD_Card_Write_Sector     ; Read the DIR sector to $2000
  rts



BootStrap:
  jsr CreateFileName           ; Convert FileNumber to FileName
  jsr Init_SD_card             ; Init the SDcard to SPI mode.
  lda #$00                     ; Check for error after SDcard init
  cmp ErrorCode                ; Check for error after SDcard init
  bne JmpMon                   ; If error then return to Monitor ROM
  jsr SD_Card_Mount            ; Read Bootsector and patition table to setup card.
  lda #$00                     ; Check for error
  cmp ErrorCode                ; Check for error
  bne JmpMon                   ; If error then return to Moinitor ROM
  lda #$00                     ; Setup Load Address to $1000
  sta FileAddrPTR              ; Store in Load address pointer (LSB)
  lda #$10                     ; Setup Load Address to $1000
  sta FileAddrPTR+1            ; Store in Load Address pointer (MSB)
  jsr LoadFile                 ; Go and load the file
  lda #$00                     ; Check for error
  cmp ErrorCode                ; Check for error
  bne JmpMon                   ; If error then return to Moinitor ROM
  jmp $1000                    ; Run code at $1000
JmpMon:
  jmp MonitorROM               ; If no file, return to monitor rom.



CreateFileName:
  lda FileNumber               ; Get FileNumber byte
  and #$F0                     ; Mask off and keep top half
  clc                          ; Clear carry before Rotate
  ror                          ; Rotate to get top half move to bottom half
  ror                          ; Rotate to get top half move to bottom half
  ror                          ; Rotate to get top half move to bottom half
  ror                          ; Rotate to get top half move to bottom half
  clc                          ; Clear carry before add
  adc #$30                     ; Add ASCII of Zero
  sta FileName                 ; Store first byte of FileName
  lda FileNumber               ; Get FileNumber
  and #$0F                     ; Mask off and keep bottom half
  clc                          ; Clear carry before add
  adc #$30                     ; Add ASCII of Zero
  sta FileName+1               ; Store next byte of FileName
  lda #$20                     ; FileName gets padding of ASCII space
  sta FileName+2               ; Store next byte of filename
  sta FileName+3               ; Store next byte of filename
  sta FileName+4               ; Store next byte of filename
  sta FileName+5               ; Store next byte of filename
  sta FileName+6               ; Store next byte of filename
  sta FileName+7               ; Store next byte of filename
  lda #$42                     ; Ascii B
  sta FileName+8
  lda #$49                     ; Ascii I
  sta FileName+9
  lda #$4E                     ; Ascii N
  sta FileName+10
  rts



IRQ:
  pha                          ; Push A to the Stack
  txa                          ; Move X to A
  pha                          ; Push A(really X) to the stack
  tya                          ; Move Y to A
  pha                          ; Push A(really X) to the stack
  tsx                          ; Move Stack Pointer to X
  lda $0104,X                  ; Move the Stack copy of Status Flags to A
  and #$10                     ; Mask off just the B flag
  bne BRK                      ; If B is not 0, then run BRK Handling code
  pla                          ; Pop A(really Y) from stack
  tay                          ; Move A to Y
  pla                          ; Pop A(really X) from stack
  tax                          ; Move A to X
  pla                          ; Pop  A
  jmp DoIRQ                    ; Do normal IRQ
BRK:
  lda $0103,X                  ; Get copy of A register from stack
  sta AREG                     ; Store A register to debug memory
  lda $0102,X                  ; Get copy of X register from stack
  sta XREG                     ; Store X register to debug memory
  lda $0101,X                  ; Get copy of Y register from stack
  sta YREG                     ; Store Y register to debug memory
  lda $0104,X                  ; Get copy of Status register from stack
  sta Status                   ; Store Status register to debug memory
  lda $0105,X                  ; Get Program Counter low byte from stack
  sta PC_Low                   ; Store Program Counter to debug memory
  lda $0106,X                  ; Get Program Counter high byte from stack
  sta PC_High                  ; Store Program Counter to debug memory
  lda #$00                     ; setup new return address to point to monitor ROM
  sta $0105,X                  ; put new return low byte on stack
  lda #$FF                     ; setup new return address to point to monitor ROM
  sta $0106,X                  ; put new return high byte on stack
EndBRK:
  pla                          ; Pop A(really Y) from stack
  tay                          ; Move A to Y
  pla                          ; Pop A(really X) from stack
  tax                          ; Move A to X
  pla                          ; Pop  A
  rti                          ; Return to monitro ROM
DoIRQ:
  jmp ($02FE)                  ; Jump to IRQ vector in RAM, default will jump back to IRQ_Exit



Continue:                      ; This resumes execution after a BRK
  lda PC_High                  ; Get Program Count High byte
  pha                          ; Push Program count High byte to stack
  lda PC_Low                   ; Get Program Count Low byte
  pha                          ; Push Program count Low byte to stack
  lda Status                   ; Get CPU status from debug memory
  pha                          ; Push to stack
  lda AREG                     ; Restore value of A register
  ldx XREG                     ; Restore value of X register
  ldy YREG                     ; Restore value of Y register
  rti                          ; Return to execution



IRQ_Exit:
  rti



NMI:
  jmp ($02FC)                  ; MNI vector in RAM, default will jump back to IRQ_Exit



KeypadArray:                   ; Converts keypad row/col to hex digits or control codes
  .byte 0xFF                   ; Null
  .byte 0xFD                   ; ENT
  .byte 0xFC                   ; X
  .byte 0xFB                   ; W
  .byte 0xFA                   ; R

  .byte 0x03                   ; Three
  .byte 0x02                   ; Two
  .byte 0x01                   ; One
  .byte 0x00                   ; Zerp

  .byte 0x07                   ; seven
  .byte 0x06                   ; six
  .byte 0x05                   ; five
  .byte 0x04                   ; four

  .byte 0x0B                   ; 0x0B
  .byte 0x0A                   ; 0x0A
  .byte 0x09                   ; nine
  .byte 0x08                   ; eight

  .byte 0x0F                   ; 0x0F
  .byte 0x0E                   ; 0x0E
  .byte 0x0D                   ; 0x0D
  .byte 0x0C                   ; 0x0C

Array_7seg:                    ; Which bits are needed for 7 segment display
  .byte 0b10111111             ; zero
  .byte 0b10000110             ; one
  .byte 0b11011011             ; two
  .byte 0b11001111             ; three
  .byte 0b11100110             ; four
  .byte 0b11101101             ; five
  .byte 0b11111101             ; six
  .byte 0b10000111             ; seven
  .byte 0b11111111             ; eight
  .byte 0b11101111             ; nine
  .byte 0b11110111             ; A
  .byte 0b11111100             ; B
  .byte 0b10111001             ; C
  .byte 0b11011110             ; D
  .byte 0b11111001             ; E
  .byte 0b11110001             ; F





 .org $FE00                    ; A jump table so that ROM calls can be at fixed address across ROM versions

 .org $FEF4
  jmp Read_Chunk               ; Setup blob file.
 .org $FEF8
  jmp Write_Chunk              ; Setup blob file.
 .org $FEFC
  jmp InitBlob                 ; Setup blob file.

 .org $FF00
  jmp MonitorROM               ; Main Monitor ROM
 .org $ff10
  jmp UpdateDisplay            ; Update the 7segmanet display
 .org $ff14
  jmp ReadKeypad               ; Reads the keypad, with debounce
 .org $ff18
  jmp ScanKeypad               ; Checks for key press, no debounce

 .org $ff20
  jmp GetRandomByte            ; Random byte returned in A

 .org $ff30
  jmp Sleep_Long               ; Sleep about 1 second
 .org $ff34
  jmp Sleep_Short              ; Sleep about 100 milliseconds

 .org $ff40
  jmp SPI_Write_Byte           ; Writes the byte in A register to SPI bus.
 .org $ff44
  jmp SPI_Read_Byte            ; Reads a byte in A register from SPI bus.

 .org $ff50
  jmp SPI_Select_SDcard        ; Changes CS_ to low for CDcard (Active low)
 .org $ff54
  jmp SPI_Unselect_SDcard      ; Changes CS_ to high for SDcard (Active low)
 .org $ff58
  jmp SPI_Select_7seg          ; Changes CS_ to low for 7 segment display (Active low)
 .org $ff5C
  jmp SPI_Unselect_7seg        ; Chanegs CS_ to high for 7 segment display (Active low)

 .org $ff60
  jmp Init_SD_card             ; Init the SDcard to SPI mode
 .org $ff64
  jmp SD_Card_Mount            ; Reads Bootsector and patition table to setup card.
 .org $ff68
  jmp SD_Card_Read_Sector      ; Reads a sector from the SDcard
 .org $ff6C
  jmp SD_Card_Write_Sector     ; Writes a sector to SDcard
 .org $ff70
  jmp CreateFileName           ; Convert a filenumber to a filename
 .org $ff74
  jmp FindFile                 ; Find a File in directory
 .org $ff78
  jmp LoadFile                 ; Loads file to memory
 .org $ff7C
  jmp LBA_Addr                 ; Calculates LBA sector from cluster number.
 .org $ff80
  jmp GetNextReadSector        ; Increments sector number, goes to FAT for next cluster if needed
 .org $ff84
  jmp GetNextReadCluster       ; Find next cluster from FAT
 .org $ff88
  jmp WriteFile                ; Find next cluster from FAT
 .org $ff8C
  jmp UpdateDir                ; Update the Directory with FileName,FileSize,FirstCluster

 .org $FF90
  jmp BootStrap                ; LEGACY - Load file and run it.

 .org $FF94
  jmp WriteFAT                 ; Write Current FAT to disk
 .org $FF98
  jmp FindNextWriteSector      ; Finds the next free Sector on disk
 .org $FF9C
  jmp FindFreeCluster          ; Finds the next free Cluster on disk
 .org $FFA0
  jmp LoadFAT                  ; Write Current FAT to disk

 .org $FFAA
  jmp Continue                 ; Resume execution from after BRK

 .org $FFFA
 .word NMI
 .word Reset
 .word IRQ
