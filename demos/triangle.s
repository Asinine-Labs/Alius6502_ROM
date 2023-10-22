; This demo draws a Sierpinski triangle using Chaos game.

FileAddrPTR = $0260           ; Pointer to file in memory, $0260(LSB),$0261(MSB)
FileSize = $025D              ; File Size, 2 bytes including $025D(LSB) ,$025E(MSB)
FileNumber = $0250            ; File Number to create FileName
FileName = $0251              ; File Name to save


WriteFile = $FF88             ; File Write in ROM
CreateFileName = $ff70        ; Create file name in ROM
GetRandomByte = $FF20         ; Random number in ROM

ImgBuf = $70                  ; Pointer to image buffer $70,$71 

X1 = $10                      ; First corner of triangle in zeropage.
Y1 = $11                      ; First corner of triangle in zeropage.
X2 = $12                      ; Second corner of triangle in zeropage.
Y2 = $13                      ; Second corner of triangle in zeropage.
X3 = $14                      ; Third corner of triangle in zeropage.
Y3 = $15                      ; Third corner of triangle in zeropage.
CurX = $16                    ; Current corner of triangle
CurY = $17                    ; Current corner of triangle

Counter16=$18                 ; 16 bit counter in Zeropage $18,$19
TempX16=$1A                   ; 16 bit, used for math $1A,$1B
TempY16=$1C                   ; 16 bit, used for math $1C,$1D

  .org $1000

  lda #$00                    ; Setup image buffer at $2000
  sta ImgBuf                  ; Setup image buffer at $2000
  lda #$20                    ; Setup image buffer at $2000
  sta ImgBuf+1                ; Setup image buffer at $2000

; Let's clear a few pages of ram, from $2000 to $7000
  ldy #$00                    ; Zero the Y index
  ldx #$50                    ; X hold how many pages to clear
ClearBufLoop:
  lda #$00                    ; load A with zero
  sta (ImgBuf),y              ; Store A to memory 
  iny                         ; Increment the index 
  bne ClearBufLoop            ; Branch if not done whole page.
  clc                         ; Clear Carry brfore add 
  lda ImgBuf+1                ; Load buffer pointer.
  adc #$01                    ; add one.
  sta ImgBuf+1                ; Store buffer pointer again. 
  dex                         ; Decrement X, to do X pages 
  bne ClearBufLoop            ; Branch if we have not done all pages.

BMPheader:                    ; Write a bitmap header to RAM
  lda #$42                    ; B
  sta $2000
  lda #$4D                    ; M
  sta $2001

  lda #$20                    ; FileSize
  sta $2002
  lda #$20
  sta $2003
  lda #$00
  sta $2004
  lda #$00
  sta $2005

  lda #$00                    ; Reserved
  sta $2006
  lda #$00
  sta $2007
  lda #$00
  sta $2008
  lda #$00
  sta $2009

  lda #$20                    ; offset to image data
  sta $200A
  lda #$00
  sta $200B
  lda #$00
  sta $200C
  lda #$00
  sta $200D

  lda #$0C                    ; DIB header size
  sta $200E
  lda #$00
  sta $200F
  lda #$00
  sta $2010
  lda #$00
  sta $2011

  lda #$FF                    ; DIB header image width
  sta $2012
  lda #$00
  sta $2013

  lda #$FF                    ; DIB header image height
  sta $2014
  lda #$00
  sta $2015

  lda #$01                    ; DIB header color planes
  sta $2016
  lda #$00
  sta $2017

  lda #$01                    ; DIB header bit per pixel
  sta $2018
  lda #$00
  sta $2019

  lda #$00                    ; color table 
  sta $201A
  lda #$00
  sta $201B
  lda #$00
  sta $201C
  lda #$ff                    ; color table 
  sta $201D
  lda #$ff
  sta $201E
  lda #$ff
  sta $201F



DrawImage:
  lda #$00                    ; Setup first corner
  sta X1                      ; Setup first corner
  lda #$00                    ; Setup first corner
  sta Y1                      ; Setup first corner

  lda #$FF                    ; Setup second corner
  sta X2                      ; Setup second corner
  lda #$00                    ; Setup second corner
  sta Y2                      ; Setup second corner

  lda #$80                    ; Setup third corner
  sta X3                      ; Setup third corner
  lda #$FF                    ; Setup third corner
  sta Y3                      ; Setup third corner

  lda #$00                    ; Clear current X
  sta CurX                    ; Clear current X
  lda #$00                    ; Clear current Y
  sta CurY                    ; Clear current Y

  lda #$00                    ; Zero out our loop counter
  sta Counter16               ; Zero out our loop counter
  sta Counter16+1             ; Zero out our loop counter
TriangleLoop:
  jsr GetRandomByte           ; Get a rondom byte
  and #$03                    ; Reduce to 0->3 range
  cmp #$00                    ; If random number is zero then try again
  beq TriangleLoop            ; Get another random number
  cmp #$01                    ; Check which corner was selected
  beq Corner1                 ; Check which corner was selected
  cmp #$02                    ; Check which corner was selected
  beq Corner2                 ; Check which corner was selected
  cmp #$03                    ; Check which corner was selected
  beq Corner3                 ; Check which corner was selected
  jmp TriangleLoop            ; This should never happen, get another random number

Corner1:
  lda X1                      ; X1 into Temp16
  sta TempX16                 ; X1 into Temp16
  lda #$00                    ; X1 into Temp16
  sta TempX16+1               ; X1 into Temp16
  lda Y1                      ; Y1 into Temp16
  sta TempY16                 ; Y1 into Temp16
  lda #$00                    ; Y1 into Temp16
  sta TempY16+1               ; Y1 into Temp16
  jmp Math

Corner2:
  lda X2                      ; X2 into Temp16
  sta TempX16                 ; X2 into Temp16
  lda #$00                    ; X2 into Temp16
  sta TempX16+1               ; X2 into Temp16
  lda Y2                      ; Y2 into Temp16
  sta TempY16                 ; Y2 into Temp16
  lda #$00                    ; Y2 into Temp16
  sta TempY16+1               ; Y2 into Temp16
  jmp Math

Corner3:
  lda X3                      ; X3 into Temp16
  sta TempX16                 ; X3 into Temp16
  lda #$00                    ; X3 into Temp16
  sta TempX16+1               ; X3 into Temp16
  lda Y3                      ; Y3 into Temp16
  sta TempY16                 ; Y3 into Temp16
  lda #$00                    ; Y3 into Temp16
  sta TempY16+1               ; Y3 into Temp16
  jmp Math


Math:
  lda Counter16               ; Display counter
  sta $D0                     ; Display counter
  lda Counter16+1             ; Display counter
  sta $D1                     ; Display counter
  lda #$00                    ; Display counter
  sta $D2                     ; Display counter
  jsr $FF10                   ; Update display

; Do the math on X
  clc                         ; Clear carry before add
  lda TempX16                 ; Add random corner to current corner
  adc CurX                    ; Add random corner to current corner
  sta TempX16                 ; Add random corner to current corner
  lda #$00                    ; Add zero to deal with carry
  adc TempX16+1               ; Add to high byte
  sta TempX16+1               ; Add to high byte
  clc                         ; Clear carry before devide
  ror TempX16+1               ; Devide by 2 (MSB)
  ror TempX16                 ; Devide by 2 (LSB)
  lda TempX16                 ; Move TempX to Current X      
  sta CurX                    ; Move TempX to Current X

; Do the math on Y
  clc                         ; Clear carry before add
  lda TempY16                 ; Add random corner to current corner
  adc CurY                    ; Add random corner to current corner
  sta TempY16                 ; Add random corner to current corner
  lda #$00                    ; Add zero to deal with carry
  adc TempY16+1               ; Add to high byte
  sta TempY16+1               ; Add to high byte
  clc                         ; Clear carry before devide
  ror TempY16+1               ; Devide by 2 (MSB)
  ror TempY16                 ; Devide by 2 (LSB)
  lda TempY16                 ; Move TempY to Current Y
  sta CurY                    ; Move TempY to Current Y

; Draw the pixel
  ldx CurX                    ; Setup for draw pixel
  ldy CurY                    ; Setup for draw pixel
  jsr PutPixel                ; Go draw pixel

  clc                         ; Clear carry before add
  lda Counter16               ; Load counter (LSB)
  adc #$01                    ; Add one to the counter (LSB)
  sta Counter16               ; Add one to the counter (LSB)
  lda Counter16+1             ; Load counter (MSD)
  adc #$00                    ; Add zero to deal with carry
  sta Counter16+1             ; Add zero to deal with carry

  lda #$FF                    ; Check counter to see if we are done.
  cmp Counter16+1             ; Check counter to see if we are done.
  beq SaveFile                ; If we are done then save file
  jmp TriangleLoop            ; If not done then go do more.



SaveFile:                     ; Writes file to disk
  lda #$00                    ; Set file pointer to $2000
  sta FileAddrPTR             ; Set file pointer to $2000
  lda #$20                    ; Set file pointer to $2000
  sta FileAddrPTR+1           ; Set file pointer to $2000
  lda #$20                    ; Set file size to $2020
  sta FileSize                ; Set file size to $2020 
  lda #$20                    ; Set file size to $2020
  sta FileSize+1              ; Set file size to $2020
; "triangle.bmp" = 74 72 69 61 6E 67 6C 65 62 6D 70
  lda #$74
  sta FileName 
  lda #$72
  sta FileName+1
  lda #$69
  sta FileName+2
  lda #$61
  sta FileName+3
  lda #$6E
  sta FileName+4
  lda #$67
  sta FileName+5
  lda #$6C
  sta FileName+6
  lda #$65
  sta FileName+7
  lda #$62
  sta FileName+8
  lda #$6D
  sta FileName+9
  lda #$70
  sta FileName+10
  jsr WriteFile               ; Write the file
  brk                         ; All done, return to monitor ROM


PutPixel:
  sty $00                     ; Store Y coordnate
  stx $01                     ; Store X coordnate
; Calculate pointer address ($02,$03) to be $2020 + (Y * 16) + (X/8)
  lda $00                     ; Load Y 
  sta $02                     ; Set screen address pointer LSB to Y
  lda #$00                    ; Set screen pointer MSB to zero
  sta $03                     ; Set screen pointer MSB to zero
  asl $02                     ; Times 2 (LSB)
  rol $03                     ; Times 2 (MSB)
  asl $02                     ; Times 4 (LSB)
  rol $03                     ; Times 4 (MSB)
  asl $02                     ; Times 8 (LSB)
  rol $03                     ; Times 8 (MSB)
  asl $02                     ; Times 16 (LSB)
  rol $03                     ; Times 16 (MSB)
  asl $02                     ; Times 32 (LSB)
  rol $03                     ; Times 32 (MSB)

  lda $01                     ; Load X
  lsr                         ; devide by 2
  lsr                         ; devide by 4
  lsr                         ; devide by 8
  clc                         ; Clear carry before add
  adc $02                     ; Add X/8 to LSB of Y*16
  sta $02                     ; Store LSB
  lda $03                     ; Add zero to MSB
  adc #$00                    ; Add zero to deal with carry
  sta $03                     ; Store MSB 

  clc                         ; Clear carry before add
  lda $02                     ; Add $2020 to pointer
  adc #$20                    ; Add $2020 to pointer
  sta $02                     ; Add $2020 to pointer
  lda $03                     ; load MSB of screen ponter
  adc #$20                    ; add $2020 to pointer
  sta $03
; pointer address is now  $2020 + (Y * 16) + (X/8)

  lda $01                     ; Load X
  and #$07                    ; Keep lower three bits
  tax                         ; Move A to X as and index for loop
  lda #$80                    ; Load A with one pixel, highest bit
PixelShift:                   ; Shift the one bit to right position.
  cpx #$00                    ; Check if we are done
  beq ShiftDone               ; Branch if done with shifting 
  lsr                         ; Do a one bit shift
  dex                         ; Decrement counter
  jmp PixelShift              ; Go do the loop again
ShiftDone:
  sta $05                     ; Store the pixel we need to add to screen   
  ldy #$00                    ; Set index to zero
  lda ($02),y                 ; Load a byte from screen memory.
  ora $05                     ; OR in our one pixel to turn it on
  sta ($02),y                 ; Store our one byte to screen memory.
  rts


