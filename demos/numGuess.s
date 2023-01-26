
Sleep_Long = $FF30
SPI_Unselect_7seg = $FF5C
SPI_Write_Byte = $FF40
SPI_Select_7seg = $FF58
ReadKeypad = $FF14
RandByte = $0200
GetRandomByte = $FF20
RandSeed = $0201

ZP_Temp = $40 ; temp storage
ZP_Guess = $41 
ZP_InputPosition = $42 
ZP_ScreenByte = $43 

TopHalf = $0900
BottomHalf = $9001

 .org $1000

  lda #$01
  sta RandSeed
  sta RandSeed+1

Init:
  lda #$00
  sta $0900
  sta $0901
  jsr Display
WaitInput:
  jsr Display
  jsr GetRandomByte
  jsr ReadKeypad
  cmp #$FF
  beq WaitInput

Game:
  lda #$00
  sta ZP_InputPosition
  lda #$00
  sta ZP_Guess

ReadGuess:
  lda ZP_Guess
  jsr Display_Decode
  jsr Display
  jsr ReadKeypad
  tax  ; store key in X for later
  cmp #$FD  ; check for ENT
  bne SetGuess  ; not ENT, so read guess
  jmp DoGuess ; else was ENT, so check Guess
SetGuess:
  and #$F0 ; mask off bottom half of key press
  cmp #$00 ; check for hex keys
  bne ReadGuess ; not a hex key, then read again.
  ldy ZP_InputPosition
  cpy #$00
  bne SecondPosition
  inc ZP_InputPosition ; move input position along
  lda ZP_Guess
  and #$0F  ; keep bottom half
  sta ZP_Guess
  txa ; get key back from X
  asl 
  asl 
  asl 
  asl 
  ora ZP_Guess
  sta ZP_Guess
  jmp ReadGuess ; get next key
SecondPosition:
  lda #$00 
  sta ZP_InputPosition ; reset input position
  lda ZP_Guess
  and #$F0  ; keep top half
  sta ZP_Guess
  txa
  ora ZP_Guess
  sta ZP_Guess
  jmp ReadGuess ; get next key


DoGuess:    ; check if Guess is good
  lda ZP_Guess
  sec  ; set carry
  sbc RandByte  ; do subtract
  beq Winner
  bcc TooLow
  jmp TooHigh


TooHigh:
  lda #$05
  sta ZP_Temp
  lda #0b00000001
  sta ZP_ScreenByte
HighLoop 
  jsr BlinkScreen
  dec ZP_Temp
  bne HighLoop
  jmp Game


TooLow:
  lda #$05
  sta ZP_Temp
  lda #0b00001000
  sta ZP_ScreenByte
LowLoop 
  jsr BlinkScreen
  dec ZP_Temp
  bne LowLoop
  jmp Game


Winner:
  lda #$10
  sta ZP_Temp
  lda #0b01000000
  sta ZP_ScreenByte
WinLoop 
  jsr BlinkScreen
  dec ZP_Temp
  bne WinLoop
  jmp Init


BlinkScreen:
  jsr SPI_Select_7seg
  lda ZP_ScreenByte
  jsr SPI_Write_Byte  ; send byte
  lda ZP_ScreenByte
  jsr SPI_Write_Byte  ; send byte
  lda ZP_ScreenByte
  jsr SPI_Write_Byte  ; send byte
  lda ZP_ScreenByte
  jsr SPI_Write_Byte  ; send byte
  lda ZP_ScreenByte
  jsr SPI_Write_Byte  ; send byte
  lda ZP_ScreenByte
  jsr SPI_Write_Byte  ; send byte
  jsr SPI_Unselect_7seg
  jsr Sleep_Long  ; sleep about 1 second
  jsr SPI_Select_7seg
  lda #0b00000000
  jsr SPI_Write_Byte  ; send byte
  lda #0b00000000
  jsr SPI_Write_Byte  ; send byte
  lda #0b00000000
  jsr SPI_Write_Byte  ; send byte
  lda #0b00000000
  jsr SPI_Write_Byte  ; send byte
  lda #0b00000000
  jsr SPI_Write_Byte  ; send byte
  lda #0b00000000
  jsr SPI_Write_Byte  ; send byte
  jsr SPI_Unselect_7seg
  jsr Sleep_Long  ; sleep about 1 second
  rts


Display:
  jsr SPI_Select_7seg
  lda #0b00111111
  jsr SPI_Write_Byte  ; send byte
  lda #0b00111000
  jsr SPI_Write_Byte  ; send byte

  lda $0901
  jsr SPI_Write_Byte  ; send byte
  lda $0900
  jsr SPI_Write_Byte  ; send byte

  lda #0b00110000
  jsr SPI_Write_Byte  ; send byte
  lda #0b01110110
  jsr SPI_Write_Byte  ; send byte
  jsr SPI_Unselect_7seg
  rts
 

Display_Decode:
  sta ZP_Temp
  and #$0F
  tax
  lda Array_7seg,x
  sta $0901
  lda ZP_Temp
  and #$F0
  lsr
  lsr
  lsr
  lsr
  tax
  lda Array_7seg,x
  sta $0900
  rts


Array_7seg:
  .byte 0b00111111 ; zero
  .byte 0b00000110 ; one
  .byte 0b01011011 ; two
  .byte 0b01001111 ; three
  .byte 0b01100110 ; four
  .byte 0b01101101 ; five
  .byte 0b01111101 ; six
  .byte 0b00000111 ; seven
  .byte 0b01111111 ; eight
  .byte 0b01101111 ; nine
  .byte 0b01110111 ; A 
  .byte 0b01111100 ; B
  .byte 0b00111001 ; C
  .byte 0b01011110 ; D
  .byte 0b01111001 ; E
  .byte 0b01110001 ; F



