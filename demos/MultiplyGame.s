; This game is a creation of Derek & Miro.


GetRandomByte = $FF20  ; This makes it nice and obvious that we want a random number
ReadKeyPad = $FF14     ; This is so we can read the keypad
UpdateDisplay = $FF10  ; Udate the display
Sleep_Long = $FF30     ; Sleep for ~0.5s
Temp = $40             ; Temporary location for keyboard reading stuff
InputPosition = $41    ; Which digit we're writing


  .org $1000

Game:
  jsr WriteProblem
  lda #$01
  sta InputPosition
  lda #$00
  sta Temp
  jsr WrightGuess
  jmp Game


WrightGuess:
  jsr ReadKeyPad
  cmp #$FF
  beq WrightGuess
  cmp #$FD
  beq CheckGuess
  sta Temp

  lda InputPosition
  cmp #$01
  beq LeftDigit

RightDigit:
  inc InputPosition
  lda $D0
  and #$F0
  sta $D0
  lda Temp
  ora $D0
  sta $D0
  jsr UpdateDisplay
  jmp WrightGuess

LeftDigit:
  dec InputPosition
  lda $D0
  and #$0F
  sta $D0
  lda Temp
  asl
  asl
  asl
  asl
  ora $D0
  sta $D0
  jsr UpdateDisplay
  jmp WrightGuess




CheckGuess:
  lda $D0
  cmp $62
  beq Correct
  jmp Wrong

Correct:
  lda #$7F
  sta $D5
  lda #$85
  sta $D1
  lda #$08
  sta $D2
  lda #$00
  sta $D0
  sta $D3
  sta $D4
  sta $D8
  lda #$6E
  sta $D7
  lda #$79
  sta $D6

  jsr UpdateDisplay

  jsr Sleep_Long
  jsr Sleep_Long
  jsr Sleep_Long
  jsr Sleep_Long

  lda #$7F
  sta $D3
  sta $D4
  sta $D5
  sta $D6
  sta $D7
  sta $D8
  rts


Wrong:
  lda #$7F
  sta $D5
  lda #$00
  sta $D0
  sta $D2
  sta $D3
  sta $D4
  sta $D7
  sta $D8
  lda #$80
  sta $D1
  lda #$37
  sta $D6

  jsr UpdateDisplay

  jsr Sleep_Long
  jsr Sleep_Long
  jsr Sleep_Long
  jsr Sleep_Long

  lda #$7F
  sta $D3
  sta $D4
  sta $D5
  sta $D6
  sta $D7
  sta $D8
  rts
  

WriteProblem:
  jsr GetNumber  ; Get a number and put it in $60
  lda $60        ; Store that number in accumulator
  sta $61        ; And put it in $61
  sta $51        ; Also put it in $51
  jsr GetNumber  ; Get another number wiping out what was in $60
  lda $60        ; Putting that new number in accumulator
  sta $50        ; Releasing it in $50
  jsr Multiply   ; Multiplying the random numbers
  lda $52        ; Placing the product in accumulator
  sta $62        ; Now we have the factors in $60 and $61 and the product in $62

  lda $60    ; Load accumulator with our second random number
  asl        ; The 4 "asl"s are to make the digit the one on the left
  asl
  asl
  asl
  sta $D2    ; Put our shifted number into D2
  lda #$00   ; Load accumulator with 0
  sta $D7    ; Clear the 7 segment display second furthest to the left
  lda #$00   ; Load accumulator with 0
  sta $D5    ; Clear the 7 segment display second furthest to the left

  lda $61    ; Load accumulator with our first random number
  asl        ; The 4 "asl"s are to make the digit the one on the left
  asl
  asl
  asl
  sta $D1    ; Put our shifted number into D1
  jsr UpdateDisplay  ; Make the screen show something, not just think it
  rts                ; We're done



Multiply:        ; Multiply $50 by $51 and putting it in $52
  lda #00        ; Empty accumulator
  sed            ; Now we're in decimal mode
  clc            ; Clearing the carry because we use it soon
Add:             ; The addition loop
  adc $51        ; Adding second factor to the accumulator
  dec $50        ; Decrementing the first factor
  bne Add        ; Return to Add until the first factor is 0
  sta $52        ; Showing us the result
  cld            ; To prevent a bunch of problems
  rts            ; This sub routine is done


GetNumber:            ; Gets random number 1-9
  jsr GetRandomByte   ; Get a random byte
  sta $60             ; Store accumulator in $60
  sec
  sbc #9              ; Subtract 9
  bcc Good            ; When 8 or below, branch to Good
  jmp GetNumber       ; If above 9, try again
  
Good:
  inc $60             ; You could end up with 0s, so we increment. We also can have 9s.
  rts                 ; We're done with this sub routine
