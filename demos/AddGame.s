; This game is a creation of Derek & Miro. It displays 2 numbers that you need to multiply, then you type in your guess of the product. It then writes "YES" or "NO" and loops back to the start.

GetRandomByte = $FF20  ; This makes it nice and obvious that we want a random number
ReadKeyPad = $FF14     ; This is so we can read the keypad
UpdateDisplay = $FF10  ; Udate the display
Sleep_Long = $FF30     ; Sleep for ~0.5s
Temp = $40             ; Temporary location for keyboard reading stuff
InputPosition = $41    ; Which digit we're writing


  .org $1000           ; The traditional starting address

Game:                  ; Our loop full of sub routines
  jsr WriteProblem     ; Display our math problem
  jsr PrepareWriteGuess; Prepares WriteGuess
  jsr WriteGuess       ; This shows what you guessed and tells you wether you're right or wrong
  jmp Game             ; Have another crack!


PrepareWriteGuess:     ; Prepares WriteGuess
  lda #$01             ; Load accumulator with 1
  sta InputPosition    ; Tell our computer that our input position is 1
  lda #$00             ; Load accumulator with 0
  sta Temp             ; This clears Temp
  rts

WriteGuess:            ; Writes your guess
  jsr ReadKeyPad       ; Read the keypad
  cmp #$FF             ; Did you press something?
  beq WriteGuess       ; If you didn't, wait until you do
  cmp #$FD             ; Did you press enter?
  beq CheckGuess       ; If you did, did you get it right?
  sta Temp             ; Store the number you pressed into Temp
  lda InputPosition    ; Load accumulator with the input position
  cmp #$01             ; Compare with 1
  beq LeftDigit        ; If it was a 1, then you're typing the left digit.

RightDigit:            ; This displays the second digit
  inc InputPosition    ; When we type the next digit, it will be on the other side
  lda $D0              ; Load accumulator with what you've typed so far
  and #$F0             ; This cuts of the bottem half leaving a 0
  sta $D0              ; Store this back in D0
  lda Temp             ; Load accumulator with Temp
  ora $D0              ; Squashes what's already in D0 and your input together
  sta $D0              ; Put that back in D0
  jsr UpdateDisplay    ; Displays the button that you pressed
  jmp WriteGuess       ; This might not be your full guess, so we will let you type more digits

LeftDigit:             ; This displays the first digit
  dec InputPosition    ; When we type the next digit, it will be on the other side
  lda $D0              ; Load accumulator with what you've typed so far
  and #$0F             ; This cuts of the top half leaving a 0
  sta $D0              ; Store this back in D0
  lda Temp             ; Load accumulator with Temp
  asl                  ; These are to move it to the side
  asl
  asl
  asl
  ora $D0              ; Squashes what's already in D0 and your input together
  sta $D0              ; Put that back in D0
  jsr UpdateDisplay    ; Displays the button that you pressed
  jmp WriteGuess       ; This might not be your full guess, so we will let you type more digits


CheckGuess:            ; This will decide wether you were right or wrong
  lda $D0              ; Get your guess into accumulator
  cmp $62              ; Compare with the sum
  beq Correct          ; If they were the same, you were correct, so lets let you know
  jmp Wrong            ; If they weren't, tough luck. We also let you know.

Correct:               ; This lets you know if you're correct
  lda #$7F             ; Load accumulator with the magic number
  sta $D5              ; This makes the 7 segment 3rd from the right appear
  lda #$E5             ; Load accumulator with E5 which looks like "ES"
  sta $D1              ; This gives us a full 7 segment display and a free S
  lda #$08             ; Load accumulator with 8
  sta $D2              ; Store that number on the left, giving us a 0 and a full 7 segment display
  lda #$00             ; Load accumulator with 0
  sta $D0              ; Zero the left part of the display
  sta $D3              ; Clear the far right part of the display
  sta $D4              ; Clear the 2nd furthest right 7 segment display
  sta $D8              ; Clear the far left part of the display
  lda #$6E             ; Load accumulator with another one of the magic numbers
  sta $D7              ; This will look kind of like a Y
  jsr UpdateDisplay    ; Show you that you were right
  jsr Sleep_Long       ; Wait a little while
  jsr Sleep_Long
  jsr Sleep_Long
  jsr Sleep_Long
  lda #$7F             ; Load accumulator with the magic number
  sta $D3              ; These will make all the digits reappear
  sta $D4
  sta $D5
  sta $D6
  sta $D7
  sta $D8
  rts                  ; The end of our sub routine

Wrong:                 ; This lets you know if you're wrong
  lda #$7F             ; Load accumulator with the magic number
  sta $D5              ; This makes the 7 segment 3rd from the right appear
  lda #$00             ; Load accumulator with 0
  sta $D0              ; These clear the 4 side 7 segment displays
  sta $D2
  sta $D3
  sta $D4
  sta $D7
  sta $D8
  lda #$80             ; Load accumulator with 80
  sta $D1              ; This gives us a full 7 segment display and a free O
  lda #$37             ; Load accumulator with another one of the magic numbers
  sta $D6              ; This will look kind of like an N
  jsr UpdateDisplay    ; Show you that you were wrong
  jsr Sleep_Long       ; Wait a little while
  jsr Sleep_Long
  jsr Sleep_Long
  jsr Sleep_Long
  lda #$7F             ; Load accumulator with the magic number
  sta $D3              ; These will make all the digits reappear
  sta $D4
  sta $D5
  sta $D6
  sta $D7
  sta $D8
  rts                  ; The end of our sub routine
  
WriteProblem:
  jsr GetNumber        ; Get a number and put it in $60
  lda $60              ; Store that number in accumulator
  sta $61              ; And put it in $61
  sta $51              ; Also put it in $51
  jsr GetNumber        ; Get another number wiping out what was in $60
  lda $60              ; Putting that new number in accumulator
  sta $50              ; Releasing it in $50
  jsr Add              ; Adding the random numbers
  lda $52              ; Placing the sum in accumulator
  sta $62              ; Now we have the addends in $60 and $61 and the sum in $62
  lda $60              ; Load accumulator with our second random number
  asl                  ; The 4 "asl"s are to make the digit the one on the left
  asl
  asl
  asl
  sta $D2              ; Put our shifted number into D2
  lda #$00             ; Load accumulator with 0
  sta $D7              ; Clear the 7 segment display second furthest to the left
  lda #$00             ; Load accumulator with 0
  sta $D5              ; Clear the 7 segment display second furthest to the left
  lda $61              ; Load accumulator with our first random number
  asl                  ; The 4 "asl"s are to make the digit the one on the left
  asl
  asl
  asl
  sta $D1              ; Put our shifted number into D1
  lda #$00
  sta $D0
  jsr UpdateDisplay    ; Make the screen show something, not just think it
  rts                  ; We're done

Add:
  clc
  sed
  lda $50
  adc $51
  sta $52
  cld
  rts

GetNumber:             ; Gets random number 1-9
  jsr GetRandomByte    ; Get a random byte
  sta $60              ; Store accumulator in $60
  sec
  sbc #9               ; Subtract 9
  bcc GoodNumber       ; When 8 or below, branch to Good
  jmp GetNumber        ; If above 9, try again
  
GoodNumber:            ; Where the random number goes to change from 0-8 to 1-9
  inc $60              ; You could end up with 0s, so we increment. We also can have 9s.
  rts                  ; We're done with this sub routine
