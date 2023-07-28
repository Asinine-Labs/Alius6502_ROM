; This game is a creation of Derek & Miro. We would appreciate it if you didn't edit the code. It gives you a 2 digit number and a single digit. You then have to divide the 2 digit number by the single digit and type that number in. It then displays "YES" or "NO".




GetRandomByte = $FF20  ; This makes it nice and obvious that we want a random number
ReadKeyPad = $FF14     ; This is so we can read the keypad
UpdateDisplay = $FF10  ; Udate the display
Sleep_Long = $FF30     ; Sleep for ~0.5s
Temp = $40             ; Temporary location for keyboard reading stuff


  .org $1000           ; The traditional starting address

Game:                  ; Our loop full of sub routines
  jsr WriteProblem     ; Display our math problem
  jsr WriteGuess       ; This shows what you guessed and tells you wether you're right or wrong
  jmp Game             ; Have another crack!

WriteGuess:            ; Writes your guess

  jsr ReadKeyPad       ; Read the keypad
  cmp #$FF             ; Did you press something?
  beq WriteGuess       ; If you didn't, wait until you do
  cmp #$FD             ; Did you press enter?
  beq CheckGuess       ; If you did, did you get it right?
  sta $D0              ; Put that back in D0
  jsr UpdateDisplay    ; Displays the button that you pressed
  jmp WriteGuess       ; This might not be your full guess, so we will let you type more digits


CheckGuess:            ; This will decide wether you were right or wrong

  lda $D0              ; Get your guess into accumulator
  cmp $61              ; Compare with the product
  beq Correct          ; If they were the same, you were correct, so lets let you know
  jmp Wrong            ; If they weren't, tough luck. We also let you know.

Correct:               ; This lets you know if you're correct

  lda #$7F             ; Load accumulator with the magic number
  sta $D5              ; This makes the 7 segment 3rd from the right appear
  sta $D6              ; This makes the 7 segment 3rd from the left appear
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
  jsr Multiply         ; Multiplying the random numbers
  lda $52              ; Placing the product in accumulator
  sta $62              ; Now we have the factors in $60 and $61 and the product in $62
  sta $D2              ; Put our number into D2
  lda #$00             ; Load accumulator with 0
  sta $D6              ; Clear the 7 segment display second furthest to the left
  sta $D4              ; Clear the 7 segment display second furthest to the left
  lda $60              ; Load accumulator with our first random number
  sta $D1              ; Put our number into D1
  jsr UpdateDisplay    ; Make the screen show something, not just think it
  rts                  ; We're done

Multiply:              ; Multiply $50 by $51 and putting it in $52

  lda #00              ; Empty accumulator
  sed                  ; Now we're in decimal mode
  clc                  ; Clearing the carry because we use it soon

Add:                   ; The addition loop

  adc $51              ; Adding second factor to the accumulator
  dec $50              ; Decrementing the first factor
  bne Add              ; Return to Add until the first factor is 0
  sta $52              ; Showing us the result
  cld                  ; To prevent a bunch of problems
  rts                  ; This sub routine is done

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