; This tool is a creation of Miro.

  .org $1000

ReadKeyPad = $FF14
UpdateDisplay = $FF10
Sleep_Long = $FF30
Temp = $40
InputPosition = $41

  lda #$01             ; Load accumulator with 1
  sta InputPosition    ; Start with the digit on the left
  lda #$00             ; Load accumulator with 0
  sta Temp             ; Clear Temp, our temporary storing place for the guess
  sta $D3              ; Wipe the furthest right 7 segment display
  lda #$B1             ; Take B1
  sta $D1              ; And put it in the center 7 segs, making a "b"
  lda #$80             ; Take 80
  sta $D0              ; And put it in the right 7 segs
  lda #$04             ; Load a magic number
  sta $D5              ; And make an "i"
  lda #$54             ; Load another magic number
  sta $D4              ; Make an "n"
  lda #$FF             ; Get a full byte
  sta $D7              ; And fill up the second leftmost 7 seg, including the dot
  jsr UpdateDisplay    ; Update the display, making it look good even before you type

WriteName:             ; Write the name of the file
  jsr ReadKeyPad       ; Read the keypad
  cmp #$FF             ; Check if it's blank
  beq WriteName        ; If it is, keep waiting
  cmp #$FD             ; Check if they pressed enter
  beq FindFile         ; If they did, find the file
  sta Temp             ; Once we've checked all those things, put the thing they pressed into Temp
  lda InputPosition    ; Take the input position
  cmp #$01             ; Check if it's 1
  beq LeftDigit        ; If it is, they're typing the leftdigit

RightDigit:            ; If it's not 1, the code comes down here, to write the right digit
  inc InputPosition    ; Next we'll be typing the left digit which requires InputPosition to be 1
  lda $D2              ; Load accumulator with their guess so far
  and #$F0             ; Mask off little end
  sta $D2              ; Put the masked number back into D0
  lda Temp             ; Temp will have the number that has been pressed, so we put it into accumulator
  ora $D2              ; D0 has been masked, so the Temp and D0 will carry through
  sta $D2              ; Place our newly cut and pasted number into $D0
  jsr UpdateDisplay    ; Update the display
  jmp WriteName        ; This allows you to continue typing

LeftDigit:             ; Displays the left digit
  dec InputPosition    ; Make InputPosition 0
  lda $D2              ; Load accumulator with the file name so far
  and #$0F             ; Mask off the big end
  sta $D2              ; Put it back
  lda Temp             ; Take the pressed digit
  asl                  ; Shift to the left 4 times
  asl
  asl
  asl
  ora $D2              ; Squish that and D2 together
  sta $D2              ; Put that back
  jsr UpdateDisplay    ; Update the display
  jmp WriteName        ; Go back to the start

FindFile:              ; Finds the file
  lda #$7F             ; Load the special number
  sta $D3              ; Leaves it in the affected registers
  sta $D4
  sta $D5
  sta $D6
  sta $D7
  lda $D2              ; Load accumulator with the name of the file
  sta $0250            ; Store it into the FileName register
  jsr $FF90            ; Bootstrap (attempt to load FileName.BIN)
  rts                  ; End the sub routine
