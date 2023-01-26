; This is a simple program that starts from 0 and increments by one
; To download this program click "Raw", and then press Ctrl+S to save the file. 
 
 
  .org $1000

start:
  lda #$00                     ; Load A with zero to reest all bytes for the counter.
  sta $D0                      ; Store A into first Display bufffer byte.
  sta $D1                      ; Store A into next Display buffer byte.
  sta $D2                      ; Store A into last Display buffer byte.

Count:
  jsr $ff10                    ; Jump to subroutine in ROM that Updates the 7 segment display
  inc $D0                      ; Increment the lowest byte of the Display buffer.
  bne Count                    ; If that byte is not 00 then jump back to top of Clock loop.
  inc $D1                      ; Increment second byte of the Display buffer. 
  bne Count                    ; If that byte is not 00 then jump back to top of Clock loop.
  inc $D2                      ; Increment last byte of the Display buffer.
  jmp Count                    ; jump back to top of Clock loop

