; WARNING - This program doesn't compile yet, we are working to solve the issue
; This program is a simple clock that starts counting from whenever you strt the program.
; To download this program click "Raw", and then press Ctrl+S to save the file.



PORTB = $8010 
PORTA = $8011 
DDRB = $8012
DDRA = $8013
VIA_ACR = $801B
VIA_T1CL = $8014
VIA_T1CH = $8015
VIA_IER = $801E

ZP_Display = $D0               ; D0,D1,D2  - WARNING: Bytes Right to Left.
IRQ_Vec = $02FE                ; IRQ vector in ram.

 .org $1000

start:
  lda #<IRQ                    ; Get the low byte of IRQ_Exit
  sta IRQ_Vec                  ; Store to IRQ Vector
  lda #>IRQ                    ; Get the high byte of IRQ_Exit
  sta IRQ_Vec+1                ; Store to IRQ Vector

  lda #$00                     ; Reset all bytes of the Display to 00
  sta $D0                      ; Store to Display buffer
  sta $D1                      ; Store to Display buffer
  sta $D2                      ; Store to Display buffer

  lda #$50                     ; Put $C350 (50,000) in the VIA's timer 1 counter low and high bytes.
  sta VIA_T1CL                 ; Note:  you must write to the counters to get T1 going.
  lda #$C3                     ; After that, you can write to the latches.
  sta VIA_T1CH                 ; 

  lda #01000000B               ; Set the bit that tells T1 to automatically
  sta  VIA_ACR                 ; Produce an interrupt at every time-out

  lda  #11000000B
  sta  VIA_IER                 ; Enable the T1 interrupt in the VIA.
  cli                          ; Enable interrupts

clock:
  jsr $ff10                    ; Update display
  nop                          ; Do nothing
  jmp clock                    ; Jump back to top of loop

IRQ:
  pha                          ; Push A to stack
  BIT VIA_T1CL                 ; clear the IRQ state by reading from T! counter low byte
  inc $40                      ; Increment the sub second counter in Zero page
  lda #$14                     ; Check if we have counted 20 sub seconds
  cmp $40                      ; Check sub seconds counter in Zero page
  bne ExitIRQ                  ; If we have not made 20 sub seconds then exit IRQ code
INC_SEC:
  lda #$00                     ; Reset the sub second counter to 00
  sta $40                      ; Reset the sub second counter in Zero page
  sed                          ; Set decimal mode
  clc                          ; Clear carry flag before add
  lda ZP_Display               ; Load the lower (Seconds) display byte
  adc #$01                     ; Add 01
  cmp #$60                     ; Check if we got to 60 seconds
  beq INC_MIN                  ; If we have 60 seconds then increment minutes
  sta ZP_Display               ; If we are not at 60 seconds then store the seconds back to the Display byte
  jmp ExitIRQ                  ; And then exit the IRQ code
INC_MIN: 
  lda #$00                     ; Reset the Seconds byte to 00
  sta ZP_Display               ; Reset seconds byte in Display
  clc                          ; Clear carry flag before add
  lda ZP_Display+1             ; Load the next (Minutes) byte from Display buffer
  adc #$01                     ; Add 01
  cmp #$60                     ; Check if we got to 60 miuntes
  beq INC_HOUR                 ; If we are at 60 minutes then increment the hours
  sta ZP_Display+1             ; Store the minutes byte back to display buffer
  jmp ExitIRQ                  ; And then exit IRQ code

INC_HOUR: 
  lda #$00                     ; Reset minutes to 00
  sta ZP_Display+1             ; Reset minutes byte in Display buffer
  clc                          ; Clear carry flag before add
  lda ZP_Display+2             ; Load the last (Hours) byte from the display buffer
  adc #$01                     ; Add 01
  cmp #$12                     ; Check if we have got to 12 hours
  beq INC_DAY                  ; After 12 hours we reset
  sta ZP_Display+2             ; Store the hours back to the display buffer
  jmp ExitIRQ                  ; Exit IRQ code

INC_DAY: 
  lda #$00                     ; Reset hours to 00
  sta ZP_Display+2             ; Reset hours

ExitIRQ:
  pla                          ; Pop A from stack to restore CPU state before exit from IRQ
  rti                          ; Exit from IRQ
