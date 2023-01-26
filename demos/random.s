; This program will produce 3 random bytes and show them on the display forever
; To download this program click "Raw", and then press Ctrl+S to save the file.


ZP_Display = $D0  ; D0,D1,D2 ; WARNING: Right to Left.
RandSeed = $0201  ; $0201, $0202
GetRandomByte = $FF20
Sleep_Long = $FF30
UpdateDisplay = $FF10

 .org $1000

  lda #$01
  sta RandSeed
  sta RandSeed+1
  jmp Loop

 .org $11E8
Loop:
  jsr UpdateDisplay

  jsr GetRandomByte
  sta ZP_Display

  jsr GetRandomByte
  sta ZP_Display+1

  jsr GetRandomByte
  sta ZP_Display+2

  jsr Sleep_Long
  jmp Loop


