  .org $8000

  .org $A000

reset:
  ldx #$FF
  txs        ; Setup stack

  lda #$ff
  sta $8012
  lda #$ff
  sta $8013

  lda #$ff
  sta $8010
  lda #$ff
  sta $8011

loop:
  jsr Sleep_Long
  lda #$ff
  sta $8010
  lda #$ff
  sta $8011
  jsr Sleep_Long
  lda #$00
  sta $8010
  lda #$00
  sta $8011
  jmp loop

Sleep_Long:  ; sleep about 1 second
  ldx #$FF
  ldy #$FF
Sleep_loop_Y:
  dey
Sleep_loop_X:
  dex
  cpx #$ff
  bne Sleep_loop_X
  cpy #$ff
  bne Sleep_loop_Y
  rts

  .org $fffc
  .word reset
  .word $0000
