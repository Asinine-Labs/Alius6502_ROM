; Demo program simply writes a file of 03.BIN with "HELLO WORLD"

FileAddrPTR = $0260            ; Address pointer - Where to save file, $0260(LSB),$0261(MSB)
FileSize = $025D               ; File Size, 2 bytes including $025D(LSB) ,$025E(MSB)
FileNumber = $0250             ; File Number to create FileName

WriteFile = $FF88              ; Address to Write file ROM code
CreateFileName = $ff70         ; Address to create file name ROM code

  .org $1000

; Put "HELLO WORLD" into memory at $2000
  lda #$48                     ; H
  sta $2000
  lda #$45                     ; E
  sta $2001
  lda #$4C                     ; L
  sta $2002
  lda #$4C                     ; L
  sta $2003
  lda #$4F                     ; O
  sta $2004
  lda #$20                     ; <SPACE>
  sta $2005
  lda #$57                     ; W
  sta $2006
  lda #$4F                     ; O
  sta $2007
  lda #$52                     ; R
  sta $2008
  lda #$4C                     ; L
  sta $2009
  lda #$44                     ; D
  sta $200A


SaveFile:
  lda #$00                     ; Setup the pointer to $2000
  sta FileAddrPTR              ; Setup LSB
  lda #$20                     ; Setup the pointer to $2000
  sta FileAddrPTR+1            ; Setup MSB
  lda #$0B                     ; Setup FileSize of $000B                  
  sta FileSize                 ; Setup LSB
  lda #$00                     ; Setup FileSize of $000B
  sta FileSize +1              ; Setup MSB
  lda #$03                     ; Setup File number to 03
  sta FileNumber               ; Setup file number
  jsr CreateFileName           ; Create file name from file number
  jsr WriteFile                ; Write the file to SDcard
  brk                          ; Exit to monitor ROM

