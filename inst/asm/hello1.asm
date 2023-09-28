*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $38, $30, $00, $00
.byte $00
*=$0820
  lda #$93
  jsr $ffd2

  lda #$0e   ; Switch to lowercase/upperase character mode
  jsr $ffd2

  ldy #$00
loop1 lda message1,y
  jsr $ffd2
  iny
  cpy #$fa
  bne loop1


wait jmp wait

message1
  .text "HIowankfetwuqqwjytdybnaapdoabmslbxtzzcrwbwdxooerksqiwehcyxliuurnnacuorhgdouuzngqpsftvahhjnfmrxlhiyqwseidrhrxjshawjjefxgctdgzhtmpiwckjlyyywzrihlfoigwiaxvgmkbsmwbmiulokixjsnwpeijcukxeoqtupkbloeqeqdhmivtjuwmvfvbvmzelvmccszwrzpjzzwtmqglpmfgpzwqbhjuyzyhoe"
