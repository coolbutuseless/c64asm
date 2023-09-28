ScreenRamText = $0400

*=$0801
  .byte $0c, $08, $0a, $00, $9e, $20
  .byte $32, $30, $38, $30, $00, $00
  .byte $00

* = $0820
    lda #<ScreenRamText
    sta $fb
    lda #>ScreenRamText
    sta $fc

    lda #$0b
    ldy #$00
    ldx #$04 ;40*25
    
loop
    sta ($fb),y
    sty $d020
    dey
    bne loop
    inc $fc
    dex
    bne loop

    rts
