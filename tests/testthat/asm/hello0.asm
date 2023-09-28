*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $38, $30, $00, $00
.byte $00
*=$0820
ldx #$00
loop lda message,x
and #$3f
sta $0400,x
inx
cpx #$0c
bne loop
rts
message
    .text "Hello World!"
