; This program rapidly updates the colors
; of the screen and the border.

*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $34, $30, $39, $36, $00, $00
.byte $00
*=$1000

BORDER = $d020

start   inc BORDER  ; increase border colour 
        jmp start   ; repeat
