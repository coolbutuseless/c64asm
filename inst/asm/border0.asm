; This program rapidly updates the colors
; of the screen and the border.

*=$c000   ; starting address of the program 

start   inc $d020  ; increase screen colour 
        inc $d021  ; increase border colour
        jmp start  ; repeat
