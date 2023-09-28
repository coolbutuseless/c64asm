*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $38, $30, $00, $00
.byte $00
*=$0820
           sei          ; turn off interrupts
           lda #$7f
           ldx #$01
           sta $dc0d    ; Turn off CIA 1 interrupts
           sta $dd0d    ; Turn off CIA 2 interrupts
           stx $d01a    ; Turn on raster interrupts

           lda #$1b
           ldx #$08
           ldy #$14
           sta $d011    ; Clear high bit of $d012, set text mode
           stx $d016    ; single-colour
           sty $d018    ; screen at $0400, charset at $2000

           lda $d018    ; Changing character set to $3800 - $3800 + $800 (2048 bytes)
           and #$f0
           ora #$08
           sta $d018


           jsr createchar
           jsr clearscr
           jsr incscreen



           lda #<int    ; low part of address of interrupt handler code
           ldx #>int    ; high part of address of interrupt handler code
           ldy #$30     ; line to trigger interrupt
           sta $0314    ; store in interrupt vector
           stx $0315
           sty $d012

           lda $dc0d    ; ACK CIA 1 interrupts
           lda $dd0d    ; ACK CIA 2 interrupts
           asl $d019    ; ACK VIC interrupts
           cli
		       ;rts          ; Return control to BASIC

loop:
           jmp loop     ; infinite loop



;--------------------------------------------------------------
; Clear screen
;--------------------------------------------------------------
createchar:
           lda #$0f
           ldx #$ff
clrchar:
           txa
           sta $2000,x
           dex
           bne clrchar

           rts

;--------------------------------------------------------------
; Clear screen
;--------------------------------------------------------------
clearscr:
           lda #$00
		       ldx #$fa
clear1:
           sta $03ff,x
           sta $04f9,x
           sta $05f3,x
           sta $06ed,x
           dex
           bne clear1

           rts


;--------------------------------------------------------------
; Inrement every character on the screen
;--------------------------------------------------------------
incscreen:
		   ldx #$fa
incscr1:
           inc $03ff,x
           inc $04f9,x
           inc $05f3,x
           inc $06ed,x
           dex
           bne incscr1

           rts


;--------------------------------------------------------------
; Interupt routine
;--------------------------------------------------------------
int:
           inc $d020    ; increment screen colour

           jsr incscreen

           dec $d020    ; decrement screen colour

           asl $d019    ; ACK interrupt (to re-enable it)
		       ;jmp $ea31    ; Return control to basic

           pla          ; prepare to return from interupt
           tay
           pla
           tax
           pla
           rti          ; return from interrupt
