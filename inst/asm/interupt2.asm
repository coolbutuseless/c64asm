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
           ldy #$80     ; line to trigger interrupt
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
; Create character set
;--------------------------------------------------------------
charseq
;.byte $01, $01, $01, $01, $03, $03, $03, $03
;.byte $07, $07, $07, $07, $0f, $0f, $0f, $0f
;.byte $1f, $1f, $1f, $1f, $3f, $3f, $3f, $3f
;.byte $7f, $7f, $7f, $7f, $ff, $ff, $ff, $ff
.byte $00, $01, $01, $03, $03, $07, $07, $0f
.byte $0f, $1f, $1f, $3f, $3f, $7f, $7f, $ff
.byte $ff, $7f, $7f, $ef, $3f, $1f, $1f, $0f
.byte $0f, $07, $07, $03, $03, $01, $01, $00

createchar:
           lda #$15
           ldx #$00
           ldy #$00
charloop:
           lda charseq, y
           sta $2000,x
           sta $2001,x
           sta $2002,x
           sta $2003,x
           sta $2004,x
           sta $2005,x
           sta $2006,x
           sta $2007,x
           sta $2100,x
           sta $2101,x
           sta $2102,x
           sta $2103,x
           sta $2104,x
           sta $2105,x
           sta $2106,x
           sta $2107,x
           sta $2200,x
           sta $2201,x
           sta $2202,x
           sta $2203,x
           sta $2204,x
           sta $2205,x
           sta $2206,x
           sta $2207,x
           sta $2300,x
           sta $2301,x
           sta $2302,x
           sta $2303,x
           sta $2304,x
           sta $2305,x
           sta $2306,x
           sta $2307,x
           sta $2400,x
           sta $2401,x
           sta $2402,x
           sta $2403,x
           sta $2404,x
           sta $2405,x
           sta $2406,x
           sta $2407,x
           sta $2500,x
           sta $2501,x
           sta $2502,x
           sta $2503,x
           sta $2504,x
           sta $2505,x
           sta $2506,x
           sta $2507,x
           sta $2600,x
           sta $2601,x
           sta $2602,x
           sta $2603,x
           sta $2604,x
           sta $2605,x
           sta $2606,x
           sta $2607,x
           sta $2700,x
           sta $2701,x
           sta $2702,x
           sta $2703,x
           sta $2704,x
           sta $2705,x
           sta $2706,x
           sta $2707,x
           iny
           inx
           inx
           inx
           inx
           inx
           inx
           inx
           inx
           beq endloop
           jmp charloop
endloop
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
           dex
           bne incscr1
		       ldx #$fa
incscr2:
           inc $04f9,x
           dex
           bne incscr2
		       ldx #$fa
incscr3:
           inc $05f3,x
           dex
           bne incscr3
		       ldx #$fa
incscr4:
           inc $06ed,x
           dex
           bne incscr4

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
