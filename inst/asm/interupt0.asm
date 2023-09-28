*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $34, $30, $39, $36, $00, $00
.byte $00
*=$1000
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
		   rts          ; Return control to BASIC

loop:
           jmp loop     ; infinite loop

int:
           inc $d021    ; flash border
           dec $d021    ; flash border


		   ldx #$ff
charup:
           inc $0400,x
           dex
           bne charup

           asl $d019    ; ACK interrupt (to re-enable it)
		   jmp $ea31    ; Return control to basic
           pla
           tay
           pla
           tax
           pla
           rti          ; return from interrupt
