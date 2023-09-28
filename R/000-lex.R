

#-----------------------------------------------------------------------------
#' Regex patterns for parsing 6502 assembly
#-----------------------------------------------------------------------------
asm_patterns <- c(
  newline    = '\n',
  whitespace = '\\s+',
  PC         = '\\*',
  immediate  = '#\\$[0-9a-fA-F]{1,2}',  #  Immediate hexademical: #$1c, #$a
  immediate  = '#[0-9]{1,5}',           #  Immediate decimal    : #127, #3
  immediate  = '#b[01]{8}',             #  Immediate binary     " #b00001001
  word       = '\\$[0-9a-fA-F]{3,4}',
  byte       = '\\$[0-9a-fA-F]{1,2}',
  opcode     = "\\b(ADC|AHX|ALR|ANC|AND|ARR|ASL|AXS|BCC|BCS|BEQ|BIT|BMI|BNE|BPL|BRK|BVC|BVS|CLC|CLD|CLI|CLV|CMP|CPX|CPY|DCP|DEC|DEX|DEY|EOR|INC|INX|INY|ISC|JMP|JSR|LAS|LAX|LAX|LDA|LDX|LDY|LSR|NOP|ORA|PHA|PHP|PLA|PLP|RLA|ROL|ROR|RRA|RTI|RTS|SAX|SBC|SEC|SED|SEI|SHX|SHY|SLO|SRE|STA|STX|STY|TAS|TAX|TAY|TSX|TXA|TXS|TYA|XAA|adc|ahx|alr|anc|and|arr|asl|axs|bcc|bcs|beq|bit|bmi|bne|bpl|brk|bvc|bvs|clc|cld|cli|clv|cmp|cpx|cpy|dcp|dec|dex|dey|eor|inc|inx|iny|isc|jmp|jsr|las|lax|lax|lda|ldx|ldy|lsr|nop|ora|pha|php|pla|plp|rla|rol|ror|rra|rti|rts|sax|sbc|sec|sed|sei|shx|shy|slo|sre|sta|stx|sty|tas|tax|tay|tsx|txa|txs|tya|xaa)\\b",
  byte_inst  = '\\.byte',
  text_inst  = '\\.text',
  rtext_inst = '\\.rtext',
  rbyte_inst = '\\.rbyte',
  lbracket   = '\\(',
  rbracket   = '\\)',
  text       = '".*?"',
  comma      = ",",
  colon      = ":",
  equals     = '=',
  comment    = '(;[^\n]*)',
  x          = '(x|X)',
  y          = '(y|Y)',
  symbol     = '#?<?>?\\{.*?\\}',  # a symbol with evaluation
  symbol     = '[^\\s:,)]+'
)



#-----------------------------------------------------------------------------
#' Split each ASM line into tokens
#'
#' @param asm a single character string containing full ASM code, including newlines
#'
#' @return A list where element contains the tokens for a single row.
#'         Rows with just comments or whitespace are discarded
#'
#' @export
#-----------------------------------------------------------------------------
create_line_tokens <- function(asm) {
  lines <- strsplit(asm, "\n")[[1]]
  
  # tokenize each line individually
  lines <- lapply(lines, \(line) lex(line, asm_patterns))
  
  # For the tokens for each line, remove whitespace etc
  lines <- lapply(lines, \(line) {
    line[!names(line) %in% c('whitespace', 'comma', 'comment', 'colon')]
  })
  
  # Discard any lines which ended up being empty
  lines <- Filter(\(x) length(x) > 0, lines)
  
  lines
}
