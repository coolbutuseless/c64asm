



#-----------------------------------------------------------------------------
#' Compile ASM to a c64 exectuable (in PRG format)
#'
#' @param asm single character string containing all ASM (including newlines)
#' @param prg Should compilation return the bytes of the compiled PRG?
#'        Default: TRUE.  If FALSE, return the main assembler data.frame which 
#'        is more useful for debugging.
#' 
#' @return prg bytes as a raw vector or \code{prg_df} main assembler data.frame
#' @export
#-----------------------------------------------------------------------------
compile <- function(asm, prg = TRUE) {
  line_tokens <- create_line_tokens(asm)
  prg_df      <- create_prg_df(line_tokens)
  prg_df      <- process_symbols(prg_df)
  prg_df      <- process_zero_padding(prg_df)

  if (isTRUE(prg)) {
    extract_prg_bytes(prg_df)
  } else {
    prg_df
  }
}