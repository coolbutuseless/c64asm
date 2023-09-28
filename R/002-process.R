
globalVariables(c('nbytes', 'addr', 'bytes_so_far', 'value',
                  'symbol_address', 'symbol_bytes', 'symbol_offset',
                  'symbol_op', 'argbytes', 'bytes', 'nbytes',
                  'hexbytes', 'next_addr', 'label_value', 'label_value_final',
                  'symbol_expr', 'symbol_value', 'symbol_lo', 'symbol_hi',
                  'row_symbol_values'))


#-----------------------------------------------------------------------------
#' Process any cross-referenced symbols to insert actual addresse/values
#'
#' @param prg_df main assembler data.frame created by `create_prg_df()`
#'
#' @return udpated prg_df with actual addresses added for each row, and
#'         cross-referenced values resolved
#'
#' @import dplyr
#' @importFrom purrr map2 map_chr map_lgl map
#' @importFrom stats setNames
#' @export
#-----------------------------------------------------------------------------
process_symbols <- function(prg_df) {
  if (any(prg_df$opmode == 'unknown', na.rm = TRUE)) {
    stop("Unknown opcodes")
  }


  #-----------------------------------------------------------------------------
  # Address calculation by using
  # - known start PC
  # - nbytes for each instruction
  #-----------------------------------------------------------------------------
  prg_df <- prg_df %>%
    mutate(
      init_addr = locf_focb(init_addr)
    ) %>% 
    mutate(addr = init_addr) %>%
    group_by(init_addr) %>%
    mutate(
      bytes_so_far = cumsum(nbytes),
      addr         = addr + lag(bytes_so_far, default = 0)
    ) %>%
    ungroup() %>%
    select(-bytes_so_far) %>%
    select(addr, everything())


  #-----------------------------------------------------------------------------
  # Address cross-reference resolution.
  # Some symbols are for addresses,
  # others are for defined constants
  #-----------------------------------------------------------------------------
  symbol <- prg_df %>%
    filter(!is.na(label)) %>%
    select(symbol = label, symbol_address = addr, label_value_final = label_value) %>%
    mutate(
      label_value_final = ifelse(is.na(label_value_final), symbol_address, label_value_final)
    )

  symbol_values <- as.list(symbol$label_value_final) %>% setNames(symbol$symbol)

  # add the address to each row's set of symbol values
  # This is so references to 'addr' or '.*' work automatically
  prg_df <- prg_df %>%
    mutate(
      row_symbol_values = addr %>% purrr::map(~c(symbol_values, list(addr=.x)))
    )

  #-----------------------------------------------------------------------------
  # All symbols are resolved by evaluating their expression, using
  # symbol_values as the environment
  #-----------------------------------------------------------------------------
  
  prg_df <- prg_df %>%
    mutate(
      symbol_value = purrr::map2_int(symbol_expr, row_symbol_values,
                                     ~as.integer(eval(parse(text=.x), envir = .y))),
      symbol_bytes = purrr::map(symbol_value, w2b),
      symbol_lo    = purrr::map(symbol_value, lo ),
      symbol_hi    = purrr::map(symbol_value, hi ),

      symbol_offset = symbol_value - addr - 2L,  # not sure where the offset sould be calculated from
      symbol_offset = ifelse(symbol_offset < 0, 256L + symbol_offset, symbol_offset), # signed byte
      symbol_offset = as.list(symbol_offset),

      argbytes      = if_else(symbol_op == 'relative'     , symbol_offset, argbytes, argbytes),
      argbytes      = if_else(grepl('absolute', symbol_op), symbol_bytes , argbytes, argbytes),
      argbytes      = if_else(symbol_op == 'low'          , symbol_lo    , argbytes, argbytes),
      argbytes      = if_else(symbol_op == 'high'         , symbol_hi    , argbytes, argbytes)
    )

  
  #-----------------------------------------------------------------------------
  # Recalc 'bytes' and produce a hexbytes representation for debugging
  #-----------------------------------------------------------------------------
  prg_df <- prg_df %>%
    mutate(
      bytes        = purrr::map2(opbyte, argbytes, function(.x, .y) {res <- c(.x, .y); res[!is.na(res)]}),
      hexbytes     = purrr::map_chr(bytes, ~paste(as.character(as.hexmode(.x)), collapse=", "))
    )

  prg_df
}



#-----------------------------------------------------------------------------
#' Pad out with zero bytes between address blocks
#'
#' @param prg_df main assembler data.frame after `process_symbols()` has been run
#'
#' @return prg_df updated with zero padding rows between address blocks so that
#'         every byte from start to finish is accounted for
#'
#' @import dplyr
#' @export
#-----------------------------------------------------------------------------
process_zero_padding <- function(prg_df) {

  zeros <- prg_df %>%
    mutate(
      next_addr = addr + nbytes,
      nbytes    = lead(addr) - next_addr
    )
  
  zeros$nbytes[is.na(zeros$nbytes)] <- 0L 
  
  zeros <- dplyr::filter(zeros, nbytes > 0)

  if (nrow(zeros) > 0) {
    zeros <- zeros %>%
      mutate(
        bytes     = purrr::map(nbytes, ~rep(0L, .x)),
        argbytes  = list(NA_integer_),
        line      = '(zero padding)',
        init_addr = next_addr,
        hexbytes  = ''
      ) %>%
      select(init_addr, addr = next_addr, bytes, nbytes, argbytes, line, hexbytes)
    
    prg_df <- prg_df %>%
      bind_rows(zeros) %>%
      arrange(addr)
  }

  prg_df
}



#-----------------------------------------------------------------------------
#' Extract the raw bytes for the assembled PRG
#'
#' @param prg_df main assembler data.frame
#'
#' @return raw vector of bytes
#'
#' @importFrom purrr flatten_int
#' @export
#-----------------------------------------------------------------------------
extract_prg_bytes <- function(prg_df) {
  as.raw(c(w2b(prg_df$addr[1]), as.integer(purrr::flatten(prg_df$bytes))))
}



#-----------------------------------------------------------------------------
#' Save the PRG to file
#'
#' @param prg_df main assembler data.frame
#' @param filename c64 PRG file to write to
#'
#' @export
#-----------------------------------------------------------------------------
save_prg <- function(prg_df, filename) {
  prg_bytes <- extract_prg_bytes(prg_df)
  writeBin(prg_bytes, filename)
}



