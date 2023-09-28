
'%||%' <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#-----------------------------------------------------------------------------
#' Reference list of opcode information
#'
#' A dataset containing opcode information for 6502 instructions
#'
#' @source \url{http://www.oxyron.de/html/opcodes02.html}
#-----------------------------------------------------------------------------
"opcodes"



#-----------------------------------------------------------------------------
#' Hex (character strings) to integer values
#'
#' Any leading '#' will be dropped.
#' Values prefixed with "$" will be treated as hexadecimal.
#' Values with a leading 'b' will be treated as binary (MSB bit first)
#'
#' @param h character vector of values e.g. "#$1a", "$12", "#127", "#b00111111"
#'
#' @return vector of integers
#'
#' @export
#-----------------------------------------------------------------------------
h2i <- function(h) {
  stopifnot(is.character(h))
  
  # Remove leading '#' if present
  h <- gsub('#'   , ''  , h)
  
  # Replace '$' with '0x' as the hexadeximal prefix
  h <- gsub('^\\$', '0x', h)
  
  # detect which are binary i.e. start with a 'b' like "b00001001'
  b8 <- !is.na(h) & startsWith(h, 'b')
  
  # Convert to int
  i      <- integer(length(h))
  i[!b8] <- strtoi   (h[!b8])
  i[ b8] <- b8_to_int(h[ b8])

  if (any(is.na(i))) {
    stop("Can't convert hex2int: ", paste(h, collapse=', '))
  }
  i
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert 8bit binary to integer
#' 
#' @param b Character vector of strings representing 8-bit binary (MSB). 
#'        Could start with letter 'b' e.g. "b00001111", "10101010"
#'        
#' @return integers
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b8_to_int <- function(b) {
  stopifnot(is.character(b))
  b <- gsub("^b", "", b)
  stopifnot(all(nchar(b) == 8))
  i <- strtoi(b, base = 2)
  if (anyNA(i)) {
    stop("b8_to_int: not a valid 8-bit binary of the form 'bxxxxxxxx': ", deparse(b))
  }
  i
}


#-----------------------------------------------------------------------------
#' String to ASM for ".text" code
#'
#' This is a complete hack to create CBMASCII bytes from plain text
#'
#' @param string single string
#' @return Vector of integers
#'
#' @importFrom purrr map_int
#' @export
#-----------------------------------------------------------------------------
s2i <- function(string) {
  stopifnot(is.numeric(string) || is.character(string))
  stopifnot(length(string) == 1)
  stopifnot(!is.na(string))

  # Remove quotes
  string <- gsub('\\"', '', string)

  # Split into individual chars
  vals <- (strsplit(string, '')[[1]])

  # Convert from chars to int
  vals <- purrr::map_int(vals, utf8ToInt)

  # Recode upper case
  uppercase <- (vals >= 65) & (vals <= 90)
  vals[uppercase] <- vals[uppercase] + 128L

  # recode lower case
  lowercase <- (vals >= 97) & (vals <=122)
  vals[lowercase] <- vals[lowercase] - 32L


  vals
}


#-----------------------------------------------------------------------------
#' Convert a 16bit address to 2 bytes (lo_byte, hi_byte) as is the 6502 way
#'
#' @param address address from $0000 to $ffff
#'
#' @return integer vector of length 2 with (lo_byte, hi_byte)
#'
#' @export
#-----------------------------------------------------------------------------
w2b <- function(address) {
  address <- as.integer(address)
  c(lo(address), hi(address))
}


#-----------------------------------------------------------------------------
#' Fetch the low byte of a 16bit address
#'
#' @param address address from $0000 to $ffff
#'
#' @return integer value for the low byte of this address
#'
#' @export
#-----------------------------------------------------------------------------
lo <- function(address) {
  address <- as.integer(address)
  address %% 256L
}


#-----------------------------------------------------------------------------
#' Fetch the high byte of a 16bit address
#'
#' @param address address from $0000 to $ffff
#'
#' @return integer value for the high byte of this address
#'
#' @export
#-----------------------------------------------------------------------------
hi <- function(address) {
  address <- as.integer(address)
  address %/% 256L
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Last-one-carried-forward and first-one-carried-back
#' 
#' Replace NA values with last non-NA value, or next non-NA value.
#' 
#' @param x vector
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
locf <- function(x) {
  v <- !is.na(x)
  c(NA_integer_, x[v])[cumsum(v) + 1L]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname locf
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
focb <- function(x) {
  x <- rev(x)
  v <- !is.na(x)
  rev(c(NA_integer_, x[v])[cumsum(v) + 1L])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' LOCF then FOCB.  Faster than doing both separately.
#' @rdname locf
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
locf_focb <- function(x) {
  v <- !is.na(x)
  c(x[v][1L], x[v])[cumsum(v) + 1L]
}

