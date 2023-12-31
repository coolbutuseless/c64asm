% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/999-utils.R
\name{w2b}
\alias{w2b}
\title{Convert a 16bit address to 2 bytes (lo_byte, hi_byte) as is the 6502 way}
\usage{
w2b(address)
}
\arguments{
\item{address}{address from $0000 to $ffff}
}
\value{
integer vector of length 2 with (lo_byte, hi_byte)
}
\description{
Convert a 16bit address to 2 bytes (lo_byte, hi_byte) as is the 6502 way
}
