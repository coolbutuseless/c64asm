context("compare_to_tass")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run code through TASS to get comparison target
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if (FALSE) {
#   library(c64tass)
#   
#   asm_files <- list.files("./tests/testthat/asm/", pattern = "asm$", full.names = TRUE)
#   asm_file <- asm_files[1]
#   
#   for (asm_file in asm_files) {
#     # tass <- TASS$new(asm = asm_file, tass_bin = "/opt/homebrew/bin/64tass")
#     tass <- TASS$new(asm = asm_file, tass_bin = "/usr/local/bin/64tass")
#     tass$compile()
#     
#     tass_prg_name <- paste0(tools::file_path_sans_ext(asm_file), "-tass.prg")
#     
#     writeBin(tass$get_prg(), tass_prg_name)
#   }
# }



#-----------------------------------------------------------------------------
# The ASM files in the vignettes/asm directory are basic programs which
# should compile to the same bytes in both `c64asm` and TASS64
#-----------------------------------------------------------------------------
test_that("compare to TASS", {
  asm_files <- list.files("asm", pattern = "asm$", full.names = TRUE)
  asm_file <- asm_files[1]
  
  for (asm_file in asm_files) {
    asm <- readLines(asm_file) |> paste(collapse="\n")

    prg_bytes <- c64asm::compile(asm)
    
    tass_prg_name <- paste0(tools::file_path_sans_ext(asm_file), "-tass.prg")
    tass_prg <- readBin(tass_prg_name, 'raw', n = file.size(tass_prg_name))

    expect_identical(prg_bytes, tass_prg)
  }
})
