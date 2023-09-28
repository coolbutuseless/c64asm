context("s2i")

test_that("s2i works", {
  expect_identical(s2i("hello"), c(72L, 69L, 76L, 76L, 79L))

  expect_error(s2i(mtcars), "character")
  expect_error(s2i(NA_character_), "is.na")
  expect_error(s2i(c("hello", "there")), "length")
})
