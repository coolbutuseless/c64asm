context("word-to-bytes")

test_that("word-to-bytes works", {
  expect_identical(w2b(0), c(0L, 0L))
  expect_identical(w2b(0x1ff), c(255L, 1L))
  expect_identical(w2b(0xff01), c(1L, 255L))
})
