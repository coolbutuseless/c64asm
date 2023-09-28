context("h2i")

test_that("h2i works", {
  expect_identical(h2i("$0a"), 10L)
  # expect_identical(h2i("$0a", "$ff"), c(10L, 255L))
  expect_identical(h2i(c("$0a", "$ff")), c(10L, 255L))
  expect_identical(h2i(c("$0a", "0xff")), c(10L, 255L))

  expect_identical(h2i(c("#$ff", "#254", "#b11111101")), c(255L, 254L, 253L))
  
  
  expect_error(h2i(20), 'character')
  expect_error(h2i(c("$0a", NA)), 'convert')
  expect_error(h2i(c("$0a", "$3g")), 'convert')
})
