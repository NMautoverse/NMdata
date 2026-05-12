context("setOrderList")

test_that("reordering moves elements to front and back", {
  l <- list(c = 3, a = 1, d = 4, b = 2)
  res <- setOrderList(l, first = c("a", "b"), last = "c")
  
  # Check order of names
  expect_equal(names(res), c("a", "b", "d", "c"))
  # Check values remain intact
  expect_equal(res$a, 1)
  expect_equal(res$c, 3)
})

test_that("function ignores names not present in the list", {
  l <- list(a = 1, b = 2)
  res <- setOrderList(l, first = "z", last = "y")
  
  expect_equal(res, l)
})

test_that("function works with only first or only last", {
  l <- list(a = 1, b = 2, c = 3)
  
  res_first <- setOrderList(l, first = "c")
  expect_equal(names(res_first), c("c", "a", "b"))
  
  res_last <- setOrderList(l, last = "a")
  expect_equal(names(res_last), c("b", "c", "a"))
})

test_that("error is thrown on overlap between first and last", {
  l <- list(a = 1, b = 2)
  
  expect_error(
    setOrderList(l, first = "a", last = "a"),
    "The same name cannot appear in both 'first' and 'last' arguments."
  )
})

test_that("relative order of 'rest' elements is preserved", {
  l <- list(z = 9, a = 1, x = 8, b = 2, y = 7)
  res <- setOrderList(l, first = "b")
  
  # b moves to front, z-a-x-y should stay in that specific order
  expect_equal(names(res), c("b", "z", "a", "x", "y"))
})
