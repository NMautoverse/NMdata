test_that("reorder2 returns a factor", {
  x <- c("b", "a", "c")
  result <- reorder2(x, x)
  expect_s3_class(result, "factor")
})

test_that("reorder2 sorts by a numeric variable", {
  x <- c("b", "a", "c")
  n <- c(3, 1, 2)
  result <- reorder2(x, n)
  expect_equal(levels(result), c("a", "c", "b"))
})

test_that("reorder2 puts TRUE matches first with logical expression", {
  x <- c("b", "a", "c")
  result <- reorder2(x, x == "a", x)
  ## reorder(x, x == "a")
  ## reorder2(x, x == "a")
  expect_equal(levels(result)[1], "b")
})

test_that("reorder2 handles multiple logical expressions in order", {
  x <- c("Adult", ">=50", "<50", ">=40")
  result <- reorder2(x, x == "Adult", x == ">=50", x)
  levs <- levels(result)
  expect_equal(levs[1], "<50")
  expect_equal(levs[2], ">=40")
})

test_that("reorder2 preserves all levels", {
  x <- c("b", "a", "c", "a", "b")
  result <- reorder2(x, x)
  expect_setequal(levels(result), c("a", "b", "c"))
})

test_that("reorder2 handles a single unique value", {
  x <- c("a", "a", "a")
  result <- reorder2(x, x)
  expect_equal(levels(result), "a")
})

test_that("reorder2 works with integer ordering", {
  x <- c("low", "high", "mid")
  n <- c(1, 3, 2)
  result <- reorder2(x, n)
  expect_equal(levels(result), c("low", "mid", "high"))
})

test_that("reorder2 values are preserved as factor values", {
  x <- c("b", "a", "c", "b")
  result <- reorder2(x, x)
  expect_equal(as.character(result), x)
})


test_that("reorder2 handles multiple logical expressions in order", {

  fileRef <- "testOutput/reorder2_01.rds"
  
x <- c("Adult", ">=50", ">=50", "[40,50)")

res <- list(
factor=reorder2(x, x == "Adult", x == ">=50", x),
order=  reorder2(x, x == "Adult", x == ">=50", x,as="order"),
rank=  reorder2(x, x == "Adult", x == ">=50", x,as="rank"),
sorted=  reorder2(x, x == "Adult", x == ">=50", x,as="sorted")
  )

expect_equal_to_reference( res,fileRef)

})
