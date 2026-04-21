x <- c(
  "SAME",
  " SAME ",
  "SAME(1)",
  "SAME (1)",
  "  SAME   (  123 ) ",
  "SAME()",
  "SAME (a)",
  "NOT SAME"
)

context("isSAME")



test_that("basic",{
  

res <- isSAME(x)

expect_equal(res,
             c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)
             )

})


context("NSAME")

test_that("basic",{
  res <- NSAME(x)

expect_equal(res,
             c(1,1,1,1,123,NA,NA,NA)
             )

})
