test_that("",{
  
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

res <- isSame(x)

expect_equal(res,
             c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)
             )

})
