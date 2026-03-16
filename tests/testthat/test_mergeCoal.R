context("mergeCoal")

test_that("mergeCoal basic overwrite works", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA), b = c(NA, "yb2"))
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(result$a[1], "ya1")
    expect_equal(result$a[2], "xa2")
    expect_equal(result$b[1], "xb1")
    expect_equal(result$b[2], "yb2")
})

test_that("mergeCoal handles multiple rows in x matched by one row in y", {
    x <- data.table(idx = 1:3, grp = c(1, 1, 2), a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(grp = 1, a = "y1")
    
    result <- mergeCoal(x, y, by = "grp")
    
    expect_equal(nrow(result), 3)
    expect_equal(result$a[1], "y1")
    expect_equal(result$a[2], "y1")
    expect_equal(result$a[3], "xa3")
})

test_that("mergeCoal adds new columns from y", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA), b = c(NA, "yb2"), c = 1)
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_true("c" %in% names(result))
    expect_equal(result$c[1], 1)
    expect_equal(result$c[2], 1)
    expect_true(is.na(result$c[3]))
})

test_that("mergeCoal respects add.new = FALSE", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA), b = c(NA, "yb2"), c = 1)
    
    result <- mergeCoal(x, y, by = "idx", add.new = FALSE)
    
    expect_false("c" %in% names(result))
})

test_that("mergeCoal works with explicit cols.coal", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA), b = c(NA, "yb2"))
    
    result <- mergeCoal(x, y, by = "idx", cols.coal = "a")
    
    expect_equal(result$a[1], "ya1")
    expect_equal(result$b[1], "xb1")
    expect_equal(result$b[2], "xb2")
})

test_that("mergeCoal works with multiple by columns", {
    x <- data.table(idx = 1:3, grp = c("A", "B", "A"), a = paste0("xa", 1:3))
    y <- data.table(idx = c(1, 2), grp = c("A", "B"), a = c("ya1", "ya2"))
    
    result <- mergeCoal(x, y, by = c("idx", "grp"))
    
    expect_equal(result$a[1], "ya1")
    expect_equal(result$a[2], "ya2")
    expect_equal(result$a[3], "xa3")
})

test_that("mergeCoal preserves all rows from x", {
    x <- data.table(idx = 1:5, a = paste0("xa", 1:5))
    y <- data.table(idx = 2:3, a = c("ya2", "ya3"))
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_equal(nrow(result), 5)
    expect_equal(result$a[1], "xa1")
    expect_equal(result$a[2], "ya2")
    expect_equal(result$a[3], "ya3")
    expect_equal(result$a[4], "xa4")
})

test_that("mergeCoal only overwrites with non-NA values", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3))
    y <- data.table(idx = 1:3, a = c("ya1", NA, "ya3"))
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_equal(result$a[1], "ya1")
    expect_equal(result$a[2], "xa2")
    expect_equal(result$a[3], "ya3")
})

test_that("mergeCoal respects as.fun argument", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA))
    
    result <- mergeCoal(x, y, by = "idx", as.fun = "data.table")
    
    expect_s3_class(result, "data.table")
})

test_that("mergeCoal works with data.frames", {
    x <- data.frame(idx = 1:3, a = paste0("xa", 1:3))
    y <- data.frame(idx = 1:2, a = c("ya1", NA))
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_s3_class(result, "data.frame")
    expect_equal(result$a[1], "ya1")
})

test_that("mergeCoal errors when by columns have NA in y", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3))
    y <- data.table(idx = c(1, NA, 3), a = c("ya1", "ya2", "ya3"))
    
    expect_error(
        mergeCoal(x, y, by = "idx"),
        "missing values are not allowed in by columns of y"
    )
})

test_that("mergeCoal errors when by columns not in both x and y", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3))
    y <- data.table(id = 1:2, a = c("ya1", NA))
    
    expect_error(
        mergeCoal(x, y, by = "idx"),
        "by must be a character vector of column names found in both x and y"
    )
})

test_that("mergeCoal errors when cols.coal not in y", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA))
    
    expect_error(
        mergeCoal(x, y, by = "idx", cols.coal = "b"),
        "cols.coal must be a character vector of column names found in y"
    )
})

test_that("mergeCoal guesses by when not provided", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", NA ))
    
    expect_error(
        result <- mergeCoal(x, y)
,
        "missing values are not allowed in by columns of y."
    )
    
})

test_that("mergeCoal handles cols.coal = FALSE", {
    x <- data.table(idx = 1:3, a = paste0("xa", 1:3), b = paste0("xb", 1:3))
    y <- data.table(idx = 1:2, a = c("ya1", "ya2"), c = 1:2)
    
    result <- mergeCoal(x, y, cols.coal = FALSE)
    
    # Should only add new columns, not overwrite existing
    expect_true("c" %in% names(result))
    expect_equal(result$a, paste0("xa", 1:3))
})

test_that("mergeCoal handles numeric columns", {
    x <- data.table(idx = 1:3, val = c(10, 20, 30))
    y <- data.table(idx = 1:2, val = c(100, NA))
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_equal(result$val[1], 100)
    expect_equal(result$val[2], 20)
    expect_equal(result$val[3], 30)
})

test_that("mergeCoal preserves row order from x", {
    x <- data.table(idx = 3:1, a = paste0("xa", 3:1))
    y <- data.table(idx = 1:2, a = c("ya1", "ya2"))
    
    result <- mergeCoal(x, y, by = "idx")
    
    expect_equal(result$idx, 3:1)
    expect_equal(result$a[1], "xa3")
    expect_equal(result$a[2], "ya2")
    expect_equal(result$a[3], "ya1")
})
