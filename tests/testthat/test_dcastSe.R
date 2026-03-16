test_that("dcastSe works with basic l and r arguments", {
    # Create test data in long format
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    # Cast to wide format
     result <- dcastSe(dt, l = "id", r = "time", value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 3)
    expect_true(all(c("id", "0", "1") %in% names(result)))
})

test_that("dcastSe works with only l specified", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    # Only specify l, r should be derived
    result <- dcastSe(dt, l = "id", value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 3)
    expect_true("id" %in% names(result))
})

test_that("dcastSe works with only r specified", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    # Only specify r, l should be derived
    result <- dcastSe(dt, r = "time", value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_true(all(c("0", "1") %in% names(result)))
})

test_that("dcastSe works with multiple l variables", {
    dt <- data.table(
        id = rep(1:2, each = 4),
        group = rep(c("A", "B"), each = 2, times = 2),
        time = rep(c(0, 1), 4),
        value = 1:8
    )
    
    result <- dcastSe(dt, l = c("id", "group"), r = "time", value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 4)
    expect_true(all(c("id", "group", "0", "1") %in% names(result)))
})

test_that("dcastSe works with multiple r variables", {
    dt <- data.table(
        id = rep(1:2, each = 4),
        var = rep(c("A", "B"), 4),
        time = rep(c(0, 1), each = 2, times = 2),
        value = 1:8
    )
    
    result <- dcastSe(dt, l = "id", r = c("var", "time"), value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 2)
    expect_true("id" %in% names(result))
})

test_that("dcastSe converts data.frame to data.table", {
    df <- data.frame(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    result <- dcastSe(df, l = "id", r = "time", value.var = "value")
    
    expect_s3_class(result, "data.table")
})

test_that("dcastSe passes additional arguments to dcast", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    # Use fun.aggregate argument
    result <- dcastSe(dt, l = "id", r = "time", value.var = "value", fun.aggregate = sum)
    
    expect_s3_class(result, "data.table")
})

test_that("dcastSe errors when both l and r are missing with value.var function", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    expect_error(
        dcastSe(dt, value.var = sum),
        "both l and r must be provided"
    )
})

test_that("dcastSe errors when l and r are missing without value.var", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    expect_error(
        dcastSe(dt)
,
        "When value.var is not provided, both l and r must be provided."
    )
})

test_that("dcastSe respects as.fun argument", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    
    # Test with as.fun = "data.frame"
    result <- dcastSe(dt, l = "id", r = "time", value.var = "value", as.fun = as.data.frame)
    
    expect_s3_class(result, "data.frame")
})

test_that("dcastSe by default returns df when given df", {
    dt <- data.table(
        id = rep(1:3, each = 2),
        time = rep(c(0, 1), 3),
        value = 1:6
    )
    df <- as.data.frame(dt)
    # Test with as.fun = "data.frame"
    result <- dcastSe(df, l = "id", r = "time", value.var = "value")
    
    expect_s3_class(result, "data.frame")
})



test_that("dcastSe handles empty l correctly", {
    dt <- data.table(
        time = rep(c(0, 1), 3),
        var = rep(c("A", "B", "C"), each = 2),
        value = 1:6
    )
    
    result <- dcastSe(dt, r = c("var"), value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 2)
})

test_that("dcastSe formula construction works correctly", {
    dt <- data.table(
        id = rep(1:2, each = 2),
        group = rep(c("A", "B"), 2),
        time = rep(c(0, 1), 2),
        value = 1:4
    )
    
    # Multiple l variables should be joined with +
    result <- dcastSe(dt, l = c("id", "group"), r = "time", value.var = "value")
    
    expect_s3_class(result, "data.table")
    expect_true(all(c("id", "group") %in% names(result)))
})
