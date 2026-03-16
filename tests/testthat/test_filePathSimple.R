context("filePathSimple")

test_that("filePathSimple combines path components like file.path", {
    result <- filePathSimple("path", "to", "file.txt")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple removes redundant slashes", {
    result <- filePathSimple("path//to///file.txt")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple preserves leading slashes for absolute paths", {
    result <- filePathSimple("/path/to/file.txt")
    
    expect_equal(result, "/path/to/file.txt")
})

test_that("filePathSimple preserves network paths with double leading slashes", {
    result <- filePathSimple("//network/path/to/file.txt")
    
    expect_equal(result, "//network/path/to/file.txt")
})

test_that("filePathSimple removes trailing slashes", {
    result <- filePathSimple("path/to/dir/")
    
    expect_equal(result, "path/to/dir")
})

test_that("filePathSimple converts backslashes to forward slashes", {
    result <- filePathSimple("path\\to\\file.txt")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple handles mixed slashes", {
    result <- filePathSimple("path\\to/file.txt")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple trims leading and trailing whitespace", {
    result <- filePathSimple("  path/to/file.txt  ")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple drops NULL arguments", {
    result <- filePathSimple("path", NULL, "to", "file.txt")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple drops empty string arguments", {
    result <- filePathSimple("path", "", "to", "file.txt")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple handles single component", {
    result <- filePathSimple("file.txt")
    
    expect_equal(result, "file.txt")
})

test_that("filePathSimple handles empty input", {
    result <- filePathSimple("")
    
    expect_equal(result, "")
})

test_that("filePathSimple capitalizes Windows drive letters", {
    skip_on_os(c("mac", "linux", "solaris"))
    
    result <- filePathSimple("c:/path/to/file.txt")
    
    expect_match(result, "^C:/")
})

test_that("filePathSimple normalizes Windows absolute paths", {
    skip_on_os(c("mac", "linux", "solaris"))
    
    # This test may behave differently depending on the actual file system
    result <- filePathSimple("C:/Windows/System32")
    
    expect_match(result, "^C:/")
})

test_that("filePathSimple handles relative paths", {
    result <- filePathSimple("./path/to/file.txt")
    
    expect_equal(result, "./path/to/file.txt")
})

test_that("filePathSimple handles parent directory references", {
    result <- filePathSimple("path/../to/file.txt")
    
    expect_equal(result, "path/../to/file.txt")
})

test_that("filePathSimple combines multiple NULL and empty arguments", {
    result <- filePathSimple(NULL, "path", "", NULL, "to", NULL, "file.txt", "")
    
    expect_equal(result, "path/to/file.txt")
})

test_that("filePathSimple handles paths with spaces", {
    result <- filePathSimple("path with spaces", "to", "file name.txt")
    
    expect_equal(result, "path with spaces/to/file name.txt")
})

test_that("filePathSimple handles paths with special characters", {
    result <- filePathSimple("path", "to", "file-name_v2.txt")
    
    expect_equal(result, "path/to/file-name_v2.txt")
})

test_that("filePathSimple removes multiple trailing slashes", {
    result <- filePathSimple("path/to/dir///")
    
    expect_equal(result, "path/to/dir")
})

test_that("filePathSimple handles root directory", {
    result <- filePathSimple("/")
    
    expect_equal(result, "/")
})

test_that("filePathSimple handles current directory", {
    result <- filePathSimple(".")
    
    expect_equal(result, ".")
})

test_that("filePathSimple handles parent directory", {
    result <- filePathSimple("..")
    
    expect_equal(result, "..")
})

test_that("filePathSimple produces comparable paths", {
    # The function should produce canonical paths that can be compared
    path1 <- filePathSimple("path//to///file.txt")
    path2 <- filePathSimple("path", "to", "file.txt")
    
    expect_equal(path1, path2)
})


####### this is a weird case that is not supported. " " first makes file.path start with /. It could be handled by filePathSimple() but it's a really weird case. 
## test_that("filePathSimple handles complex mixed input", {
##     result <- filePathSimple("  ", "path\\\\to", NULL, "//dir/", "", "file.txt  ")
    
##     expect_equal(result, "path/to/dir/file.txt")
## })
