# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(NMdata)

# Set testthat edition 2 (classic behavior)
# testthat::edition_set(2)

options(testthat.deprecation = FALSE)

# Run all tests in the testthat directory
test_dir <- system.file("tests/testthat", package = "NMdata", mustWork = FALSE)
if (length(test_dir) == 0) {
  # fallback for development
  test_dir <- "tests/testthat"
}


test_check("NMdata")
