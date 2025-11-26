# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
# Load afsetools without conflict warnings
# The package exports functions that may appear to mask data objects when
# load_general_data() is called, but this is expected and harmless
library(afsetools, warn.conflicts = FALSE)

test_check("afsetools")
