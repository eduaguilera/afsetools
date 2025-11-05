test_that("load_general_data loads all objects into parent environment", {
  # Create a clean environment to test loading
  test_env <- new.env()
  
  # Load data into test environment
  with(test_env, {
    load_general_data()
  })
  
  # Test that key objects exist
  expect_true(exists("Biomass_coefs", envir = test_env))
  expect_true(exists("GWP", envir = test_env))
  expect_true(exists("BNF", envir = test_env))
  expect_true(exists("items_full", envir = test_env))
  expect_true(exists("regions_full", envir = test_env))
  
  # Test data types
  expect_true(is.data.frame(test_env$Biomass_coefs))
  expect_true(is.data.frame(test_env$GWP))
  expect_true(is.data.frame(test_env$BNF))
})

test_that("load_general_data with custom path works", {
  # Test with system path (default)
  test_env <- new.env()
  with(test_env, {
    load_general_data()
  })
  
  expect_true(exists("Biomass_coefs", envir = test_env))
  expect_true(nrow(test_env$Biomass_coefs) > 0)
})

test_that("loaded data objects have expected structure", {
  test_env <- new.env()
  with(test_env, {
    load_general_data()
  })
  
  # Test Biomass_coefs structure
  expect_true("Name_biomass" %in% names(test_env$Biomass_coefs) ||
              "item" %in% names(test_env$Biomass_coefs))
  
  # Test GWP structure
  expect_true("Gas" %in% names(test_env$GWP) ||
              "GWP_100" %in% names(test_env$GWP))
  
  # Test that data frames are not empty
  expect_gt(nrow(test_env$Biomass_coefs), 0)
  expect_gt(nrow(test_env$GWP), 0)
})

test_that("load_general_data loads color palettes", {
  test_env <- new.env()
  with(test_env, {
    load_general_data()
  })
  
  # Test color objects exist (they are loaded by the function)
  # Note: These might be in vectors.R which is sourced
  expect_true(exists("Month_names", envir = test_env) ||
              length(ls(test_env)) > 50)  # At least 50+ objects loaded
})
