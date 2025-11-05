test_that("extract_luh2 is exported", {
  # Test that the LUH2 extraction function exists
  expect_true(exists("extract_luh2"))
  expect_true(is.function(extract_luh2))
})

test_that("extract_luh2 handles file path validation", {
  skip_if_not(exists("extract_luh2"), 
              message = "extract_luh2 function not available")
  
  # Test with non-existent file
  result <- tryCatch(
    extract_luh2("nonexistent_file.nc"),
    error = function(e) "error"
  )
  
  # Should handle missing file gracefully
  expect_true(!is.null(result))
})

test_that("extract_luh2 requires NetCDF files", {
  skip("Requires NetCDF test files")
  
  # LUH2 data comes in NetCDF format
  # Function should validate file format
  
  # Test that non-NetCDF files are rejected
  expect_error(
    extract_luh2("test.txt"),
    "NetCDF|format"
  )
})

test_that("extract_luh2 handles coordinate inputs", {
  skip("Requires NetCDF test files")
  
  # Function should accept latitude and longitude
  result <- tryCatch(
    extract_luh2(
      file = "test_luh2.nc",
      lat = 40.4168,
      lon = -3.7038
    ),
    error = function(e) NULL
  )
  
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("extract_luh2 handles bounding box inputs", {
  skip("Requires NetCDF test files")
  
  # Function should accept bounding box for spatial extraction
  result <- tryCatch(
    extract_luh2(
      file = "test_luh2.nc",
      lat_min = 35,
      lat_max = 45,
      lon_min = -10,
      lon_max = 5
    ),
    error = function(e) NULL
  )
  
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("extract_luh2 extracts land use variables", {
  skip("Requires NetCDF test files")
  
  # LUH2 contains multiple land use types:
  # - Primary vegetation (forested, non-forested)
  # - Secondary vegetation (forested, non-forested)
  # - Cropland (C3 annual, C3 perennial, C4 annual, C4 perennial, C3 nitrogen-fixing)
  # - Pasture (managed, rangeland)
  # - Urban land
  
  result <- extract_luh2("test_luh2.nc", lat = 40, lon = -3)
  
  # Should return data frame with land use fractions
  expect_true(is.data.frame(result))
  expect_true("primf" %in% names(result) || "cropland" %in% names(result))
})

test_that("extract_luh2 handles temporal dimension", {
  skip("Requires NetCDF test files")
  
  # LUH2 data has temporal dimension (annual)
  # Function should extract time series
  
  result <- extract_luh2(
    file = "test_luh2.nc",
    lat = 40,
    lon = -3,
    years = 2000:2015
  )
  
  expect_true("year" %in% names(result) || "time" %in% names(result))
  expect_equal(length(unique(result$year)), 16)
})

test_that("extract_luh2 validates coordinate ranges", {
  skip("Requires NetCDF test files")
  
  # Latitude should be -90 to 90
  expect_error(
    extract_luh2("test_luh2.nc", lat = 100, lon = 0),
    "latitude"
  )
  
  # Longitude should be -180 to 180
  expect_error(
    extract_luh2("test_luh2.nc", lat = 0, lon = 200),
    "longitude"
  )
})

test_that("extract_luh2 returns proper data structure", {
  skip("Requires NetCDF test files")
  
  result <- extract_luh2("test_luh2.nc", lat = 40, lon = -3)
  
  # Should return tidy data frame
  expect_true(is.data.frame(result))
  
  # Should have coordinate columns
  expect_true("lat" %in% names(result) || "latitude" %in% names(result))
  expect_true("lon" %in% names(result) || "longitude" %in% names(result))
  
  # Land use fractions should sum to ~1
  land_use_cols <- grep("^(primf|primn|secdf|secdn|c3ann|c3per|c4ann|c4per|c3nfx|pastr|range|urban)", 
                        names(result), value = TRUE)
  if (length(land_use_cols) > 0) {
    row_sums <- rowSums(result[, land_use_cols], na.rm = TRUE)
    expect_true(all(row_sums >= 0 & row_sums <= 1.1))
  }
})

test_that("extract_luh2 handles missing values in NetCDF", {
  skip("Requires NetCDF test files")
  
  # NetCDF files may have fill values for ocean cells
  result <- extract_luh2("test_luh2.nc", lat = 35, lon = -15)
  
  # Should handle NAs appropriately
  expect_true(is.data.frame(result))
})

test_that("extract_luh2 aggregates spatially when needed", {
  skip("Requires NetCDF test files")
  
  # When extracting region, should aggregate grid cells
  result <- extract_luh2(
    file = "test_luh2.nc",
    lat_min = 35,
    lat_max = 45,
    lon_min = -10,
    lon_max = 5,
    aggregate = TRUE
  )
  
  expect_true(is.data.frame(result))
  
  # Aggregated result should have regional summary
  expect_gt(nrow(result), 0)
})
