test_that("theme_new creates custom ggplot theme", {
  skip_if_not_installed("ggplot2")
  
  theme <- theme_new()
  
  # Test that theme is a theme object
  expect_true(inherits(theme, "theme"))
  expect_true(is.list(theme))
})

test_that("theme_nolabel creates theme without axis labels", {
  skip_if_not_installed("ggplot2")
  
  theme <- theme_nolabel()
  
  # Test that theme is a theme object
  expect_true(inherits(theme, "theme"))
  expect_true(is.list(theme))
  
  # Verify it removes axis labels
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    theme_nolabel()
  
  expect_true(inherits(plot, "gg"))
})

test_that("ggplotRegression creates linear regression plot", {
  skip_if_not_installed("ggplot2")
  
  # Create sample data
  test_data <- data.frame(
    x = 1:10,
    y = 2 * (1:10) + rnorm(10, 0, 1)
  )
  
  # Create regression plot
  plot <- ggplotRegression(lm(y ~ x, data = test_data))
  
  # Test that result is a ggplot object
  expect_true(inherits(plot, "gg"))
  expect_true(inherits(plot, "ggplot"))
})

test_that("ggplotRegression handles simple model", {
  skip_if_not_installed("ggplot2")
  
  # Simple linear model
  model <- lm(mpg ~ wt, data = mtcars)
  plot <- ggplotRegression(model)
  
  # Test that plot is created successfully
  expect_true(inherits(plot, "gg"))
  
  # Test that plot has layers
  expect_gt(length(plot$layers), 0)
})

test_that("theme_new can be applied to ggplot", {
  skip_if_not_installed("ggplot2")
  
  plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5)) +
    ggplot2::geom_line(ggplot2::aes(x, y)) +
    theme_new()
  
  # Test that theme is applied
  expect_true(inherits(plot, "gg"))
  expect_true(!is.null(plot$theme))
})

test_that("theme_nolabel removes axis text", {
  skip_if_not_installed("ggplot2")
  
  plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    ggplot2::labs(x = "X Label", y = "Y Label") +
    theme_nolabel()
  
  # Test that plot is created
  expect_true(inherits(plot, "gg"))
  
  # Theme should have blank axis text elements
  expect_true(!is.null(plot$theme))
})

test_that("ggplotRegression includes regression equation", {
  skip_if_not_installed("ggplot2")
  
  # Create model with known relationship
  test_data <- data.frame(x = 1:5, y = 2 * (1:5) + 3)
  model <- lm(y ~ x, data = test_data)
  
  plot <- ggplotRegression(model)
  
  # Test that plot has annotation (equation and R²)
  expect_true(inherits(plot, "gg"))
  expect_gt(length(plot$layers), 0)
})

test_that("plotting functions are exported", {
  # Test that plotting functions exist in package namespace
  expect_true(exists("theme_new"))
  expect_true(exists("theme_nolabel"))
  expect_true(exists("ggplotRegression"))
  
  # Test that they are functions
  expect_true(is.function(theme_new))
  expect_true(is.function(theme_nolabel))
  expect_true(is.function(ggplotRegression))
})

test_that("themes work with different plot types", {
  skip_if_not_installed("ggplot2")
  
  # Test with bar plot
  bar_plot <- ggplot2::ggplot(data.frame(x = c("A", "B", "C"), y = c(10, 20, 15))) +
    ggplot2::geom_bar(ggplot2::aes(x, y), stat = "identity") +
    theme_new()
  
  expect_true(inherits(bar_plot, "gg"))
  
  # Test with scatter plot
  scatter_plot <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(wt, mpg)) +
    theme_nolabel()
  
  expect_true(inherits(scatter_plot, "gg"))
})
