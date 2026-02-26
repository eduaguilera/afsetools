#' ggplot2 Themes for Scientific Plots
#'
#' Custom ggplot2 themes for creating publication-ready scientific figures.

#' Custom ggplot2 Theme
#'
#' A clean theme for scientific plots with white background and minimal grid lines.
#'
#' @param base_size Base font size. Default is 7.
#' @param base_family Base font family. Default is "".
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_new()
#' }
theme_new <- function(base_size = 7, base_family = "") {
  ggplot2::theme(
    axis.line = ggplot2::element_line(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 7),
    strip.background = ggplot2::element_rect(colour = "#FFFFFF", fill = "#FFFFFF"),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 7),
    axis.title.y = ggplot2::element_text(size = 7),
    axis.title.x = ggplot2::element_text(size = 7),
    axis.text = ggplot2::element_text(size = 7, color = "black")
  )
}

#' ggplot2 Theme Without Labels
#'
#' A theme similar to theme_new() but with strip text removed, useful for multi-panel plots.
#'
#' @param base_size Base font size. Default is 7.
#' @param base_family Base font family. Default is "".
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_nolabel()
#' }
theme_nolabel <- function(base_size = 7, base_family = "") {
  ggplot2::theme(
    axis.line = ggplot2::element_line(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour = "#FFFFFF", fill = "#FFFFFF"),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 7),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 7),
    axis.text = ggplot2::element_text(size = 7, color = "black")
  )
}

#' Plot Linear Regression with Statistics
#'
#' Creates a scatter plot with linear regression line and displays model statistics
#' (R², intercept, slope, and p-value) in the title.
#'
#' @param fit A linear model object created with lm()
#'
#' @return A ggplot2 plot object
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt, data = mtcars)
#' ggplotRegression(model)
#' }
ggplotRegression <- function(fit) {
  ggplot2::ggplot(fit$model, ggplot2::aes(
    x = .data[[names(fit$model)[2]]],
    y = .data[[names(fit$model)[1]]]
  )) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(method = "lm", col = "red") +
    ggplot2::labs(title = paste(
      "Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
      "Intercept =", signif(fit$coef[[1]], 5),
      " Slope =", signif(fit$coef[[2]], 5),
      " P =", signif(summary(fit)$coef[2, 4], 5)
    ))
}
