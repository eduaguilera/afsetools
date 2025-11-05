# Script to generate documentation from roxygen2 comments
# Run this in R to create man/ files and update NAMESPACE

# Install required packages if needed
if (!require(roxygen2, quietly = TRUE)) {
  install.packages("roxygen2")
}
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
}

# Generate documentation
roxygen2::roxygenise()

# Optional: Create pkgdown site for web documentation
# if (!require(pkgdown, quietly = TRUE)) {
#   install.packages("pkgdown")
# }
# pkgdown::build_site()