# Run this script in R to generate documentation
# This will create .Rd files in man/ directory from roxygen2 comments

# Install required packages if needed
if (!require(roxygen2, quietly = TRUE)) {
  install.packages("roxygen2")
}
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
}

# Set working directory to package root
# setwd("path/to/afsetools")  # Uncomment and adjust if needed

# Generate documentation from roxygen2 comments
cat("Generating documentation...\n")
roxygen2::roxygenise()

cat("\nDocumentation generated successfully!\n")
cat("Now commit and push the man/ directory:\n")
cat("  git add man/\n")
cat("  git commit -m 'Add generated documentation files'\n")
cat("  git push\n")