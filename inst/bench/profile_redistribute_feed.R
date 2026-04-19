#' Coarse self-time profiler for redistribute_feed()
#'
#' Uses Rprof to identify hot spots. Run from the repo root:
#'   Rscript inst/bench/profile_redistribute_feed.R

suppressPackageStartupMessages({
  library(afsetools)
  library(dplyr)
  library(tibble)
})

# Load fixture-building helpers from the bench script without running the
# measurement section. The bench script is self-contained — we just source
# it and then discard its measurement output.
bench_env <- new.env()
sys.source("inst/bench/bench_redistribute_feed.R", envir = bench_env)
Feed_demand <- bench_env$Feed_demand
Feed_avail  <- bench_env$Feed_avail

# Make sure package env objects are set
assign("items_full",  bench_env$items_full,  envir = .GlobalEnv)
assign("Cats",        bench_env$Cats,        envir = .GlobalEnv)
assign("Monogastric", bench_env$monogastric, envir = .GlobalEnv)

prof_file <- tempfile(fileext = ".out")
Rprof(prof_file, interval = 0.005)
result <- redistribute_feed(Feed_demand = Feed_demand,
                            Feed_avail = Feed_avail,
                            verbose = FALSE)
Rprof(NULL)

s <- summaryRprof(prof_file)
cat("\n=== Top self-time functions ===\n")
print(head(s$by.self[order(-s$by.self$self.time), ], 20))

cat("\n=== Top total-time functions ===\n")
print(head(s$by.total[order(-s$by.total$total.time), ], 20))

cat("\n=== Sampling summary ===\n")
print(s$sampling.time)
