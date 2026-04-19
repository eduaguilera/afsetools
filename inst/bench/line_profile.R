#' Line-level profile using devtools::load_all so Rprof can see line info.
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(devtools)
})
devtools::load_all(".", quiet = TRUE)

bench_env <- new.env()
sys.source("inst/bench/bench_redistribute_feed.R", envir = bench_env)
Feed_demand <- bench_env$Feed_demand
Feed_avail  <- bench_env$Feed_avail

pf <- tempfile(fileext = ".out")
Rprof(pf, interval = 0.005, line.profiling = TRUE)
redistribute_feed(Feed_demand, Feed_avail, verbose = FALSE) -> .null
Rprof(NULL)

s <- summaryRprof(pf, lines = "show")
cat("\n=== Top self-time lines ===\n")
print(head(s$by.line[order(-s$by.line$self.time), ], 25))
