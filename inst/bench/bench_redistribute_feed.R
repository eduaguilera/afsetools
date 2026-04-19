#' Benchmark harness for redistribute_feed()
#'
#' Generates a realistic-sized synthetic fixture (many years x provinces x
#' items x livestock categories) and measures wall time + peak memory.
#' Saves results to inst/bench/results.csv so successive runs can be compared.
#'
#' Run from the repo root with:
#'   Rscript inst/bench/bench_redistribute_feed.R [label]
#'
#' Where `label` tags the row in results.csv (default: Sys.time()).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})
# Use the sources on disk (not the installed package) so fresh edits are
# reflected in the benchmark without a devtools::install cycle.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(afsetools)
}

set.seed(1)

# --- Fixture dimensions -----------------------------------------------------
n_years      <- 10
n_provinces  <- 20
items_crop   <- c("Barley", "Maize (corn)", "Wheat", "Oats", "Rye",
                  "Rice", "Sorghum", "Millet", "Pulses", "Soya_cake",
                  "Rape_cake", "Sunflower_cake", "Bran", "Molasses",
                  "Silage", "Hay", "Straw", "Acorns", "Grassland")
items_zoot   <- c("Milk_feed", "Fish_oil")
items_all    <- c(items_crop, items_zoot)
livestock    <- c("Cattle_milk", "Cattle_meat", "Sheep", "Goats",
                  "Pigs", "Poultry", "Other_birds", "Aquaculture",
                  "Fur animals", "Horses")
monogastric  <- c("Pigs", "Poultry", "Other_birds",
                  "Fur animals", "Other", "Pets", "Aquaculture")

cat_feed_map <- c(
  "Barley" = "High_quality", "Maize (corn)" = "High_quality",
  "Wheat" = "High_quality", "Oats" = "Low_quality", "Rye" = "High_quality",
  "Rice" = "High_quality", "Sorghum" = "Low_quality",
  "Millet" = "Low_quality", "Pulses" = "High_quality",
  "Soya_cake" = "High_quality", "Rape_cake" = "Low_quality",
  "Sunflower_cake" = "Low_quality", "Bran" = "Residues",
  "Molasses" = "Residues", "Silage" = "Low_quality",
  "Hay" = "Low_quality", "Straw" = "Residues",
  "Acorns" = "Grass", "Grassland" = "Grass",
  "Milk_feed" = "Zoot_fixed", "Fish_oil" = "Zoot_fixed"
)

feedtype <- c(
  "Barley" = "cereal", "Maize (corn)" = "cereal", "Wheat" = "cereal",
  "Oats" = "cereal", "Rye" = "cereal", "Rice" = "cereal",
  "Sorghum" = "cereal", "Millet" = "cereal", "Pulses" = "legume",
  "Soya_cake" = "oilcake", "Rape_cake" = "oilcake",
  "Sunflower_cake" = "oilcake", "Bran" = "byproduct",
  "Molasses" = "byproduct",
  "Silage" = NA_character_, "Hay" = NA_character_,
  "Straw" = NA_character_, "Acorns" = NA_character_,
  "Grassland" = NA_character_,
  "Milk_feed" = NA_character_, "Fish_oil" = NA_character_
)

items_full <- tibble(
  item_cbs = items_all,
  Cat_1 = items_all,
  feedtype_graniv = feedtype[items_all]
)
Cats <- tibble(
  Cat_1 = items_all,
  Cat_feed = cat_feed_map[items_all]
)
assign("items_full", items_full, envir = .GlobalEnv)
assign("Cats", Cats, envir = .GlobalEnv)
assign("Monogastric", monogastric, envir = .GlobalEnv)

years     <- seq_len(n_years) + 2009L
provinces <- sprintf("P%02d", seq_len(n_provinces))

demand_grid <- expand.grid(
  Year = years, Sub_territory = provinces,
  Livestock_cat = livestock, item_cbs = items_all,
  stringsAsFactors = FALSE
)
# Subsample to ~60% to create realistic sparsity
demand_grid <- demand_grid[runif(nrow(demand_grid)) < 0.6, , drop = FALSE]

Feed_demand <- demand_grid |>
  mutate(
    Territory = "T1",
    Cat_1 = item_cbs,
    Cat_feed = cat_feed_map[item_cbs],
    demand_MgDM = round(runif(dplyr::n(), 5, 500), 2),
    fixed_demand = TRUE
  )

avail_grid_prov <- expand.grid(
  Year = years, Sub_territory = provinces,
  item_cbs = items_all, stringsAsFactors = FALSE
)
avail_grid_prov <- avail_grid_prov[runif(nrow(avail_grid_prov)) < 0.5, , drop = FALSE]

Feed_avail_prov <- avail_grid_prov |>
  mutate(
    Territory = "T1",
    Cat_1 = item_cbs,
    Cat_feed = cat_feed_map[item_cbs],
    Avail_MgDM = round(runif(dplyr::n(), 10, 1200), 2)
  )

# National availability for a handful of items
avail_grid_nat <- expand.grid(
  Year = years, Sub_territory = NA_character_,
  item_cbs = c("Soya_cake", "Rape_cake", "Sunflower_cake", "Maize (corn)"),
  stringsAsFactors = FALSE
)
Feed_avail_nat <- avail_grid_nat |>
  mutate(
    Territory = "T1",
    Cat_1 = item_cbs,
    Cat_feed = cat_feed_map[item_cbs],
    Avail_MgDM = round(runif(dplyr::n(), 500, 5000), 2)
  )

Feed_avail <- bind_rows(Feed_avail_prov, Feed_avail_nat)

cat(sprintf(
  "Fixture: demand = %d rows, avail = %d rows (prov = %d, nat = %d)\n",
  nrow(Feed_demand), nrow(Feed_avail),
  nrow(Feed_avail_prov), nrow(Feed_avail_nat)
))

# --- Measure ----------------------------------------------------------------
label <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(label) || !nzchar(label)) label <- format(Sys.time(), "%Y%m%d-%H%M%S")

gc(reset = TRUE, full = TRUE)
t0 <- Sys.time()
result <- redistribute_feed(
  Feed_demand = Feed_demand,
  Feed_avail = Feed_avail,
  verbose = FALSE
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
gc_info <- gc()
# gc() returns a matrix; max-used column is in Mb for Ncells/Vcells rows
m1 <- sum(gc_info[, "max used"])
m0 <- 0

cat(sprintf("\n[%s] elapsed = %.2fs, gc_used_delta = %.1f MB, result = %d rows\n",
            label, elapsed, (m1 - m0), nrow(result)))

# --- Record -----------------------------------------------------------------
results_path <- file.path("inst", "bench", "results.csv")
row <- data.frame(
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  label = label,
  demand_rows = nrow(Feed_demand),
  avail_rows = nrow(Feed_avail),
  result_rows = nrow(result),
  elapsed_s = round(elapsed, 2),
  gc_mb = round(m1 - m0, 1)
)
if (file.exists(results_path)) {
  old <- read.csv(results_path, stringsAsFactors = FALSE)
  row <- rbind(old, row)
}
write.csv(row, results_path, row.names = FALSE)
cat("Results appended to ", results_path, "\n", sep = "")
