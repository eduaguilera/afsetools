#' Redistribute available supply among Livestock_cat based on
#' their demand
#'
#' @description
#' `redistribute_feed()` matches livestock feed demand to
#' available feed items through a hierarchical allocation that
#' follows the remaining-share principle to avoid exceeding
#' availability.The redistribution path adapts to the
#' `fixed_demand` column in the demand table.
#' `fixed_demand = TRUE` uses a six-level hierarchy that
#' guarantees all demand of each
#' Livestock_cat-Province_name-Year group is met (by resorting
#' to provincial grassland), while `fixed_demand = FALSE` uses
#' a five-level hierarchy that honours observed availability and
#' can exceed the original demand when supply is abundant.
#'
#' The modes operate in each year following hierarchy levels in
#' order:
#'
#' * **Fixed demand (`fixed_demand = TRUE`)** -- six levels:
#'   1. Item-level matches (provincial then national)
#'   2. Substitution within `Cat_1` (prov. then national)
#'   3. Substitution within `Cat_feed` (prov. then national)
#'   4. Inter-provincial trade of Provincial items, excluding
#'      those in `Cat_feed == "Grass"`
#'   5. Substitution within all remaining non-grassland
#'      availability (provincial then national)
#'   6. Fulfil any residual demand with unlimited provincial
#'      grassland
#'
#' * **Availability-driven (`fixed_demand = FALSE`)** -- five
#'   levels:
#'   1. Item-level matches (provincial then national)
#'   2. Substitution within `Cat_1` (prov. then national)
#'   3. Substitution within `Cat_feed` (prov. then national)
#'   4. Substitution within all remaining non-grassland
#'      availability (provincial then national)
#'   5. Distribute any surplus availability of each `item_cbs`
#'      across all Year-Province-Livestock demand combinations
#'      proportionally to their demand, respecting the
#'      provincial or national nature of the item. Intake can
#'      exceed the original demand at this stage.
#'
#' The function relies on the environment objects `items_full`,
#' `Cats`, and optionally `Monogastric` (character vector of
#' monogastric livestock categories). Call
#' `load_general_data()` before using this function.
#'
#' @param Feed_demand A data frame with columns `Year`,
#'   `Livestock_cat`, `item_cbs`, `Cat_1`, `Cat_feed`,
#'   `demand_MgDM`, and `fixed_demand` (logical), plus the
#'   user-selected territory columns indicated by
#'   `territory_col` and `sub_territory_col`. The
#'   `fixed_demand` column controls the redistribution mode
#'   row-wise for each demand record, allowing mixed fixed and
#'   availability-driven livestock categories within the same
#'   `Year x Territory` combination.
#' @param Feed_avail A data frame with columns `Year`,
#'   `item_cbs`, `Cat_1`, `Cat_feed`, `Avail_MgDM`, plus the
#'   user-selected territory columns indicated by
#'   `territory_col` and `sub_territory_col`.
#'   `Feed_scale` is inferred in the function from
#'   `sub_territory_col` (`"Provincial"` when present,
#'   `"National"` otherwise). The table must be
#'   pre-aggregated at the item level.
#' @param zoot_fixed_max_multiplier Numeric multiplier
#'   (default 3) controlling the maximum ratio of intake to
#'   availability for Zoot_fixed items. Applied **per
#'   Livestock_cat** row: each demand row's intake is bounded by
#'   `zoot_fixed_max_multiplier * Avail_MgDM`, independently of
#'   other Livestock_cats competing for the same
#'   `(Year, Territory, Province, item_cbs)` pool. When
#'   availability of Zoot_fixed is zero or NA, no cap is applied
#'   and intake equals demand.
#' @param prioritize_monogastric Logical (default `TRUE`). When
#'   `TRUE`, monogastric demand (with a defined
#'   `feedtype_graniv`) is allocated first through the primary
#'   hierarchy, so monogastrics get first pick on high-priority
#'   feed before ruminants. When `FALSE`, monogastric and
#'   ruminant demand are allocated in a single combined pass;
#'   monogastrics receive no preferential access.
#' @param territory_col Character scalar indicating the broad
#'   territory column name shared by `Feed_demand` and
#'   `Feed_avail`. Redistribution boundaries are defined by
#'   `Year x territory_col`.
#' @param sub_territory_col Character scalar indicating the
#'   local territory column name shared by `Feed_demand` and
#'   `Feed_avail` (e.g., provinces).
#' @param verbose Logical (default `TRUE`). When `TRUE`,
#'   prints stage-by-stage diagnostic messages. Set to
#'   `FALSE` for silent operation.
#'
#' @return A tibble with the columns `Year`, the column named
#'   by `territory_col`, the column named by
#'   `sub_territory_col`, `Livestock_cat`, `item_cbs`,
#'   `Cat_1`, `Cat_feed`, `demand_MgDM`, `intake_MgDM`,
#'   `scaling_factor`, `hierarchy_level`,
#'   `original_demand_item`, `original_Province`, and
#'   `fixed_demand`.
#'
#'   `item_cbs` shows the actual item consumed (from
#'   availability), while `original_demand_item` shows what
#'   was originally requested. When substitution occurs, these
#'   differ. `original_Province` shows the province the feed
#'   came from (NA for national items). `Cat_1` is appended at
#'   the end using the lookup table `items_full`, and
#'   `Cat_feed` using the lookup table `Cats`.
#'
#'   `hierarchy_level` values depend on the redistribution
#'   mode:
#'
#'   * Fixed demand (`TRUE`):
#'     1. `"1_item_exact"`
#'     2. `"2_Cat_1_sub"`
#'     3. `"3_Cat_feed_sub"`
#'     4. `"4_inter_prov_trade"`
#'     5. `"5_all_substitute"`
#'     6. `"6_grassland_unlimited"`
#'
#'   * Availability-driven (`FALSE`):
#'     1. `"1_item_exact"`
#'     2. `"2_Cat_1_sub"`
#'     3. `"3_Cat_feed_sub"`
#'     4. `"4_all_substitute"`
#'     5. `"5_surplus_distribution"`
#'
#'   If both modes are present for any demand records in the
#'   same run, the function executes the mixed workflow: fixed
#'   rows can reach `"6_grassland_unlimited"`, while variable
#'   groups receive surplus allocation.
#'
#' @details
#' The allocation follows these rules:
#'
#' 1. Items in `Cat_feed == "Zoot_fixed"` are kept unchanged
#'    (intake equals demand, even if availability is lower),
#'    but capped per Livestock_cat at
#'    `zoot_fixed_max_multiplier * availability` to prevent
#'    extreme violations. When availability is zero or NA, no
#'    cap is applied. If a cap is applied, the excess demand
#'    should be met through other available items following
#'    the hierarchy.
#' 2. When `prioritize_monogastric = TRUE` (default),
#'    monogastric categories are allocated first.
#'    `Monogastric` is a vector present in the environment
#'    defining which Livestock_cat are monogastric. Set the
#'    parameter to `FALSE` to let monogastrics and ruminants
#'    share a single allocation pass with no preferential
#'    ordering.
#' 3. Items without a defined `feedtype_graniv` are ignored
#'    for monogastrics. `feedtype_graniv` is joined from the
#'    `items_full` lookup table.
#' 4. Ruminant Livestock_cat categories are processed with
#'    the remaining availability (skipped when
#'    `prioritize_monogastric = FALSE`).
#' 5. The availability of each item_cbs is redistributed
#'    based on demand, respecting the hierarchy levels and
#'    the provincial/national nature of each item.
#' 6. During levels 1-3, it must be ensured that, if the
#'    availability of a given feed category is higher than
#'    the demand, all the demand should be satisfied before
#'    moving to the next level.
#' 7. During overall redistribution at level 5
#'    (`fixed_demand=TRUE`) or 4 (`fixed_demand=FALSE`),
#'    available items are prioritized according to
#'    `catfeed_priority` (Lactation, High_quality,
#'    Low_quality, Residues, Grass). This should result in
#'    all available high-priority feedstuff being allocated
#'    before lower-priority items are considered.
#'
#' `original_demand_item` always records the item requested
#' in the input demand, even when intake is met by a
#' different item after substitution. The resulting
#' `scaling_factor` is computed per demand row as
#' `sum(intake_MgDM) / demand_MgDM`.
#'
#' When the `max_intake_share` table caps an item for a
#' Livestock_cat, any DM that would exceed the cap is removed
#' from the violating row(s) and redirected to Grassland — the
#' unlimited fallback in this model — so conservation is not
#' broken by the redirect. If Grassland is itself capped for
#' that Livestock_cat, the excess is redirected to the next
#' non-capped item in `items_full`, chosen in the same priority
#' order as the main allocation hierarchy (Lactation /
#' High_quality first, then Low_quality, Residues, Grass). If
#' every item in `items_full` is capped for the Livestock_cat,
#' the excess is dropped and a warning is emitted.
#'
#' @export
#' @importFrom data.table as.data.table data.table setnames fifelse `:=` .SD .N
#'
#' @examples
#' \dontrun{
#' library(afsetools)
#' load_general_data()
#'
#' Feed_intake <- redistribute_feed(
#'   Feed_demand = Feed_demand_ygiac |>
#'     dplyr::mutate(fixed_demand = TRUE),
#'   Feed_avail = Feed_avail_ygi
#' )
#' }
redistribute_feed <- function(
  Feed_demand,
  Feed_avail,
  zoot_fixed_max_multiplier = 3,
  prioritize_monogastric = TRUE,
  territory_col = "Territory",
  sub_territory_col = "Sub_territory",
  verbose = TRUE
) {
  start_time <- Sys.time()
  if (verbose) {
    cat("\n+----------------------------------------------------------+\n")
    cat("|          redistribute_feed() diagnostics                 |\n")
    cat("+----------------------------------------------------------+\n")
    cat("\u25b6 Running redistribute_feed(): allocation engine engaged.\n")
    cat("\u25b6 Diagnostics active: stage-by-stage insights below.\n")
  }
  
  # ============================================================================
  # 1. VALIDATION
  # ============================================================================
  if (!is.numeric(zoot_fixed_max_multiplier) || length(zoot_fixed_max_multiplier) != 1L ||
      !is.finite(zoot_fixed_max_multiplier) || zoot_fixed_max_multiplier < 1) {
    stop("zoot_fixed_max_multiplier must be a finite number >= 1")
  }
  
  Feed_demand <- tibble::as_tibble(Feed_demand)
  Feed_avail <- tibble::as_tibble(Feed_avail)

  demand_sub_col <- dplyr::case_when(
    sub_territory_col %in% names(Feed_demand) ~ sub_territory_col,
    TRUE ~ NA_character_
  )
  avail_sub_col <- dplyr::case_when(
    sub_territory_col %in% names(Feed_avail) ~ sub_territory_col,
    TRUE ~ NA_character_
  )

  if (is.na(demand_sub_col)) {
    stop("Feed_demand must contain sub-territory column '", sub_territory_col, "'.")
  }
  if (is.na(avail_sub_col)) {
    stop("Feed_avail must contain sub-territory column '", sub_territory_col, "'.")
  }

  demand_has_territory <- territory_col %in% names(Feed_demand)
  avail_has_territory <- territory_col %in% names(Feed_avail)

  Feed_demand <- Feed_demand |>
    dplyr::mutate(Province_name = as.character(.data[[demand_sub_col]]))
  if (demand_has_territory) {
    Feed_demand <- Feed_demand |>
      dplyr::mutate(Territory = as.character(.data[[territory_col]]))
  } else {
    Feed_demand <- Feed_demand |>
      dplyr::mutate(Territory = "All_territories")
  }

  Feed_avail <- Feed_avail |>
    dplyr::mutate(Province_name = as.character(.data[[avail_sub_col]]))
  if (avail_has_territory) {
    Feed_avail <- Feed_avail |>
      dplyr::mutate(Territory = as.character(.data[[territory_col]]))
  } else {
    Feed_avail <- Feed_avail |>
      dplyr::mutate(Territory = "All_territories")
  }

  output_sub_territory_col <- sub_territory_col

  required_demand <- c("Year", "Territory", "Province_name", "Livestock_cat", "item_cbs",
                       "Cat_1", "Cat_feed", "demand_MgDM", "fixed_demand")
  missing_demand <- setdiff(required_demand, names(Feed_demand))
  if (length(missing_demand) > 0) {
    stop("Feed_demand is missing columns: ", paste(missing_demand, collapse = ", "))
  }
  
  required_avail <- c("Year", "Territory", "Province_name", "item_cbs", "Cat_1", "Cat_feed", "Avail_MgDM")
  missing_avail <- setdiff(required_avail, names(Feed_avail))
  if (length(missing_avail) > 0) {
    stop("Feed_avail is missing columns: ", paste(missing_avail, collapse = ", "))
  }
  
  if (!is.logical(Feed_demand$fixed_demand)) {
    stop("`fixed_demand` must be logical (TRUE/FALSE) for every row")
  }

  if (any(is.na(Feed_demand$fixed_demand))) {
    stop("`fixed_demand` cannot contain NA values")
  }
  
  has_fixed <- any(Feed_demand$fixed_demand)
  has_variable <- any(!Feed_demand$fixed_demand)
  
  allocation_mode <- dplyr::case_when(
    has_fixed && has_variable ~ "mixed",
    has_fixed ~ "fixed",
    TRUE ~ "variable"
  )
  
  diag_counter <- 1L
  allocations <- list()
  
  catfeed_priority <- c(
    "Lactation" = 1,
    "High_quality" = 1,
    "Low_quality" = 2,
    "Residues" = 3,
    "Grass" = 4
  )
  
  priority_labels <- if (allocation_mode %in% c("fixed", "mixed")) {
    c("1_item_exact", "2_Cat_1_sub", "3_Cat_feed_sub",
      "4_inter_prov_trade", "5_all_substitute", "6_grassland_unlimited")
  } else {
    c("1_item_exact", "2_Cat_1_sub", "3_Cat_feed_sub",
      "4_all_substitute", "5_surplus_distribution")
  }

  # Defaults can be overridden by defining objects in the global environment.
  Monogastric <- get0(
    "Monogastric",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (is.null(Monogastric)) {
    livestock_coefs_path_m <- system.file(
      "extdata", "Livestock_coefs.xlsx",
      package = "afsetools"
    )
    Monogastric <- tryCatch(
      openxlsx::read.xlsx(
        livestock_coefs_path_m,
        sheet = "Monogastric", startRow = 1
      )[[1]],
      error = function(e) {
        warning(
          "Could not read `Monogastric` from ",
          "Livestock_coefs.xlsx, sheet `Monogastric`. ",
          "Using built-in default. Details: ",
          conditionMessage(e), call. = FALSE
        )
        c("Pigs", "Poultry", "Other_birds",
          "Fur animals", "Other", "Pets",
          "Aquaculture")
      }
    )
  }
  Monogastric <- as.character(Monogastric)

  # max_intake_share can be pre-loaded (e.g., via load_general_data()) or read from package extdata.
  max_intake_share <- get0("max_intake_share", envir = .GlobalEnv, inherits = FALSE)
  if (is.null(max_intake_share)) {
    livestock_coefs_path <- system.file("extdata", "Livestock_coefs.xlsx", package = "afsetools")
    max_intake_share <- tryCatch(
      openxlsx::read.xlsx(livestock_coefs_path, sheet = "max_intake", startRow = 1),
      error = function(e) {
        warning(
          "Could not read `max_intake_share` from package extdata (Livestock_coefs.xlsx, sheet `max_intake`). ",
          "Proceeding without max intake caps. Details: ", conditionMessage(e),
          call. = FALSE
        )
        tibble::tibble(
          Livestock_cat = character(),
          item_cbs = character(),
          max_intake_share = numeric()
        )
      }
    )
  }

  # Normalise cap-table schema. Two supported shapes:
  #   - Legacy: (Livestock_cat, item_cbs, max_intake_share). Every row is
  #     treated as a per-item cap (`var = "item_cbs"`, value = item_cbs).
  #   - New: (Livestock_cat, var, var_value, max_intake_share). `var` is
  #     either "item_cbs" (per-item cap) or "Cat_feed" (joint cap across
  #     every item in that Cat_feed for the livestock). `var_value` carries
  #     the Acorns / Grass / ... value to match against result_tbl.
  max_intake_limits <- tibble::as_tibble(max_intake_share)
  if (!"var" %in% names(max_intake_limits)) {
    max_intake_limits$var <- "item_cbs"
  }
  if (!"var_value" %in% names(max_intake_limits)) {
    # Legacy schema or new schema without var_value: fall back to item_cbs.
    if ("item_cbs" %in% names(max_intake_limits)) {
      max_intake_limits$var_value <- max_intake_limits$item_cbs
    } else {
      max_intake_limits$var_value <- NA_character_
    }
  }
  max_intake_limits <- max_intake_limits |>
    dplyr::filter(is.finite(max_intake_share)) |>
    dplyr::mutate(
      Livestock_cat    = as.character(Livestock_cat),
      var              = as.character(var),
      var_value        = as.character(var_value),
      max_intake_share = dplyr::if_else(max_intake_share < 0, 0,
                                        pmin(max_intake_share, 1))
    )
  # Guard against summarize() emitting a warning when no rows are present
  # (empty .by groups → max(NA, na.rm=TRUE) → -Inf + warning).
  max_intake_limits <- if (nrow(max_intake_limits) == 0) {
    tibble::tibble(Livestock_cat = character(),
                   var = character(),
                   var_value = character(),
                   max_intake_share = numeric())
  } else {
    max_intake_limits |>
      dplyr::summarize(
        .by = c("Livestock_cat", "var", "var_value"),
        max_intake_share = max(max_intake_share, na.rm = TRUE)
      )
  }

  pretty_number <- function(x) {
    ifelse(is.finite(x), format(round(x, 3), nsmall = 3, trim = TRUE), "NA")
  }
  
  format_tg <- function(x) {
    paste0(pretty_number(x / 1e6), " Tg")
  }
  
  diag_snapshot <- function(stage_label, note = NULL) {
    # Short-circuit when silent. Computing the per-stage summaries cost ~15 %
    # of total runtime even with verbose = FALSE before this guard.
    if (!verbose) {
      diag_counter <<- diag_counter + 1L
      return(invisible(NULL))
    }

    total_demand <- sum(demand$demand_MgDM, na.rm = TRUE)
    remaining_demand <- sum(demand$remaining, na.rm = TRUE)
    satisfied <- total_demand - remaining_demand
    satisfied_pct <- if (total_demand > 0) satisfied / total_demand * 100 else 100

    hp_avail <- avail |>
      dplyr::filter(Cat_feed %in% c("Lactation", "High_quality")) |>
      dplyr::summarize(total = sum(avail_remaining, na.rm = TRUE)) |>
      dplyr::pull(total)
    hp_avail <- dplyr::coalesce(hp_avail, 0)

    hp_unmet <- demand |>
      dplyr::filter(Cat_feed %in% c("Lactation", "High_quality"), remaining > 1e-6) |>
      dplyr::summarize(total = sum(remaining, na.rm = TRUE)) |>
      dplyr::pull(total)
    hp_unmet <- dplyr::coalesce(hp_unmet, 0)

    top_unmet <- demand |>
      dplyr::filter(remaining > 1e-6) |>
      dplyr::mutate(key = paste(Year, Territory, Province_name, Livestock_cat, sep = " | ")) |>
      dplyr::arrange(dplyr::desc(remaining)) |>
      dplyr::slice_head(n = 3) |>
      dplyr::transmute(msg = sprintf("%s (%.3f Tg)", key, remaining / 1e6)) |>
      dplyr::pull(msg)

    cat(sprintf("\n[Diag %02d] Stage: %s\n", diag_counter, stage_label))
    cat(sprintf("  - Demand satisfied: %s / %s (%.1f%%)\n",
                format_tg(satisfied), format_tg(total_demand), satisfied_pct))
    cat(sprintf("  - Remaining demand: %s | HP unmet: %s | HP avail: %s\n",
                format_tg(remaining_demand), format_tg(hp_unmet), format_tg(hp_avail)))
    if (length(top_unmet)) {
      cat("  - Top unmet groups: ", paste(top_unmet, collapse = "; "), "\n", sep = "")
    }
    if (!is.null(note)) {
      cat("  - Notes: ", note, "\n", sep = "")
    }
    diag_counter <<- diag_counter + 1L
  }
  
  # ============================================================================
  # 2. PREPROCESSING
  # ============================================================================
  demand <- Feed_demand |>
    dplyr::mutate(
      Territory = dplyr::if_else(Territory == "" | Territory == "NA", "All_territories", Territory),
      Province_name = dplyr::if_else(Province_name == "" | Province_name == "NA", NA_character_, Province_name),
      demand_id = dplyr::row_number(),
      original_demand_item = item_cbs,
      original_Province = Province_name,
      remaining = demand_MgDM
    ) |>
    dplyr::left_join(
      items_full |>
        dplyr::select(item_cbs, Cat_1_items = Cat_1, feedtype_graniv),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      Cat_1 = dplyr::coalesce(Cat_1, Cat_1_items),
      Cat_feed = dplyr::coalesce(Cat_feed, Cats$Cat_feed[match(Cat_1, Cats$Cat_1)]),
      is_monogastric = Livestock_cat %in% Monogastric
    ) |>
    dplyr::select(-Cat_1_items)
  
  demand_cap <- demand |>
    dplyr::filter(Cat_feed != "Zoot_fixed") |>
    dplyr::summarize(
      .by = c(Year, Territory, Province_name, Livestock_cat),
      total_demand = sum(demand_MgDM, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      allocated_so_far = 0,
      remaining_cap = total_demand
    )

  # Composite-key index for O(1) lookup/update inside add_alloc(). Without
  # this, every add_alloc call did two full left_joins on demand_cap and
  # demand, which dominated runtime on large fixtures.
  demand_cap_key <- paste(
    demand_cap$Year, demand_cap$Territory,
    demand_cap$Province_name, demand_cap$Livestock_cat,
    sep = "\001"
  )
  demand_cap_idx <- setNames(seq_len(nrow(demand_cap)), demand_cap_key)

  # Pre-bucket demand row indices by (Year, Territory). The priority-pool
  # and surplus loops iterate over (y, t) availability groups and each
  # iteration used to do a full O(|demand|) filter to find the matching
  # rows. The index lets them subset in O(1) — the `demand` table itself is
  # mutated in place (remaining via indexed writes) so row positions are
  # stable across the whole run.
  demand_yt_key <- paste(demand$Year, demand$Territory, sep = "\001")
  demand_yt_idx <- split(seq_along(demand_yt_key), demand_yt_key)
  
  avail <- Feed_avail |>
    dplyr::mutate(
      Territory = dplyr::if_else(Territory == "" | Territory == "NA", "All_territories", Territory),
      Province_name = dplyr::if_else(Province_name == "" | Province_name == "NA", NA_character_, Province_name)
    ) |>
    dplyr::left_join(
      items_full |>
        dplyr::select(item_cbs, Cat_1_items = Cat_1),
      by = "item_cbs"
    ) |>
    dplyr::mutate(
      Cat_1 = dplyr::coalesce(Cat_1, Cat_1_items),
      Cat_feed = dplyr::coalesce(Cat_feed, Cats$Cat_feed[match(Cat_1, Cats$Cat_1)])
    ) |>
    dplyr::select(-Cat_1_items)
  
  if (!"Feed_scale" %in% names(avail)) {
    avail <- avail |>
      dplyr::mutate(Feed_scale = dplyr::if_else(is.na(Province_name), "National", "Provincial"))
  } else {
    avail <- avail |>
      dplyr::mutate(
        Feed_scale = dplyr::case_when(
          tolower(Feed_scale) %in% c("national", "nacional", "import") ~ "National",
          tolower(Feed_scale) %in% c("provincial", "province", "local") ~ "Provincial",
          is.na(Province_name) ~ "National",
          TRUE ~ "Provincial"
        )
      )
  }
  
  avail <- avail |>
    dplyr::mutate(Province_name = dplyr::if_else(Feed_scale == "National", NA_character_, Province_name)) |>
    dplyr::summarize(
      .by = c(Year, Territory, Province_name, Feed_scale, item_cbs, Cat_1, Cat_feed),
      Avail_MgDM = sum(Avail_MgDM, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      avail_id = dplyr::row_number(),
      avail_remaining = Avail_MgDM
    )
  
  diag_snapshot("Inputs validated")
  
  # ============================================================================
  # 3. HELPER FUNCTIONS
  # ============================================================================
  ensure_item_metadata <- function(df) {
    if (!"item_cbs" %in% names(df)) {
      stop("Availability subset must contain `item_cbs` column")
    }
    has_cat1 <- "Cat_1" %in% names(df)
    has_catfeed <- "Cat_feed" %in% names(df)
    if (has_cat1 && has_catfeed) {
      return(df)
    }
    
    df <- df |>
      dplyr::left_join(
        items_full |>
          dplyr::select(item_cbs, Cat_1_lookup = Cat_1),
        by = "item_cbs"
      )
    
    if (has_cat1) {
      df <- df |>
        dplyr::mutate(Cat_1 = dplyr::coalesce(Cat_1, Cat_1_lookup))
    } else {
      df <- df |>
        dplyr::mutate(Cat_1 = Cat_1_lookup)
    }
    
    catfeed_lookup <- Cats$Cat_feed[match(df$Cat_1, Cats$Cat_1)]
    if (has_catfeed) {
      df <- df |>
        dplyr::mutate(Cat_feed = dplyr::coalesce(Cat_feed, catfeed_lookup))
    } else {
      df <- df |>
        dplyr::mutate(Cat_feed = catfeed_lookup)
    }
    
    df |>
      dplyr::select(-dplyr::any_of("Cat_1_lookup"))
  }
  
  # Rebuild the composite cap key for a frame (vectorized).
  build_cap_key <- function(df) {
    paste(df$Year, df$Territory, df$Province_name, df$Livestock_cat,
          sep = "\001")
  }

  add_alloc <- function(df, respect_cap = TRUE) {
    if (is.null(df) || nrow(df) == 0) {
      return(invisible(NULL))
    }
    df <- df[!is.na(df$intake_MgDM) & df$intake_MgDM > 1e-9, , drop = FALSE]
    if (nrow(df) == 0) {
      return(invisible(NULL))
    }

    if (respect_cap) {
      zoot_mask <- df$Cat_feed == "Zoot_fixed"
      uncapped <- df[zoot_mask, , drop = FALSE]
      capped <- df[!zoot_mask, , drop = FALSE]

      if (nrow(capped) > 0) {
        # Pull remaining_cap via the pre-built index instead of a full join.
        cap_keys <- build_cap_key(capped)
        cap_idx <- demand_cap_idx[cap_keys]
        remaining_cap_vec <- demand_cap$remaining_cap[cap_idx]

        # Deterministic tiebreaker: items are processed in ascending Cat_feed
        # priority (Lactation/High_quality first), then by descending
        # intake_MgDM, then by stable demand/avail_id. Without this the
        # cumsum cap depends on arbitrary row order (P1). Base R avoids the
        # dplyr overhead that dominated runtime on small per-call frames.
        tie_priority_vec <- unname(catfeed_priority[capped$Cat_feed])
        tie_priority_vec[is.na(tie_priority_vec)] <- 999
        if ("avail_id" %in% names(capped)) {
          tie_avail_vec <- as.numeric(capped$avail_id)
          tie_avail_vec[is.na(tie_avail_vec)] <- Inf
        } else {
          tie_avail_vec <- rep(Inf, nrow(capped))
        }
        tie_demand_vec <- as.numeric(capped$demand_id)
        tie_demand_vec[is.na(tie_demand_vec)] <- Inf

        ord <- order(capped$Year, capped$Territory, capped$Province_name,
                     capped$Livestock_cat, tie_priority_vec,
                     -capped$intake_MgDM, tie_demand_vec, tie_avail_vec)
        capped <- capped[ord, , drop = FALSE]
        remaining_cap_vec <- remaining_cap_vec[ord]
        cap_keys <- cap_keys[ord]

        # Group-wise cumsum via ave(), then apply the cap.
        cumsum_intake <- stats::ave(capped$intake_MgDM, cap_keys,
                                    FUN = cumsum)
        prev_intake <- cumsum_intake - capped$intake_MgDM
        capped_intake <- ifelse(
          is.na(remaining_cap_vec),
          capped$intake_MgDM,
          pmax(0, pmin(capped$intake_MgDM,
                       remaining_cap_vec - prev_intake))
        )
        capped$intake_MgDM <- capped_intake
        capped <- capped[capped$intake_MgDM > 1e-9, , drop = FALSE]

        if (nrow(capped) > 0) {
          # Aggregate intake by cap key and update demand_cap in place.
          cap_keys2 <- build_cap_key(capped)
          alloc_sum <- rowsum(capped$intake_MgDM,
                              group = cap_keys2, reorder = FALSE)
          uidx <- demand_cap_idx[rownames(alloc_sum)]
          demand_cap$allocated_so_far[uidx] <<-
            demand_cap$allocated_so_far[uidx] + alloc_sum[, 1]
          demand_cap$remaining_cap[uidx] <<- pmax(
            0,
            demand_cap$total_demand[uidx] - demand_cap$allocated_so_far[uidx]
          )
        }
      }

      df <- dplyr::bind_rows(uncapped, capped)

      if (nrow(df) > 0) {
        # demand_id is the row number of `demand` (assigned once at
        # preprocessing; left_join preserves order), so indexed writes are
        # safe. rowsum collapses duplicate demand_ids first.
        d_sum <- rowsum(df$intake_MgDM,
                        group = df$demand_id, reorder = FALSE)
        did <- as.integer(rownames(d_sum))
        demand$remaining[did] <<- pmax(0,
                                        demand$remaining[did] - d_sum[, 1])
      }
    }

    if ("avail_id" %in% names(df)) {
      df_a <- df[!is.na(df$avail_id), , drop = FALSE]
      if (nrow(df_a) > 0) {
        a_sum <- rowsum(df_a$intake_MgDM,
                        group = df_a$avail_id, reorder = FALSE)
        aid <- as.integer(rownames(a_sum))
        avail$avail_remaining[aid] <<- pmax(
          0, avail$avail_remaining[aid] - a_sum[, 1]
        )
      }
    }

    allocations[[length(allocations) + 1]] <<- df
    invisible(NULL)
  }
  
  # data.table implementation: keyed joins are ~3-5x faster than dplyr's
  # inner_join for the repeated summarize+join pattern in primary levels,
  # which dominates runtime on large fixtures.
  allocate_cartesian <- function(demand_subset, avail_subset, group_cols, level_label) {
    if (nrow(demand_subset) == 0 || nrow(avail_subset) == 0) return(NULL)

    dD <- data.table::as.data.table(demand_subset)
    dA <- data.table::as.data.table(avail_subset)
    dD <- dD[remaining > 1e-9]
    dA <- dA[avail_remaining > 1e-9]
    if (nrow(dD) == 0 || nrow(dA) == 0) return(NULL)

    # Metadata guard (mirror ensure_item_metadata semantics).
    if (!("Cat_1" %in% names(dA)) || !("Cat_feed" %in% names(dA))) {
      if (!"Cat_1" %in% names(dA)) {
        dA[, Cat_1 := items_full$Cat_1[match(item_cbs, items_full$item_cbs)]]
      }
      if (!"Cat_feed" %in% names(dA)) {
        dA[, Cat_feed := Cats$Cat_feed[match(Cat_1, Cats$Cat_1)]]
      }
    }

    demand_groups <- dD[, .(demand_group = sum(remaining, na.rm = TRUE)),
                        by = group_cols]
    avail_groups  <- dA[, .(avail_group = sum(avail_remaining, na.rm = TRUE)),
                        by = group_cols]
    matched <- demand_groups[avail_groups, on = group_cols, nomatch = NULL][
      demand_group > 1e-9 & avail_group > 1e-9
    ]
    if (nrow(matched) == 0) return(NULL)

    dw <- dD[matched, on = group_cols, allow.cartesian = TRUE,
             nomatch = NULL]
    dw[, demand_share := remaining / demand_group]

    aw <- dA[matched, on = group_cols, allow.cartesian = TRUE,
             nomatch = NULL]
    aw[, `:=`(avail_share = avail_remaining / avail_group,
              source_Province = Province_name)]

    # Rename item/Cat columns on the availability side that aren't in
    # group_cols, to avoid clobbering the demand-side values on join.
    item_cols <- c("item_cbs", "Cat_1", "Cat_feed")
    rename_cols <- setdiff(item_cols, group_cols)
    for (nm in rename_cols) {
      data.table::setnames(aw, nm, paste0(nm, "_sub"))
    }

    extra_cols <- intersect(paste0(item_cols, "_sub"), names(aw))
    keep_cols  <- unique(c(group_cols, "avail_id", extra_cols,
                           "avail_share", "source_Province"))
    aw_small <- aw[, keep_cols, with = FALSE]

    # Drop demand-side original_Province so the avail-side source_Province
    # carries through cleanly (mirrors dplyr version).
    if ("original_Province" %in% names(dw)) {
      dw[, original_Province := NULL]
    }

    res <- dw[aw_small, on = group_cols, allow.cartesian = TRUE,
              nomatch = NULL]
    res[, `:=`(intake_MgDM = pmin(demand_group, avail_group) *
                  demand_share * avail_share,
               hierarchy_level = level_label)]
    res <- res[intake_MgDM > 1e-9]
    if (nrow(res) == 0) return(NULL)

    out <- data.table::data.table(
      demand_id     = res$demand_id,
      Year          = res$Year,
      Territory     = res$Territory,
      Province_name = res$Province_name,
      Livestock_cat = res$Livestock_cat,
      item_cbs      = if ("item_cbs_sub" %in% names(res))
        data.table::fifelse(is.na(res$item_cbs_sub), res$item_cbs, res$item_cbs_sub)
      else res$item_cbs,
      Cat_1         = if ("Cat_1_sub" %in% names(res))
        data.table::fifelse(is.na(res$Cat_1_sub), res$Cat_1, res$Cat_1_sub)
      else res$Cat_1,
      Cat_feed      = if ("Cat_feed_sub" %in% names(res))
        data.table::fifelse(is.na(res$Cat_feed_sub), res$Cat_feed, res$Cat_feed_sub)
      else res$Cat_feed,
      intake_MgDM          = res$intake_MgDM,
      hierarchy_level      = res$hierarchy_level,
      original_demand_item = res$original_demand_item,
      original_Province    = res$source_Province,
      avail_id             = res$avail_id
    )
    tibble::as_tibble(out)
  }
  
  allocate_grouped <- function(demand_subset, avail_subset, group_cols, level_label) {
    if (nrow(demand_subset) == 0 || nrow(avail_subset) == 0) return(NULL)

    dD <- data.table::as.data.table(demand_subset)
    dA <- data.table::as.data.table(avail_subset)
    dD <- dD[remaining > 1e-9]
    dA <- dA[avail_remaining > 1e-9]
    if (nrow(dD) == 0 || nrow(dA) == 0) return(NULL)

    if (!("Cat_1" %in% names(dA)) || !("Cat_feed" %in% names(dA))) {
      if (!"Cat_1" %in% names(dA)) {
        dA[, Cat_1 := items_full$Cat_1[match(item_cbs, items_full$item_cbs)]]
      }
      if (!"Cat_feed" %in% names(dA)) {
        dA[, Cat_feed := Cats$Cat_feed[match(Cat_1, Cats$Cat_1)]]
      }
    }

    demand_groups <- dD[, .(demand_group = sum(remaining, na.rm = TRUE)),
                        by = group_cols]
    avail_groups  <- dA[, .(avail_group = sum(avail_remaining, na.rm = TRUE)),
                        by = group_cols]
    matched <- demand_groups[avail_groups, on = group_cols, nomatch = NULL][
      demand_group > 1e-9 & avail_group > 1e-9
    ]
    if (nrow(matched) == 0) return(NULL)

    dw <- dD[matched, on = group_cols, allow.cartesian = TRUE,
             nomatch = NULL]
    dw[, `:=`(scale_factor = pmin(1, avail_group / demand_group))]
    dw[, intake_MgDM := remaining * scale_factor]

    item_cols <- c("item_cbs", "Cat_1", "Cat_feed")
    rename_cols <- setdiff(item_cols, group_cols)
    aw <- data.table::copy(dA)
    for (nm in rename_cols) {
      data.table::setnames(aw, nm, paste0(nm, "_sub"))
    }
    aw <- aw[matched, on = group_cols, allow.cartesian = TRUE,
             nomatch = NULL]
    aw[, `:=`(share = avail_remaining / avail_group,
              source_Province = Province_name)]

    extra_cols <- intersect(paste0(item_cols, "_sub"), names(aw))
    keep_cols <- unique(c(group_cols, "avail_id", extra_cols,
                          "share", "source_Province"))
    aw_small <- aw[, keep_cols, with = FALSE]

    if ("original_Province" %in% names(dw)) dw[, original_Province := NULL]

    res <- dw[aw_small, on = group_cols, allow.cartesian = TRUE,
              nomatch = NULL]
    res[, `:=`(intake_MgDM = intake_MgDM * share,
               hierarchy_level = level_label)]
    res <- res[intake_MgDM > 1e-9]
    if (nrow(res) == 0) return(NULL)

    out <- data.table::data.table(
      demand_id     = res$demand_id,
      Year          = res$Year,
      Territory     = res$Territory,
      Province_name = res$Province_name,
      Livestock_cat = res$Livestock_cat,
      item_cbs      = if ("item_cbs_sub" %in% names(res))
        data.table::fifelse(is.na(res$item_cbs_sub), res$item_cbs, res$item_cbs_sub)
      else res$item_cbs,
      Cat_1         = if ("Cat_1_sub" %in% names(res))
        data.table::fifelse(is.na(res$Cat_1_sub), res$Cat_1, res$Cat_1_sub)
      else res$Cat_1,
      Cat_feed      = if ("Cat_feed_sub" %in% names(res))
        data.table::fifelse(is.na(res$Cat_feed_sub), res$Cat_feed, res$Cat_feed_sub)
      else res$Cat_feed,
      intake_MgDM          = res$intake_MgDM,
      hierarchy_level      = res$hierarchy_level,
      original_demand_item = res$original_demand_item,
      original_Province    = res$source_Province,
      avail_id             = res$avail_id
    )
    tibble::as_tibble(out)
  }
  
  # target_mask: logical vector aligned with demand rows (e.g. is_monogastric).
  execute_level <- function(target_mask, avail_filter, group_cols, label) {
    keep <- target_mask & demand$remaining > 1e-9 & demand$Cat_feed != "Zoot_fixed"
    if (!any(keep)) {
      return()
    }
    demand_subset <- demand[keep, , drop = FALSE]
    avail_subset <- avail_filter(avail)
    alloc <- allocate_cartesian(demand_subset, avail_subset, group_cols, label)
    add_alloc(alloc)
  }

  execute_grouped <- function(target_mask, avail_filter, group_cols, label) {
    keep <- target_mask & demand$remaining > 1e-9 & demand$Cat_feed != "Zoot_fixed"
    if (!any(keep)) {
      return()
    }
    demand_subset <- demand[keep, , drop = FALSE]
    avail_subset <- avail_filter(avail)
    alloc <- allocate_grouped(demand_subset, avail_subset, group_cols, label)
    add_alloc(alloc)
  }
  
  # ============================================================================
  # 4. ZOOT_FIXED PROCESSING
  # ============================================================================
  zoot_demand <- demand |>
    dplyr::filter(Cat_feed == "Zoot_fixed")
  
  zoot_avail_prov <- avail |>
    dplyr::filter(Cat_feed == "Zoot_fixed", Feed_scale == "Provincial") |>
    dplyr::select(Year, Territory, Province_name, item_cbs, avail_prov = Avail_MgDM, avail_id_prov = avail_id)
  
  zoot_avail_nat <- avail |>
    dplyr::filter(Cat_feed == "Zoot_fixed", Feed_scale == "National") |>
    dplyr::select(Year, Territory, item_cbs, avail_nat = Avail_MgDM, avail_id_nat = avail_id)
  
  zoot_alloc <- tibble::tibble()
  
  if (nrow(zoot_demand) > 0) {
    zoot_enriched <- zoot_demand |>
      dplyr::left_join(zoot_avail_prov, by = c("Year", "Territory", "Province_name", "item_cbs")) |>
      dplyr::left_join(zoot_avail_nat, by = c("Year", "Territory", "item_cbs"))
    
    zoot_prov <- zoot_enriched |>
      dplyr::filter(!is.na(avail_prov))
    zoot_nat <- zoot_enriched |>
      dplyr::filter(is.na(avail_prov))
    
    # Zoot_fixed cap is applied per demand row (i.e., per Livestock_cat) rather
    # than to the group sum. Each row's intake is bounded by
    # `zoot_fixed_max_multiplier * Avail_MgDM` independently of other
    # Livestock_cats competing for the same (Year, Territory, Province, item).
    zoot_prov_alloc <- tibble::tibble()
    if (nrow(zoot_prov) > 0) {
      zoot_prov_alloc <- zoot_prov |>
        dplyr::mutate(
          cap_per_row = dplyr::case_when(
            is.na(avail_prov) ~ NA_real_,
            avail_prov <= 0 ~ NA_real_,
            TRUE ~ zoot_fixed_max_multiplier * avail_prov
          ),
          intake_MgDM = dplyr::case_when(
            is.na(cap_per_row) ~ demand_MgDM,
            demand_MgDM <= 0 ~ demand_MgDM,
            TRUE ~ pmin(demand_MgDM, cap_per_row)
          ),
          hierarchy_level = priority_labels[1],
          avail_id = avail_id_prov
        ) |>
        dplyr::select(demand_id, Year, Territory, Province_name, Livestock_cat,
                      item_cbs, Cat_1, Cat_feed, intake_MgDM,
                      hierarchy_level, original_demand_item, original_Province,
                      avail_id)
    }

    zoot_nat_alloc <- tibble::tibble()
    if (nrow(zoot_nat) > 0) {
      zoot_nat_alloc <- zoot_nat |>
        dplyr::mutate(
          cap_per_row = dplyr::case_when(
            is.na(avail_nat) ~ NA_real_,
            avail_nat <= 0 ~ NA_real_,
            TRUE ~ zoot_fixed_max_multiplier * avail_nat
          ),
          intake_MgDM = dplyr::case_when(
            is.na(cap_per_row) ~ demand_MgDM,
            demand_MgDM <= 0 ~ demand_MgDM,
            TRUE ~ pmin(demand_MgDM, cap_per_row)
          ),
          hierarchy_level = priority_labels[1],
          avail_id = avail_id_nat
        ) |>
        dplyr::select(demand_id, Year, Territory, Province_name, Livestock_cat,
                      item_cbs, Cat_1, Cat_feed, intake_MgDM,
                      hierarchy_level, original_demand_item, original_Province,
                      avail_id)
    }

    zoot_alloc <- dplyr::bind_rows(zoot_prov_alloc, zoot_nat_alloc)
  }
  
  add_alloc(zoot_alloc)
  
  demand <- demand |>
    dplyr::mutate(
      remaining = dplyr::if_else(Cat_feed == "Zoot_fixed", 0, remaining)
    )
  
  diag_snapshot("After Zoot_fixed", note = sprintf("Allocated %.3f Tg", sum(zoot_alloc$intake_MgDM, na.rm = TRUE) / 1e6))
  
  # ============================================================================
  # 5. PRIMARY LEVELS (Monogastric first, then ruminants)
  # ============================================================================
  run_primary_levels <- function(target_mask, stage_note) {
    if (!any(target_mask)) {
      diag_snapshot(stage_note, note = "No eligible demand")
      return()
    }

    execute_level(target_mask,
                  function(x) dplyr::filter(x, Feed_scale == "Provincial"),
                  c("Year", "Territory", "Province_name", "item_cbs"),
                  priority_labels[1])
    execute_grouped(target_mask,
                    function(x) dplyr::filter(x, Feed_scale == "National"),
                    c("Year", "Territory", "item_cbs"),
                    priority_labels[1])

    execute_level(target_mask,
                  function(x) dplyr::filter(x, Feed_scale == "Provincial"),
                  c("Year", "Territory", "Province_name", "Cat_1"),
                  priority_labels[2])
    execute_grouped(target_mask,
                    function(x) dplyr::filter(x, Feed_scale == "National"),
                    c("Year", "Territory", "Cat_1"),
                    priority_labels[2])

    execute_level(target_mask,
                  function(x) dplyr::filter(x, Feed_scale == "Provincial"),
                  c("Year", "Territory", "Province_name", "Cat_feed"),
                  priority_labels[3])
    execute_grouped(target_mask,
                    function(x) dplyr::filter(x, Feed_scale == "National"),
                    c("Year", "Territory", "Cat_feed"),
                    priority_labels[3])

    diag_snapshot(stage_note)
  }

  # Monogastric rows eligible for the priority hierarchy need a feedtype_graniv;
  # the remaining rows (ruminants + graniv-less monogastric) go through the
  # ruminant pass. When `prioritize_monogastric = FALSE`, the two passes are
  # collapsed into one so monogastric and ruminant demand compete equally for
  # availability rather than monogastric getting first pick.
  mono_mask <- demand$is_monogastric & !is.na(demand$feedtype_graniv)
  if (prioritize_monogastric) {
    run_primary_levels(mono_mask, "After monogastric hierarchy")
    run_primary_levels(!mono_mask, "After ruminant hierarchy")
  } else {
    run_primary_levels(rep(TRUE, nrow(demand)),
                       "After combined hierarchy (no monogastric priority)")
  }
  
  # ============================================================================
  # 6. MODE-SPECIFIC LEVELS
  # ============================================================================
  allocate_trade <- function() {
    trade_block_items <- c("Grassland", "Acorns")
    
    demand_subset <- demand |>
      dplyr::filter(remaining > 1e-9, Cat_feed != "Zoot_fixed", !is.na(Province_name))
    tradeable_avail <- avail |>
      dplyr::filter(
        Feed_scale == "Provincial",
        !(item_cbs %in% trade_block_items),
        avail_remaining > 1e-9,
        !is.na(Province_name)
      ) |>
      dplyr::mutate(
        original_Province = Province_name,
        Province_name = NA_character_
      )
    
    if (nrow(tradeable_avail) == 0 || nrow(demand_subset) == 0) {
      return()
    }
    
    alloc <- allocate_cartesian(
      demand_subset,
      tradeable_avail,
      c("Year", "Territory", "item_cbs"),
      priority_labels[4]
    )
    
    if (is.null(alloc) || nrow(alloc) == 0) {
      return()
    }
    
    origin_lookup <- tradeable_avail |>
      dplyr::select(avail_id, trade_origin = original_Province)
    
    alloc <- alloc |>
      dplyr::left_join(origin_lookup, by = "avail_id")
    
    if (!"original_Province" %in% names(alloc)) {
      alloc <- alloc |>
        dplyr::mutate(original_Province = NA_character_)
    }
    
    alloc <- alloc |>
      dplyr::mutate(
        original_Province = dplyr::coalesce(original_Province, trade_origin)
      ) |>
      dplyr::filter(
        !is.na(original_Province),
        is.na(Province_name) | original_Province != Province_name
      ) |>
      dplyr::select(-trade_origin)
    
    add_alloc(alloc)
  }
  
  allocate_priority_pool <- function(label, cat_feed_filter = NULL, respect_cap = TRUE, allow_trade = FALSE) {
    pool_avail <- avail |>
      dplyr::filter(
        avail_remaining > 1e-9,
        Cat_feed != "Zoot_fixed",
        !(allow_trade & item_cbs %in% c("Grassland", "Acorns"))
      )
    
    if (!is.null(cat_feed_filter)) {
      pool_avail <- pool_avail |> dplyr::filter(cat_feed_filter(Cat_feed))
    }
    
    if (nrow(pool_avail) == 0) {
      return()
    }
    
    pool_avail <- pool_avail |>
      dplyr::mutate(feed_rank = dplyr::coalesce(catfeed_priority[Cat_feed], 999)) |>
      dplyr::arrange(feed_rank, dplyr::desc(avail_remaining))
    
    allocate_supply <- function(current_row, eligible_tbl) {
      supply_left <- current_row$avail_remaining
      if (!is.finite(supply_left) || supply_left <= 1e-9 || nrow(eligible_tbl) == 0) {
        return(NULL)
      }
      
      eligible_tbl <- eligible_tbl |>
        dplyr::mutate(priority_value = dplyr::coalesce(catfeed_priority[Cat_feed], 999))
      
      priority_sequence <- c(1, 2, 3, 4, sort(unique(eligible_tbl$priority_value)))
      priority_sequence <- priority_sequence[!duplicated(priority_sequence)]
      
      outputs <- list()
      
      for (priority_level in priority_sequence) {
        if (supply_left <= 1e-9) {
          break
        }
        
        bucket <- eligible_tbl |>
          dplyr::filter(priority_value == !!priority_level)
        bucket_total <- sum(bucket$remaining, na.rm = TRUE)
        if (bucket_total <= 1e-9) {
          next
        }
        
        alloc_amount <- min(supply_left, bucket_total)
        bucket_alloc <- bucket |>
          dplyr::mutate(intake_MgDM = alloc_amount * (remaining / bucket_total))
        outputs[[length(outputs) + 1L]] <- bucket_alloc
        supply_left <- supply_left - alloc_amount
      }
      
      if (!length(outputs)) {
        return(NULL)
      }
      
      # original_Province tracks the province the feed came from. For a
      # provincial supply row, that is the availability's Province_name; for
      # a national supply row it is NA. Without this the release pass
      # (allow_trade = TRUE) would propagate the demand's own province and
      # lose the trade origin.
      source_province <- if (identical(current_row$Feed_scale, "National")) {
        NA_character_
      } else {
        current_row$Province_name
      }

      dplyr::bind_rows(outputs) |>
        dplyr::filter(intake_MgDM > 1e-9) |>
        dplyr::transmute(
          demand_id,
          Year,
          Territory,
          Province_name,
          Livestock_cat,
          item_cbs = current_row$item_cbs,
          Cat_1 = current_row$Cat_1,
          Cat_feed = current_row$Cat_feed,
          intake_MgDM,
          hierarchy_level = label,
          original_demand_item,
          original_Province = source_province,
          avail_id = current_row$avail_id
        )
    }
    
    # Optimization: Process by Year-Territory groups to minimize demand filtering overhead
    territory_years <- pool_avail |>
      dplyr::distinct(Year, Territory)
    
    for (row_idx in seq_len(nrow(territory_years))) {
      y <- territory_years$Year[row_idx]
      t <- territory_years$Territory[row_idx]
      items_year <- pool_avail |> dplyr::filter(Year == y, Territory == t)

      # O(1) lookup into the pre-built (Year, Territory) bucket.
      yt_rows <- demand_yt_idx[[paste(y, t, sep = "\001")]]
      if (is.null(yt_rows) || length(yt_rows) == 0) next
      demand_subset <- demand[yt_rows, , drop = FALSE]
      demand_subset <- demand_subset[
        demand_subset$remaining > 1e-9 &
          demand_subset$Cat_feed != "Zoot_fixed", ,
        drop = FALSE
      ]

      if (nrow(demand_subset) == 0) next
      
      for (idx in seq_len(nrow(items_year))) {
        current <- items_year[idx, ]
        if (current$avail_remaining <= 1e-9) next
        
        # Filter eligible from demand_subset (fast in-memory subsetting)
        if (!allow_trade && current$Feed_scale == "Provincial") {
          eligible_indices <- which(
            demand_subset$Province_name == current$Province_name &
              demand_subset$Territory == current$Territory &
              demand_subset$remaining > 1e-9
          )
        } else {
          eligible_indices <- which(
            demand_subset$Territory == current$Territory &
              demand_subset$remaining > 1e-9
          )
        }
        
        if (length(eligible_indices) == 0) next
        
        eligible_rows <- demand_subset[eligible_indices, ]
        alloc <- allocate_supply(current, eligible_rows)
        
        if (!is.null(alloc)) {
          add_alloc(alloc, respect_cap = respect_cap)

          # Update local demand_subset to reflect the changes made by
          # add_alloc. `rowsum` collapses duplicate demand_ids in one pass;
          # `stats::aggregate` is ~10x slower per call in this loop.
          agg <- rowsum(alloc$intake_MgDM,
                        group = alloc$demand_id, reorder = FALSE)
          ids <- as.integer(rownames(agg))
          idx_in_subset <- match(ids, demand_subset$demand_id)
          demand_subset$remaining[idx_in_subset] <- pmax(
            0,
            demand_subset$remaining[idx_in_subset] - agg[, 1]
          )
        }
      }
    }
  }
  
  run_release_pass <- function(level_label, diag_label) {
    eligible_demand <- demand |>
      dplyr::filter(remaining > 1e-6, Cat_feed != "Zoot_fixed")
    avail_releasable <- avail |>
      dplyr::filter(avail_remaining > 1e-6, Cat_feed != "Grass", Cat_feed != "Zoot_fixed")
    
    if (nrow(eligible_demand) == 0 || nrow(avail_releasable) == 0) {
      diag_snapshot(paste0(diag_label, " (skip)"),
                    note = "No releasable availability or demand")
      return()
    }
    
    prev_remaining <- sum(eligible_demand$remaining, na.rm = TRUE)
    allocate_priority_pool(level_label, allow_trade = TRUE)
    
    current_remaining <- sum(demand$remaining[demand$Cat_feed != "Zoot_fixed"], na.rm = TRUE)
    reduction <- prev_remaining - current_remaining
    
    diag_snapshot(
      diag_label,
      note = sprintf("Released %.3f Tg", max(reduction, 0) / 1e6)
    )
  }
  
  allocate_grassland <- function(label, only_fixed = FALSE) {
    target_demand <- demand |>
      dplyr::filter(remaining > 1e-9, Cat_feed != "Zoot_fixed")

    if (only_fixed) {
      target_demand <- target_demand |> dplyr::filter(fixed_demand)
    }

    leftover <- target_demand |>
      dplyr::mutate(
        intake_MgDM = remaining,
        item_cbs = "Grassland",
        Cat_1 = "Grassland",
        Cat_feed = "Grass",
        hierarchy_level = label,
        original_Province = Province_name
      ) |>
      dplyr::select(demand_id, Year, Territory, Province_name, Livestock_cat,
                    item_cbs, Cat_1, Cat_feed, intake_MgDM,
                    hierarchy_level, original_demand_item, original_Province)
    # `add_alloc` may cap intake below `remaining` (e.g. if the per-LC cap
    # already bound from earlier substitutions). Decrement `demand$remaining`
    # only by what was actually allocated, so the invariant
    #   demand$remaining == 0  <=>  fully fulfilled
    # holds even in edge cases.
    add_alloc(leftover)
  }
  
  distribute_surplus <- function(label, only_variable = FALSE) {
    surplus_avail <- avail |>
      dplyr::filter(avail_remaining > 1e-9, Cat_feed != "Zoot_fixed")
    if (nrow(surplus_avail) == 0) {
      return()
    }
    
    # Optimization: Process by Year-Territory groups to minimize demand filtering overhead
    territory_years <- surplus_avail |>
      dplyr::distinct(Year, Territory)
    
    for (row_idx in seq_len(nrow(territory_years))) {
      y <- territory_years$Year[row_idx]
      t <- territory_years$Territory[row_idx]
      items_year <- surplus_avail |> dplyr::filter(Year == y, Territory == t)

      yt_rows <- demand_yt_idx[[paste(y, t, sep = "\001")]]
      if (is.null(yt_rows) || length(yt_rows) == 0) next
      demand_subset <- demand[yt_rows, , drop = FALSE]
      demand_subset$weight <- demand_subset$demand_MgDM
      demand_subset <- demand_subset[demand_subset$weight > 0, , drop = FALSE]

      if (only_variable) {
        demand_subset <- demand_subset[!demand_subset$fixed_demand, , drop = FALSE]
      }

      if (nrow(demand_subset) == 0) next
      
      for (idx in seq_len(nrow(items_year))) {
        current <- items_year[idx, ]
        surplus <- current$avail_remaining
        if (surplus <= 1e-9) next
        
        # Filter eligible from demand_subset (fast in-memory subsetting)
        if (current$Feed_scale == "Provincial") {
          eligible_indices <- which(
            demand_subset$Province_name == current$Province_name &
              demand_subset$Territory == current$Territory
          )
        } else {
          eligible_indices <- which(demand_subset$Territory == current$Territory)
        }
        
        if (length(eligible_indices) == 0) next
        
        eligible <- demand_subset[eligible_indices, ]
        total_weight <- sum(eligible$weight, na.rm = TRUE)
        if (total_weight <= 1e-9) next
        
        alloc <- eligible |>
          dplyr::mutate(
            intake_MgDM = surplus * (weight / total_weight),
            item_cbs = current$item_cbs,
            Cat_1 = current$Cat_1,
            Cat_feed = current$Cat_feed,
            hierarchy_level = label,
            original_Province = current$Province_name
          ) |>
          dplyr::filter(intake_MgDM > 1e-9) |>
          dplyr::select(demand_id, Year, Territory, Province_name, Livestock_cat,
                        item_cbs, Cat_1, Cat_feed, intake_MgDM,
                        hierarchy_level, original_demand_item, original_Province) |>
          dplyr::mutate(avail_id = current$avail_id)
        
        add_alloc(alloc, respect_cap = FALSE)
        # Weights in surplus distribution are the immutable demand_MgDM; no
        # local refresh is needed (dead code previously lived here).
      }
    }
  }

  reroute_excess_grass <- function(result_tbl) {
    if (allocation_mode == "variable") {
      return(result_tbl)
    }
    
    grass_limits <- avail |>
      dplyr::filter(Cat_feed == "Grass", item_cbs != "Grassland") |>
      dplyr::summarize(
        .by = c(Year, Territory, item_cbs),
        avail_limit = sum(Avail_MgDM, na.rm = TRUE)
      )
    
    if (nrow(grass_limits) == 0) {
      return(result_tbl)
    }
    
    intake_summary <- result_tbl |>
      dplyr::filter(Cat_feed == "Grass", item_cbs != "Grassland") |>
      dplyr::summarize(
        .by = c(Year, Territory, item_cbs),
        total_intake = sum(intake_MgDM, na.rm = TRUE)
      )
    
    violations <- intake_summary |>
      dplyr::left_join(grass_limits, by = c("Year", "Territory", "item_cbs")) |>
      dplyr::mutate(
        avail_limit = dplyr::coalesce(avail_limit, 0),
        excess = total_intake - avail_limit
      ) |>
      dplyr::filter(excess > 1e-6)
    
    if (nrow(violations) == 0) {
      return(result_tbl)
    }
    
    final_level <- if (length(priority_labels) >= 6) {
      priority_labels[6]
    } else {
      utils::tail(priority_labels, 1)
    }

    # Vectorized: mark Grass rows, compute per-row reduction proportional to
    # intake, then split into kept rows (with reduced intake) and new
    # Grassland rows carrying the removed DM. Replaces an O(n_violations)
    # R-level loop.
    result_tbl$row_id <- seq_len(nrow(result_tbl))
    grass_rows <- result_tbl |>
      dplyr::filter(Cat_feed == "Grass", item_cbs != "Grassland") |>
      dplyr::inner_join(
        violations |>
          dplyr::select(Year, Territory, item_cbs, total_intake, excess),
        by = c("Year", "Territory", "item_cbs")
      ) |>
      dplyr::mutate(
        reduction = dplyr::if_else(
          total_intake > 1e-9,
          pmin(intake_MgDM, intake_MgDM / total_intake * pmin(excess, total_intake)),
          0
        ),
        reduction = pmax(0, dplyr::coalesce(reduction, 0))
      )

    if (nrow(grass_rows) > 0) {
      reductions <- dplyr::coalesce(grass_rows$reduction, 0)
      idx <- grass_rows$row_id
      result_tbl$intake_MgDM[idx] <- pmax(0, result_tbl$intake_MgDM[idx] - reductions)

      new_rows <- result_tbl[idx, , drop = FALSE]
      new_rows$intake_MgDM <- reductions
      new_rows$item_cbs <- "Grassland"
      new_rows$Cat_1 <- "Grassland"
      new_rows$Cat_feed <- "Grass"
      new_rows$hierarchy_level <- final_level
      new_rows$original_Province <- new_rows$Province_name
      if ("avail_id" %in% names(new_rows)) {
        new_rows$avail_id <- NA_integer_
      }
      additions <- new_rows[new_rows$intake_MgDM > 1e-9, , drop = FALSE]
      additions$row_id <- NULL
      result_tbl$row_id <- NULL
      result_tbl <- dplyr::bind_rows(result_tbl, additions)
    } else {
      result_tbl$row_id <- NULL
    }

    result_tbl
  }
  
  apply_max_intake_caps <- function(result_tbl) {
    if (!nrow(max_intake_limits)) return(result_tbl)

    # Partition cap rules by `var` (which result_tbl column the cap is
    # keyed on). Two flavours supported:
    #   - `item_cbs`: per-item cap on (Livestock_cat, item_cbs).
    #   - `Cat_feed`: joint cap on the sum of intake across every item in
    #     that Cat_feed for the livestock, EXCLUDING items that have their
    #     own per-item cap (so an Acorns row at 40 % survives next to a
    #     Cat_feed=Grass cap at 10 %).
    item_caps    <- max_intake_limits[max_intake_limits$var == "item_cbs",   , drop = FALSE]
    catfeed_caps <- max_intake_limits[max_intake_limits$var == "Cat_feed",   , drop = FALSE]

    items_with_own_cap <- if (nrow(item_caps) > 0) {
      tibble::tibble(
        Livestock_cat = as.character(item_caps$Livestock_cat),
        item_cbs      = as.character(item_caps$var_value)
      ) |> dplyr::distinct() |> dplyr::mutate(has_own_cap = TRUE)
    } else {
      tibble::tibble(Livestock_cat = character(),
                     item_cbs = character(),
                     has_own_cap = logical())
    }

    totals <- result_tbl |>
      dplyr::summarize(
        .by = c("Year", "Territory", "Province_name", "Livestock_cat"),
        total_dm = sum(intake_MgDM, na.rm = TRUE)
      )

    # Strict cap math: enforces item_new / (other + item_new) <= max_share
    # AFTER reduction. Derivation:
    #   item_new <= max_share * (other + item_new)
    #   item_new <= other * max_share / (1 - max_share)
    # Edge cases:
    #   max_share = 1 -> 1 - max_share = 0 -> limit = +Inf (no binding cap).
    #   max_share = 0 -> limit = 0 (full removal).
    # Replaces the previous "share-of-original-total" formula
    # `limit_dm = total_dm * max_intake_share` which left systematic
    # residual violations: after reducing the item, the new total shrinks
    # and the resulting share exceeds max_share whenever the item is a
    # major fraction of the original diet.
    strict_limit <- function(other_dm, max_share) {
      dplyr::if_else(
        max_share >= 1 - 1e-9, Inf,
        other_dm * max_share / pmax(1 - max_share, 1e-9)
      )
    }

    item_viol <- NULL
    if (nrow(item_caps) > 0) {
      res_filt <- result_tbl |>
        dplyr::semi_join(
          item_caps |> dplyr::transmute(Livestock_cat, item_cbs = var_value),
          by = c("Livestock_cat", "item_cbs")
        )
      if (nrow(res_filt) > 0) {
        item_totals <- res_filt |>
          dplyr::summarize(
            .by = c("Year", "Territory", "Province_name", "Livestock_cat", "item_cbs"),
            item_dm = sum(intake_MgDM, na.rm = TRUE)
          )
        item_viol <- item_totals |>
          dplyr::left_join(totals, by = c("Year", "Territory", "Province_name", "Livestock_cat")) |>
          dplyr::left_join(
            item_caps |> dplyr::transmute(Livestock_cat,
                                          item_cbs = var_value,
                                          max_intake_share),
            by = c("Livestock_cat", "item_cbs")
          ) |>
          dplyr::mutate(
            total_dm         = dplyr::coalesce(total_dm, 0),
            max_intake_share = dplyr::coalesce(max_intake_share, 0),
            other_dm         = total_dm - item_dm,
            limit_dm         = strict_limit(other_dm, max_intake_share),
            excess           = item_dm - limit_dm
          ) |>
          dplyr::filter(is.finite(excess), excess > 1e-6)
      }
    }

    catfeed_viol <- NULL
    if (nrow(catfeed_caps) > 0) {
      eligible_tbl <- result_tbl |>
        dplyr::left_join(items_with_own_cap,
                         by = c("Livestock_cat", "item_cbs")) |>
        dplyr::filter(is.na(has_own_cap)) |>
        dplyr::select(-has_own_cap) |>
        dplyr::semi_join(
          catfeed_caps |> dplyr::transmute(Livestock_cat, Cat_feed = var_value),
          by = c("Livestock_cat", "Cat_feed")
        )
      if (nrow(eligible_tbl) > 0) {
        cf_totals <- eligible_tbl |>
          dplyr::summarize(
            .by = c("Year", "Territory", "Province_name", "Livestock_cat", "Cat_feed"),
            item_dm = sum(intake_MgDM, na.rm = TRUE)
          )
        catfeed_viol <- cf_totals |>
          dplyr::left_join(totals, by = c("Year", "Territory", "Province_name", "Livestock_cat")) |>
          dplyr::left_join(
            catfeed_caps |> dplyr::transmute(Livestock_cat,
                                             Cat_feed = var_value,
                                             max_intake_share),
            by = c("Livestock_cat", "Cat_feed")
          ) |>
          dplyr::mutate(
            total_dm         = dplyr::coalesce(total_dm, 0),
            max_intake_share = dplyr::coalesce(max_intake_share, 0),
            other_dm         = total_dm - item_dm,
            limit_dm         = strict_limit(other_dm, max_intake_share),
            excess           = item_dm - limit_dm
          ) |>
          dplyr::filter(is.finite(excess), excess > 1e-6)
      }
    }

    has_item_viol <- !is.null(item_viol) && nrow(item_viol) > 0
    has_cf_viol   <- !is.null(catfeed_viol) && nrow(catfeed_viol) > 0
    if (!has_item_viol && !has_cf_viol) return(result_tbl)

    # ---- Reduce violating rows ------------------------------------------
    result_tbl$row_id <- seq_len(nrow(result_tbl))

    if (has_item_viol) {
      keys <- item_viol |>
        dplyr::select(Year, Territory, Province_name, Livestock_cat,
                      item_cbs, excess, item_dm)
      matches <- result_tbl |>
        dplyr::inner_join(keys,
                          by = c("Year", "Territory", "Province_name",
                                 "Livestock_cat", "item_cbs")) |>
        dplyr::mutate(
          reduction_factor = pmax(0, (item_dm - excess) / item_dm),
          new_intake       = intake_MgDM * reduction_factor
        )
      if (nrow(matches) > 0) {
        result_tbl$intake_MgDM[matches$row_id] <- matches$new_intake
      }
    }

    if (has_cf_viol) {
      keys <- catfeed_viol |>
        dplyr::select(Year, Territory, Province_name, Livestock_cat,
                      Cat_feed, excess, item_dm)
      matches <- result_tbl |>
        dplyr::left_join(items_with_own_cap,
                         by = c("Livestock_cat", "item_cbs")) |>
        dplyr::filter(is.na(has_own_cap)) |>
        dplyr::inner_join(keys,
                          by = c("Year", "Territory", "Province_name",
                                 "Livestock_cat", "Cat_feed")) |>
        dplyr::mutate(
          reduction_factor = pmax(0, (item_dm - excess) / item_dm),
          new_intake       = intake_MgDM * reduction_factor
        )
      if (nrow(matches) > 0) {
        result_tbl$intake_MgDM[matches$row_id] <- matches$new_intake
      }
    }

    result_tbl$row_id <- NULL

    # ---- Aggregate freed DM per (Y, T, P, LC) for reallocation ----------
    excess_per_lc <- dplyr::bind_rows(
      if (has_item_viol) {
        item_viol |> dplyr::transmute(Year, Territory, Province_name,
                                      Livestock_cat,
                                      capped_excess = pmin(excess, item_dm))
      } else NULL,
      if (has_cf_viol) {
        catfeed_viol |> dplyr::transmute(Year, Territory, Province_name,
                                         Livestock_cat,
                                         capped_excess = pmin(excess, item_dm))
      } else NULL
    ) |>
      dplyr::summarize(
        .by = c(Year, Territory, Province_name, Livestock_cat),
        total_excess = sum(capped_excess, na.rm = TRUE)
      ) |>
      dplyr::filter(total_excess > 1e-9)

    # ---- Reallocation: substitute freed DM via remaining availability ----
    # Strategy:
    #   Phase 1. Grassland absorbs excess where Grassland is not capped
    #            for the livestock (and not part of a Cat_feed cap group).
    #   Phase 2. Chain through non-Grassland items in catfeed_priority
    #            order, decrementing avail$avail_remaining. Skips any item
    #            that is capped (per-item or via its Cat_feed) for the
    #            livestock.
    #   Phase 3 (STRICT). Leftover that no substitute could absorb is
    #            DROPPED — scaling_factor on the affected demand_id falls
    #            below 1, signalling supply-constrained intake. This
    #            replaces the previous "force back to Grassland with
    #            warning" behaviour, which silently violated the
    #            biological cap.
    if (nrow(excess_per_lc) > 0) {
      # Build item_cbs -> Cat_feed mapping for the is_capped lookup.
      item_cbs_to_cat_feed <- tibble::tibble(
        item_cbs = as.character(items_full$item_cbs),
        Cat_1    = as.character(items_full$Cat_1)
      ) |>
        dplyr::mutate(Cat_feed = unname(Cats$Cat_feed[match(Cat_1, Cats$Cat_1)])) |>
        dplyr::distinct(item_cbs, Cat_feed)

      item_capped_keys <- if (nrow(item_caps) > 0) {
        paste0(item_caps$Livestock_cat, "\001", item_caps$var_value)
      } else character(0)
      catfeed_capped_keys <- if (nrow(catfeed_caps) > 0) {
        paste0(catfeed_caps$Livestock_cat, "\001", catfeed_caps$var_value)
      } else character(0)

      is_capped <- function(lc, item) {
        if (paste0(lc, "\001", item) %in% item_capped_keys) return(TRUE)
        cf <- item_cbs_to_cat_feed$Cat_feed[match(item, item_cbs_to_cat_feed$item_cbs)]
        if (length(cf) == 0L || is.na(cf)) return(FALSE)
        paste0(lc, "\001", cf) %in% catfeed_capped_keys
      }

      non_zoot_subs <- tibble::tibble(item_cbs = items_full$item_cbs,
                                      Cat_1    = items_full$Cat_1) |>
        dplyr::mutate(
          Cat_feed  = unname(Cats$Cat_feed[match(Cat_1, Cats$Cat_1)]),
          feed_rank = dplyr::coalesce(catfeed_priority[Cat_feed], 999)
        ) |>
        dplyr::filter(!is.na(Cat_feed),
                      Cat_feed != "Zoot_fixed",
                      item_cbs != "Grassland") |>
        dplyr::arrange(feed_rank) |>
        dplyr::select(item_cbs, Cat_1, Cat_feed)

      donors <- result_tbl |>
        dplyr::group_by(Year, Territory, Province_name, Livestock_cat) |>
        dplyr::slice_head(n = 1L) |>
        dplyr::ungroup() |>
        dplyr::select(-dplyr::any_of(c("item_cbs", "Cat_1", "Cat_feed",
                                       "intake_MgDM", "hierarchy_level",
                                       "original_Province", "avail_id")))

      state <- excess_per_lc |>
        dplyr::mutate(remaining_excess = total_excess,
                      grassland_capped = vapply(
                        Livestock_cat,
                        function(lc) is_capped(lc, "Grassland"),
                        logical(1)
                      ))

      final_level <- utils::tail(priority_labels, 1)
      additions_list <- list()

      emit_grassland_rows <- function(sub_state) {
        if (nrow(sub_state) == 0) return(NULL)
        donors |>
          dplyr::inner_join(
            sub_state |> dplyr::select(Year, Territory, Province_name,
                                       Livestock_cat, remaining_excess),
            by = c("Year", "Territory", "Province_name", "Livestock_cat")
          ) |>
          dplyr::mutate(
            item_cbs          = "Grassland",
            Cat_1             = "Grassland",
            Cat_feed          = "Grass",
            intake_MgDM       = remaining_excess,
            hierarchy_level   = final_level,
            original_Province = Province_name,
            avail_id          = NA_integer_
          ) |>
          dplyr::select(-remaining_excess)
      }

      # Phase 1: Grassland absorbs excess where not capped.
      phase1 <- state[!state$grassland_capped &
                        state$remaining_excess > 1e-9, , drop = FALSE]
      if (nrow(phase1) > 0) {
        additions_list[[length(additions_list) + 1L]] <-
          emit_grassland_rows(phase1)
        state$remaining_excess[!state$grassland_capped] <- 0
      }

      # Phase 2: chain through non-Grassland substitutes.
      for (si in seq_len(nrow(non_zoot_subs))) {
        if (all(state$remaining_excess <= 1e-9)) break

        sub <- non_zoot_subs[si, , drop = FALSE]
        active_mask <- state$remaining_excess > 1e-9 &
          !vapply(state$Livestock_cat,
                  function(lc) is_capped(lc, sub$item_cbs),
                  logical(1))
        if (!any(active_mask)) next

        active <- state[active_mask, , drop = FALSE]
        sub_avail <- avail[avail$item_cbs == sub$item_cbs &
                             avail$avail_remaining > 1e-9, , drop = FALSE]
        if (nrow(sub_avail) == 0) next

        matched <- active |>
          dplyr::select(Year, Territory, Province_name, Livestock_cat,
                        remaining_excess) |>
          dplyr::inner_join(
            sub_avail |>
              dplyr::select(Year, Territory,
                            avail_Province = Province_name,
                            Feed_scale, avail_id, avail_remaining),
            by = c("Year", "Territory"),
            relationship = "many-to-many"
          ) |>
          dplyr::filter(Feed_scale == "National" |
                          (!is.na(avail_Province) &
                             !is.na(Province_name) &
                             avail_Province == Province_name)) |>
          dplyr::mutate(prov_first = as.integer(Feed_scale == "Provincial")) |>
          dplyr::arrange(Year, Territory, Province_name, Livestock_cat,
                         dplyr::desc(prov_first), avail_id) |>
          dplyr::group_by(Year, Territory, Province_name, Livestock_cat) |>
          dplyr::mutate(
            prev_cum = cumsum(avail_remaining) - avail_remaining,
            alloc    = pmax(0, pmin(avail_remaining,
                                    remaining_excess - prev_cum))
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(alloc > 1e-9)

        if (nrow(matched) == 0) next

        decrement <- rowsum(matched$alloc,
                            group = matched$avail_id, reorder = FALSE)
        aid <- as.integer(rownames(decrement))
        avail$avail_remaining[aid] <<- pmax(
          0, avail$avail_remaining[aid] - decrement[, 1]
        )

        new_rows <- matched |>
          dplyr::inner_join(donors,
                            by = c("Year", "Territory", "Province_name",
                                   "Livestock_cat")) |>
          dplyr::mutate(
            item_cbs          = sub$item_cbs,
            Cat_1             = sub$Cat_1,
            Cat_feed          = sub$Cat_feed,
            intake_MgDM       = alloc,
            hierarchy_level   = final_level,
            original_Province = dplyr::if_else(
              Feed_scale == "National",
              NA_character_, avail_Province
            )
          ) |>
          dplyr::select(-prev_cum, -alloc, -remaining_excess,
                        -avail_Province, -Feed_scale,
                        -prov_first, -avail_remaining)
        additions_list[[length(additions_list) + 1L]] <- new_rows

        per_group <- matched |>
          dplyr::summarize(
            .by = c(Year, Territory, Province_name, Livestock_cat),
            alloc_total = sum(alloc, na.rm = TRUE)
          )
        idx <- match(
          paste(per_group$Year, per_group$Territory,
                per_group$Province_name, per_group$Livestock_cat,
                sep = "\001"),
          paste(state$Year, state$Territory,
                state$Province_name, state$Livestock_cat,
                sep = "\001")
        )
        state$remaining_excess[idx] <- pmax(
          0, state$remaining_excess[idx] - per_group$alloc_total
        )
      }

      additions <- dplyr::bind_rows(additions_list)
      if (nrow(additions) > 0) {
        additions <- additions[, intersect(names(result_tbl),
                                           names(additions)),
                               drop = FALSE]
        result_tbl <- dplyr::bind_rows(result_tbl, additions)
      }
    }

    # ---- Diagnostic trace ----------------------------------------------
    if (verbose) {
      all_viol <- dplyr::bind_rows(
        if (has_item_viol) {
          item_viol |> dplyr::transmute(Year, Territory, Province_name,
                                        Livestock_cat,
                                        cap_var = "item_cbs",
                                        cap_value = item_cbs,
                                        item_dm, limit_dm, excess,
                                        excess_pct = excess / pmax(item_dm, 1e-9) * 100)
        } else NULL,
        if (has_cf_viol) {
          catfeed_viol |> dplyr::transmute(Year, Territory, Province_name,
                                           Livestock_cat,
                                           cap_var = "Cat_feed",
                                           cap_value = Cat_feed,
                                           item_dm, limit_dm, excess,
                                           excess_pct = excess / pmax(item_dm, 1e-9) * 100)
        } else NULL
      )
      cat(sprintf("[apply_max_intake_caps] %d cap violations reduced (item_cbs %d, Cat_feed %d).\n",
                  nrow(all_viol),
                  if (has_item_viol) nrow(item_viol) else 0L,
                  if (has_cf_viol)   nrow(catfeed_viol) else 0L))
      if (nrow(all_viol) > 0) {
        top_viol <- all_viol |> dplyr::arrange(dplyr::desc(excess)) |> utils::head(5)
        for (i in seq_len(nrow(top_viol))) {
          v <- top_viol[i, ]
          cat(sprintf("  top%02d: %s | %s | %s [%s=%s] | item_dm=%.1f Mg, limit=%.1f Mg, excess=%.1f Mg (%.1f%%)\n",
                      i, as.character(v$Year), as.character(v$Province_name),
                      as.character(v$Livestock_cat),
                      as.character(v$cap_var), as.character(v$cap_value),
                      v$item_dm, v$limit_dm, v$excess, v$excess_pct))
        }
      }
    }

    result_tbl |>
      dplyr::filter(intake_MgDM > 1e-9)
  }
  
  if (allocation_mode == "fixed") {
    allocate_trade()
    diag_snapshot("After inter-provincial trade")
    
    # Step 1: Non-grass
    allocate_priority_pool(priority_labels[5], cat_feed_filter = function(x) x != "Grass")
    diag_snapshot("After non-grass priority sweep")
    
    # Step 2: Available Grass
    allocate_priority_pool(priority_labels[5], cat_feed_filter = function(x) x == "Grass")
    diag_snapshot("After available grass sweep")
    
    run_release_pass(priority_labels[5], "After released non-grass sweep")
    allocate_grassland(priority_labels[6])
    diag_snapshot("After unlimited grassland")
  } else if (allocation_mode == "variable") {
    # Step 1: Non-grass
    allocate_priority_pool(priority_labels[4], cat_feed_filter = function(x) x != "Grass")
    diag_snapshot("After availability-driven pool (non-grass)")
    
    # Step 2: Available Grass
    allocate_priority_pool(priority_labels[4], cat_feed_filter = function(x) x == "Grass")
    diag_snapshot("After availability-driven pool (grass)")
    
    run_release_pass(priority_labels[4], "After released availability pool")
    distribute_surplus(priority_labels[5])
    diag_snapshot("After surplus distribution")
  } else {
    # Mixed mode
    allocate_trade()
    diag_snapshot("After inter-provincial trade")
    
    # Step 1: Non-grass
    allocate_priority_pool(priority_labels[5], cat_feed_filter = function(x) x != "Grass")
    diag_snapshot("After mixed priority sweep (non-grass)")
    
    # Step 2: Available Grass
    allocate_priority_pool(priority_labels[5], cat_feed_filter = function(x) x == "Grass")
    diag_snapshot("After mixed priority sweep (grass)")
    
    run_release_pass(priority_labels[5], "After released mixed sweep")
    
    # Allocate unlimited grassland ONLY to fixed_demand = TRUE
    allocate_grassland(priority_labels[6], only_fixed = TRUE)
    diag_snapshot("After unlimited grassland (fixed only)")
    
    # Distribute surplus ONLY to fixed_demand = FALSE
    distribute_surplus(priority_labels[5], only_variable = TRUE)
    diag_snapshot("After surplus distribution (variable only)")
  }
  
  # ============================================================================
  # 7. ASSEMBLE RESULTS
  # ============================================================================
  if (!length(allocations)) {
    empty_result <- dplyr::tibble(
      Year = numeric(), Territory = character(), Province_name = character(), Livestock_cat = character(),
      item_cbs = character(), Cat_1 = character(), Cat_feed = character(),
      demand_MgDM = numeric(), intake_MgDM = numeric(), scaling_factor = numeric(),
      hierarchy_level = character(), original_demand_item = character(),
      fixed_demand = logical(), original_Province = character()
    )

    if (output_sub_territory_col != "Province_name") {
      empty_result <- empty_result |>
        dplyr::rename(!!output_sub_territory_col := Province_name)
    }
    if (territory_col != "Territory") {
      empty_result <- empty_result |>
        dplyr::rename(!!territory_col := Territory)
    }

    return(empty_result)
  }
  
  result <- dplyr::bind_rows(allocations)
  missing_ids <- setdiff(demand$demand_id, result$demand_id)
  if (length(missing_ids)) {
    zero_rows <- demand |>
      dplyr::filter(demand_id %in% missing_ids) |>
      dplyr::mutate(
        intake_MgDM = 0,
        hierarchy_level = priority_labels[1]
      ) |>
      dplyr::select(demand_id, Year, Territory, Province_name, Livestock_cat,
                    item_cbs, Cat_1, Cat_feed, intake_MgDM,
                    hierarchy_level, original_demand_item, original_Province)
    result <- dplyr::bind_rows(result, zero_rows)
  }
  
  result <- reroute_excess_grass(result)

  # Attach demand_MgDM and fixed_demand via indexed lookup (demand_id is the
  # row index of `demand`).
  result$demand_MgDM   <- demand$demand_MgDM[result$demand_id]
  result$fixed_demand  <- demand$fixed_demand[result$demand_id]

  # scaling_factor is computed once at the end of the function. The first
  # pass that used to live here was always overwritten by the final pass
  # below and never read between assignments — removed.
  compute_scaling <- function(res) {
    # Vectorised: one rowsum + indexed write, no join. Preserves the
    # original case_when semantics:
    #   demand_MgDM > 0              -> total_intake / demand_MgDM
    #   demand_MgDM <= 0, intake > 0 -> NA  (ill-defined ratio)
    #   demand_MgDM <= 0, intake = 0 -> 0
    # Demand rows with no corresponding result row get 0.
    intake_by_id <- rowsum(res$intake_MgDM,
                           group = res$demand_id, reorder = FALSE)
    sf <- numeric(nrow(demand))  # init 0
    ids <- as.integer(rownames(intake_by_id))
    tot <- intake_by_id[, 1]
    dm  <- demand$demand_MgDM[ids]
    # Assign per-case without collapsing NA back to 0.
    vals <- ifelse(dm > 0, tot / dm,
                   ifelse(tot > 0, NA_real_, 0))
    sf[ids] <- vals
    sf[res$demand_id]
  }
  
  # ============================================================================
  # 8. SCALE ZOOT_FIXED (Variable Mode)
  # ============================================================================
  # In availability-driven mode, zoot-fixed items can be upscaled with the
  # expansion of non-zoot intake but are not downscaled below their allocated
  # level in this step.
  
  if (allocation_mode != "fixed") {
    # Identify relevant groups: Year, Province, Livestock_cat
    # Calculate Non-Zoot Demand and Intake
    
    non_zoot_stats <- result |>
      dplyr::filter(Cat_feed != "Zoot_fixed") |>
      dplyr::group_by(Year, Territory, Province_name, Livestock_cat) |>
      dplyr::summarize(
        non_zoot_intake = sum(intake_MgDM, na.rm = TRUE),
        .groups = "drop"
      )
    
    non_zoot_demand <- demand |>
      dplyr::filter(Cat_feed != "Zoot_fixed") |>
      dplyr::group_by(Year, Territory, Province_name, Livestock_cat) |>
      dplyr::summarize(
        non_zoot_demand = sum(demand_MgDM, na.rm = TRUE),
        .groups = "drop"
      )
    
    scaling_ratios <- non_zoot_stats |>
      dplyr::left_join(non_zoot_demand, by = c("Year", "Territory", "Province_name", "Livestock_cat")) |>
      dplyr::mutate(
        zoot_scale_factor = dplyr::case_when(
          non_zoot_demand > 1e-9 ~ non_zoot_intake / non_zoot_demand,
          TRUE ~ 1.0
        )
      ) |>
      dplyr::select(Year, Territory, Province_name, Livestock_cat, zoot_scale_factor)
    
    # Apply scaling to Zoot_fixed rows where fixed_demand is FALSE
    # We need to join back to result and update intake_MgDM
    
    result <- result |>
      dplyr::left_join(scaling_ratios, by = c("Year", "Territory", "Province_name", "Livestock_cat")) |>
      dplyr::mutate(
        intake_MgDM = dplyr::case_when(
          Cat_feed == "Zoot_fixed" & !fixed_demand ~ intake_MgDM * dplyr::coalesce(zoot_scale_factor, 1.0),
          TRUE ~ intake_MgDM
        )
      ) |>
      dplyr::select(-zoot_scale_factor)

    diag_snapshot("After Zoot_fixed scaling (variable mode)")
  }

  # Apply cap AFTER Zoot scaling. The cap uses total_dm (sum of intake
  # across all Cat_feed for the group). Running it before Zoot scaling let
  # the subsequent Zoot rescaling shrink the total, which silently inflated
  # the post-function Grass share above max_share (~1-2 pp drift in variable
  # /mixed mode where Zoot scales down on a cap-reduced non-Zoot intake).
  # Placing it here makes the cap operate on the same total the caller
  # observes in the returned data frame, so the strict cap holds exactly.
  result <- apply_max_intake_caps(result)

  # Recalculate final scaling factors after all intake adjustments
  # (vectorized; same logic as the first pass).
  result$scaling_factor <- compute_scaling(result)
  
  result <- result |>
    dplyr::select(Year, Territory, Province_name, Livestock_cat, item_cbs, Cat_1, Cat_feed,
                  demand_MgDM, intake_MgDM, scaling_factor, hierarchy_level,
                  original_demand_item, fixed_demand, original_Province) |>
    # Deterministic tiebreakers: when rows match on the semantic keys, fall
    # through to item_cbs and original_Province so row order is stable across
    # runs even if upstream operations produce different internal orderings.
    dplyr::arrange(Year, Territory, Province_name, Livestock_cat,
                   original_demand_item, hierarchy_level, item_cbs,
                   original_Province)

  if (output_sub_territory_col != "Province_name") {
    result <- result |>
      dplyr::rename(!!output_sub_territory_col := Province_name)
  }

  if (territory_col != "Territory") {
    result <- result |>
      dplyr::rename(!!territory_col := Territory)
  }
  
  total_runtime <- difftime(Sys.time(), start_time, units = "secs")
  if (verbose) {
    cat(sprintf(
      "\nCompleted redistribute_feed() in %.1f seconds\n",
      as.numeric(total_runtime)
    ))
  }
  result
}
