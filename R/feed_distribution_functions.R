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
#'   availability for Zoot_fixed items. When availability of
#'   Zoot_fixed is zero or NA, no cap is applied and intake
#'   equals demand.
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
#'    but capped at
#'    `zoot_fixed_max_multiplier * availability` to prevent
#'    extreme violations. When availability is zero or NA, no
#'    cap is applied. If a cap is applied, the excess demand
#'    should be met through other available items following
#'    the hierarchy.
#' 2. Monogastric categories are allocated first.
#'    `Monogastric` is a vector present in the environment
#'    defining which Livestock_cat are monogastric.
#' 3. Items without a defined `feedtype_graniv` are ignored
#'    for monogastrics. `feedtype_graniv` is joined from the
#'    `items_full` lookup table.
#' 4. Ruminant Livestock_cat categories are processed with
#'    the remaining availability.
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
#' @export
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
  territory_col = "Territory",
  sub_territory_col = "Sub_territory",
  verbose = TRUE
) {
  start_time <- Sys.time()
  if (verbose) {
    cat("\n╔══════════════════════════════════════════════════════════╗\n")
    cat("║          redistribute_feed() diagnostics                 ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n")
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

  max_intake_limits <- tibble::as_tibble(max_intake_share) |>
    dplyr::filter(is.finite(max_intake_share)) |>
    dplyr::mutate(
      Livestock_cat = as.character(Livestock_cat),
      item_cbs = as.character(item_cbs),
      max_intake_share = dplyr::if_else(max_intake_share < 0, 0, pmin(max_intake_share, 1))
    ) |>
    dplyr::summarize(
      .by = c("Livestock_cat", "item_cbs"),
      max_intake_share = max(max_intake_share, na.rm = TRUE)
    )
  
  pretty_number <- function(x) {
    ifelse(is.finite(x), format(round(x, 3), nsmall = 3, trim = TRUE), "NA")
  }
  
  format_tg <- function(x) {
    paste0(pretty_number(x / 1e6), " Tg")
  }
  
  diag_snapshot <- function(stage_label, note = NULL) {
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
    
    if (verbose) {
      cat(sprintf("\n[Diag %02d] Stage: %s\n", diag_counter, stage_label))
      cat(sprintf("  • Demand satisfied: %s / %s (%.1f%%)\n",
                  format_tg(satisfied), format_tg(total_demand), satisfied_pct))
      cat(sprintf("  • Remaining demand: %s | HP unmet: %s | HP avail: %s\n",
                  format_tg(remaining_demand), format_tg(hp_unmet), format_tg(hp_avail)))
      if (length(top_unmet)) {
        cat("  • Top unmet groups: ", paste(top_unmet, collapse = "; "), "\n", sep = "")
      }
      if (!is.null(note)) {
        cat("  • Notes: ", note, "\n", sep = "")
      }
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
      is_monogastric = Livestock_cat %in% Monogastric,
      can_receive_priority = !(is_monogastric & is.na(feedtype_graniv))
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
  
  add_alloc <- function(df, respect_cap = TRUE) {
    if (is.null(df) || nrow(df) == 0) {
      return(invisible(NULL))
    }
    df <- df |>
      dplyr::filter(intake_MgDM > 1e-9)
    if (nrow(df) == 0) {
      return(invisible(NULL))
    }
    
    if (respect_cap) {
      zoot_mask <- df$Cat_feed == "Zoot_fixed"
      uncapped <- df[zoot_mask, , drop = FALSE]
      capped <- df[!zoot_mask, , drop = FALSE]
      
      if (nrow(capped) > 0) {
        capped <- capped |>
          dplyr::left_join(
            demand_cap |>
              dplyr::select(Year, Territory, Province_name, Livestock_cat, remaining_cap),
            by = c("Year", "Territory", "Province_name", "Livestock_cat")
          ) |>
          dplyr::group_by(Year, Territory, Province_name, Livestock_cat) |>
          dplyr::mutate(
            cumsum_intake = cumsum(intake_MgDM),
            capped_intake = dplyr::case_when(
              is.na(remaining_cap) ~ intake_MgDM,
              cumsum_intake <= remaining_cap ~ intake_MgDM,
              (cumsum_intake - intake_MgDM) < remaining_cap ~ remaining_cap - (cumsum_intake - intake_MgDM),
              TRUE ~ 0
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(intake_MgDM = capped_intake) |>
          dplyr::select(-capped_intake, -cumsum_intake, -remaining_cap) |>
          dplyr::filter(intake_MgDM > 1e-9)
        
        if (nrow(capped) > 0) {
          cap_updates <- capped |>
            dplyr::summarize(
              .by = c(Year, Territory, Province_name, Livestock_cat),
              alloc = sum(intake_MgDM, na.rm = TRUE)
            )
          demand_cap <<- demand_cap |>
            dplyr::left_join(cap_updates, by = c("Year", "Territory", "Province_name", "Livestock_cat")) |>
            dplyr::mutate(
              allocated_so_far = allocated_so_far + dplyr::coalesce(alloc, 0),
              remaining_cap = pmax(0, total_demand - allocated_so_far)
            ) |>
            dplyr::select(-alloc)
        }
      }
      
      df <- dplyr::bind_rows(uncapped, capped)
      
      if (nrow(df) > 0) {
        demand_updates <- df |>
          dplyr::summarize(.by = demand_id, alloc = sum(intake_MgDM, na.rm = TRUE))
        demand <<- demand |>
          dplyr::left_join(demand_updates, by = "demand_id") |>
          dplyr::mutate(
            remaining = dplyr::if_else(!is.na(alloc), pmax(0, remaining - alloc), remaining),
            alloc = NULL
          )
      }
    }
    
    if ("avail_id" %in% names(df)) {
      avail_updates <- df |>
        dplyr::summarize(.by = avail_id, used = sum(intake_MgDM, na.rm = TRUE))
      avail <<- avail |>
        dplyr::left_join(avail_updates, by = "avail_id") |>
        dplyr::mutate(
          avail_remaining = dplyr::if_else(!is.na(used), pmax(0, avail_remaining - used), avail_remaining),
          used = NULL
        )
    }
    
    allocations[[length(allocations) + 1]] <<- df
    invisible(NULL)
  }
  
  allocate_cartesian <- function(demand_subset, avail_subset, group_cols, level_label) {
    if (nrow(demand_subset) == 0 || nrow(avail_subset) == 0) {
      return(NULL)
    }
    demand_subset <- demand_subset |>
      dplyr::filter(remaining > 1e-9)
    avail_subset <- avail_subset |>
      dplyr::filter(avail_remaining > 1e-9)
    if (nrow(demand_subset) == 0 || nrow(avail_subset) == 0) {
      return(NULL)
    }
    avail_subset <- ensure_item_metadata(avail_subset)
    
    demand_groups <- demand_subset |>
      dplyr::summarize(.by = dplyr::all_of(group_cols), demand_group = sum(remaining, na.rm = TRUE))
    avail_groups <- avail_subset |>
      dplyr::summarize(.by = dplyr::all_of(group_cols), avail_group = sum(avail_remaining, na.rm = TRUE))
    
    matched <- dplyr::inner_join(demand_groups, avail_groups, by = group_cols) |>
      dplyr::filter(demand_group > 1e-9, avail_group > 1e-9)
    if (nrow(matched) == 0) {
      return(NULL)
    }
    
    demand_weighted <- demand_subset |>
      dplyr::inner_join(matched, by = group_cols) |>
      dplyr::mutate(demand_share = remaining / demand_group)
    avail_weighted <- avail_subset |>
      dplyr::inner_join(matched, by = group_cols) |>
      dplyr::mutate(avail_share = avail_remaining / avail_group)
    
    item_cols <- c("item_cbs", "Cat_1", "Cat_feed")
    rename_cols <- setdiff(item_cols, group_cols)
    avail_weighted <- avail_weighted |>
      dplyr::mutate(source_Province = Province_name)
    for (nm in rename_cols) {
      avail_weighted <- dplyr::rename(avail_weighted, !!paste0(nm, "_sub") := !!rlang::sym(nm))
    }
    
    extra_cols <- intersect(paste0(item_cols, "_sub"), names(avail_weighted))
    result <- demand_weighted |>
      dplyr::select(-dplyr::any_of("original_Province")) |>
      dplyr::inner_join(
        avail_weighted |>
          dplyr::select(dplyr::all_of(c(group_cols, "avail_id", extra_cols,
                                        "avail_share", "source_Province"))),
        by = group_cols,
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        intake_MgDM = pmin(demand_group, avail_group) * demand_share * avail_share,
        hierarchy_level = level_label
      ) |>
      dplyr::filter(intake_MgDM > 1e-9)
    if (nrow(result) == 0) {
      return(NULL)
    }
    
    item_cbs_out <- if ("item_cbs_sub" %in% names(result)) {
      dplyr::coalesce(result$item_cbs_sub, result$item_cbs)
    } else {
      result$item_cbs
    }
    cat1_out <- if ("Cat_1_sub" %in% names(result)) {
      dplyr::coalesce(result$Cat_1_sub, result$Cat_1)
    } else {
      result$Cat_1
    }
    catfeed_out <- if ("Cat_feed_sub" %in% names(result)) {
      dplyr::coalesce(result$Cat_feed_sub, result$Cat_feed)
    } else {
      result$Cat_feed
    }
    
    out <- dplyr::tibble(
      demand_id = result$demand_id,
      Year = result$Year,
      Territory = result$Territory,
      Province_name = result$Province_name,
      Livestock_cat = result$Livestock_cat,
      item_cbs = item_cbs_out,
      Cat_1 = cat1_out,
      Cat_feed = catfeed_out,
      intake_MgDM = result$intake_MgDM,
      hierarchy_level = result$hierarchy_level,
      original_demand_item = result$original_demand_item,
      original_Province = result$source_Province,
      avail_id = result$avail_id
    )
    out
  }
  
  allocate_grouped <- function(demand_subset, avail_subset, group_cols, level_label) {
    if (nrow(demand_subset) == 0 || nrow(avail_subset) == 0) {
      return(NULL)
    }
    demand_subset <- demand_subset |>
      dplyr::filter(remaining > 1e-9)
    avail_subset <- avail_subset |>
      dplyr::filter(avail_remaining > 1e-9)
    if (nrow(demand_subset) == 0 || nrow(avail_subset) == 0) {
      return(NULL)
    }
    avail_subset <- ensure_item_metadata(avail_subset)
    
    demand_groups <- demand_subset |>
      dplyr::summarize(.by = dplyr::all_of(group_cols), demand_group = sum(remaining, na.rm = TRUE))
    avail_groups <- avail_subset |>
      dplyr::summarize(.by = dplyr::all_of(group_cols), avail_group = sum(avail_remaining, na.rm = TRUE))
    matched <- dplyr::inner_join(demand_groups, avail_groups, by = group_cols) |>
      dplyr::filter(demand_group > 1e-9, avail_group > 1e-9)
    if (nrow(matched) == 0) {
      return(NULL)
    }
    
    demand_weighted <- demand_subset |>
      dplyr::inner_join(matched, by = group_cols) |>
      dplyr::mutate(scale_factor = pmin(1, avail_group / demand_group))
    
    demand_scaled <- demand_weighted |>
      dplyr::mutate(intake_MgDM = remaining * scale_factor)
    
    avail_weighted <- avail_subset |>
      ensure_item_metadata()
    
    item_cols <- c("item_cbs", "Cat_1", "Cat_feed")
    rename_cols <- setdiff(item_cols, group_cols)
    for (nm in rename_cols) {
      avail_weighted <- dplyr::rename(avail_weighted, !!paste0(nm, "_sub") := !!rlang::sym(nm))
    }
    
    avail_weighted <- avail_weighted |>
      dplyr::inner_join(matched, by = group_cols) |>
      dplyr::mutate(
        share = avail_remaining / avail_group,
        source_Province = Province_name
      )
    
    extra_cols <- intersect(paste0(item_cols, "_sub"), names(avail_weighted))
    result <- demand_scaled |>
      dplyr::select(-dplyr::any_of("original_Province")) |>
      dplyr::inner_join(
        avail_weighted |>
          dplyr::select(dplyr::all_of(c(group_cols, "avail_id", extra_cols,
                                        "share", "source_Province"))),
        by = group_cols,
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        intake_MgDM = intake_MgDM * share,
        hierarchy_level = level_label
      ) |>
      dplyr::filter(intake_MgDM > 1e-9)
    if (nrow(result) == 0) {
      return(NULL)
    }
    
    item_cbs_out <- if ("item_cbs_sub" %in% names(result)) {
      dplyr::coalesce(result$item_cbs_sub, result$item_cbs)
    } else {
      result$item_cbs
    }
    cat1_out <- if ("Cat_1_sub" %in% names(result)) {
      dplyr::coalesce(result$Cat_1_sub, result$Cat_1)
    } else {
      result$Cat_1
    }
    catfeed_out <- if ("Cat_feed_sub" %in% names(result)) {
      dplyr::coalesce(result$Cat_feed_sub, result$Cat_feed)
    } else {
      result$Cat_feed
    }
    
    dplyr::tibble(
      demand_id = result$demand_id,
      Year = result$Year,
      Territory = result$Territory,
      Province_name = result$Province_name,
      Livestock_cat = result$Livestock_cat,
      item_cbs = item_cbs_out,
      Cat_1 = cat1_out,
      Cat_feed = catfeed_out,
      intake_MgDM = result$intake_MgDM,
      hierarchy_level = result$hierarchy_level,
      original_demand_item = result$original_demand_item,
      original_Province = result$source_Province,
      avail_id = result$avail_id
    )
  }
  
  execute_level <- function(target_ids, avail_filter, group_cols, label) {
    demand_subset <- demand |>
      dplyr::filter(demand_id %in% target_ids,
                    remaining > 1e-9,
                    Cat_feed != "Zoot_fixed")
    if (nrow(demand_subset) == 0) {
      return()
    }
    avail_subset <- avail_filter(avail)
    alloc <- allocate_cartesian(demand_subset, avail_subset, group_cols, label)
    add_alloc(alloc)
  }
  
  execute_grouped <- function(target_ids, avail_filter, group_cols, label) {
    demand_subset <- demand |>
      dplyr::filter(demand_id %in% target_ids,
                    remaining > 1e-9,
                    Cat_feed != "Zoot_fixed")
    if (nrow(demand_subset) == 0) {
      return()
    }
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
    
    zoot_prov_alloc <- tibble::tibble()
    if (nrow(zoot_prov) > 0) {
      prov_factors <- zoot_prov |>
        dplyr::summarize(
          .by = c(Year, Territory, Province_name, item_cbs, avail_prov, avail_id_prov),
          demand_tot = sum(demand_MgDM, na.rm = TRUE)
        ) |>
        dplyr::mutate(
          cap_total = dplyr::case_when(
            is.na(avail_prov) ~ NA_real_,
            avail_prov <= 0 ~ NA_real_,
            TRUE ~ zoot_fixed_max_multiplier * avail_prov
          ),
          scale_factor = dplyr::case_when(
            is.na(cap_total) ~ 1,
            demand_tot <= 0 ~ 1,
            TRUE ~ pmin(1, cap_total / demand_tot)
          )
        ) |>
        dplyr::select(Year, Territory, Province_name, item_cbs, scale_factor, avail_id = avail_id_prov)
      
      zoot_prov_alloc <- zoot_prov |>
        dplyr::left_join(prov_factors, by = c("Year", "Territory", "Province_name", "item_cbs")) |>
        dplyr::mutate(
          scale_factor = dplyr::coalesce(scale_factor, 1),
          intake_MgDM = demand_MgDM * scale_factor,
          hierarchy_level = priority_labels[1],
          avail_id = dplyr::coalesce(avail_id, avail_id_prov)
        ) |>
        dplyr::select(demand_id, Year, Territory, Province_name, Livestock_cat,
                      item_cbs, Cat_1, Cat_feed, intake_MgDM,
                      hierarchy_level, original_demand_item, original_Province,
                      avail_id)
    }
    
    zoot_nat_alloc <- tibble::tibble()
    if (nrow(zoot_nat) > 0) {
      nat_factors <- zoot_nat |>
        dplyr::summarize(
          .by = c(Year, Territory, item_cbs, avail_nat, avail_id_nat),
          demand_tot = sum(demand_MgDM, na.rm = TRUE)
        ) |>
        dplyr::mutate(
          cap_total = dplyr::case_when(
            is.na(avail_nat) ~ NA_real_,
            avail_nat <= 0 ~ NA_real_,
            TRUE ~ zoot_fixed_max_multiplier * avail_nat
          ),
          scale_factor = dplyr::case_when(
            is.na(cap_total) ~ 1,
            demand_tot <= 0 ~ 1,
            TRUE ~ pmin(1, cap_total / demand_tot)
          )
        ) |>
        dplyr::select(Year, Territory, item_cbs, scale_factor, avail_id = avail_id_nat)
      
      zoot_nat_alloc <- zoot_nat |>
        dplyr::left_join(nat_factors, by = c("Year", "Territory", "item_cbs")) |>
        dplyr::mutate(
          scale_factor = dplyr::coalesce(scale_factor, 1),
          intake_MgDM = demand_MgDM * scale_factor,
          hierarchy_level = priority_labels[1],
          avail_id = dplyr::coalesce(avail_id, avail_id_nat)
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
      remaining = dplyr::if_else(Cat_feed == "Zoot_fixed", 0, remaining),
      can_receive_priority = !(Livestock_cat %in% Monogastric & is.na(feedtype_graniv))
    )
  
  diag_snapshot("After Zoot_fixed", note = sprintf("Allocated %.3f Tg", sum(zoot_alloc$intake_MgDM, na.rm = TRUE) / 1e6))
  
  # ============================================================================
  # 5. PRIMARY LEVELS (Monogastric first, then ruminants)
  # ============================================================================
  run_primary_levels <- function(target_ids, stage_note) {
    if (!length(target_ids)) {
      diag_snapshot(stage_note, note = "No eligible demand")
      return()
    }
    
    execute_level(target_ids,
                  function(x) dplyr::filter(x, Feed_scale == "Provincial"),
                  c("Year", "Territory", "Province_name", "item_cbs"),
                  priority_labels[1])
    execute_grouped(target_ids,
                    function(x) dplyr::filter(x, Feed_scale == "National"),
                    c("Year", "Territory", "item_cbs"),
                    priority_labels[1])
    
    execute_level(target_ids,
                  function(x) dplyr::filter(x, Feed_scale == "Provincial"),
                  c("Year", "Territory", "Province_name", "Cat_1"),
                  priority_labels[2])
    execute_grouped(target_ids,
                    function(x) dplyr::filter(x, Feed_scale == "National"),
                    c("Year", "Territory", "Cat_1"),
                    priority_labels[2])
    
    execute_level(target_ids,
                  function(x) dplyr::filter(x, Feed_scale == "Provincial"),
                  c("Year", "Territory", "Province_name", "Cat_feed"),
                  priority_labels[3])
    execute_grouped(target_ids,
                    function(x) dplyr::filter(x, Feed_scale == "National"),
                    c("Year", "Territory", "Cat_feed"),
                    priority_labels[3])
    
    diag_snapshot(stage_note)
  }
  
  monogastric_ids <- demand$demand_id[demand$is_monogastric & !is.na(demand$feedtype_graniv)]
  run_primary_levels(monogastric_ids, "After monogastric hierarchy")
  
  ruminant_ids <- demand$demand_id[!(demand$demand_id %in% monogastric_ids)]
  run_primary_levels(ruminant_ids, "After ruminant hierarchy")
  
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
          original_Province,
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
      
      # Pre-filter demand for this year
      demand_subset <- demand |> 
        dplyr::filter(Year == y, Territory == t, remaining > 1e-9, Cat_feed != "Zoot_fixed")
      
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
          
          # Update local demand_subset to reflect changes made by add_alloc
          # We map alloc$demand_id back to demand_subset indices
          idx_in_subset <- match(alloc$demand_id, demand_subset$demand_id)
          demand_subset$remaining[idx_in_subset] <- pmax(0, demand_subset$remaining[idx_in_subset] - alloc$intake_MgDM)
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
    add_alloc(leftover)
    demand <<- demand |>
      dplyr::mutate(remaining = dplyr::if_else(demand_id %in% leftover$demand_id, 0, remaining))
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
      
      # Pre-filter demand for this year
      demand_subset <- demand |> 
        dplyr::mutate(weight = demand_MgDM) |>
        dplyr::filter(Year == y, Territory == t, weight > 0)
      
      if (only_variable) {
        demand_subset <- demand_subset |> dplyr::filter(!fixed_demand)
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
        
        # Refresh local demand weights from parent demand
        refreshed <- demand[demand$demand_id %in% demand_subset$demand_id, ]
        demand_subset$weight[match(refreshed$demand_id, demand_subset$demand_id)] <-
          refreshed$demand_MgDM
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
    
    rerouted <- list()
    
    for (i in seq_len(nrow(violations))) {
      violation <- violations[i, ]
      idx <- which(
        result_tbl$Year == violation$Year &
          result_tbl$Territory == violation$Territory &
          result_tbl$item_cbs == violation$item_cbs &
          result_tbl$Cat_feed == "Grass"
      )
      if (!length(idx)) {
        next
      }
      
      total_item <- sum(result_tbl$intake_MgDM[idx], na.rm = TRUE)
      if (total_item <= 1e-9) {
        next
      }
      
      target <- min(violation$excess, total_item)
      if (target <= 1e-9) {
        next
      }
      
      reduction <- result_tbl$intake_MgDM[idx] / total_item * target
      reduction_total <- sum(reduction)
      if (!is.finite(reduction_total) || reduction_total <= 0) {
        next
      }
      adjust <- target - reduction_total
      if (abs(adjust) > 1e-6) {
        reduction[1] <- reduction[1] + adjust
      }
      reduction <- pmin(result_tbl$intake_MgDM[idx], reduction)
      reduction <- pmax(0, reduction)
      
      result_tbl$intake_MgDM[idx] <- pmax(0, result_tbl$intake_MgDM[idx] - reduction)
      
      new_rows <- result_tbl[idx, , drop = FALSE]
      new_rows$intake_MgDM <- reduction
      new_rows$item_cbs <- "Grassland"
      new_rows$Cat_1 <- "Grassland"
      new_rows$Cat_feed <- "Grass"
      new_rows$hierarchy_level <- final_level
      new_rows$original_Province <- new_rows$Province_name
      new_rows$avail_id <- NA_integer_
      rerouted[[length(rerouted) + 1L]] <- new_rows
    }
    
    if (length(rerouted)) {
      additions <- dplyr::bind_rows(rerouted) |>
        dplyr::filter(intake_MgDM > 1e-9)
      result_tbl <- dplyr::bind_rows(result_tbl, additions)
    }
    
    result_tbl
  }
  
  apply_max_intake_caps <- function(result_tbl) {
    if (!nrow(max_intake_limits)) {
      return(result_tbl)
    }
    
    limited_rows <- result_tbl |>
      dplyr::semi_join(max_intake_limits, by = c("Livestock_cat", "item_cbs"))
    if (nrow(limited_rows) == 0) {
      return(result_tbl)
    }
    
    totals <- result_tbl |>
      dplyr::summarize(
        .by = c("Year", "Territory", "Province_name", "Livestock_cat"),
        total_dm = sum(intake_MgDM, na.rm = TRUE)
      )
    
    item_totals <- limited_rows |>
      dplyr::summarize(
        .by = c("Year", "Territory", "Province_name", "Livestock_cat", "item_cbs"),
        item_dm = sum(intake_MgDM, na.rm = TRUE)
      )
    
    violations <- item_totals |>
      dplyr::left_join(totals, by = c("Year", "Territory", "Province_name", "Livestock_cat")) |>
      dplyr::left_join(max_intake_limits, by = c("Livestock_cat", "item_cbs")) |>
      dplyr::mutate(
        total_dm = dplyr::coalesce(total_dm, 0),
        max_intake_share = dplyr::coalesce(max_intake_share, 0),
        limit_dm = total_dm * max_intake_share,
        limit_dm = dplyr::if_else(!is.finite(limit_dm), 0, limit_dm),
        excess = item_dm - limit_dm
      ) |>
      dplyr::filter(excess > 1e-6)
    
    if (nrow(violations) == 0) {
      return(result_tbl)
    }
    
    # Optimization: Vectorized reduction instead of row-by-row loop
    # Join violations back to result_tbl to identify rows to reduce
    
    rows_to_reduce <- result_tbl |>
      dplyr::inner_join(
        violations |> dplyr::select(Year, Territory, Province_name, Livestock_cat, item_cbs, excess, item_dm),
        by = c("Year", "Territory", "Province_name", "Livestock_cat", "item_cbs")
      )
    
    if (nrow(rows_to_reduce) == 0) return(result_tbl)
    
    # Use row-index approach: join violations back and
    # scale intake proportionally so total equals the cap.
    result_tbl$row_id <- seq_len(nrow(result_tbl))
    
    keys <- violations |> 
      dplyr::select(Year, Territory, Province_name, Livestock_cat, item_cbs, excess, item_dm)
    
    # Find indices of rows to reduce
    # This join might be expensive if result_tbl is huge, but it's necessary
    matches <- result_tbl |>
      dplyr::inner_join(keys, by = c("Year", "Territory", "Province_name", "Livestock_cat", "item_cbs")) |>
      dplyr::mutate(
        reduction_factor = pmax(0, (item_dm - excess) / item_dm),
        new_intake = intake_MgDM * reduction_factor
      )
    
    if (nrow(matches) > 0) {
      result_tbl$intake_MgDM[matches$row_id] <- matches$new_intake
    }
    
    result_tbl$row_id <- NULL
    
    # Note: We do NOT substitute with Grassland anymore, prioritizing the constraint.
    
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
  result <- apply_max_intake_caps(result)
  
  result <- result |>
    dplyr::left_join(demand |>
                       dplyr::select(demand_id, demand_MgDM, fixed_demand),
                     by = "demand_id")
  
  scaling_factors <- result |>
    dplyr::summarize(.by = demand_id, total_intake = sum(intake_MgDM, na.rm = TRUE)) |>
    dplyr::left_join(demand |>
                       dplyr::select(demand_id, demand_MgDM),
                     by = "demand_id") |>
    dplyr::mutate(
      scaling_factor = dplyr::case_when(
        demand_MgDM > 0 ~ total_intake / demand_MgDM,
        total_intake > 0 ~ NA_real_,
        TRUE ~ 0
      )
    ) |>
    dplyr::select(demand_id, scaling_factor)
  
  result <- result |>
    dplyr::left_join(scaling_factors, by = "demand_id")
  
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

  # Recalculate final scaling factors after all intake adjustments
  scaling_factors_final <- result |>
    dplyr::summarize(.by = demand_id, total_intake = sum(intake_MgDM, na.rm = TRUE)) |>
    dplyr::left_join(demand |>
                       dplyr::select(demand_id, demand_MgDM),
                     by = "demand_id") |>
    dplyr::mutate(
      scaling_factor = dplyr::case_when(
        demand_MgDM > 0 ~ total_intake / demand_MgDM,
        total_intake > 0 ~ NA_real_,
        TRUE ~ 0
      )
    ) |>
    dplyr::select(demand_id, scaling_factor)

  result <- result |>
    dplyr::select(-scaling_factor) |>
    dplyr::left_join(scaling_factors_final, by = "demand_id")
  
  result <- result |>
    dplyr::select(Year, Territory, Province_name, Livestock_cat, item_cbs, Cat_1, Cat_feed,
                  demand_MgDM, intake_MgDM, scaling_factor, hierarchy_level,
                  original_demand_item, fixed_demand, original_Province) |>
    dplyr::arrange(Year, Territory, Province_name, Livestock_cat, original_demand_item, hierarchy_level)

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
