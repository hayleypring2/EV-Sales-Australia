options(stringsAsFactors = FALSE)

read_csv_base <- function(path) {
  read.csv(path, na.strings = c("", "NA"), check.names = FALSE)
}

observations <- read_csv_base("data/seed/observations.csv")
sources <- read_csv_base("data/seed/source_register.csv")
model_sales <- read_csv_base("data/seed/model_sales_observations.csv")
model_lookup <- read_csv_base("data/seed/model_lookup.csv")
model_powertrain_observations <- read_csv_base("data/seed/model_powertrain_observations.csv")
scrape_targets <- read_csv_base("data/seed/scrape_targets.csv")

required_observation_cols <- c(
  "period", "period_type", "geography", "metric", "vehicle_type",
  "powertrain", "value", "unit", "estimate_status", "source_id"
)
missing_observation_cols <- setdiff(required_observation_cols, names(observations))
if (length(missing_observation_cols) > 0) {
  stop("Missing observation columns: ", paste(missing_observation_cols, collapse = ", "))
}

missing_sources <- setdiff(unique(observations$source_id), sources$source_id)
if (length(missing_sources) > 0) {
  stop("Observations reference unknown source_id values: ", paste(missing_sources, collapse = ", "))
}

required_model_sales_cols <- c(
  "period", "geography", "rank", "make", "model",
  "vehicle_type_reported", "sales", "source_id"
)
missing_model_sales_cols <- setdiff(required_model_sales_cols, names(model_sales))
if (length(missing_model_sales_cols) > 0) {
  stop("Missing model sales columns: ", paste(missing_model_sales_cols, collapse = ", "))
}

required_model_lookup_cols <- c(
  "make", "model", "canonical_model", "vehicle_type",
  "powertrain_profile", "confidence", "source_id"
)
missing_model_lookup_cols <- setdiff(required_model_lookup_cols, names(model_lookup))
if (length(missing_model_lookup_cols) > 0) {
  stop("Missing model lookup columns: ", paste(missing_model_lookup_cols, collapse = ", "))
}

model_source_ids <- unique(c(
  model_sales$source_id,
  model_lookup$source_id,
  model_powertrain_observations$source_id,
  scrape_targets$target_id[scrape_targets$status == "validated"]
))
missing_model_sources <- setdiff(model_source_ids, sources$source_id)
if (length(missing_model_sources) > 0) {
  stop("Model tables reference unknown source_id values: ", paste(missing_model_sources, collapse = ", "))
}

model_key <- function(make, model) paste(make, model, sep = "\r")
missing_lookup <- setdiff(
  model_key(model_sales$make, model_sales$model),
  model_key(model_lookup$make, model_lookup$model)
)
if (length(missing_lookup) > 0) {
  stop("Model sales rows missing lookup entries: ", paste(missing_lookup, collapse = "; "))
}

annual_plugin <- observations[
  observations$period_type == "annual_sales" &
    observations$metric == "new_vehicle_sales" &
    observations$vehicle_type == "total" &
    observations$powertrain %in% c("BEV", "BEV_or_PHEV"),
]

annual_plugin <- annual_plugin[order(annual_plugin$period, annual_plugin$powertrain), ]

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write.csv(annual_plugin, "data/processed/annual_plugin_sales_seed.csv", row.names = FALSE)

annual_category <- observations[
  observations$period_type == "annual_sales" &
    observations$metric == "new_vehicle_sales" &
    observations$vehicle_type %in% c("SUV", "light_commercial", "passenger", "heavy_commercial") &
    observations$powertrain == "total",
]
annual_category <- annual_category[order(annual_category$period, annual_category$vehicle_type), ]
write.csv(annual_category, "data/processed/annual_category_sales_seed.csv", row.names = FALSE)

annual_fuel <- observations[
  observations$period_type == "annual_sales" &
    observations$metric == "new_vehicle_sales" &
    observations$vehicle_type == "total" &
    observations$powertrain != "total",
]
annual_fuel <- annual_fuel[order(annual_fuel$period, annual_fuel$powertrain), ]
write.csv(annual_fuel, "data/processed/annual_fuel_sales_seed.csv", row.names = FALSE)

model_sales_enriched <- merge(
  model_sales,
  model_lookup,
  by = c("make", "model"),
  all.x = TRUE,
  sort = FALSE
)
model_sales_enriched <- model_sales_enriched[order(model_sales_enriched$period, model_sales_enriched$rank), ]
write.csv(model_sales_enriched, "data/processed/model_sales_enriched.csv", row.names = FALSE)

top100_by_vehicle_type <- aggregate(
  sales ~ period + geography + vehicle_type,
  data = model_sales_enriched,
  FUN = sum
)
top100_by_vehicle_type <- top100_by_vehicle_type[order(
  top100_by_vehicle_type$period,
  -top100_by_vehicle_type$sales
), ]
write.csv(top100_by_vehicle_type, "data/processed/model_sales_by_vehicle_type.csv", row.names = FALSE)

top100_by_powertrain_profile <- aggregate(
  sales ~ period + geography + powertrain_profile,
  data = model_sales_enriched,
  FUN = sum
)
top100_by_powertrain_profile <- top100_by_powertrain_profile[order(
  top100_by_powertrain_profile$period,
  -top100_by_powertrain_profile$sales
), ]
write.csv(
  top100_by_powertrain_profile,
  "data/processed/model_sales_by_powertrain_profile.csv",
  row.names = FALSE
)

model_sales_period_summary <- aggregate(
  sales ~ period + geography,
  data = model_sales,
  FUN = sum
)
names(model_sales_period_summary)[names(model_sales_period_summary) == "sales"] <- "model_sales_sum"
model_sales_row_counts <- aggregate(
  rank ~ period + geography,
  data = model_sales,
  FUN = length
)
names(model_sales_row_counts)[names(model_sales_row_counts) == "rank"] <- "model_rows"
model_sales_period_summary <- merge(
  model_sales_period_summary,
  model_sales_row_counts,
  by = c("period", "geography"),
  all.x = TRUE
)
annual_market_totals <- observations[
  observations$period_type == "annual_sales" &
    observations$metric == "new_vehicle_sales" &
    observations$vehicle_type == "total" &
    observations$powertrain == "total",
  c("period", "geography", "value")
]
names(annual_market_totals)[names(annual_market_totals) == "value"] <- "reported_total_market_sales"
model_sales_period_summary <- merge(
  model_sales_period_summary,
  annual_market_totals,
  by = c("period", "geography"),
  all.x = TRUE
)
model_sales_period_summary$model_market_coverage_percent <- round(
  model_sales_period_summary$model_sales_sum /
    model_sales_period_summary$reported_total_market_sales * 100,
  1
)
model_sales_period_summary <- model_sales_period_summary[order(model_sales_period_summary$period), ]
write.csv(
  model_sales_period_summary,
  "data/processed/model_sales_period_summary.csv",
  row.names = FALSE
)

write.csv(
  model_powertrain_observations,
  "data/processed/model_powertrain_observations_seed.csv",
  row.names = FALSE
)

known_model_powertrain_by_type <- aggregate(
  sales ~ period + geography + vehicle_type + powertrain,
  data = model_powertrain_observations,
  FUN = sum
)
known_model_powertrain_by_type <- known_model_powertrain_by_type[order(
  known_model_powertrain_by_type$period,
  known_model_powertrain_by_type$vehicle_type,
  known_model_powertrain_by_type$powertrain
), ]
write.csv(
  known_model_powertrain_by_type,
  "data/processed/known_model_powertrain_2024_by_type.csv",
  row.names = FALSE
)

make_panel_rows <- function(data, observation_level, source_id, estimate_status, notes) {
  data.frame(
    period = as.character(data$period),
    geography = data$geography,
    vehicle_type = data$vehicle_type,
    powertrain = data$powertrain,
    sales = as.numeric(data$sales),
    unit = "vehicles",
    period_type = "annual_sales",
    metric = "new_vehicle_sales",
    source_id = source_id,
    estimate_status = estimate_status,
    coverage_status = data$coverage_status,
    observation_level = observation_level,
    preferred_for_projection = data$preferred_for_projection,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

annual_panel_parts <- list()
if (file.exists("data/processed/aaa_sales_by_broad_vehicle_type_fuel_year.csv")) {
  aaa_year <- read_csv_base("data/processed/aaa_sales_by_broad_vehicle_type_fuel_year.csv")
  aaa_year$period <- as.character(aaa_year$year)
  aaa_year$vehicle_type <- aaa_year$broad_vehicle_type
  aaa_year$powertrain <- aaa_year$fuel_type
  aaa_year$preferred_for_projection <- TRUE
  annual_panel_parts[["aaa_vehicle_type_powertrain"]] <- make_panel_rows(
    aaa_year,
    "vehicle_type_powertrain",
    "aaa_ev_index",
    "observed",
    "AAA EV Index direct annual aggregation from state/model/vehicle-type/fuel quarterly rows."
  )

  aaa_vehicle_total <- aggregate(
    sales ~ year + geography + broad_vehicle_type + quarters_available + coverage_status,
    data = aaa_year,
    FUN = sum
  )
  aaa_vehicle_total$period <- as.character(aaa_vehicle_total$year)
  aaa_vehicle_total$vehicle_type <- aaa_vehicle_total$broad_vehicle_type
  aaa_vehicle_total$powertrain <- "total"
  aaa_vehicle_total$preferred_for_projection <- TRUE
  annual_panel_parts[["aaa_vehicle_type_total"]] <- make_panel_rows(
    aaa_vehicle_total,
    "vehicle_type_total",
    "aaa_ev_index",
    "observed",
    "AAA EV Index rollup across fuel types."
  )

  aaa_powertrain_total <- aggregate(
    sales ~ year + geography + fuel_type + quarters_available + coverage_status,
    data = aaa_year,
    FUN = sum
  )
  aaa_powertrain_total$period <- as.character(aaa_powertrain_total$year)
  aaa_powertrain_total$vehicle_type <- "total"
  aaa_powertrain_total$powertrain <- aaa_powertrain_total$fuel_type
  aaa_powertrain_total$preferred_for_projection <- TRUE
  annual_panel_parts[["aaa_powertrain_total"]] <- make_panel_rows(
    aaa_powertrain_total,
    "powertrain_total",
    "aaa_ev_index",
    "observed",
    "AAA EV Index rollup across broad light-vehicle types."
  )

  aaa_market_total <- aggregate(
    sales ~ year + geography + quarters_available + coverage_status,
    data = aaa_year,
    FUN = sum
  )
  aaa_market_total$period <- as.character(aaa_market_total$year)
  aaa_market_total$vehicle_type <- "total"
  aaa_market_total$powertrain <- "total"
  aaa_market_total$preferred_for_projection <- TRUE
  annual_panel_parts[["aaa_market_total"]] <- make_panel_rows(
    aaa_market_total,
    "market_total",
    "aaa_ev_index",
    "observed",
    "AAA EV Index light-vehicle market rollup across broad vehicle and fuel types."
  )
}

seed_sales <- observations[
  observations$period_type == "annual_sales" &
    observations$metric == "new_vehicle_sales" &
    observations$unit == "vehicles",
]
if (nrow(seed_sales) > 0) {
  seed_sales$year_int <- suppressWarnings(as.integer(seed_sales$period))
  seed_sales$sales <- seed_sales$value
  seed_sales$coverage_status <- "source_reported_period"
  seed_sales$preferred_for_projection <- is.na(seed_sales$year_int) |
    seed_sales$year_int < 2022 |
    seed_sales$vehicle_type == "heavy_commercial"
  seed_sales$observation_level <- ifelse(
    seed_sales$vehicle_type == "total" & seed_sales$powertrain == "total",
    "market_total",
    ifelse(
      seed_sales$vehicle_type == "total",
      "powertrain_total",
      ifelse(seed_sales$powertrain == "total", "vehicle_type_total", "vehicle_type_powertrain")
    )
  )
  annual_panel_parts[["seed_observations"]] <- data.frame(
    period = as.character(seed_sales$period),
    geography = seed_sales$geography,
    vehicle_type = seed_sales$vehicle_type,
    powertrain = seed_sales$powertrain,
    sales = as.numeric(seed_sales$sales),
    unit = seed_sales$unit,
    period_type = seed_sales$period_type,
    metric = seed_sales$metric,
    source_id = seed_sales$source_id,
    estimate_status = seed_sales$estimate_status,
    coverage_status = seed_sales$coverage_status,
    observation_level = seed_sales$observation_level,
    preferred_for_projection = seed_sales$preferred_for_projection,
    notes = seed_sales$notes,
    stringsAsFactors = FALSE
  )
}

if (length(annual_panel_parts) > 0) {
  annual_panel <- do.call(rbind, annual_panel_parts)
  annual_panel <- merge(
    annual_panel,
    sources[c("source_id", "grade", "source_type", "publisher")],
    by = "source_id",
    all.x = TRUE,
    sort = FALSE
  )
  names(annual_panel)[names(annual_panel) == "grade"] <- "source_grade"

  annual_panel$tesla_scope <- ifelse(
    grepl("excl_Tesla|excluding Tesla|excludes Tesla", annual_panel$powertrain) |
      grepl("excl_Tesla|excluding Tesla|excludes Tesla", annual_panel$notes, ignore.case = TRUE),
    "excludes_tesla",
    ifelse(
      annual_panel$source_id == "aaa_ev_index" |
        (annual_panel$period >= "2025" & annual_panel$source_id == "carexpert_2025_full"),
      "includes_tesla",
      "unknown_or_mixed"
    )
  )
  annual_panel$powertrain_canonical <- annual_panel$powertrain
  annual_panel$powertrain_canonical[annual_panel$powertrain %in% c("ICE_petrol", "ICE_diesel", "ICE")] <- annual_panel$powertrain[annual_panel$powertrain %in% c("ICE_petrol", "ICE_diesel", "ICE")]
  annual_panel$powertrain_canonical[annual_panel$powertrain == "BEV_excl_Tesla"] <- "BEV"
  annual_panel$powertrain_canonical[annual_panel$powertrain == "BEV_or_PHEV_excl_Tesla"] <- "BEV_or_PHEV"
  annual_panel$market_scope <- ifelse(
    annual_panel$source_id == "aaa_ev_index" |
      annual_panel$vehicle_type %in% c("passenger", "SUV", "light_commercial", "people_mover"),
    "light_vehicle",
    ifelse(
      annual_panel$vehicle_type == "heavy_commercial",
      "heavy_commercial_only",
      "all_new_vehicles"
    )
  )

  annual_panel$row_quality <- ifelse(
    annual_panel$source_id == "aaa_ev_index" & annual_panel$coverage_status == "complete_year",
    "direct_dashboard_complete",
    ifelse(
      annual_panel$source_id == "aaa_ev_index" & annual_panel$coverage_status == "partial_year",
      "direct_dashboard_partial",
      ifelse(
        annual_panel$estimate_status == "derived",
        "derived_from_public_percent_or_residual",
        ifelse(
          annual_panel$estimate_status == "approximate",
          "approximate_reported",
          ifelse(
            annual_panel$source_grade == "A",
            "direct_official_or_primary",
            ifelse(
              annual_panel$source_grade == "B",
              "direct_media_vfacts_quote",
              ifelse(annual_panel$source_grade == "C", "tertiary_compilation", "ungraded")
            )
          )
        )
      )
    )
  )
  annual_panel$uncertainty_band_percent <- ifelse(
    annual_panel$row_quality == "direct_dashboard_complete",
    0.5,
    ifelse(
      annual_panel$row_quality == "direct_dashboard_partial",
      3,
      ifelse(
        annual_panel$row_quality == "direct_official_or_primary",
        1,
        ifelse(
          annual_panel$row_quality == "direct_media_vfacts_quote",
          2,
          ifelse(
            annual_panel$row_quality == "derived_from_public_percent_or_residual",
            8,
            ifelse(annual_panel$row_quality == "approximate_reported", 12, 20)
          )
        )
      )
    )
  )
  annual_panel$lower_sales <- round(annual_panel$sales * (1 - annual_panel$uncertainty_band_percent / 100))
  annual_panel$upper_sales <- round(annual_panel$sales * (1 + annual_panel$uncertainty_band_percent / 100))
  annual_panel$projection_caveat <- ifelse(
    annual_panel$tesla_scope == "excludes_tesla",
    "excludes Tesla; not directly comparable with later all-brand BEV totals",
    ifelse(
      annual_panel$coverage_status == "partial_year",
      "partial year; do not compare with full-year totals without annualisation",
      ifelse(
        annual_panel$observation_level == "market_total" &
          annual_panel$market_scope == "light_vehicle",
        "light-vehicle market only; excludes heavy commercial",
        ifelse(
          annual_panel$powertrain %in% c("BEV_or_PHEV", "BEV_or_PHEV_excl_Tesla"),
        "combined plug-in bucket; do not sum with separate BEV/PHEV rows",
        ""
        )
      )
    )
  )
  annual_panel <- annual_panel[order(
    annual_panel$period,
    !annual_panel$preferred_for_projection,
    annual_panel$vehicle_type,
    annual_panel$powertrain,
    annual_panel$source_id
  ), ]
  write.csv(annual_panel, "data/processed/sales_panel_annual.csv", row.names = FALSE)

  preferred_annual_panel <- annual_panel[annual_panel$preferred_for_projection, ]
  write.csv(
    preferred_annual_panel,
    "data/processed/sales_panel_annual_preferred.csv",
    row.names = FALSE
  )

  write.csv(
    annual_panel,
    "data/processed/sales_panel_annual_quality_scored.csv",
    row.names = FALSE
  )

  duplicate_key_counts <- aggregate(
    sales ~ period + geography + vehicle_type + powertrain + observation_level,
    data = annual_panel,
    FUN = length
  )
  names(duplicate_key_counts)[names(duplicate_key_counts) == "sales"] <- "row_count"
  duplicate_keys <- duplicate_key_counts[duplicate_key_counts$row_count > 1, ]
  if (nrow(duplicate_keys) > 0) {
    overlap <- merge(
      annual_panel,
      duplicate_keys[
        c("period", "geography", "vehicle_type", "powertrain", "observation_level", "row_count")
      ],
      by = c("period", "geography", "vehicle_type", "powertrain", "observation_level"),
      all.y = TRUE,
      sort = FALSE
    )
    overlap_range <- aggregate(
      sales ~ period + geography + vehicle_type + powertrain + observation_level,
      data = overlap,
      FUN = function(x) max(x) - min(x)
    )
    names(overlap_range)[names(overlap_range) == "sales"] <- "range_sales"
    overlap <- merge(
      overlap,
      overlap_range,
      by = c("period", "geography", "vehicle_type", "powertrain", "observation_level"),
      all.x = TRUE,
      sort = FALSE
    )
    overlap <- overlap[order(
      overlap$period,
      overlap$vehicle_type,
      overlap$powertrain,
      !overlap$preferred_for_projection,
      overlap$source_id
    ), ]
  } else {
    overlap <- data.frame(
      period = character(),
      geography = character(),
      vehicle_type = character(),
      powertrain = character(),
      observation_level = character(),
      row_count = integer(),
      range_sales = numeric(),
      stringsAsFactors = FALSE
    )
  }
  write.csv(overlap, "data/processed/sales_panel_overlap_diagnostics.csv", row.names = FALSE)

  canonical_powertrains <- c(
    "total", "ICE", "ICE_petrol", "ICE_diesel", "HEV", "PHEV", "BEV",
    "FCEV", "BEV_or_PHEV"
  )
  projection_input <- preferred_annual_panel[
    preferred_annual_panel$powertrain_canonical %in% canonical_powertrains &
      !preferred_annual_panel$powertrain %in% c("LPG"),
  ]
  projection_input$projection_ready <- projection_input$coverage_status != "partial_year" &
    projection_input$tesla_scope != "excludes_tesla" &
    projection_input$row_quality != "tertiary_compilation"
  projection_input$projection_ready[
    projection_input$period < "2022" &
      projection_input$observation_level == "vehicle_type_powertrain"
  ] <- FALSE
  projection_input <- projection_input[order(
    projection_input$period,
    projection_input$observation_level,
    projection_input$vehicle_type,
    projection_input$powertrain_canonical
  ), ]
  write.csv(
    projection_input,
    "data/processed/sales_panel_projection_input.csv",
    row.names = FALSE
  )

  annual_coverage_summary <- aggregate(
    sales ~ period + observation_level + row_quality,
    data = preferred_annual_panel,
    FUN = length
  )
  names(annual_coverage_summary)[names(annual_coverage_summary) == "sales"] <- "row_count"
  annual_coverage_summary <- annual_coverage_summary[order(
    annual_coverage_summary$period,
    annual_coverage_summary$observation_level,
    annual_coverage_summary$row_quality
  ), ]
  write.csv(
    annual_coverage_summary,
    "data/processed/sales_panel_coverage_summary.csv",
    row.names = FALSE
  )

  consistency_parts <- list()
  preferred_vehicle_totals <- preferred_annual_panel[
    preferred_annual_panel$observation_level == "vehicle_type_total",
  ]
  if (nrow(preferred_vehicle_totals) > 0) {
    class_sum <- aggregate(
      sales ~ period + geography + market_scope,
      data = preferred_vehicle_totals,
      FUN = sum
    )
    names(class_sum)[names(class_sum) == "sales"] <- "component_sales"
    market_totals <- preferred_annual_panel[
      preferred_annual_panel$observation_level == "market_total",
      c("period", "geography", "market_scope", "sales")
    ]
    names(market_totals)[names(market_totals) == "sales"] <- "reported_total_sales"
    class_check <- merge(
      class_sum,
      market_totals,
      by = c("period", "geography", "market_scope"),
      all.x = TRUE,
      sort = FALSE
    )
    class_check$check_type <- "vehicle_type_totals_vs_market_total"
    class_check$difference <- class_check$component_sales - class_check$reported_total_sales
    class_check$difference_percent <- round(class_check$difference / class_check$reported_total_sales * 100, 2)
    consistency_parts[["class_check"]] <- class_check

    all_class_sum <- aggregate(
      sales ~ period + geography,
      data = preferred_vehicle_totals,
      FUN = sum
    )
    names(all_class_sum)[names(all_class_sum) == "sales"] <- "component_sales"
    all_market_totals <- preferred_annual_panel[
      preferred_annual_panel$observation_level == "market_total" &
        preferred_annual_panel$market_scope == "all_new_vehicles",
      c("period", "geography", "sales")
    ]
    names(all_market_totals)[names(all_market_totals) == "sales"] <- "reported_total_sales"
    all_class_check <- merge(
      all_class_sum,
      all_market_totals,
      by = c("period", "geography"),
      all.x = TRUE,
      sort = FALSE
    )
    all_class_check$market_scope <- "all_new_vehicles"
    all_class_check$check_type <- "all_vehicle_type_totals_vs_all_market_total"
    all_class_check$difference <- all_class_check$component_sales - all_class_check$reported_total_sales
    all_class_check$difference_percent <- round(
      all_class_check$difference / all_class_check$reported_total_sales * 100,
      2
    )
    consistency_parts[["all_class_check"]] <- all_class_check
  }

  preferred_powertrain_totals <- preferred_annual_panel[
    preferred_annual_panel$observation_level == "powertrain_total" &
      preferred_annual_panel$powertrain %in% c("ICE", "ICE_petrol", "ICE_diesel", "HEV", "PHEV", "BEV", "FCEV"),
  ]
  if (nrow(preferred_powertrain_totals) > 0) {
    fuel_sum <- aggregate(
      sales ~ period + geography + market_scope,
      data = preferred_powertrain_totals,
      FUN = sum
    )
    names(fuel_sum)[names(fuel_sum) == "sales"] <- "component_sales"
    market_totals <- preferred_annual_panel[
      preferred_annual_panel$observation_level == "market_total",
      c("period", "geography", "market_scope", "sales")
    ]
    names(market_totals)[names(market_totals) == "sales"] <- "reported_total_sales"
    fuel_check <- merge(
      fuel_sum,
      market_totals,
      by = c("period", "geography", "market_scope"),
      all.x = TRUE,
      sort = FALSE
    )
    fuel_check$check_type <- "powertrain_totals_vs_market_total"
    fuel_check$difference <- fuel_check$component_sales - fuel_check$reported_total_sales
    fuel_check$difference_percent <- round(fuel_check$difference / fuel_check$reported_total_sales * 100, 2)
    consistency_parts[["fuel_check"]] <- fuel_check
  }
  if (length(consistency_parts) > 0) {
    consistency <- do.call(rbind, consistency_parts)
    consistency$comparison_status <- ifelse(
      is.na(consistency$reported_total_sales),
      "no_matching_total",
      ifelse(
        is.na(consistency$difference_percent),
        "not_comparable",
        ifelse(
          abs(consistency$difference_percent) <= 0.5,
          "matches_within_0.5_percent",
          ifelse(
            consistency$difference_percent < -5,
            "component_rows_incomplete",
            "review_difference"
          )
        )
      )
    )
    consistency <- consistency[
      order(consistency$period, consistency$market_scope, consistency$check_type),
      c(
        "period", "geography", "market_scope", "check_type",
        "component_sales", "reported_total_sales", "difference", "difference_percent",
        "comparison_status"
      )
    ]
  } else {
    consistency <- data.frame(
      period = character(),
      geography = character(),
      market_scope = character(),
      check_type = character(),
      component_sales = numeric(),
      reported_total_sales = numeric(),
      difference = numeric(),
      difference_percent = numeric(),
      stringsAsFactors = FALSE
    )
  }
  write.csv(
    consistency,
    "data/processed/sales_panel_internal_consistency.csv",
    row.names = FALSE
  )
}

market_2024 <- observations[
  observations$period == "2024" &
    observations$period_type == "annual_sales" &
    observations$metric == "new_vehicle_sales" &
    observations$vehicle_type == "total" &
    observations$powertrain == "total",
]
top100_2024_sales <- sum(model_sales$sales[model_sales$period == "2024"])
diagnostics <- data.frame(
  period = "2024",
  geography = "Australia",
  metric = c(
    "reported_total_market_sales",
    "top100_model_sales_sum",
    "top100_market_coverage_percent",
    "known_bev_top100_sales",
    "approx_reported_total_bev_sales",
    "known_bev_top100_capture_percent"
  ),
  value = c(
    market_2024$value[1],
    top100_2024_sales,
    round(top100_2024_sales / market_2024$value[1] * 100, 1),
    sum(model_powertrain_observations$sales[
      model_powertrain_observations$period == "2024" &
        model_powertrain_observations$powertrain == "BEV"
    ]),
    observations$value[
      observations$period == "2024" &
        observations$period_type == "annual_sales" &
        observations$metric == "new_vehicle_sales" &
        observations$vehicle_type == "total" &
        observations$powertrain == "BEV"
    ][1],
    round(
      sum(model_powertrain_observations$sales[
        model_powertrain_observations$period == "2024" &
          model_powertrain_observations$powertrain == "BEV"
      ]) /
        observations$value[
          observations$period == "2024" &
            observations$period_type == "annual_sales" &
            observations$metric == "new_vehicle_sales" &
            observations$vehicle_type == "total" &
            observations$powertrain == "BEV"
        ][1] * 100,
      1
    )
  ),
  unit = c("vehicles", "vehicles", "percent", "vehicles", "vehicles", "percent"),
  notes = c(
    "Reported by Guardian from VFACTS.",
    "Sum of Chasing Cars top-100 model table; commercial vans excluded by source.",
    "Top-100 model sales divided by reported 2024 total market sales.",
    "Sum of BEV-only models in explicit model_powertrain_observations.",
    "Approximate EV sales reported by Guardian.",
    "Known BEV-only top-100 sales divided by approximate total BEV sales."
  )
)
write.csv(diagnostics, "data/processed/diagnostics_2024.csv", row.names = FALSE)

message("Validated ", nrow(observations), " observations against ", nrow(sources), " sources.")
message("Validated ", nrow(model_sales), " model sales rows against ", nrow(model_lookup), " lookup rows.")
message("Wrote data/processed/annual_plugin_sales_seed.csv")
message("Wrote data/processed/annual_category_sales_seed.csv")
message("Wrote data/processed/annual_fuel_sales_seed.csv")
message("Wrote data/processed/model_sales_enriched.csv")
message("Wrote data/processed/model_sales_by_vehicle_type.csv")
message("Wrote data/processed/model_sales_by_powertrain_profile.csv")
message("Wrote data/processed/model_sales_period_summary.csv")
message("Wrote data/processed/known_model_powertrain_2024_by_type.csv")
message("Wrote data/processed/diagnostics_2024.csv")
if (file.exists("data/processed/sales_panel_annual.csv")) {
  message("Wrote data/processed/sales_panel_annual.csv")
  message("Wrote data/processed/sales_panel_annual_preferred.csv")
  message("Wrote data/processed/sales_panel_annual_quality_scored.csv")
  message("Wrote data/processed/sales_panel_projection_input.csv")
  message("Wrote data/processed/sales_panel_overlap_diagnostics.csv")
  message("Wrote data/processed/sales_panel_coverage_summary.csv")
  message("Wrote data/processed/sales_panel_internal_consistency.csv")
}
