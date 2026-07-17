options(stringsAsFactors = FALSE)

source_id <- "aaa_ev_index"
aaa_graphql_url <- "https://www.aaa.asn.au/graphql"
aaa_graphql_query <- '{"query":"{evIndexData{data}}"}'
aaa_csv_fallback_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQg5xNouUcxGFku6zkpxJ2Uw1eNn4fU1pvIs7PfVR6K1977ACQWmM8lWImLYnKQNUKzkAYpTeoC-uVo/pub?output=csv"

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

run_date <- as.character(Sys.Date())
request_json_path <- tempfile("aaa_ev_index_graphql_request_", fileext = ".json")
raw_json_path <- file.path("data/raw", paste0("aaa_ev_index_graphql_", run_date, ".json"))
raw_path <- file.path("data/raw", paste0("aaa_ev_index_", run_date, ".csv"))

download_graphql <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package jsonlite is required for AAA GraphQL ingest.")
  }

  writeLines(aaa_graphql_query, request_json_path, useBytes = TRUE)
  status <- system2(
    "curl",
    c(
      "-sS",
      aaa_graphql_url,
      "-H", "content-type:application/json",
      "--data-binary", paste0("@", request_json_path),
      "-o", raw_json_path
    )
  )
  if (!identical(status, 0L)) {
    stop("curl failed while downloading AAA GraphQL data.")
  }

  response <- jsonlite::fromJSON(raw_json_path, simplifyVector = TRUE)
  if (!is.null(response$errors)) {
    stop("AAA GraphQL returned errors: ", paste(response$errors$message, collapse = "; "))
  }
  if (is.null(response$data$evIndexData$data)) {
    stop("AAA GraphQL response did not include evIndexData.data.")
  }

  row_json <- response$data$evIndexData$data
  parsed_rows <- lapply(row_json, jsonlite::fromJSON, simplifyVector = TRUE)
  all_cols <- unique(unlist(lapply(parsed_rows, names), use.names = FALSE))
  base_cols <- c("STATE", "VEHICLE TYPE", "MANUFACTURER", "MODEL", "FUEL TYPE")
  quarter_cols <- grep("^Q[1-4] [0-9]{4}$", all_cols, value = TRUE)
  quarter_cols <- quarter_cols[order(
    as.integer(sub("^Q[1-4] ", "", quarter_cols)),
    as.integer(sub("^Q([1-4]) [0-9]{4}$", "\\1", quarter_cols))
  )]
  all_cols <- c(base_cols, setdiff(all_cols, c(base_cols, quarter_cols)), quarter_cols)

  raw <- do.call(rbind, lapply(parsed_rows, function(row) {
    row[setdiff(all_cols, names(row))] <- NA
    as.data.frame(row[all_cols], check.names = FALSE, stringsAsFactors = FALSE)
  }))
  write.csv(raw, raw_path, row.names = FALSE)
  raw
}

download_csv_fallback <- function() {
  download.file(aaa_csv_fallback_url, raw_path, mode = "wb", quiet = TRUE)
  read.csv(raw_path, check.names = FALSE, na.strings = c("", "NA"))
}

ingest_method <- "graphql"
raw <- tryCatch(
  download_graphql(),
  error = function(err) {
    warning("AAA GraphQL ingest failed; falling back to published Google Sheets CSV. ", conditionMessage(err))
    ingest_method <<- "google_sheets_csv_fallback"
    download_csv_fallback()
  }
)

required_cols <- c("STATE", "VEHICLE TYPE", "MANUFACTURER", "MODEL", "FUEL TYPE")
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("AAA CSV missing required columns: ", paste(missing_cols, collapse = ", "))
}

quarter_cols <- grep("^Q[1-4] [0-9]{4}$", names(raw), value = TRUE)
if (length(quarter_cols) == 0) {
  stop("AAA CSV does not contain quarter columns like 'Q1 2024'.")
}
quarter_cols <- quarter_cols[order(
  as.integer(sub("^Q[1-4] ", "", quarter_cols)),
  as.integer(sub("^Q([1-4]) [0-9]{4}$", "\\1", quarter_cols))
)]

standardise_fuel <- function(x) {
  out <- as.character(x)
  out[out == "Hybrid"] <- "HEV"
  out[out == "HFCEV"] <- "FCEV"
  out
}

standardise_broad_vehicle_type <- function(x) {
  out <- as.character(x)
  out[grepl("SUV", out)] <- "SUV"
  out[grepl("Car|Sports", out)] <- "passenger"
  out[grepl("Ute|Van", out)] <- "light_commercial"
  out[grepl("People Mover", out)] <- "people_mover"
  out
}

long_parts <- lapply(quarter_cols, function(q) {
  sales <- suppressWarnings(as.numeric(raw[[q]]))
  sales[is.na(sales)] <- 0
  quarter_num <- as.integer(sub("^Q([1-4]) [0-9]{4}$", "\\1", q))
  year <- as.integer(sub("^Q[1-4] ", "", q))
  data.frame(
    quarter = q,
    year = year,
    quarter_num = quarter_num,
    state = raw[["STATE"]],
    vehicle_type = raw[["VEHICLE TYPE"]],
    broad_vehicle_type = standardise_broad_vehicle_type(raw[["VEHICLE TYPE"]]),
    manufacturer = raw[["MANUFACTURER"]],
    model = raw[["MODEL"]],
    fuel_type = standardise_fuel(raw[["FUEL TYPE"]]),
    sales = sales,
    source_id = source_id,
    stringsAsFactors = FALSE
  )
})

long <- do.call(rbind, long_parts)
long <- long[order(long$year, long$quarter_num, long$state, long$manufacturer, long$model, long$fuel_type), ]
write.csv(long, "data/processed/aaa_ev_index_long.csv", row.names = FALSE)

agg_sum <- function(data, by) {
  out <- aggregate(sales ~ ., data = data[c(by, "sales")], FUN = sum)
  out <- out[order(out[[by[1]]], out[[by[2]]]), ]
  out
}

state_quarter <- agg_sum(
  long,
  c("year", "quarter_num", "quarter", "state", "vehicle_type", "broad_vehicle_type", "fuel_type")
)
write.csv(
  state_quarter,
  "data/processed/aaa_sales_by_state_vehicle_type_fuel_quarter.csv",
  row.names = FALSE
)

national_quarter <- aggregate(
  sales ~ year + quarter_num + quarter + vehicle_type + broad_vehicle_type + fuel_type,
  data = long,
  FUN = sum
)
national_quarter$geography <- "Australia"
national_quarter <- national_quarter[
  order(
    national_quarter$year,
    national_quarter$quarter_num,
    national_quarter$broad_vehicle_type,
    national_quarter$fuel_type
  ),
  c("year", "quarter_num", "quarter", "geography", "vehicle_type", "broad_vehicle_type", "fuel_type", "sales")
]
write.csv(
  national_quarter,
  "data/processed/aaa_sales_by_vehicle_type_fuel_quarter.csv",
  row.names = FALSE
)

broad_national_quarter <- aggregate(
  sales ~ year + quarter_num + quarter + broad_vehicle_type + fuel_type,
  data = long,
  FUN = sum
)
broad_national_quarter$geography <- "Australia"
broad_national_quarter <- broad_national_quarter[
  order(
    broad_national_quarter$year,
    broad_national_quarter$quarter_num,
    broad_national_quarter$broad_vehicle_type,
    broad_national_quarter$fuel_type
  ),
  c("year", "quarter_num", "quarter", "geography", "broad_vehicle_type", "fuel_type", "sales")
]
write.csv(
  broad_national_quarter,
  "data/processed/aaa_sales_by_broad_vehicle_type_fuel_quarter.csv",
  row.names = FALSE
)

broad_national_year <- aggregate(
  sales ~ year + broad_vehicle_type + fuel_type,
  data = long,
  FUN = sum
)
quarters_by_year <- aggregate(
  quarter ~ year,
  data = unique(long[c("year", "quarter")]),
  FUN = length
)
names(quarters_by_year)[names(quarters_by_year) == "quarter"] <- "quarters_available"
broad_national_year <- merge(broad_national_year, quarters_by_year, by = "year", all.x = TRUE)
broad_national_year$geography <- "Australia"
broad_national_year$coverage_status <- ifelse(
  broad_national_year$quarters_available == 4,
  "complete_year",
  "partial_year"
)
broad_national_year <- broad_national_year[
  order(broad_national_year$year, broad_national_year$broad_vehicle_type, broad_national_year$fuel_type),
  c(
    "year", "geography", "broad_vehicle_type", "fuel_type",
    "sales", "quarters_available", "coverage_status"
  )
]
write.csv(
  broad_national_year,
  "data/processed/aaa_sales_by_broad_vehicle_type_fuel_year.csv",
  row.names = FALSE
)

if (file.exists("data/seed/observations.csv")) {
  observations <- read.csv("data/seed/observations.csv", check.names = FALSE, na.strings = c("", "NA"))
  observed_light_categories <- observations[
    observations$period_type == "annual_sales" &
      observations$metric == "new_vehicle_sales" &
      observations$vehicle_type %in% c("SUV", "light_commercial", "passenger") &
      observations$powertrain == "total",
    c("period", "vehicle_type", "value")
  ]
  observed_light_categories$year <- suppressWarnings(as.integer(observed_light_categories$period))
  observed_light_categories <- observed_light_categories[!is.na(observed_light_categories$year), ]
  observed_light_totals <- aggregate(
    value ~ year,
    data = observed_light_categories,
    FUN = sum
  )
  names(observed_light_totals)[names(observed_light_totals) == "value"] <- "observed_light_market_sales"

  aaa_year_totals <- aggregate(
    sales ~ year,
    data = broad_national_year,
    FUN = sum
  )
  names(aaa_year_totals)[names(aaa_year_totals) == "sales"] <- "aaa_light_market_sales"

  reconciliation <- merge(aaa_year_totals, observed_light_totals, by = "year", all.x = TRUE)
  reconciliation$difference <- reconciliation$aaa_light_market_sales -
    reconciliation$observed_light_market_sales
  reconciliation$difference_percent <- round(
    reconciliation$difference / reconciliation$observed_light_market_sales * 100,
    2
  )
  reconciliation$note <- ifelse(
    is.na(reconciliation$observed_light_market_sales),
    "No comparable observed annual light-vehicle category total in seed observations.",
    "AAA light-vehicle total compared with seeded SUV + light_commercial + passenger annual category total."
  )
  write.csv(
    reconciliation,
    "data/processed/aaa_reconciliation_with_seed_totals.csv",
    row.names = FALSE
  )
}

coverage <- data.frame(
  source_id = source_id,
  ingest_method = ingest_method,
  source_url = ifelse(ingest_method == "graphql", aaa_graphql_url, aaa_csv_fallback_url),
  raw_json_path = ifelse(ingest_method == "graphql" && file.exists(raw_json_path), raw_json_path, NA),
  raw_path = raw_path,
  source_rows = nrow(raw),
  long_rows = nrow(long),
  quarter_count = length(quarter_cols),
  first_quarter = quarter_cols[1],
  last_quarter = quarter_cols[length(quarter_cols)],
  state_count = length(unique(long$state)),
  vehicle_type_count = length(unique(long$vehicle_type)),
  model_count = length(unique(long$model)),
  total_sales_in_csv = sum(long$sales),
  stringsAsFactors = FALSE
)
write.csv(coverage, "data/processed/aaa_ev_index_coverage.csv", row.names = FALSE)

message("Ingested AAA EV Index using ", ingest_method, ".")
message("Wrote normalized AAA CSV to ", raw_path)
if (ingest_method == "graphql" && file.exists(raw_json_path)) {
  message("Archived raw AAA GraphQL response to ", raw_json_path)
}
message("Wrote ", nrow(long), " long rows across ", length(quarter_cols), " quarters.")
message("Quarter coverage: ", quarter_cols[1], " to ", quarter_cols[length(quarter_cols)])
