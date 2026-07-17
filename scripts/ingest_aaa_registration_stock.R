options(stringsAsFactors = FALSE)

source_id <- "aaa_ev_index_registration"
registration_url <- "https://www.aaa.asn.au/wp-content/uploads/2025/11/EV_Index_Registration_Data_2021-2025.xlsx"
raw_path <- "data/raw/EV_Index_Registration_Data_2021-2025.xlsx"

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(raw_path)) {
  download.file(registration_url, raw_path, mode = "wb", quiet = TRUE)
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package readxl is required to ingest the AAA registration workbook.")
}

raw <- readxl::read_excel(
  raw_path,
  sheet = "Registration Numbers",
  .name_repair = "minimal"
)
raw <- as.data.frame(raw, check.names = FALSE, stringsAsFactors = FALSE)

required_cols <- c("Postcode", "State", "Fuel Type")
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("AAA registration workbook missing required columns: ", paste(missing_cols, collapse = ", "))
}

date_cols <- grep("^[0-9]{2}-[A-Za-z]{3}-[0-9]{4}$", names(raw), value = TRUE)
if (length(date_cols) == 0) {
  stop("AAA registration workbook has no stock-date columns like '31-Jan-2025'.")
}

standardise_fuel <- function(x) {
  out <- as.character(x)
  out[out == "Hybrid/PHEV"] <- "HEV_or_PHEV"
  out
}

format_postcode <- function(x) {
  out <- suppressWarnings(as.integer(x))
  ifelse(is.na(out), NA, sprintf("%04d", out))
}

long_parts <- lapply(date_cols, function(date_col) {
  stock <- suppressWarnings(as.numeric(raw[[date_col]]))
  stock[is.na(stock)] <- 0
  stock_date <- as.Date(date_col, format = "%d-%b-%Y")
  if (is.na(stock_date)) {
    stop("Could not parse stock date column: ", date_col)
  }
  data.frame(
    period = as.integer(format(stock_date, "%Y")),
    stock_date = as.character(stock_date),
    postcode = format_postcode(raw[["Postcode"]]),
    state = raw[["State"]],
    geography = paste0("postcode:", format_postcode(raw[["Postcode"]])),
    vehicle_type = "light_vehicle",
    powertrain = standardise_fuel(raw[["Fuel Type"]]),
    stock = stock,
    unit = "registered_vehicles",
    period_type = "annual_stock",
    metric = "registered_stock",
    source_id = source_id,
    market_scope = "light_vehicle",
    coverage_status = "stock_as_at_31_january",
    notes = "AAA EV Index registration workbook; postcode data excludes non-ABS post office areas noted in workbook disclaimer.",
    stringsAsFactors = FALSE
  )
})

postcode_long <- do.call(rbind, long_parts)
postcode_long <- postcode_long[order(
  postcode_long$period,
  postcode_long$state,
  postcode_long$postcode,
  postcode_long$powertrain
), ]
write.csv(
  postcode_long,
  "data/processed/stock_panel_postcode_annual.csv",
  row.names = FALSE
)

state_panel <- aggregate(
  stock ~ period + stock_date + state + vehicle_type + powertrain + unit +
    period_type + metric + source_id + market_scope + coverage_status,
  data = postcode_long,
  FUN = sum
)
state_panel$geography <- paste0("state:", state_panel$state)
state_panel$notes <- "AAA EV Index registration workbook aggregated from postcode rows to state."
state_panel <- state_panel[
  order(state_panel$period, state_panel$state, state_panel$powertrain),
  c(
    "period", "stock_date", "geography", "state", "vehicle_type", "powertrain",
    "stock", "unit", "period_type", "metric", "source_id", "market_scope",
    "coverage_status", "notes"
  )
]

national_panel <- aggregate(
  stock ~ period + stock_date + vehicle_type + powertrain + unit +
    period_type + metric + source_id + market_scope + coverage_status,
  data = postcode_long,
  FUN = sum
)
national_panel$geography <- "Australia"
national_panel$state <- NA
national_panel$notes <- "AAA EV Index registration workbook aggregated from postcode rows to national total."
national_panel <- national_panel[
  order(national_panel$period, national_panel$powertrain),
  c(
    "period", "stock_date", "geography", "state", "vehicle_type", "powertrain",
    "stock", "unit", "period_type", "metric", "source_id", "market_scope",
    "coverage_status", "notes"
  )
]

combined_panel <- rbind(state_panel, national_panel)

total_base <- combined_panel
total_base$state_group <- ifelse(is.na(total_base$state), "", total_base$state)
total_rows <- aggregate(
  stock ~ period + stock_date + geography + state_group + vehicle_type + unit +
    period_type + metric + source_id + market_scope + coverage_status,
  data = total_base,
  FUN = sum
)
names(total_rows)[names(total_rows) == "state_group"] <- "state"
total_rows$state[total_rows$state == ""] <- NA
total_rows$powertrain <- "total"
total_rows$notes <- "AAA EV Index registration workbook aggregated across fuel types."
total_rows <- total_rows[
  c(
    "period", "stock_date", "geography", "state", "vehicle_type", "powertrain",
    "stock", "unit", "period_type", "metric", "source_id", "market_scope",
    "coverage_status", "notes"
  )
]

stock_panel <- rbind(combined_panel, total_rows)
stock_panel <- stock_panel[order(
  stock_panel$period,
  stock_panel$geography,
  stock_panel$powertrain
), ]
write.csv(stock_panel, "data/processed/stock_panel_annual.csv", row.names = FALSE)

national_stock <- stock_panel[
  stock_panel$geography == "Australia" &
    stock_panel$powertrain != "total",
  c("period", "vehicle_type", "powertrain", "stock")
]
names(national_stock)[names(national_stock) == "stock"] <- "registered_stock"
national_stock_total <- stock_panel[
  stock_panel$geography == "Australia" &
    stock_panel$powertrain == "total",
  c("period", "stock")
]
names(national_stock_total)[names(national_stock_total) == "stock"] <- "total_registered_stock"
national_stock <- merge(national_stock, national_stock_total, by = "period", all.x = TRUE)
national_stock$stock_share <- national_stock$registered_stock / national_stock$total_registered_stock

if (file.exists("data/processed/sales_panel_projection_input.csv")) {
  sales <- read.csv("data/processed/sales_panel_projection_input.csv", check.names = FALSE)
  sales <- sales[
    sales$geography == "Australia" &
      sales$market_scope == "light_vehicle" &
      sales$observation_level == "powertrain_total" &
      sales$projection_ready &
      sales$powertrain_canonical %in% c("ICE", "BEV", "HEV", "PHEV", "FCEV"),
  ]
  sales$stock_powertrain <- ifelse(
    sales$powertrain_canonical %in% c("HEV", "PHEV"),
    "HEV_or_PHEV",
    sales$powertrain_canonical
  )
  sales_by_stock_bucket <- aggregate(
    sales ~ period + stock_powertrain,
    data = sales,
    FUN = sum
  )
  names(sales_by_stock_bucket) <- c("period", "powertrain", "new_sales")
  sales_total <- aggregate(new_sales ~ period, data = sales_by_stock_bucket, FUN = sum)
  names(sales_total)[names(sales_total) == "new_sales"] <- "total_new_sales"
  sales_by_stock_bucket <- merge(sales_by_stock_bucket, sales_total, by = "period", all.x = TRUE)
  sales_by_stock_bucket$sales_share <- sales_by_stock_bucket$new_sales /
    sales_by_stock_bucket$total_new_sales

  sales_stock <- merge(
    sales_by_stock_bucket,
    national_stock,
    by = c("period", "powertrain"),
    all = TRUE,
    sort = FALSE
  )
  sales_stock$sales_share_minus_stock_share <- sales_stock$sales_share - sales_stock$stock_share
  sales_stock$sales_to_stock_share_ratio <- sales_stock$sales_share / sales_stock$stock_share
  sales_stock$comparison_note <- ifelse(
    is.na(sales_stock$registered_stock),
    "No matching AAA registration stock bucket; stock workbook groups only BEV, Hybrid/PHEV, and ICE.",
    "Sales are calendar-year new registrations; stock is fleet registered as at 31 January of the same year."
  )
  sales_stock <- sales_stock[order(sales_stock$period, sales_stock$powertrain), ]
  write.csv(
    sales_stock,
    "data/processed/sales_stock_share_alignment.csv",
    row.names = FALSE
  )
}

coverage <- data.frame(
  source_id = source_id,
  raw_path = raw_path,
  source_rows = nrow(raw),
  postcode_long_rows = nrow(postcode_long),
  stock_panel_rows = nrow(stock_panel),
  first_period = min(postcode_long$period),
  last_period = max(postcode_long$period),
  geography_count = length(unique(stock_panel$geography)),
  postcode_count = length(unique(postcode_long$postcode)),
  state_count = length(unique(postcode_long$state)),
  powertrain_count = length(unique(postcode_long$powertrain)),
  total_stock_latest_year = sum(
    national_panel$stock[national_panel$period == max(national_panel$period)]
  ),
  stringsAsFactors = FALSE
)
write.csv(coverage, "data/processed/stock_panel_coverage.csv", row.names = FALSE)

message("Ingested AAA registration workbook from ", raw_path)
message("Wrote ", nrow(postcode_long), " postcode stock rows.")
message("Wrote stock panel with ", nrow(stock_panel), " national/state rows.")
message("Stock coverage: ", min(postcode_long$period), " to ", max(postcode_long$period))
