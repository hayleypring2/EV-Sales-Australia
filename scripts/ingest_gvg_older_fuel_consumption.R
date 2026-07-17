options(stringsAsFactors = FALSE)

source_id <- "gvg_older_fuel_consumption"
raw_url <- "https://www.greenvehicleguide.gov.au/Content/OlderModels/FuelConsumptionGuide1986-2003.csv"
raw_path <- "data/raw/FuelConsumptionGuide1986-2003.csv"

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(raw_path)) {
  download.file(raw_url, raw_path, mode = "wb", quiet = TRUE)
}

raw <- read.csv(raw_path, check.names = FALSE, na.strings = c("", "NA"))

names(raw) <- tolower(names(raw))
names(raw) <- gsub("[^a-z0-9]+", "_", names(raw))
names(raw) <- gsub("_$", "", names(raw))

required_cols <- c("vehicle_id", "manufacturer", "model", "year", "fuel", "type_of_car", "city_cycle", "highway_cycle")
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("GVG older fuel-consumption CSV missing required columns: ", paste(missing_cols, collapse = ", "))
}

raw$source_id <- source_id
raw$source_url <- raw_url
raw$year <- suppressWarnings(as.integer(raw$year))
raw$city_cycle_l_100km <- suppressWarnings(as.numeric(raw$city_cycle))
raw$highway_cycle_l_100km <- suppressWarnings(as.numeric(raw$highway_cycle))
raw$adr8101_cycle_l_100km <- if ("adr8101_cycle" %in% names(raw)) suppressWarnings(as.numeric(raw$adr8101_cycle)) else NA_real_

out <- raw[
  ,
  c(
    "source_id", "vehicle_id", "manufacturer", "model", "year", "transmission",
    "body_style", "fuel", "type_of_car", "city_cycle_l_100km", "highway_cycle_l_100km",
    "adr8101_cycle_l_100km", "fuel_system", "engine_displacement",
    "engine_displacement_comment", "no_cylinders", "no_gear_ratios", "seating_capacity",
    "axle_ratio", "source_url"
  )
]
out <- out[order(out$year, out$manufacturer, out$model), ]

coverage <- data.frame(
  source_id = source_id,
  raw_path = raw_path,
  rows = nrow(out),
  min_year = min(out$year, na.rm = TRUE),
  max_year = max(out$year, na.rm = TRUE),
  fuels = paste(sort(unique(out$fuel)), collapse = ";"),
  vehicle_types = paste(sort(unique(out$type_of_car)), collapse = ";"),
  notes = "Official GVG older vehicle fuel-consumption CSV. Covers 1986-2003 and is not directly comparable with the post-2004 GVG search tables.",
  stringsAsFactors = FALSE
)

write.csv(out, "data/processed/gvg_older_fuel_consumption_1986_2003.csv", row.names = FALSE)
write.csv(coverage, "data/processed/gvg_older_fuel_consumption_coverage.csv", row.names = FALSE)

message("Wrote ", nrow(out), " older GVG fuel-consumption rows.")
