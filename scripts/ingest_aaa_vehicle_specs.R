options(stringsAsFactors = FALSE)

source_id <- "aaa_ev_index_vehicle_specs"
specs_url <- "https://www.aaa.asn.au/wp-content/uploads/2025/11/Vehicle_Specifications_SEP_25.csv"
raw_path <- "data/raw/Vehicle_Specifications_SEP_25.csv"

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(raw_path)) {
  download.file(specs_url, raw_path, mode = "wb", quiet = TRUE)
}

raw <- read.csv(raw_path, check.names = FALSE, na.strings = c("", "NA"))

required_cols <- c(
  "VEHICLE TYPE", "FUEL TYPE", "MODEL", "VARIANT DETAILS",
  "LISTED PRICE ($AUD)", "FAST CHARGE TIME (minutes)",
  "ANCAP RATING", "RANGE (km)", "ENERGY CONSUMPTION"
)
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("AAA vehicle specs CSV missing required columns: ", paste(missing_cols, collapse = ", "))
}

normalise_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("&", " and ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

standardise_broad_vehicle_type <- function(x) {
  out <- as.character(x)
  out[grepl("SUV", out)] <- "SUV"
  out[grepl("Car|Sports", out)] <- "passenger"
  out[grepl("Ute|Van", out)] <- "light_commercial"
  out[grepl("People Mover", out)] <- "people_mover"
  out
}

extract_first_number <- function(x) {
  x <- as.character(x)
  m <- regexpr("[0-9]+(\\.[0-9]+)?", x)
  out <- rep(NA_real_, length(x))
  has_match <- !is.na(m) & m > 0
  matched <- substring(x[has_match], m[has_match], m[has_match] + attr(m, "match.length")[has_match] - 1)
  out[has_match] <- suppressWarnings(as.numeric(matched))
  out
}

extract_kw <- function(x) {
  x <- as.character(x)
  m <- regexpr("[0-9]+\\s*kW", x, ignore.case = TRUE)
  out <- rep(NA_real_, length(x))
  has_match <- !is.na(m) & m > 0
  matched <- substring(x[has_match], m[has_match], m[has_match] + attr(m, "match.length")[has_match] - 1)
  out[has_match] <- suppressWarnings(as.numeric(gsub("[^0-9]", "", matched)))
  out
}

extract_ancap_stars <- function(x) {
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  has_match <- !is.na(x) & grepl("^[0-9]+ star", x)
  out[has_match] <- suppressWarnings(as.numeric(sub("^([0-9]+) star.*$", "\\1", x[has_match])))
  out
}

extract_ancap_year <- function(x) {
  x <- as.character(x)
  m <- regexpr("[0-9]{4}", x)
  out <- rep(NA_integer_, length(x))
  has_match <- !is.na(m) & m > 0
  matched <- substring(x[has_match], m[has_match], m[has_match] + attr(m, "match.length")[has_match] - 1)
  out[has_match] <- suppressWarnings(as.integer(matched))
  out
}

specs <- data.frame(
  source_id = source_id,
  model = raw[["MODEL"]],
  model_key = normalise_key(raw[["MODEL"]]),
  variant_details = raw[["VARIANT DETAILS"]],
  vehicle_type = raw[["VEHICLE TYPE"]],
  broad_vehicle_type = standardise_broad_vehicle_type(raw[["VEHICLE TYPE"]]),
  powertrain = raw[["FUEL TYPE"]],
  listed_price_aud = suppressWarnings(as.numeric(raw[["LISTED PRICE ($AUD)"]])),
  fast_charge_time_minutes = extract_first_number(raw[["FAST CHARGE TIME (minutes)"]]),
  fast_charge_power_kw = extract_kw(raw[["FAST CHARGE TIME (minutes)"]]),
  ancap_rating_raw = raw[["ANCAP RATING"]],
  ancap_stars = extract_ancap_stars(raw[["ANCAP RATING"]]),
  ancap_year = extract_ancap_year(raw[["ANCAP RATING"]]),
  ancap_van_rating = if ("ANCAP VAN RATING" %in% names(raw)) raw[["ANCAP VAN RATING"]] else NA,
  range_km = suppressWarnings(as.numeric(raw[["RANGE (km)"]])),
  energy_consumption = suppressWarnings(as.numeric(raw[["ENERGY CONSUMPTION"]])),
  notes = "AAA EV Index vehicle specifications CSV, September 2025. Covers BEV and PHEV models/variants listed by AAA.",
  stringsAsFactors = FALSE
)
specs <- specs[order(specs$powertrain, specs$broad_vehicle_type, specs$model, specs$listed_price_aud), ]
write.csv(specs, "data/processed/vehicle_specs_aaa.csv", row.names = FALSE)

model_summary <- aggregate(
  listed_price_aud ~ model + model_key + broad_vehicle_type + powertrain,
  data = specs,
  FUN = function(x) min(x, na.rm = TRUE)
)
names(model_summary)[names(model_summary) == "listed_price_aud"] <- "min_listed_price_aud"
model_max_price <- aggregate(
  listed_price_aud ~ model + model_key + broad_vehicle_type + powertrain,
  data = specs,
  FUN = function(x) max(x, na.rm = TRUE)
)
names(model_max_price)[names(model_max_price) == "listed_price_aud"] <- "max_listed_price_aud"
model_max_range <- aggregate(
  range_km ~ model + model_key + broad_vehicle_type + powertrain,
  data = specs,
  FUN = function(x) max(x, na.rm = TRUE)
)
names(model_max_range)[names(model_max_range) == "range_km"] <- "max_range_km"
model_min_energy <- aggregate(
  energy_consumption ~ model + model_key + broad_vehicle_type + powertrain,
  data = specs,
  FUN = function(x) min(x, na.rm = TRUE)
)
names(model_min_energy)[names(model_min_energy) == "energy_consumption"] <- "min_energy_consumption"
variant_counts <- aggregate(
  variant_details ~ model + model_key + broad_vehicle_type + powertrain,
  data = specs,
  FUN = length
)
names(variant_counts)[names(variant_counts) == "variant_details"] <- "variant_count"

model_summary <- merge(model_summary, model_max_price, by = c("model", "model_key", "broad_vehicle_type", "powertrain"), all = TRUE)
model_summary <- merge(model_summary, model_max_range, by = c("model", "model_key", "broad_vehicle_type", "powertrain"), all = TRUE)
model_summary <- merge(model_summary, model_min_energy, by = c("model", "model_key", "broad_vehicle_type", "powertrain"), all = TRUE)
model_summary <- merge(model_summary, variant_counts, by = c("model", "model_key", "broad_vehicle_type", "powertrain"), all = TRUE)
model_summary$source_id <- source_id
model_summary <- model_summary[order(model_summary$powertrain, model_summary$model), ]
write.csv(model_summary, "data/processed/vehicle_specs_model_summary.csv", row.names = FALSE)

if (file.exists("data/processed/model_sales_enriched.csv")) {
  model_sales <- read.csv("data/processed/model_sales_enriched.csv", check.names = FALSE)
  sales_models <- unique(model_sales[
    ,
    c("period", "make", "model", "canonical_model", "vehicle_type", "powertrain_profile")
  ])
  sales_models$sales_model_key <- normalise_key(sales_models$canonical_model)
  joined <- merge(
    sales_models,
    model_summary,
    by.x = "sales_model_key",
    by.y = "model_key",
    all.x = TRUE,
    sort = FALSE
  )
  joined$spec_match_status <- ifelse(is.na(joined$model.y), "no_exact_spec_match", "exact_model_name_match")
  joined$powertrain_match_status <- ifelse(
    joined$spec_match_status == "no_exact_spec_match",
    "no_spec_match",
    ifelse(
      joined$powertrain == "BEV" &
        grepl("BEV", joined$powertrain_profile),
      "compatible",
      ifelse(
        joined$powertrain == "PHEV" &
          grepl("PHEV", joined$powertrain_profile),
        "compatible",
        "model_name_match_only_powertrain_mismatch"
      )
    )
  )
  joined$usable_spec_match <- joined$powertrain_match_status == "compatible"
  names(joined)[names(joined) == "model.x"] <- "sales_model"
  names(joined)[names(joined) == "model.y"] <- "spec_model"
  joined <- joined[order(joined$period, joined$make, joined$sales_model), ]
  write.csv(joined, "data/processed/model_sales_vehicle_specs_coverage.csv", row.names = FALSE)
}

coverage <- data.frame(
  source_id = source_id,
  raw_path = raw_path,
  source_rows = nrow(raw),
  model_summary_rows = nrow(model_summary),
  fuel_types = paste(sort(unique(specs$powertrain)), collapse = ";"),
  broad_vehicle_types = paste(sort(unique(specs$broad_vehicle_type)), collapse = ";"),
  min_price_aud = min(specs$listed_price_aud, na.rm = TRUE),
  max_price_aud = max(specs$listed_price_aud, na.rm = TRUE),
  max_range_km = max(specs$range_km, na.rm = TRUE),
  stringsAsFactors = FALSE
)
write.csv(coverage, "data/processed/vehicle_specs_coverage.csv", row.names = FALSE)

message("Ingested AAA vehicle specs CSV from ", raw_path)
message("Wrote ", nrow(specs), " variant-level spec rows.")
message("Wrote ", nrow(model_summary), " model-level spec rows.")
