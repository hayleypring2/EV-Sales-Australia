options(stringsAsFactors = FALSE)

required_packages <- c("httr", "rvest", "xml2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "))
}

source_id <- "gvg_current_search"
base_url <- "https://www.greenvehicleguide.gov.au/Vehicle/Search"
raw_dir <- "data/raw/gvg_search"
processed_dir <- "data/processed"

dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

fuel_lookup <- data.frame(
  fuel_type_id = c(1, 2, 3, 4, 5, 6, 22, 15, 8, 9, 10, 11, 12, 13, 16, 17, 18, 19, 20, 21, 7, 14),
  fuel_type_label = c(
    "Petrol 91RON", "Petrol 95RON", "Petrol 98RON", "Diesel", "LPG", "Petrol/LPG", "E85",
    "Pure Electric", "Electric/Petrol 91RON", "Electric/Petrol 95RON", "Electric/Petrol 98RON",
    "Electric/Diesel", "Electric/LPG", "Electric/NG", "Plug-in Electric/Petrol 91RON",
    "Plug-in Electric/Petrol 95RON", "Plug-in Electric/Petrol 98RON", "Plug-in Electric/Diesel",
    "Plug-in Electric/LPG", "Plug-in Electric/NG", "NG", "Hydrogen"
  ),
  powertrain_group = c(
    "ICE_petrol", "ICE_petrol", "ICE_petrol", "ICE_diesel", "other_or_unknown", "other_or_unknown", "other_or_unknown",
    "BEV", "HEV", "HEV", "HEV", "HEV", "HEV", "HEV", "PHEV", "PHEV", "PHEV", "PHEV", "PHEV", "PHEV",
    "other_or_unknown", "FCEV"
  )
)

vehicle_class_lookup <- data.frame(
  vehicle_class_id = c(2, 3, 4, 5, 6, 7, 8),
  vehicle_class_label = c("Small Car", "Medium Car", "Large Car", "Offroad", "Van", "Ute or Light Truck", "People Mover")
)

clean_text <- function(x) {
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

get_manufacturer_lookup <- function() {
  path <- file.path(raw_dir, "gvg_search_form.html")
  if (!file.exists(path)) {
    response <- httr::GET(
      base_url,
      httr::user_agent("EV-Sales-Australia research scraper; contact via repository")
    )
    httr::stop_for_status(response)
    writeBin(httr::content(response, as = "raw"), path)
  }
  doc <- rvest::read_html(path)
  options <- rvest::html_elements(doc, "#VS_4_SelectedManufacturer option")
  out <- data.frame(
    manufacturer_id = suppressWarnings(as.integer(rvest::html_attr(options, "value"))),
    manufacturer_label = clean_text(rvest::html_text2(options)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$manufacturer_id) & out$manufacturer_id != -1, ]
  out[order(out$manufacturer_label), ]
}

parse_integer_list <- function(x, default) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(default)
  }
  parts <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  out <- integer()
  for (part in trimws(parts)) {
    if (grepl("^[0-9]+:[0-9]+$", part)) {
      bounds <- as.integer(strsplit(part, ":", fixed = TRUE)[[1]])
      out <- c(out, seq(bounds[1], bounds[2]))
    } else {
      out <- c(out, as.integer(part))
    }
  }
  unique(out[!is.na(out)])
}

cell_value <- function(cells, i) {
  if (length(cells) < i) {
    return(NA_character_)
  }
  out <- rvest::html_attr(cells[[i]], "data-sort")
  if (is.na(out) || !nzchar(out)) {
    out <- rvest::html_text2(cells[[i]])
  }
  clean_text(out)
}

as_number <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "N/A", "NA")] <- NA_character_
  suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))
}

query_file <- function(year, fuel_type_id, vehicle_class_id = NA_integer_, manufacturer_id = NA_integer_) {
  class_part <- ifelse(is.na(vehicle_class_id), "allclass", paste0("class", vehicle_class_id))
  make_part <- ifelse(is.na(manufacturer_id), "allmake", paste0("make", manufacturer_id))
  file.path(raw_dir, paste0("gvg_year", year, "_fuel", fuel_type_id, "_", class_part, "_", make_part, ".html"))
}

query_gvg <- function(year, fuel_type_id, vehicle_class_id = NA_integer_, manufacturer_id = NA_integer_, delay_seconds = 0.25) {
  path <- query_file(year, fuel_type_id, vehicle_class_id, manufacturer_id)
  if (!file.exists(path)) {
    body <- list(
      "VehicleSearchParameterList.Index" = "4",
      "VehicleSearchParameterList[4].SelectedYearStart" = as.character(year),
      "VehicleSearchParameterList[4].SelectedYearEnd" = as.character(year),
      "VehicleSearchParameterList[4].SelectedManufacturer" = ifelse(is.na(manufacturer_id), "-1", as.character(manufacturer_id)),
      "VehicleSearchParameterList[4].VehicleModel" = "-1",
      "VehicleSearchParameterList[4].Variant" = "-1",
      "VehicleSearchParameterList[4].Transmission" = "-1",
      "VehicleSearchParameterList[4].VehicleClass" = ifelse(is.na(vehicle_class_id), "-1", as.character(vehicle_class_id)),
      "VehicleSearchParameterList[4].BodyStyle" = "-1",
      "VehicleSearchParameterList[4].FuelType" = as.character(fuel_type_id),
      "VehicleSearchParameterList[4].SeatingCapacity" = "-1",
      "VehicleSearchParameterList[4].DrivenWheels" = "-1",
      "submitType" = "Search vehicles"
    )
    response <- httr::POST(
      base_url,
      body = body,
      encode = "form",
      httr::user_agent("EV-Sales-Australia research scraper; contact via repository"),
      httr::add_headers(Referer = base_url)
    )
    httr::stop_for_status(response)
    writeBin(httr::content(response, as = "raw"), path)
    Sys.sleep(delay_seconds)
  }
  path
}

parse_gvg_html <- function(path, year, fuel_type_id, vehicle_class_id = NA_integer_, manufacturer_id = NA_integer_) {
  doc <- rvest::read_html(path)
  page_title <- clean_text(rvest::html_text2(rvest::html_element(doc, "title")))
  if (grepl("GVG - Error", page_title, fixed = TRUE)) {
    return(data.frame())
  }
  rows <- rvest::html_elements(doc, "tr.vehicle-item")
  if (length(rows) == 0) {
    return(data.frame())
  }

  parsed <- lapply(rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    link <- rvest::html_element(cells[[1]], "a")
    href <- rvest::html_attr(link, "href")
    vehicle_display_id <- sub("^.*vehicleDisplayId=([0-9]+).*$", "\\1", href)
    if (identical(vehicle_display_id, href)) vehicle_display_id <- NA_character_

    data.frame(
      source_id = source_id,
      query_year = year,
      fuel_type_id = fuel_type_id,
      vehicle_class_id = vehicle_class_id,
      manufacturer_id = manufacturer_id,
      vehicle_display_id = vehicle_display_id,
      model = cell_value(cells, 1),
      body = cell_value(cells, 2),
      engine = cell_value(cells, 3),
      transmission = cell_value(cells, 4),
      drivetrain = cell_value(cells, 5),
      tailpipe_co2_comb_g_km = as_number(cell_value(cells, 6)),
      tailpipe_co2_urban_g_km = as_number(cell_value(cells, 7)),
      tailpipe_co2_extra_g_km = as_number(cell_value(cells, 8)),
      annual_fuel_cost_aud = as_number(cell_value(cells, 9)),
      fuel_consumption_comb_l_100km = as_number(cell_value(cells, 10)),
      fuel_consumption_urban_l_100km = as_number(cell_value(cells, 11)),
      fuel_consumption_extra_l_100km = as_number(cell_value(cells, 12)),
      energy_consumption_wh_km = as_number(cell_value(cells, 13)),
      electric_range_km = as_number(cell_value(cells, 14)),
      air_pollution_standard = cell_value(cells, 15),
      annual_tailpipe_co2_tonnes = as_number(cell_value(cells, 16)),
      fuel_lifecycle_co2_g_km = as_number(cell_value(cells, 17)),
      noise_data = cell_value(cells, 18),
      test_cycle = cell_value(cells, 19),
      source_url = base_url,
      raw_html_path = path,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, parsed)
}

years <- parse_integer_list(Sys.getenv("GVG_YEARS"), 2025)
fuel_ids <- parse_integer_list(Sys.getenv("GVG_FUEL_IDS"), c(1, 4, 15, 16, 17, 18, 19))
delay_seconds <- suppressWarnings(as.numeric(Sys.getenv("GVG_DELAY_SECONDS", "0.25")))
split_class_if_capped <- Sys.getenv("GVG_SPLIT_CLASS_IF_CAPPED", "1") != "0"
split_make_if_class_capped <- Sys.getenv("GVG_SPLIT_MAKE_IF_CLASS_CAPPED", "1") == "1"
manufacturer_lookup <- if (split_make_if_class_capped) get_manufacturer_lookup() else data.frame()

message("GVG search years: ", paste(years, collapse = ", "))
message("GVG fuel IDs: ", paste(fuel_ids, collapse = ", "))

all_rows <- list()
query_log <- list()

for (year in years) {
  for (fuel_type_id in fuel_ids) {
    path <- query_gvg(year, fuel_type_id, delay_seconds = delay_seconds)
    rows <- parse_gvg_html(path, year, fuel_type_id)
    n_rows <- nrow(rows)
    capped <- n_rows >= 200
    query_log[[length(query_log) + 1]] <- data.frame(
      query_year = year,
      fuel_type_id = fuel_type_id,
      vehicle_class_id = NA_integer_,
      manufacturer_id = NA_integer_,
      rows_returned = n_rows,
      capped_at_200 = capped,
      used_in_output = !capped || !split_class_if_capped,
      raw_html_path = path
    )

    if (!capped || !split_class_if_capped) {
      all_rows[[length(all_rows) + 1]] <- rows
      next
    }

    for (vehicle_class_id in vehicle_class_lookup$vehicle_class_id) {
      class_path <- query_gvg(year, fuel_type_id, vehicle_class_id = vehicle_class_id, delay_seconds = delay_seconds)
      class_rows <- parse_gvg_html(class_path, year, fuel_type_id, vehicle_class_id = vehicle_class_id)
      class_capped <- nrow(class_rows) >= 200
      query_log[[length(query_log) + 1]] <- data.frame(
        query_year = year,
        fuel_type_id = fuel_type_id,
        vehicle_class_id = vehicle_class_id,
        manufacturer_id = NA_integer_,
        rows_returned = nrow(class_rows),
        capped_at_200 = class_capped,
        used_in_output = !class_capped || !split_make_if_class_capped,
        raw_html_path = class_path
      )

      if (class_capped && split_make_if_class_capped) {
        for (manufacturer_id in manufacturer_lookup$manufacturer_id) {
          make_path <- query_gvg(
            year,
            fuel_type_id,
            vehicle_class_id = vehicle_class_id,
            manufacturer_id = manufacturer_id,
            delay_seconds = delay_seconds
          )
          make_rows <- parse_gvg_html(
            make_path,
            year,
            fuel_type_id,
            vehicle_class_id = vehicle_class_id,
            manufacturer_id = manufacturer_id
          )
          query_log[[length(query_log) + 1]] <- data.frame(
            query_year = year,
            fuel_type_id = fuel_type_id,
            vehicle_class_id = vehicle_class_id,
            manufacturer_id = manufacturer_id,
            rows_returned = nrow(make_rows),
            capped_at_200 = nrow(make_rows) >= 200,
            used_in_output = TRUE,
            raw_html_path = make_path
          )
          all_rows[[length(all_rows) + 1]] <- make_rows
        }
      } else {
        all_rows[[length(all_rows) + 1]] <- class_rows
      }
    }
  }
}

out <- if (length(all_rows) > 0) do.call(rbind, all_rows) else data.frame()
if (nrow(out) > 0) {
  out <- merge(out, fuel_lookup, by = "fuel_type_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, vehicle_class_lookup, by = "vehicle_class_id", all.x = TRUE, sort = FALSE)
  if (nrow(manufacturer_lookup) > 0) {
    out <- merge(out, manufacturer_lookup, by = "manufacturer_id", all.x = TRUE, sort = FALSE)
  }
  out <- out[!duplicated(paste(out$query_year, out$fuel_type_id, out$vehicle_display_id, sep = "\r")), ]
  out <- out[order(out$query_year, out$powertrain_group, out$fuel_type_label, out$model), ]
}

query_log <- if (length(query_log) > 0) do.call(rbind, query_log) else data.frame()
if (nrow(query_log) > 0) {
  query_log <- merge(query_log, fuel_lookup, by = "fuel_type_id", all.x = TRUE, sort = FALSE)
  query_log <- merge(query_log, vehicle_class_lookup, by = "vehicle_class_id", all.x = TRUE, sort = FALSE)
  if (nrow(manufacturer_lookup) > 0) {
    query_log <- merge(query_log, manufacturer_lookup, by = "manufacturer_id", all.x = TRUE, sort = FALSE)
  }
  query_log <- query_log[order(query_log$query_year, query_log$fuel_type_id, query_log$vehicle_class_id), ]
}

coverage <- data.frame(
  source_id = source_id,
  years = paste(range(years), collapse = "-"),
  fuel_type_ids = paste(fuel_ids, collapse = ";"),
  output_rows = nrow(out),
  unique_vehicle_display_ids = if (nrow(out) > 0) length(unique(out$vehicle_display_id)) else 0,
  capped_queries = if (nrow(query_log) > 0) sum(query_log$capped_at_200, na.rm = TRUE) else 0,
  class_split_enabled = split_class_if_capped,
  manufacturer_split_enabled = split_make_if_class_capped,
  notes = "Public GVG search table extraction. Queries returning 200 rows may be capped; script splits capped year/fuel queries by GVG vehicle class by default.",
  stringsAsFactors = FALSE
)

write.csv(out, file.path(processed_dir, "gvg_search_vehicle_specs.csv"), row.names = FALSE)
write.csv(query_log, file.path(processed_dir, "gvg_search_query_log.csv"), row.names = FALSE)
write.csv(coverage, file.path(processed_dir, "gvg_search_coverage.csv"), row.names = FALSE)

message("Wrote ", nrow(out), " GVG vehicle spec/emissions rows.")
message("Wrote ", nrow(query_log), " GVG query-log rows.")
