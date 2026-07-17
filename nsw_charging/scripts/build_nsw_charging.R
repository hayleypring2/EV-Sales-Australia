options(stringsAsFactors = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Missing required R package: jsonlite")
}

root <- "nsw_charging"
raw_dir <- file.path(root, "data", "raw")
processed_dir <- file.path(root, "data", "processed")
outputs_dir <- file.path(root, "outputs")

dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)

source_csv <- file.path(raw_dir, "ev_20251216.csv")
download_csv <- "/Users/pring/Downloads/ev_20251216 (1).csv"
if (!file.exists(source_csv) && file.exists(download_csv)) {
  file.copy(download_csv, source_csv, overwrite = TRUE)
}
if (!file.exists(source_csv)) {
  stop("Missing raw charging CSV at ", source_csv)
}

wealth_panel_path <- "wealth_evs/data/processed/postcode_ev_wealth_panel.csv"
wealth_geojson_path <- "wealth_evs/data/processed/postcode_ev_wealth_map.geojson"
if (!file.exists(wealth_panel_path) || !file.exists(wealth_geojson_path)) {
  stop("Missing wealth_evs processed files. Run Rscript wealth_evs/scripts/build_wealth_evs.R first.")
}

clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

normalise_poa <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  numeric_like <- grepl("^[0-9]+$", x)
  x[numeric_like] <- sprintf("%04d", as.integer(x[numeric_like]))
  x
}

extract_postcode <- function(primary, fallback) {
  primary <- as.character(primary)
  fallback <- as.character(fallback)
  out <- rep(NA_character_, length(primary))

  primary_pos <- regexpr("[0-9]{4}", primary)
  has_primary <- !is.na(primary_pos) & primary_pos > 0
  out[has_primary] <- substring(
    primary[has_primary],
    primary_pos[has_primary],
    primary_pos[has_primary] + attr(primary_pos, "match.length")[has_primary] - 1
  )

  needs_fallback <- is.na(out) | !nzchar(out)
  fallback_matches <- regmatches(fallback[needs_fallback], gregexpr("[0-9]{4}", fallback[needs_fallback]))
  fallback_out <- vapply(fallback_matches, function(m) {
    if (length(m) == 0) return(NA_character_)
    tail(m, 1)
  }, character(1))
  out[needs_fallback] <- fallback_out
  normalise_poa(out)
}

extract_numbers <- function(x) {
  m <- gregexpr("[0-9]+(\\.[0-9]+)?", x)
  regmatches(x, m)
}

parse_kw_max <- function(x) {
  nums <- extract_numbers(as.character(x))
  vapply(nums, function(n) {
    if (length(n) == 0) return(NA_real_)
    max(suppressWarnings(as.numeric(n)), na.rm = TRUE)
  }, numeric(1))
}

parse_kw_capacity <- function(rating, plugs, kw_max) {
  rating <- as.character(rating)
  out <- rep(NA_real_, length(rating))
  for (i in seq_along(rating)) {
    parts <- regmatches(rating[i], gregexpr("([0-9]+)\\s*x\\s*([0-9]+)", rating[i], ignore.case = TRUE))[[1]]
    if (length(parts) > 0) {
      total <- 0
      for (part in parts) {
        vals <- suppressWarnings(as.numeric(regmatches(part, gregexpr("[0-9]+", part))[[1]]))
        if (length(vals) >= 2) total <- total + vals[1] * vals[2]
      }
      out[i] <- total
    } else if (!is.na(kw_max[i]) && !is.na(plugs[i])) {
      out[i] <- kw_max[i] * plugs[i]
    }
  }
  out
}

safe_sum <- function(x) sum(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_n_distinct <- function(x) length(unique(x[!is.na(x) & nzchar(x)]))

chargers <- read.csv(source_csv, check.names = FALSE)
names(chargers) <- clean_names(names(chargers))

required <- c(
  "station_address", "operator", "number_of_plugs", "charger_type",
  "charger_rating", "latitude", "longitude", "lganame", "pcode", "source"
)
missing_cols <- setdiff(required, names(chargers))
if (length(missing_cols) > 0) {
  stop("Charging CSV missing required columns: ", paste(missing_cols, collapse = ", "))
}

chargers$site_id <- seq_len(nrow(chargers))
chargers$postcode <- extract_postcode(chargers$pcode, chargers$station_address)
chargers$station_name_clean <- chargers$station_name
missing_name <- is.na(chargers$station_name_clean) | !nzchar(chargers$station_name_clean)
chargers$station_name_clean[missing_name] <- sub(",.*$", "", chargers$station_address[missing_name])
chargers$station_name_clean <- trimws(chargers$station_name_clean)
chargers$charger_kw_max <- parse_kw_max(chargers$charger_rating)
chargers$estimated_site_kw_capacity <- parse_kw_capacity(
  chargers$charger_rating,
  chargers$number_of_plugs,
  chargers$charger_kw_max
)
chargers$status <- ifelse(tolower(chargers$charger_type) == "upcoming", "upcoming", "existing")
chargers$is_ac <- toupper(chargers$charger_type) == "AC"
chargers$is_dc <- toupper(chargers$charger_type) == "DC"
chargers$is_upcoming <- chargers$status == "upcoming"
chargers$is_fast_or_upcoming <- chargers$is_dc | chargers$is_upcoming | chargers$charger_kw_max >= 50
chargers$is_ultrafast_or_upcoming <- chargers$is_dc & chargers$charger_kw_max >= 150 | chargers$is_upcoming & chargers$charger_kw_max >= 150
chargers$coordinate_status <- ifelse(
  !is.na(chargers$latitude) & !is.na(chargers$longitude),
  "has_coordinates",
  "missing_coordinates"
)
chargers$source_csv <- "ev_20251216.csv"
chargers <- chargers[order(chargers$postcode, chargers$operator, chargers$station_address), ]

write.csv(chargers, file.path(processed_dir, "nsw_charging_sites_clean.csv"), row.names = FALSE)

wealth_panel <- read.csv(wealth_panel_path, check.names = FALSE)
wealth_2025 <- wealth_panel[wealth_panel$period == 2025, ]
wealth_2025$postcode <- normalise_poa(wealth_2025$postcode)

agg_one <- function(x) {
  data.frame(
    postcode = unique(x$postcode)[1],
    charger_site_count = nrow(x),
    plug_count = safe_sum(x$number_of_plugs),
    ac_site_count = safe_sum(x$is_ac),
    ac_plug_count = safe_sum(ifelse(x$is_ac, x$number_of_plugs, 0)),
    dc_site_count = safe_sum(x$is_dc),
    dc_plug_count = safe_sum(ifelse(x$is_dc, x$number_of_plugs, 0)),
    upcoming_site_count = safe_sum(x$is_upcoming),
    upcoming_plug_count = safe_sum(ifelse(x$is_upcoming, x$number_of_plugs, 0)),
    fast_or_upcoming_site_count = safe_sum(x$is_fast_or_upcoming),
    fast_or_upcoming_plug_count = safe_sum(ifelse(x$is_fast_or_upcoming, x$number_of_plugs, 0)),
    max_kw = safe_max(x$charger_kw_max),
    estimated_kw_capacity = safe_sum(x$estimated_site_kw_capacity),
    operator_count = safe_n_distinct(x$operator),
    source_count = safe_n_distinct(x$source),
    stringsAsFactors = FALSE
  )
}

chargers_for_agg <- chargers[!is.na(chargers$postcode) & nzchar(chargers$postcode), ]
charging_by_postcode <- do.call(rbind, lapply(split(chargers_for_agg, chargers_for_agg$postcode), agg_one))

nsw_postcodes <- unique(wealth_2025$postcode[wealth_2025$state == "NSW"])
all_nsw_postcodes <- sort(unique(c(nsw_postcodes, charging_by_postcode$postcode)))
postcode_panel <- merge(
  data.frame(postcode = all_nsw_postcodes),
  wealth_2025,
  by = "postcode",
  all.x = TRUE,
  sort = FALSE
)
postcode_panel <- merge(
  postcode_panel,
  charging_by_postcode,
  by = "postcode",
  all.x = TRUE,
  sort = FALSE
)

count_cols <- c(
  "charger_site_count", "plug_count", "ac_site_count", "ac_plug_count",
  "dc_site_count", "dc_plug_count", "upcoming_site_count", "upcoming_plug_count",
  "fast_or_upcoming_site_count", "fast_or_upcoming_plug_count", "estimated_kw_capacity",
  "operator_count", "source_count"
)
for (col in count_cols) {
  if (!col %in% names(postcode_panel)) postcode_panel[[col]] <- 0
  postcode_panel[[col]][is.na(postcode_panel[[col]])] <- 0
}

postcode_panel$plugs_per_1000_vehicles <- ifelse(postcode_panel$total_stock > 0, 1000 * postcode_panel$plug_count / postcode_panel$total_stock, NA_real_)
postcode_panel$dc_plugs_per_1000_vehicles <- ifelse(postcode_panel$total_stock > 0, 1000 * postcode_panel$dc_plug_count / postcode_panel$total_stock, NA_real_)
postcode_panel$fast_plugs_per_1000_vehicles <- ifelse(postcode_panel$total_stock > 0, 1000 * postcode_panel$fast_or_upcoming_plug_count / postcode_panel$total_stock, NA_real_)
postcode_panel$plugs_per_1000_bevs <- ifelse(postcode_panel$BEV > 0, 1000 * postcode_panel$plug_count / postcode_panel$BEV, NA_real_)
postcode_panel$plugs_per_10000_people <- ifelse(postcode_panel$usual_resident_population > 0, 10000 * postcode_panel$plug_count / postcode_panel$usual_resident_population, NA_real_)
postcode_panel$dc_plugs_per_10000_people <- ifelse(postcode_panel$usual_resident_population > 0, 10000 * postcode_panel$dc_plug_count / postcode_panel$usual_resident_population, NA_real_)
postcode_panel$charger_access_status <- ifelse(postcode_panel$plug_count > 0, "has_charging", "no_known_charging")
postcode_panel <- postcode_panel[order(postcode_panel$state, postcode_panel$postcode), ]

write.csv(postcode_panel, file.path(processed_dir, "nsw_charging_postcode_panel.csv"), row.names = FALSE)

decile_data <- postcode_panel[!is.na(postcode_panel$irsad_decile), ]
decile_summary <- do.call(rbind, lapply(split(decile_data, decile_data$irsad_decile), function(x) {
  data.frame(
    irsad_decile = unique(x$irsad_decile),
    postcode_count = nrow(x),
    postcode_with_charging_count = safe_sum(x$plug_count > 0),
    charger_site_count = safe_sum(x$charger_site_count),
    plug_count = safe_sum(x$plug_count),
    dc_plug_count = safe_sum(x$dc_plug_count),
    upcoming_plug_count = safe_sum(x$upcoming_plug_count),
    fast_or_upcoming_plug_count = safe_sum(x$fast_or_upcoming_plug_count),
    total_stock = safe_sum(x$total_stock),
    bev_stock = safe_sum(x$BEV),
    usual_resident_population = safe_sum(x$usual_resident_population),
    bev_share_weighted = safe_sum(x$BEV) / safe_sum(x$total_stock),
    plugs_per_1000_vehicles = 1000 * safe_sum(x$plug_count) / safe_sum(x$total_stock),
    dc_plugs_per_1000_vehicles = 1000 * safe_sum(x$dc_plug_count) / safe_sum(x$total_stock),
    plugs_per_1000_bevs = 1000 * safe_sum(x$plug_count) / safe_sum(x$BEV),
    plugs_per_10000_people = 10000 * safe_sum(x$plug_count) / safe_sum(x$usual_resident_population),
    dc_plugs_per_10000_people = 10000 * safe_sum(x$dc_plug_count) / safe_sum(x$usual_resident_population),
    stringsAsFactors = FALSE
  )
}))
decile_summary <- decile_summary[order(decile_summary$irsad_decile), ]
decile_summary$share_of_nsw_plugs <- decile_summary$plug_count / sum(decile_summary$plug_count, na.rm = TRUE)
decile_summary$share_of_nsw_dc_plugs <- decile_summary$dc_plug_count / sum(decile_summary$dc_plug_count, na.rm = TRUE)
write.csv(decile_summary, file.path(processed_dir, "nsw_charging_irsad_decile_summary.csv"), row.names = FALSE)

headline <- data.frame(
  sites = nrow(chargers),
  plugs = safe_sum(chargers$number_of_plugs),
  dc_sites = safe_sum(chargers$is_dc),
  dc_plugs = safe_sum(ifelse(chargers$is_dc, chargers$number_of_plugs, 0)),
  upcoming_sites = safe_sum(chargers$is_upcoming),
  upcoming_plugs = safe_sum(ifelse(chargers$is_upcoming, chargers$number_of_plugs, 0)),
  postcodes_with_charging = safe_sum(postcode_panel$plug_count > 0),
  matched_to_seifa_postcodes = safe_sum(!is.na(postcode_panel$irsad_decile)),
  top_decile_plugs_per_10000_people = decile_summary$plugs_per_10000_people[decile_summary$irsad_decile == 10],
  bottom_decile_plugs_per_10000_people = decile_summary$plugs_per_10000_people[decile_summary$irsad_decile == 1],
  top_decile_plugs_per_1000_vehicles = decile_summary$plugs_per_1000_vehicles[decile_summary$irsad_decile == 10],
  bottom_decile_plugs_per_1000_vehicles = decile_summary$plugs_per_1000_vehicles[decile_summary$irsad_decile == 1],
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(processed_dir, "nsw_charging_headline_stats.csv"), row.names = FALSE)

wealth_geojson <- jsonlite::fromJSON(wealth_geojson_path, simplifyVector = FALSE)
panel_by_postcode <- split(postcode_panel, postcode_panel$postcode)

round_or_null <- function(x, digits = 4) {
  if (length(x) == 0 || is.na(x)) return(NULL)
  round(as.numeric(x), digits)
}
integer_or_null <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NULL)
  as.integer(round(as.numeric(x)))
}

map_features <- list()
for (feature in wealth_geojson$features) {
  poa <- feature$properties$postcode
  if (!poa %in% names(panel_by_postcode)) next
  row <- panel_by_postcode[[poa]][1, ]
  if (!is.na(row$state) && row$state != "NSW" && row$plug_count == 0) next
  feature$properties <- list(
    postcode = poa,
    state = row$state,
    irsad_score = round_or_null(row$irsad_score, 1),
    irsad_decile = integer_or_null(row$irsad_decile),
    usual_resident_population = integer_or_null(row$usual_resident_population),
    total_vehicle_stock = integer_or_null(row$total_stock),
    bev_stock = integer_or_null(row$BEV),
    bev_share = round_or_null(row$bev_share, 5),
    charger_site_count = integer_or_null(row$charger_site_count),
    plug_count = integer_or_null(row$plug_count),
    dc_plug_count = integer_or_null(row$dc_plug_count),
    upcoming_plug_count = integer_or_null(row$upcoming_plug_count),
    fast_or_upcoming_plug_count = integer_or_null(row$fast_or_upcoming_plug_count),
    max_kw = round_or_null(row$max_kw, 1),
    estimated_kw_capacity = round_or_null(row$estimated_kw_capacity, 1),
    plugs_per_1000_vehicles = round_or_null(row$plugs_per_1000_vehicles, 4),
    dc_plugs_per_1000_vehicles = round_or_null(row$dc_plugs_per_1000_vehicles, 4),
    fast_plugs_per_1000_vehicles = round_or_null(row$fast_plugs_per_1000_vehicles, 4),
    plugs_per_1000_bevs = round_or_null(row$plugs_per_1000_bevs, 4),
    plugs_per_10000_people = round_or_null(row$plugs_per_10000_people, 4),
    dc_plugs_per_10000_people = round_or_null(row$dc_plugs_per_10000_people, 4)
  )
  map_features[[length(map_features) + 1]] <- feature
}

site_points <- chargers[chargers$coordinate_status == "has_coordinates", ]
site_points <- site_points[
  ,
  c(
    "site_id", "station_name_clean", "station_address", "operator", "number_of_plugs",
    "charger_type", "charger_rating", "charger_kw_max", "estimated_site_kw_capacity",
    "latitude", "longitude", "lganame", "postcode", "source", "status"
  )
]
site_points$station_name_clean[is.na(site_points$station_name_clean)] <- ""
site_points$source[is.na(site_points$source)] <- ""
site_points$lganame[is.na(site_points$lganame)] <- ""
site_points$charger_kw_max[is.na(site_points$charger_kw_max)] <- NA_real_

map_geojson <- list(
  type = "FeatureCollection",
  metadata = list(
    title = "NSW EV charging sites by postcode and SEIFA decile",
    source_charging = "User supplied ev_20251216.csv",
    source_seifa = "ABS SEIFA 2021 Postal Area Indexes via wealth_evs build",
    source_boundaries = "AIHW-hosted simplified ABS ASGS Edition 3 POA 2021 boundary service via wealth_evs build"
  ),
  features = map_features
)
geojson_path <- file.path(processed_dir, "nsw_charging_postcode_map.geojson")
jsonlite::write_json(map_geojson, geojson_path, auto_unbox = TRUE, digits = 6, null = "null")

site_json <- jsonlite::toJSON(site_points, dataframe = "rows", auto_unbox = TRUE, digits = 7, na = "null")
decile_json <- jsonlite::toJSON(decile_summary, dataframe = "rows", auto_unbox = TRUE, digits = 6, na = "null")
geojson_text <- paste(readLines(geojson_path, warn = FALSE), collapse = "\n")

fmt_num <- function(x, digits = 0) format(round(x, digits), big.mark = ",", trim = TRUE, nsmall = digits)

html <- paste0(
'<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>NSW EV Charging and Wealth</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <style>
    html, body { height: 100%; margin: 0; font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #17212b; }
    body { display: grid; grid-template-columns: minmax(330px, 430px) 1fr; background: #f8f7f3; }
    aside { overflow-y: auto; padding: 22px; background: #fbfaf7; border-right: 1px solid #d7d1c7; box-sizing: border-box; }
    #map { height: 100vh; width: 100%; }
    h1 { margin: 0 0 10px; font-size: 28px; line-height: 1.08; font-weight: 780; color: #101820; }
    h2 { margin: 20px 0 8px; font-size: 16px; color: #263440; }
    p { line-height: 1.45; font-size: 14px; margin: 0 0 12px; }
    .lede { font-size: 15px; color: #35434f; }
    .control { margin: 13px 0; }
    label { display: block; font-size: 12px; font-weight: 740; text-transform: uppercase; letter-spacing: .04em; color: #5a6872; margin-bottom: 6px; }
    select, input { width: 100%; box-sizing: border-box; border: 1px solid #bdc7cc; border-radius: 6px; padding: 9px 10px; background: white; color: #17212b; font-size: 14px; }
    .stats { display: grid; grid-template-columns: 1fr 1fr; gap: 9px; margin: 15px 0; }
    .stat { border: 1px solid #ded8cf; border-radius: 7px; padding: 10px; background: white; }
    .stat b { display: block; font-size: 20px; color: #005f73; }
    .stat span { display: block; font-size: 12px; color: #59666f; margin-top: 3px; }
    .bar-row { display: grid; grid-template-columns: 24px 1fr 58px; gap: 8px; align-items: center; font-size: 12px; margin: 5px 0; }
    .bar { height: 14px; border-radius: 3px; background: #e6e0d8; overflow: hidden; }
    .bar > div { height: 100%; background: #0a9396; }
    .legend { padding: 10px; background: white; border-radius: 6px; border: 1px solid #cfd8dc; line-height: 1.4; box-shadow: 0 1px 6px rgba(0,0,0,.12); }
    .legend-row { display: flex; align-items: center; gap: 6px; font-size: 12px; margin: 3px 0; }
    .swatch { width: 16px; height: 11px; border-radius: 2px; border: 1px solid rgba(0,0,0,.2); }
    .note { font-size: 12px; color: #5b666f; border-top: 1px solid #ded8cf; padding-top: 12px; margin-top: 16px; }
    .leaflet-popup-content { font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }
    .popup-title { font-weight: 800; font-size: 15px; margin-bottom: 4px; }
    .popup-grid { display: grid; grid-template-columns: auto auto; gap: 3px 14px; font-size: 12px; }
    @media (max-width: 840px) {
      body { grid-template-columns: 1fr; grid-template-rows: auto 65vh; }
      aside { border-right: none; border-bottom: 1px solid #d8d2c8; }
      #map { height: 65vh; }
    }
  </style>
</head>
<body>
  <aside>
    <h1>Is NSW charging infrastructure clustered in wealthier postcodes?</h1>
    <p class="lede">This map combines a NSW EV charging-site CSV with ABS SEIFA Postal Area deciles and 2025 vehicle registration stock.</p>
    <div class="control">
      <label for="metric">Postcode layer</label>
      <select id="metric">
        <option value="plug_count">Charging plugs</option>
        <option value="dc_plug_count">DC plugs</option>
        <option value="plugs_per_1000_vehicles">Plugs per 1,000 vehicles</option>
        <option value="plugs_per_10000_people">Plugs per 10,000 residents</option>
        <option value="plugs_per_1000_bevs">Plugs per 1,000 BEVs</option>
        <option value="irsad_decile">ABS IRSAD wealth decile</option>
      </select>
    </div>
    <div class="control">
      <label for="pointMode">Charger points</label>
      <select id="pointMode">
        <option value="all">Show all charger sites</option>
        <option value="dc">Show DC only</option>
        <option value="upcoming">Show upcoming only</option>
        <option value="none">Hide charger sites</option>
      </select>
    </div>
    <div class="control">
      <label for="postcodeSearch">Find postcode</label>
      <input id="postcodeSearch" placeholder="e.g. 2000">
    </div>
    <div class="stats">
      <div class="stat"><b>', fmt_num(headline$sites), '</b><span>charger sites</span></div>
      <div class="stat"><b>', fmt_num(headline$plugs), '</b><span>charging plugs</span></div>
      <div class="stat"><b>', fmt_num(headline$dc_plugs), '</b><span>DC plugs</span></div>
      <div class="stat"><b>', fmt_num(headline$postcodes_with_charging), '</b><span>postcodes with charging</span></div>
    </div>
    <h2>Charging access by wealth decile</h2>
    <div id="bars"></div>
    <p class="note">The postcode layer aggregates sites to ABS Postal Areas. Charger points use the CSV coordinates. Wealth proxy: ABS SEIFA 2021 IRSAD decile. Charging data are a user-supplied snapshot file named <code>ev_20251216.csv</code>.</p>
  </aside>
  <main id="map"></main>
  <script>
    const postcodeData = ', geojson_text, ';
    const siteData = ', site_json, ';
    const decileData = ', decile_json, ';
  </script>
  <script>
    const map = L.map("map", { zoomControl: false }).setView([-32.8, 147.5], 6);
    L.control.zoom({ position: "bottomright" }).addTo(map);
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 18,
      attribution: "&copy; OpenStreetMap contributors"
    }).addTo(map);

    const metricSelect = document.getElementById("metric");
    const pointMode = document.getElementById("pointMode");
    const searchInput = document.getElementById("postcodeSearch");
    const postcodeIndex = new Map();
    let pointLayer = L.layerGroup().addTo(map);

    const ramp = ["#f4f1e8", "#d8eadb", "#a9d6c4", "#6db9b0", "#27949c", "#005f73"];
    const decileRamp = ["#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"];

    function valueOf(feature) {
      return feature.properties[metricSelect.value];
    }
    function colorFor(value) {
      const metric = metricSelect.value;
      if (value === null || value === undefined || Number.isNaN(value)) return "#d8d8d8";
      if (metric === "irsad_decile") return decileRamp[Math.max(1, Math.min(10, value)) - 1];
      if (metric === "plug_count") {
        if (value >= 80) return ramp[5];
        if (value >= 40) return ramp[4];
        if (value >= 20) return ramp[3];
        if (value >= 8) return ramp[2];
        if (value >= 1) return ramp[1];
        return ramp[0];
      }
      if (metric === "dc_plug_count") {
        if (value >= 20) return ramp[5];
        if (value >= 10) return ramp[4];
        if (value >= 5) return ramp[3];
        if (value >= 2) return ramp[2];
        if (value >= 1) return ramp[1];
        return ramp[0];
      }
      if (metric === "plugs_per_1000_bevs") {
        if (value >= 1500) return ramp[5];
        if (value >= 800) return ramp[4];
        if (value >= 400) return ramp[3];
        if (value >= 150) return ramp[2];
        if (value > 0) return ramp[1];
        return ramp[0];
      }
      if (value >= 12) return ramp[5];
      if (value >= 6) return ramp[4];
      if (value >= 3) return ramp[3];
      if (value >= 1) return ramp[2];
      if (value > 0) return ramp[1];
      return ramp[0];
    }
    function formatNum(x, digits = 0) {
      if (x === null || x === undefined || Number.isNaN(x)) return "n/a";
      return Number(x).toLocaleString(undefined, { maximumFractionDigits: digits, minimumFractionDigits: digits });
    }
    function formatPct(x) {
      if (x === null || x === undefined || Number.isNaN(x)) return "n/a";
      return (100 * x).toFixed(2) + "%";
    }
    function postcodePopup(feature) {
      const p = feature.properties;
      return `<div class="popup-title">Postcode ${p.postcode}</div>
        <div class="popup-grid">
          <span>Plugs</span><b>${formatNum(p.plug_count)}</b>
          <span>DC plugs</span><b>${formatNum(p.dc_plug_count)}</b>
          <span>Sites</span><b>${formatNum(p.charger_site_count)}</b>
          <span>Plugs / 1,000 vehicles</span><b>${formatNum(p.plugs_per_1000_vehicles, 2)}</b>
          <span>Plugs / 10,000 residents</span><b>${formatNum(p.plugs_per_10000_people, 2)}</b>
          <span>BEV share</span><b>${formatPct(p.bev_share)}</b>
          <span>IRSAD decile</span><b>${p.irsad_decile ?? "n/a"}</b>
        </div>`;
    }
    const postcodeLayer = L.geoJSON(postcodeData, {
      style: f => ({ color: "#37454f", weight: 0.45, fillColor: colorFor(valueOf(f)), fillOpacity: 0.74 }),
      onEachFeature: (feature, lyr) => {
        postcodeIndex.set(feature.properties.postcode, lyr);
        lyr.bindPopup(() => postcodePopup(feature));
        lyr.on("mouseover", () => lyr.setStyle({ weight: 1.7, color: "#111820" }));
        lyr.on("mouseout", () => postcodeLayer.resetStyle(lyr));
      }
    }).addTo(map);
    map.fitBounds(postcodeLayer.getBounds(), { padding: [8, 8] });

    function siteColor(site) {
      if (site.status === "upcoming") return "#9b5de5";
      if (String(site.charger_type).toUpperCase() === "DC") return "#e76f51";
      return "#0077b6";
    }
    function includeSite(site) {
      if (pointMode.value === "none") return false;
      if (pointMode.value === "dc") return String(site.charger_type).toUpperCase() === "DC";
      if (pointMode.value === "upcoming") return site.status === "upcoming";
      return true;
    }
    function redrawPoints() {
      pointLayer.clearLayers();
      siteData.filter(includeSite).forEach(site => {
        const radius = Math.max(4, Math.min(13, 3 + Math.sqrt(site.number_of_plugs || 1) * 2));
        const marker = L.circleMarker([site.latitude, site.longitude], {
          radius,
          color: "#ffffff",
          weight: 1,
          fillColor: siteColor(site),
          fillOpacity: 0.86
        });
        marker.bindPopup(`<div class="popup-title">${site.station_name_clean || "Charging site"}</div>
          <div class="popup-grid">
            <span>Address</span><b>${site.station_address || "n/a"}</b>
            <span>Operator</span><b>${site.operator || "n/a"}</b>
            <span>Type</span><b>${site.charger_type || "n/a"}</b>
            <span>Rating</span><b>${site.charger_rating || "n/a"}</b>
            <span>Plugs</span><b>${formatNum(site.number_of_plugs)}</b>
            <span>Source</span><b>${site.source || "n/a"}</b>
          </div>`);
        marker.addTo(pointLayer);
      });
    }
    redrawPoints();

    function updateLayer() {
      postcodeLayer.setStyle(f => ({ color: "#37454f", weight: 0.45, fillColor: colorFor(valueOf(f)), fillOpacity: 0.74 }));
      updateBars();
      legend.update();
    }
    function updateBars() {
      const metric = metricSelect.value === "dc_plug_count" ? "dc_plugs_per_10000_people" : "plugs_per_10000_people";
      const max = Math.max(...decileData.map(d => d[metric] || 0));
      document.getElementById("bars").innerHTML = decileData.map(d => {
        const val = d[metric] || 0;
        const width = max > 0 ? 100 * val / max : 0;
        return `<div class="bar-row"><b>${d.irsad_decile}</b><div class="bar"><div style="width:${width}%"></div></div><span>${formatNum(val, 1)}</span></div>`;
      }).join("");
    }
    const legend = L.control({ position: "bottomleft" });
    legend.onAdd = function() { this._div = L.DomUtil.create("div", "legend"); this.update(); return this._div; };
    legend.update = function() {
      const metric = metricSelect.value;
      let rows;
      if (metric === "irsad_decile") {
        rows = decileRamp.map((c, i) => `<div class="legend-row"><span class="swatch" style="background:${c}"></span>Decile ${i + 1}</div>`);
      } else if (metric === "plug_count") {
        rows = [["0", ramp[0]], ["1-7", ramp[1]], ["8-19", ramp[2]], ["20-39", ramp[3]], ["40-79", ramp[4]], ["80+", ramp[5]]].map(d => `<div class="legend-row"><span class="swatch" style="background:${d[1]}"></span>${d[0]} plugs</div>`);
      } else if (metric === "dc_plug_count") {
        rows = [["0", ramp[0]], ["1", ramp[1]], ["2-4", ramp[2]], ["5-9", ramp[3]], ["10-19", ramp[4]], ["20+", ramp[5]]].map(d => `<div class="legend-row"><span class="swatch" style="background:${d[1]}"></span>${d[0]} DC plugs</div>`);
      } else {
        rows = [["0", ramp[0]], [">0-1", ramp[1]], ["1-3", ramp[2]], ["3-6", ramp[3]], ["6-12", ramp[4]], ["12+", ramp[5]]].map(d => `<div class="legend-row"><span class="swatch" style="background:${d[1]}"></span>${d[0]}</div>`);
      }
      this._div.innerHTML = `<b>${metricSelect.options[metricSelect.selectedIndex].text}</b>` + rows.join("") + `<hr><div class="legend-row"><span class="swatch" style="background:#0077b6"></span>AC site</div><div class="legend-row"><span class="swatch" style="background:#e76f51"></span>DC site</div><div class="legend-row"><span class="swatch" style="background:#9b5de5"></span>Upcoming</div>`;
    };
    legend.addTo(map);

    metricSelect.addEventListener("change", updateLayer);
    pointMode.addEventListener("change", redrawPoints);
    searchInput.addEventListener("keydown", event => {
      if (event.key !== "Enter") return;
      const pc = searchInput.value.trim().padStart(4, "0");
      const target = postcodeIndex.get(pc);
      if (target) {
        map.fitBounds(target.getBounds(), { maxZoom: 12 });
        target.openPopup();
      }
    });
    updateLayer();
  </script>
</body>
</html>')

html_path <- file.path(outputs_dir, "interactive_nsw_charging_wealth_map.html")
writeLines(html, html_path, useBytes = TRUE)

index <- '<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="refresh" content="0; url=outputs/interactive_nsw_charging_wealth_map.html">
  <title>NSW EV Charging and Wealth</title>
</head>
<body>
  <p><a href="outputs/interactive_nsw_charging_wealth_map.html">Open the NSW EV charging and wealth map</a></p>
</body>
</html>'
writeLines(index, file.path(root, "index.html"))

readme <- paste0(
  "# NSW Charging and Wealth\n\n",
  "This folder is a separate public-facing exhibit for NSW EV charging infrastructure. It maps charging sites directly from coordinates and aggregates plugs/sites to postcodes for SEIFA wealth analysis.\n\n",
  "Main artifact: [`outputs/interactive_nsw_charging_wealth_map.html`](outputs/interactive_nsw_charging_wealth_map.html)\n\n",
  "If GitHub Pages is enabled for the repository, `nsw_charging/index.html` redirects to the interactive map.\n\n",
  "## Headline\n\n",
  "- Charging sites: ", fmt_num(headline$sites), "\n",
  "- Charging plugs: ", fmt_num(headline$plugs), "\n",
  "- DC plugs: ", fmt_num(headline$dc_plugs), "\n",
  "- Upcoming plugs: ", fmt_num(headline$upcoming_plugs), "\n",
  "- Postcodes with at least one known charger: ", fmt_num(headline$postcodes_with_charging), "\n\n",
  "The wealth relationship should be read cautiously: dense inner-city and tourism postcodes can have lots of charging even where resident vehicle stock is small. The map therefore includes both absolute plug counts and normalised rates.\n\n",
  "## Rebuild\n\n",
  "From the repository root:\n\n",
  "```r\n",
  "Rscript wealth_evs/scripts/build_wealth_evs.R\n",
  "Rscript nsw_charging/scripts/build_nsw_charging.R\n",
  "```\n\n",
  "## Data Products\n\n",
  "- `data/processed/nsw_charging_sites_clean.csv`: cleaned site-level charger data.\n",
  "- `data/processed/nsw_charging_postcode_panel.csv`: postcode-level charging, SEIFA, and 2025 EV stock panel.\n",
  "- `data/processed/nsw_charging_irsad_decile_summary.csv`: charging access by IRSAD wealth decile.\n",
  "- `data/processed/nsw_charging_postcode_map.geojson`: map-ready postcode polygons.\n\n",
  "## Caveats\n\n",
  "- The charging CSV is a snapshot supplied by the project owner and should be source-checked before publication.\n",
  "- The map uses CSV coordinates for site points and parsed postcode/address values for postcode aggregation.\n",
  "- SEIFA is fixed to ABS 2021 Postal Area indexes.\n",
  "- Postcode denominators use 2025 AAA registration stock from the `wealth_evs` build.\n"
)
writeLines(readme, file.path(root, "README.md"))

message("Wrote ", nrow(chargers), " charging-site rows.")
message("Wrote ", nrow(postcode_panel), " NSW postcode panel rows.")
message("Wrote ", length(map_features), " postcode map features.")
message("Wrote ", html_path)
