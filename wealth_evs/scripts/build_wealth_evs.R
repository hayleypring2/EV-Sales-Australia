options(stringsAsFactors = FALSE)

required_packages <- c("readxl", "jsonlite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "))
}

root <- "wealth_evs"
raw_dir <- file.path(root, "data", "raw")
processed_dir <- file.path(root, "data", "processed")
outputs_dir <- file.path(root, "outputs")

dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)

seifa_url <- "https://www.abs.gov.au/statistics/people/people-and-communities/socio-economic-indexes-areas-seifa-australia/2021/Postal%20Area%2C%20Indexes%2C%20SEIFA%202021.xlsx"
seifa_path <- file.path(raw_dir, "Postal_Area_Indexes_SEIFA_2021.xlsx")
if (!file.exists(seifa_path)) {
  download.file(seifa_url, seifa_path, mode = "wb", quiet = TRUE)
}

boundary_base <- "https://maps.arcgis.aihw.gov.au/server/rest/services/Hosted/POA_2021_AUST_simplify_IR/FeatureServer/1/query"

download_boundary_page <- function(offset) {
  path <- file.path(raw_dir, paste0("poa_2021_boundaries_offset_", offset, ".geojson"))
  if (!file.exists(path)) {
    query <- paste(
      "where=1%3D1",
      "outFields=poa_code21,poa_name21,areasqkm21",
      "returnGeometry=true",
      "outSR=4326",
      "resultRecordCount=2000",
      paste0("resultOffset=", offset),
      "geometryPrecision=4",
      "f=geojson",
      sep = "&"
    )
    download.file(paste0(boundary_base, "?", query), path, mode = "wb", quiet = TRUE)
  }
  path
}

normalise_poa <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  numeric_like <- grepl("^[0-9]+$", x)
  x[numeric_like] <- sprintf("%04d", as.integer(x[numeric_like]))
  x
}

as_num <- function(x) suppressWarnings(as.numeric(x))

seifa_raw <- readxl::read_excel(seifa_path, sheet = "Table 1", skip = 6, col_names = FALSE)
names(seifa_raw) <- c(
  "poa_code", "irsd_score", "irsd_decile", "irsad_score", "irsad_decile",
  "ier_score", "ier_decile", "ieo_score", "ieo_decile", "usual_resident_population",
  "datazones", "poa_name"
)
seifa <- data.frame(
  poa_code = normalise_poa(seifa_raw$poa_code),
  irsd_score = as_num(seifa_raw$irsd_score),
  irsd_decile = as.integer(as_num(seifa_raw$irsd_decile)),
  irsad_score = as_num(seifa_raw$irsad_score),
  irsad_decile = as.integer(as_num(seifa_raw$irsad_decile)),
  ier_score = as_num(seifa_raw$ier_score),
  ier_decile = as.integer(as_num(seifa_raw$ier_decile)),
  ieo_score = as_num(seifa_raw$ieo_score),
  ieo_decile = as.integer(as_num(seifa_raw$ieo_decile)),
  usual_resident_population = as.integer(as_num(seifa_raw$usual_resident_population)),
  source_seifa_url = seifa_url
)
seifa <- seifa[grepl("^[0-9]{4}$", seifa$poa_code), ]

stock_path <- "data/processed/stock_panel_postcode_annual.csv"
if (!file.exists(stock_path)) {
  stop("Missing ", stock_path, ". Run scripts/ingest_aaa_registration_stock.R first.")
}
stock <- read.csv(stock_path, check.names = FALSE)
stock$postcode <- normalise_poa(stock$postcode)

stock_wide <- reshape(
  stock[, c("period", "stock_date", "postcode", "state", "powertrain", "stock")],
  idvar = c("period", "stock_date", "postcode", "state"),
  timevar = "powertrain",
  direction = "wide"
)
names(stock_wide) <- sub("^stock\\.", "", names(stock_wide))
for (col in c("BEV", "HEV_or_PHEV", "ICE")) {
  if (!col %in% names(stock_wide)) stock_wide[[col]] <- 0
  stock_wide[[col]][is.na(stock_wide[[col]])] <- 0
}

panel <- merge(
  stock_wide,
  seifa,
  by.x = "postcode",
  by.y = "poa_code",
  all.x = TRUE,
  sort = FALSE
)
panel$total_stock <- panel$BEV + panel$HEV_or_PHEV + panel$ICE
panel$bev_share <- ifelse(panel$total_stock > 0, panel$BEV / panel$total_stock, NA_real_)
panel$electrified_share <- ifelse(panel$total_stock > 0, (panel$BEV + panel$HEV_or_PHEV) / panel$total_stock, NA_real_)
panel$bev_per_1000_vehicles <- panel$bev_share * 1000
panel$wealth_data_status <- ifelse(is.na(panel$irsad_decile), "missing_seifa", "matched_seifa")
panel <- panel[order(panel$period, panel$state, panel$postcode), ]

write.csv(panel, file.path(processed_dir, "postcode_ev_wealth_panel.csv"), row.names = FALSE)

decile_summary <- do.call(rbind, lapply(split(panel[!is.na(panel$irsad_decile), ], list(panel$period[!is.na(panel$irsad_decile)], panel$irsad_decile[!is.na(panel$irsad_decile)]), drop = TRUE), function(x) {
  data.frame(
    period = unique(x$period),
    irsad_decile = unique(x$irsad_decile),
    postcode_count = length(unique(x$postcode)),
    total_stock = sum(x$total_stock, na.rm = TRUE),
    bev_stock = sum(x$BEV, na.rm = TRUE),
    hev_or_phev_stock = sum(x$HEV_or_PHEV, na.rm = TRUE),
    ice_stock = sum(x$ICE, na.rm = TRUE),
    bev_share_weighted = sum(x$BEV, na.rm = TRUE) / sum(x$total_stock, na.rm = TRUE),
    bev_share_unweighted = mean(x$bev_share, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
decile_summary <- decile_summary[order(decile_summary$period, decile_summary$irsad_decile), ]
decile_summary$share_of_national_bevs <- ave(decile_summary$bev_stock, decile_summary$period, FUN = function(x) x / sum(x, na.rm = TRUE))
write.csv(decile_summary, file.path(processed_dir, "irsad_decile_ev_summary.csv"), row.names = FALSE)

latest_year <- max(panel$period, na.rm = TRUE)
top_2025 <- panel[panel$period == latest_year & panel$total_stock >= 500, ]
top_2025 <- top_2025[order(-top_2025$bev_share, -top_2025$BEV), ]
write.csv(top_2025[seq_len(min(50, nrow(top_2025))), ], file.path(processed_dir, "top_bev_share_postcodes_2025.csv"), row.names = FALSE)

boundary_paths <- c(download_boundary_page(0), download_boundary_page(2000))
features <- list()
for (path in boundary_paths) {
  fc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  features <- c(features, fc$features)
}

panel_by_postcode <- split(panel, panel$postcode)

round_or_null <- function(x, digits = 4) {
  if (length(x) == 0 || is.na(x)) return(NULL)
  round(as.numeric(x), digits)
}

integer_or_null <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NULL)
  as.integer(x)
}

map_features <- list()
for (feature in features) {
  poa <- feature$properties$poa_code21
  if (is.null(poa) || !poa %in% names(panel_by_postcode)) next
  rows <- panel_by_postcode[[poa]]
  years <- list()
  for (i in seq_len(nrow(rows))) {
    year_key <- as.character(rows$period[i])
    years[[year_key]] <- list(
      bev_stock = integer_or_null(rows$BEV[i]),
      hev_or_phev_stock = integer_or_null(rows$HEV_or_PHEV[i]),
      ice_stock = integer_or_null(rows$ICE[i]),
      total_stock = integer_or_null(rows$total_stock[i]),
      bev_share = round_or_null(rows$bev_share[i], 5),
      electrified_share = round_or_null(rows$electrified_share[i], 5)
    )
  }
  first_row <- rows[1, ]
  feature$properties <- list(
    postcode = poa,
    state = first_row$state,
    area_sqkm = round_or_null(feature$properties$areasqkm21, 2),
    irsad_score = round_or_null(first_row$irsad_score, 1),
    irsad_decile = integer_or_null(first_row$irsad_decile),
    ier_score = round_or_null(first_row$ier_score, 1),
    ier_decile = integer_or_null(first_row$ier_decile),
    usual_resident_population = integer_or_null(first_row$usual_resident_population),
    years = years
  )
  map_features[[length(map_features) + 1]] <- feature
}

geojson <- list(
  type = "FeatureCollection",
  metadata = list(
    title = "Australian BEV registration share by Postal Area and SEIFA decile",
    source_ev_stock = "Australian Automobile Association EV Index registration workbook, 2021-2025",
    source_seifa = "ABS SEIFA 2021 Postal Area Indexes",
    source_boundaries = "AIHW-hosted simplified ABS ASGS Edition 3 POA 2021 boundary service",
    caveat = "Registration stock snapshots as at 31 January, not new sales."
  ),
  features = map_features
)
geojson_path <- file.path(processed_dir, "postcode_ev_wealth_map.geojson")
jsonlite::write_json(geojson, geojson_path, auto_unbox = TRUE, digits = 6, null = "null")

decile_json <- jsonlite::toJSON(decile_summary, dataframe = "rows", auto_unbox = TRUE, digits = 6, na = "null")
geojson_text <- paste(readLines(geojson_path, warn = FALSE), collapse = "\n")

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "n/a", paste0(round(100 * x, digits), "%"))
}

latest <- panel[panel$period == latest_year, ]
headline <- data.frame(
  latest_year = latest_year,
  postcodes = length(unique(latest$postcode)),
  total_bev_stock = sum(latest$BEV, na.rm = TRUE),
  national_bev_share = sum(latest$BEV, na.rm = TRUE) / sum(latest$total_stock, na.rm = TRUE),
  top_decile_bev_share = decile_summary$bev_share_weighted[decile_summary$period == latest_year & decile_summary$irsad_decile == 10],
  bottom_decile_bev_share = decile_summary$bev_share_weighted[decile_summary$period == latest_year & decile_summary$irsad_decile == 1],
  stringsAsFactors = FALSE
)
write.csv(headline, file.path(processed_dir, "wealth_evs_headline_stats.csv"), row.names = FALSE)

html <- paste0(
'<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>EVs and Wealth by Australian Postcode</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <style>
    html, body { height: 100%; margin: 0; font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #182027; }
    body { display: grid; grid-template-columns: minmax(320px, 420px) 1fr; background: #f7f4ef; }
    aside { overflow-y: auto; padding: 22px; background: #fbfaf7; border-right: 1px solid #d8d2c8; box-sizing: border-box; }
    #map { height: 100vh; width: 100%; }
    h1 { margin: 0 0 10px; font-size: 28px; line-height: 1.08; font-weight: 760; color: #101820; }
    h2 { margin: 22px 0 8px; font-size: 16px; color: #23313d; }
    p { line-height: 1.45; font-size: 14px; margin: 0 0 12px; }
    .lede { font-size: 15px; color: #33414d; }
    .control { margin: 14px 0; }
    label { display: block; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: .04em; color: #5a6872; margin-bottom: 6px; }
    select, input { width: 100%; box-sizing: border-box; border: 1px solid #bfc7cc; border-radius: 6px; padding: 9px 10px; background: white; color: #17212b; font-size: 14px; }
    .stats { display: grid; grid-template-columns: 1fr 1fr; gap: 9px; margin: 16px 0; }
    .stat { border: 1px solid #ded8cf; border-radius: 7px; padding: 10px; background: white; }
    .stat b { display: block; font-size: 20px; color: #005f73; }
    .stat span { display: block; font-size: 12px; color: #59666f; margin-top: 3px; }
    .bar-row { display: grid; grid-template-columns: 24px 1fr 54px; gap: 8px; align-items: center; font-size: 12px; margin: 5px 0; }
    .bar { height: 14px; border-radius: 3px; background: #e6e0d8; overflow: hidden; }
    .bar > div { height: 100%; background: #0a9396; }
    .legend { padding: 10px; background: white; border-radius: 6px; border: 1px solid #cfd8dc; line-height: 1.4; box-shadow: 0 1px 6px rgba(0,0,0,.12); }
    .legend-row { display: flex; align-items: center; gap: 6px; font-size: 12px; margin: 3px 0; }
    .swatch { width: 16px; height: 11px; border-radius: 2px; border: 1px solid rgba(0,0,0,.2); }
    .note { font-size: 12px; color: #5b666f; border-top: 1px solid #ded8cf; padding-top: 12px; margin-top: 16px; }
    .leaflet-popup-content { font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }
    .popup-title { font-weight: 800; font-size: 15px; margin-bottom: 4px; }
    .popup-grid { display: grid; grid-template-columns: auto auto; gap: 3px 14px; font-size: 12px; }
    @media (max-width: 820px) {
      body { grid-template-columns: 1fr; grid-template-rows: auto 65vh; }
      aside { max-height: none; border-right: none; border-bottom: 1px solid #d8d2c8; }
      #map { height: 65vh; }
    }
  </style>
</head>
<body>
  <aside>
    <h1>Are BEVs clustering in wealthy postcodes?</h1>
    <p class="lede">This map joins AAA EV registration stock to ABS SEIFA Postal Area deciles. It shows registered vehicle stock, not annual sales.</p>
    <div class="control">
      <label for="year">Year</label>
      <select id="year"></select>
    </div>
    <div class="control">
      <label for="metric">Map layer</label>
      <select id="metric">
        <option value="bev_share">BEV share of registered vehicles</option>
        <option value="bev_stock">BEV stock count</option>
        <option value="electrified_share">BEV + HEV/PHEV share</option>
        <option value="irsad_decile">ABS IRSAD wealth decile</option>
      </select>
    </div>
    <div class="control">
      <label for="postcodeSearch">Find postcode</label>
      <input id="postcodeSearch" placeholder="e.g. 2600">
    </div>
    <div class="stats">
      <div class="stat"><b id="totalBevs">', format(sum(latest$BEV, na.rm = TRUE), big.mark = ","), '</b><span>BEVs in ', latest_year, '</span></div>
      <div class="stat"><b id="nationalShare">', fmt_pct(headline$national_bev_share, 2), '</b><span>national BEV stock share</span></div>
      <div class="stat"><b id="topDecile">', fmt_pct(headline$top_decile_bev_share, 2), '</b><span>BEV share in IRSAD decile 10</span></div>
      <div class="stat"><b id="bottomDecile">', fmt_pct(headline$bottom_decile_bev_share, 2), '</b><span>BEV share in IRSAD decile 1</span></div>
    </div>
    <h2>BEV share by wealth decile</h2>
    <div id="bars"></div>
    <p class="note">Wealth proxy: ABS SEIFA 2021 Index of Relative Socio-economic Advantage and Disadvantage (IRSAD). Decile 1 is most disadvantaged; decile 10 is most advantaged. EV data: AAA registration stock snapshots as at 31 January.</p>
  </aside>
  <main id="map"></main>
  <script>
    const mapData = ', geojson_text, ';
    const decileData = ', decile_json, ';
  </script>
  <script>
    const years = [...new Set(decileData.map(d => d.period))].sort();
    const yearSelect = document.getElementById("year");
    years.forEach(y => {
      const option = document.createElement("option");
      option.value = y;
      option.textContent = y;
      yearSelect.appendChild(option);
    });
    yearSelect.value = years[years.length - 1];

    const metricSelect = document.getElementById("metric");
    const searchInput = document.getElementById("postcodeSearch");
    const map = L.map("map", { zoomControl: false }).setView([-25.5, 134.5], 4);
    L.control.zoom({ position: "bottomright" }).addTo(map);
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 18,
      attribution: "&copy; OpenStreetMap contributors"
    }).addTo(map);

    const colorsShare = ["#f1f0e8", "#cce6df", "#86c7bd", "#3b9c9c", "#087f8c", "#004f63"];
    const colorsStock = ["#f1f0e8", "#d4e7ec", "#9ccbd5", "#58a9b6", "#168092", "#00536b"];
    const colorsDecile = ["#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"];

    function metricValue(feature) {
      const y = feature.properties.years[yearSelect.value] || {};
      const metric = metricSelect.value;
      if (metric === "irsad_decile") return feature.properties.irsad_decile;
      return y[metric];
    }

    function colorFor(value) {
      const metric = metricSelect.value;
      if (value === null || value === undefined || Number.isNaN(value)) return "#d8d8d8";
      if (metric === "irsad_decile") return colorsDecile[Math.max(1, Math.min(10, value)) - 1];
      if (metric === "bev_stock") {
        if (value >= 1000) return colorsStock[5];
        if (value >= 500) return colorsStock[4];
        if (value >= 200) return colorsStock[3];
        if (value >= 50) return colorsStock[2];
        if (value >= 10) return colorsStock[1];
        return colorsStock[0];
      }
      if (value >= 0.08) return colorsShare[5];
      if (value >= 0.04) return colorsShare[4];
      if (value >= 0.02) return colorsShare[3];
      if (value >= 0.01) return colorsShare[2];
      if (value >= 0.005) return colorsShare[1];
      return colorsShare[0];
    }

    function formatPct(x) {
      if (x === null || x === undefined || Number.isNaN(x)) return "n/a";
      return (100 * x).toFixed(2) + "%";
    }

    function formatNum(x) {
      if (x === null || x === undefined || Number.isNaN(x)) return "n/a";
      return Math.round(x).toLocaleString();
    }

    function popup(feature) {
      const p = feature.properties;
      const y = p.years[yearSelect.value] || {};
      return `<div class="popup-title">Postcode ${p.postcode} (${p.state || "n/a"})</div>
        <div class="popup-grid">
          <span>BEV share</span><b>${formatPct(y.bev_share)}</b>
          <span>BEVs</span><b>${formatNum(y.bev_stock)}</b>
          <span>BEV + HEV/PHEV share</span><b>${formatPct(y.electrified_share)}</b>
          <span>Total vehicles</span><b>${formatNum(y.total_stock)}</b>
          <span>IRSAD decile</span><b>${p.irsad_decile ?? "n/a"}</b>
          <span>IRSAD score</span><b>${p.irsad_score ?? "n/a"}</b>
        </div>`;
    }

    const postcodeIndex = new Map();
    const layer = L.geoJSON(mapData, {
      style: feature => ({
        color: "#3e4a52",
        weight: 0.35,
        fillColor: colorFor(metricValue(feature)),
        fillOpacity: 0.78
      }),
      onEachFeature: (feature, lyr) => {
        postcodeIndex.set(feature.properties.postcode, lyr);
        lyr.bindPopup(() => popup(feature));
        lyr.on("mouseover", () => lyr.setStyle({ weight: 1.6, color: "#101820" }));
        lyr.on("mouseout", () => layer.resetStyle(lyr));
      }
    }).addTo(map);
    map.fitBounds(layer.getBounds(), { padding: [8, 8] });

    function updateLayer() {
      layer.setStyle(feature => ({
        color: "#3e4a52",
        weight: 0.35,
        fillColor: colorFor(metricValue(feature)),
        fillOpacity: 0.78
      }));
      updateBars();
      updateHeadline();
      updateLegend();
    }

    function updateHeadline() {
      const year = Number(yearSelect.value);
      const rows = decileData.filter(d => d.period === year);
      const bevs = rows.reduce((acc, d) => acc + d.bev_stock, 0);
      const total = rows.reduce((acc, d) => acc + d.total_stock, 0);
      const top = rows.find(d => d.irsad_decile === 10);
      const bottom = rows.find(d => d.irsad_decile === 1);
      document.getElementById("totalBevs").textContent = formatNum(bevs);
      document.getElementById("nationalShare").textContent = formatPct(bevs / total);
      document.getElementById("topDecile").textContent = top ? formatPct(top.bev_share_weighted) : "n/a";
      document.getElementById("bottomDecile").textContent = bottom ? formatPct(bottom.bev_share_weighted) : "n/a";
    }

    function updateBars() {
      const year = Number(yearSelect.value);
      const rows = decileData.filter(d => d.period === year).sort((a, b) => a.irsad_decile - b.irsad_decile);
      const max = Math.max(...rows.map(d => d.bev_share_weighted));
      document.getElementById("bars").innerHTML = rows.map(d => {
        const width = max > 0 ? 100 * d.bev_share_weighted / max : 0;
        return `<div class="bar-row"><b>${d.irsad_decile}</b><div class="bar"><div style="width:${width}%"></div></div><span>${formatPct(d.bev_share_weighted)}</span></div>`;
      }).join("");
    }

    const legend = L.control({ position: "bottomleft" });
    legend.onAdd = function() {
      this._div = L.DomUtil.create("div", "legend");
      this.update();
      return this._div;
    };
    legend.update = function() {
      const metric = metricSelect.value;
      let rows;
      if (metric === "irsad_decile") {
        rows = colorsDecile.map((c, i) => `<div class="legend-row"><span class="swatch" style="background:${c}"></span>Decile ${i + 1}</div>`);
      } else if (metric === "bev_stock") {
        rows = [["0-9", colorsStock[0]], ["10-49", colorsStock[1]], ["50-199", colorsStock[2]], ["200-499", colorsStock[3]], ["500-999", colorsStock[4]], ["1000+", colorsStock[5]]].map(d => `<div class="legend-row"><span class="swatch" style="background:${d[1]}"></span>${d[0]} BEVs</div>`);
      } else {
        rows = [["<0.5%", colorsShare[0]], ["0.5-1%", colorsShare[1]], ["1-2%", colorsShare[2]], ["2-4%", colorsShare[3]], ["4-8%", colorsShare[4]], ["8%+", colorsShare[5]]].map(d => `<div class="legend-row"><span class="swatch" style="background:${d[1]}"></span>${d[0]}</div>`);
      }
      this._div.innerHTML = `<b>${metricSelect.options[metricSelect.selectedIndex].text}</b>` + rows.join("");
    };
    legend.addTo(map);
    function updateLegend() { legend.update(); }

    yearSelect.addEventListener("change", updateLayer);
    metricSelect.addEventListener("change", updateLayer);
    searchInput.addEventListener("keydown", event => {
      if (event.key !== "Enter") return;
      const pc = searchInput.value.trim().padStart(4, "0");
      const target = postcodeIndex.get(pc);
      if (target) {
        map.fitBounds(target.getBounds(), { maxZoom: 11 });
        target.openPopup();
      }
    });

    updateLayer();
  </script>
</body>
</html>')

html_path <- file.path(outputs_dir, "interactive_ev_wealth_map.html")
writeLines(html, html_path, useBytes = TRUE)

notes <- paste0(
  "# Wealth and EVs by Postcode\n\n",
  "This folder is a small public-facing exhibit that asks whether BEV registration stock is clustered in more advantaged Australian postcodes.\n\n",
  "Main artifact: [`outputs/interactive_ev_wealth_map.html`](../outputs/interactive_ev_wealth_map.html)\n\n",
  "Data products:\n\n",
  "- `data/processed/postcode_ev_wealth_panel.csv`: annual postcode panel, 2021-2025, joined to ABS SEIFA 2021.\n",
  "- `data/processed/irsad_decile_ev_summary.csv`: BEV stock/share summarized by IRSAD decile and year.\n",
  "- `data/processed/postcode_ev_wealth_map.geojson`: map-ready joined GeoJSON.\n",
  "- `data/processed/top_bev_share_postcodes_2025.csv`: high-BEV-share postcodes among postcodes with at least 500 registered vehicles.\n\n",
  "Caveats:\n\n",
  "- The AAA postcode data are registration stock snapshots as at 31 January, not new vehicle sales.\n",
  "- BEV is clean; HEV and PHEV are combined in the AAA registration workbook.\n",
  "- Postal Areas are ABS approximations of postcodes for statistical use.\n",
  "- SEIFA is measured from the 2021 Census and held fixed across the EV stock years.\n\n",
  "Sources:\n\n",
  "- Australian Automobile Association EV Index registration workbook.\n",
  "- ABS SEIFA 2021 Postal Area Indexes.\n",
  "- AIHW-hosted simplified ABS ASGS Edition 3 Postal Area 2021 boundary service.\n"
)
writeLines(notes, file.path(root, "README.md"))

message("Wrote ", nrow(panel), " postcode-year rows.")
message("Wrote ", length(map_features), " mapped postcode features.")
message("Wrote ", html_path)
