# =============================================================================
# MSISNET Water System Violation Interactive Map — Final Version
# =============================================================================
#
# FEATURES:
#   - Water system polygons colored by selectable variable (toggle panel)
#   - Fixed meaningful thresholds for legend categories
#   - Legend updates dynamically when color variable is changed
#   - Filter polygons simultaneously by region, system size, dominant violation type
#     (non-matching polygons are greyed out, not hidden)
#   - Respondent points clustered
#   - Minimap, scale bar
#   - Polygon hover labels
#   - Zipcode search with autocomplete
#   - Reset button (clears search AND filters)
#   - Collapsible "How to Use" instructions
#   - Percentile rank + sparkline trend in polygon popup
#   - Respondent summary in polygon popup
#   - Respondent popups with survey responses (averaged across waves)
#   - Data source footnote bar at bottom of map
#
# INPUTS:
#   1. MSISNET_CWS.rds   — merged survey + violation dataset
#   2. CWS_2_0.gpkg      — water system boundary polygons (GeoPackage)
#
# OUTPUT:
#   water_violation_map.html
#
# DATA SOURCES:
#   Violations: EPA ECHO Drinking Water Dashboard
#               https://echo.epa.gov/trends/comparative-maps-dashboards/drinking-water-dashboard
#   Survey:     MSISNET Dataverse (Harvard Dataverse)
#               https://dataverse.harvard.edu/dataverse/msisnet
#
# TODO — GEOLOCATION UPGRADE:
#   Replace centroid block in Section 5 with a join to a
#   respondent_geolocations.csv file (columns: p_id, lat, lon).
# =============================================================================


# ── 0.  Packages ──────────────────────────────────────────────────────────────

required <- c("tidyverse", "sf", "leaflet", "leaflet.extras",
              "htmltools", "htmlwidgets")

new_pkgs <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

invisible(lapply(required, library, character.only = TRUE))
sf_use_s2(FALSE)


# ── 1.  File paths ────────────────────────────────────────────────────────────

RDS_PATH  <- "/Users/kaitlindiodosio/Dropbox/Dissertation/Data/ShinyApp/MSISNET_CWS.rds"
GPKG_PATH <- "/Users/kaitlindiodosio/Dropbox/Dissertation/Data/ShinyApp/CWS_2_0.gpkg"
OUT_HTML  <- "/Users/kaitlindiodosio/Dropbox/Dissertation/Data/ShinyApp/water_violation_map.html"


# ── 2.  Load data ─────────────────────────────────────────────────────────────

message("Loading survey data...")
df_raw <- readRDS(RDS_PATH)
if (inherits(df_raw, "sf")) df_raw <- st_drop_geometry(df_raw)
names(df_raw) <- tolower(names(df_raw))


# ── 2a. Violation summary ─────────────────────────────────────────────────────

VIOL_TYPES <- c(
  "Acute Health-Based",
  "Health-Based",
  "Monitoring & Reporting",
  "Public Notification & Other"
)

VIOL_COLORS <- c(
  "Acute Health-Based"           = "#c0392b",
  "Health-Based"                 = "#e67e22",
  "Monitoring & Reporting"       = "#2980b9",
  "Public Notification & Other"  = "#7f8c8d"
)

violations <- df_raw |>
  select(pws_id, calendar_year, violation_type, violations) |>
  distinct() |>
  filter(!is.na(pws_id), !is.na(calendar_year)) |>
  mutate(
    violations     = as.integer(violations),
    calendar_year  = as.integer(calendar_year),
    violation_type = case_when(
      str_detect(tolower(violation_type), "acute")               ~ "Acute Health-Based",
      str_detect(tolower(violation_type), "health")              ~ "Health-Based",
      str_detect(tolower(violation_type), "monitor|reporting")   ~ "Monitoring & Reporting",
      str_detect(tolower(violation_type), "public|notif|other")  ~ "Public Notification & Other",
      TRUE ~ str_to_title(violation_type)
    )
  )

system_totals <- violations |>
  group_by(pws_id) |>
  summarise(total_violations = sum(violations, na.rm = TRUE), .groups = "drop")

yearly_totals <- violations |>
  group_by(pws_id, calendar_year) |>
  summarise(year_total = sum(violations, na.rm = TRUE), .groups = "drop")

# Use select() for renaming — compatible with all dplyr versions
recent_year_totals <- yearly_totals |>
  filter(year_total > 0) |>
  group_by(pws_id) |>
  slice_max(calendar_year, n = 1) |>
  ungroup() |>
  select(pws_id, recent_violations = year_total, recent_year = calendar_year)

dominant_type <- violations |>
  group_by(pws_id, violation_type) |>
  summarise(type_total = sum(violations, na.rm = TRUE), .groups = "drop") |>
  group_by(pws_id) |>
  slice_max(type_total, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(pws_id, dominant_viol_type = violation_type)

compute_trend <- function(pwsid) {
  yt <- yearly_totals |> filter(pws_id == pwsid) |> arrange(calendar_year)
  if (nrow(yt) < 2) return("stable")
  mid   <- ceiling(nrow(yt) / 2)
  early <- mean(yt$year_total[1:mid],            na.rm = TRUE)
  late  <- mean(yt$year_total[(mid+1):nrow(yt)], na.rm = TRUE)
  if (late > early * 1.15) return("increasing")
  if (late < early * 0.85) return("decreasing")
  return("stable")
}

system_trends <- tibble(
  pws_id = unique(violations$pws_id),
  trend  = sapply(unique(violations$pws_id), compute_trend)
)


# ── 2b. System metadata ───────────────────────────────────────────────────────

system_meta <- df_raw |>
  filter(!is.na(pws_id), !is.na(system_size_y)) |>
  distinct(pws_id, .keep_all = TRUE) |>
  select(pws_id, system_size = system_size_y, ocwp_region, region, study_area) |>
  mutate(across(where(is.character), str_to_title)) |>
  mutate(pws_id = tolower(pws_id))


# ── 2c. Zipcode lookup ────────────────────────────────────────────────────────

zip_lookup <- df_raw |>
  select(p_id, current_zip, pws_id) |>
  distinct() |>
  filter(!is.na(current_zip), !is.na(pws_id)) |>
  mutate(current_zip = str_pad(
    as.character(as.integer(current_zip)), 5, pad = "0"
  ))

zip_to_pws <- zip_lookup |> select(current_zip, pws_id) |> distinct()

message(sprintf("  %d zipcodes linked to %d water systems",
                n_distinct(zip_to_pws$current_zip),
                n_distinct(zip_to_pws$pws_id)))


# ── 3.  Load polygon boundaries ───────────────────────────────────────────────

message("Loading GeoPackage boundaries...")
boundaries_all   <- st_read(GPKG_PATH, layer = "Boundaries", quiet = TRUE)
survey_ids_upper <- toupper(unique(zip_to_pws$pws_id))

boundaries <- boundaries_all |>
  filter(PWSID %in% survey_ids_upper) |>
  mutate(pws_id = tolower(PWSID)) |>
  st_transform(crs = 4326) |>
  st_make_valid() |>
  left_join(system_totals,      by = "pws_id") |>
  left_join(system_meta,        by = "pws_id") |>
  left_join(recent_year_totals, by = "pws_id") |>
  left_join(dominant_type,      by = "pws_id") |>
  left_join(system_trends,      by = "pws_id") |>
  mutate(
    total_violations  = replace_na(total_violations, 0),
    recent_violations = replace_na(recent_violations, 0),
    viol_per_capita   = ifelse(
      !is.na(Population_Served_Count) & Population_Served_Count > 0,
      round(total_violations / Population_Served_Count * 1000, 2),
      NA_real_
    ),
    viol_percentile = round(percent_rank(total_violations) * 100),
    trend           = replace_na(trend, "stable")
  )

message(sprintf("  %d polygons loaded.", nrow(boundaries)))

# Filter dropdown options
# system_size uses manual ordering (small to large) rather than alphabetical
filter_regions <- sort(na.omit(unique(boundaries$region)))
filter_sizes   <- c("Very Small", "Small", "Medium", "Large", "Very Large") |>
  intersect(na.omit(unique(boundaries$system_size)))
filter_types   <- sort(na.omit(unique(boundaries$dominant_viol_type)))


# ── 4.  Violation bar chart HTML ──────────────────────────────────────────────

make_html_chart <- function(pwsid, sys_name) {
  v <- violations |> filter(pws_id == pwsid, violations > 0)
  
  if (nrow(v) == 0) {
    return(paste0(
      '<div style="font-family:Arial,sans-serif;padding:14px 16px;min-width:340px;">',
      '<div style="font-size:14px;font-weight:700;color:#1a1a2e;margin-bottom:10px;">',
      htmlEscape(sys_name), '</div>',
      '<div style="color:#27ae60;font-size:13px;">No violations recorded (2015\u20132025)</div>',
      '</div>'
    ))
  }
  
  all_years_v <- sort(unique(v$calendar_year))
  
  year_rows_html <- sapply(all_years_v, function(yr) {
    yr_data  <- v |> filter(calendar_year == yr)
    yr_total <- sum(yr_data$violations, na.rm = TRUE)
    if (yr_total == 0) return("")
    segments <- sapply(VIOL_TYPES, function(vt) {
      n <- yr_data$violations[yr_data$violation_type == vt]
      n <- if (length(n) == 0 || is.na(n)) 0L else as.integer(n)
      if (n == 0) return("")
      pct <- round(n / yr_total * 100, 1)
      paste0('<div title="', htmlEscape(vt), ': ', n, '" ',
             'style="display:inline-block;width:', pct, '%;height:18px;',
             'background:', VIOL_COLORS[[vt]], ';"></div>')
    })
    paste0('<tr>',
           '<td style="font-size:11px;color:#555;padding:2px 6px 2px 0;white-space:nowrap;width:36px;">', yr, '</td>',
           '<td style="width:100%;"><div style="display:flex;width:100%;height:18px;border-radius:2px;overflow:hidden;background:#f0f0f0;">',
           paste(segments, collapse = ""), '</div></td>',
           '<td style="font-size:11px;color:#333;padding:2px 0 2px 6px;white-space:nowrap;">', yr_total, '</td></tr>')
  })
  
  legend_items <- sapply(VIOL_TYPES, function(vt) {
    paste0('<span style="display:inline-flex;align-items:center;margin-right:10px;font-size:10.5px;color:#444;">',
           '<span style="display:inline-block;width:10px;height:10px;background:', VIOL_COLORS[[vt]],
           ';border-radius:2px;margin-right:4px;flex-shrink:0;"></span>', htmlEscape(vt), '</span>')
  })
  
  total_all <- sum(v$violations, na.rm = TRUE)
  badge_col <- if (total_all == 0) "#27ae60" else if (total_all <= 10) "#f1c40f" else
    if (total_all <= 30) "#e67e22" else if (total_all <= 75) "#e74c3c" else "#7b241c"
  
  paste0(
    '<div style="font-family:Arial,sans-serif;padding:14px 16px 10px;min-width:360px;max-width:460px;">',
    '<div style="display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:10px;">',
    '<div style="font-size:13.5px;font-weight:700;color:#1a1a2e;line-height:1.3;max-width:300px;">', htmlEscape(sys_name), '</div>',
    '<div style="font-size:11px;background:', badge_col, ';color:#fff;border-radius:12px;padding:2px 9px;white-space:nowrap;margin-left:8px;font-weight:600;">', total_all, ' total</div>',
    '</div>',
    '<table style="width:100%;border-collapse:collapse;">', paste(year_rows_html[year_rows_html != ""], collapse = "\n"), '</table>',
    '<div style="margin-top:10px;line-height:1.8;">', paste(legend_items, collapse = ""), '</div>',
    '</div>'
  )
}


# ── 5.  Respondent points ─────────────────────────────────────────────────────

message("Building respondent points...")

survey_cols_present <- intersect(
  c("p_id", "current_zip", "pws_id", "wave", "gend", "race", "hispanic",
    "party", "ideol", "rate_color", "rate_safety", "rate_smell", "rate_softness",
    "rate_taste", "concern_water_quality", "know_source", "wtr_trust_gov",
    "wtr_trust_sci", "wtr_trust_tech", "wtr_inside", "no_trt", "portable_trt",
    "single_faucet_trt", "whole_home_trt"),
  names(df_raw)
)

numeric_cols_present <- intersect(
  c("rate_color", "rate_safety", "rate_smell", "rate_softness", "rate_taste",
    "wtr_trust_gov", "wtr_trust_sci", "wtr_trust_tech", "wtr_inside", "no_trt",
    "portable_trt", "single_faucet_trt", "whole_home_trt", "concern_water_quality"),
  names(df_raw)
)

categorical_cols_present <- intersect(
  c("current_zip", "pws_id", "gend", "race", "hispanic", "party", "ideol", "know_source"),
  names(df_raw)
)

df_survey <- df_raw |>
  select(all_of(survey_cols_present)) |>
  filter(!is.na(p_id), !is.na(pws_id))

numeric_means <- df_survey |>
  select(p_id, all_of(numeric_cols_present)) |>
  mutate(across(all_of(numeric_cols_present), as.numeric)) |>
  group_by(p_id) |>
  summarise(across(all_of(numeric_cols_present),
                   ~ round(mean(.x, na.rm = TRUE), 2)), .groups = "drop")

categorical_vals <- df_survey |>
  select(p_id, all_of(categorical_cols_present)) |>
  group_by(p_id) |>
  summarise(across(all_of(categorical_cols_present),
                   ~ if (all(is.na(.x))) NA_character_
                   else as.character(.x[!is.na(.x)][1])),
            .groups = "drop")

wave_counts <- df_survey |>
  group_by(p_id) |>
  summarise(n_waves = n_distinct(wave, na.rm = TRUE), .groups = "drop")

# TODO — GEOLOCATION UPGRADE: replace this centroid block with:
#   geo_coords  <- read_csv("respondent_geolocations.csv")  # p_id, lat, lon
#   respondents <- respondents |> left_join(geo_coords, by = "p_id") |>
#                  filter(!is.na(lat), !is.na(lon))
centroids_sf <- boundaries |> st_centroid()
centroids <- centroids_sf |>
  mutate(lon = st_coordinates(centroids_sf)[, 1],
         lat = st_coordinates(centroids_sf)[, 2]) |>
  st_drop_geometry() |>
  select(pws_id, lon, lat)

respondents <- numeric_means |>
  left_join(categorical_vals, by = "p_id") |>
  left_join(wave_counts,      by = "p_id") |>
  mutate(current_zip = str_pad(as.character(as.integer(current_zip)), 5, pad = "0")) |>
  left_join(centroids, by = "pws_id") |>
  filter(!is.na(lat), !is.na(lon)) |>
  left_join(boundaries |> st_drop_geometry() |>
              select(pws_id, PWS_Name, ocwp_region, system_size, total_violations),
            by = "pws_id")

message(sprintf("  %d respondents placed on map.", nrow(respondents)))


# ── 5a. Respondent summary per system ────────────────────────────────────────

system_respondent_summary <- respondents |>
  group_by(pws_id) |>
  summarise(n_respondents = n(),
            avg_concern   = round(mean(concern_water_quality, na.rm = TRUE), 1),
            .groups = "drop")

boundaries <- boundaries |>
  left_join(system_respondent_summary, by = "pws_id") |>
  mutate(n_respondents = replace_na(n_respondents, 0),
         avg_concern   = replace_na(avg_concern, NA_real_))


# ── 5b. Respondent popup HTML ─────────────────────────────────────────────────

get_val <- function(row, col) {
  val <- row[[col]]
  if (is.null(val) || length(val) == 0 || is.na(val) || val == "" || val == "NA")
    return("\u2014")
  as.character(val)
}

resp_row <- function(label, value) {
  if (is.na(value) || value == "" || value == "NA") value <- "\u2014"
  paste0('<div><div style="color:#888;font-size:10px;text-transform:uppercase;letter-spacing:.5px;">', label, '</div>',
         '<div style="color:#1a1a2e;font-weight:600;font-size:12px;">', value, '</div></div>')
}

dot_rating <- function(value, max_val = 5) {
  n <- suppressWarnings(as.numeric(value))
  if (is.na(n) || value == "\u2014") return("\u2014")
  dots <- paste0(sapply(seq_len(max_val), function(i) {
    col <- if (i <= round(n)) "#2980b9" else "#dde2ea"
    paste0('<span style="display:inline-block;width:10px;height:10px;border-radius:50%;background:', col, ';margin-right:3px;"></span>')
  }), collapse = "")
  paste0('<span style="vertical-align:middle;">', dots, '</span> <span style="font-size:10.5px;color:#888;">(', round(n,1), '/', max_val, ')</span>')
}

yn <- function(value) {
  v <- suppressWarnings(as.numeric(value))
  if (is.na(v) || value == "\u2014") return("\u2014")
  if (round(v) == 1) return('<span style="color:#27ae60;font-weight:700;">Yes</span>')
  return('<span style="color:#bdc3c7;">No</span>')
}

make_respondent_popup <- function(row) {
  paste0(
    '<div style="border-radius:8px;overflow:hidden;box-shadow:0 4px 18px rgba(0,0,0,.14);">',
    '<div style="font-family:Arial,sans-serif;padding:12px 16px 10px;min-width:320px;max-width:420px;">',
    '<div style="font-size:13px;font-weight:700;color:#1a1a2e;border-bottom:2px solid #2980b9;padding-bottom:6px;margin-bottom:10px;display:flex;justify-content:space-between;align-items:center;">',
    '<span>Respondent&nbsp;<span style="font-weight:400;color:#888;font-size:11px;">', htmlEscape(get_val(row, "p_id")), '</span></span>',
    '<span style="font-size:10px;background:#eaf2fb;color:#2980b9;border-radius:10px;padding:2px 8px;font-weight:600;">', get_val(row, "n_waves"), ' wave(s)</span></div>',
    '<div style="font-size:11.5px;font-weight:700;color:#555;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;">Water System</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 16px;margin-bottom:12px;">',
    resp_row("System Name", get_val(row, "PWS_Name")),
    resp_row("System ID", toupper(get_val(row, "pws_id"))),
    resp_row("ZIP Code", get_val(row, "current_zip")),
    resp_row("Region", get_val(row, "ocwp_region")),
    resp_row("System Size", get_val(row, "system_size")),
    resp_row("Total Violations (2015\u20132025)", get_val(row, "total_violations")),
    '</div>',
    '<div style="font-size:11.5px;font-weight:700;color:#555;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;">Demographics</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 16px;margin-bottom:12px;">',
    resp_row("Gender", get_val(row, "gend")), resp_row("Race", get_val(row, "race")),
    resp_row("Hispanic", get_val(row, "hispanic")), resp_row("Party", get_val(row, "party")),
    resp_row("Ideology", get_val(row, "ideol")),
    '</div>',
    '<div style="font-size:11.5px;font-weight:700;color:#555;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;">Water Quality Ratings <span style="font-size:10px;font-weight:400;color:#aaa;">(mean across waves)</span></div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 16px;margin-bottom:12px;">',
    resp_row("Safety", dot_rating(get_val(row, "rate_safety"))),
    resp_row("Taste", dot_rating(get_val(row, "rate_taste"))),
    resp_row("Smell", dot_rating(get_val(row, "rate_smell"))),
    resp_row("Color", dot_rating(get_val(row, "rate_color"))),
    resp_row("Softness", dot_rating(get_val(row, "rate_softness"))),
    '</div>',
    '<div style="font-size:11.5px;font-weight:700;color:#555;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;">Trust & Concern <span style="font-size:10px;font-weight:400;color:#aaa;">(mean across waves)</span></div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 16px;margin-bottom:12px;">',
    resp_row("Trust Gov", dot_rating(get_val(row, "wtr_trust_gov"))),
    resp_row("Trust Science", dot_rating(get_val(row, "wtr_trust_sci"))),
    resp_row("Trust Tech", dot_rating(get_val(row, "wtr_trust_tech"))),
    resp_row("Water Quality Concern", dot_rating(get_val(row, "concern_water_quality"))),
    resp_row("Knows Source", get_val(row, "know_source")),
    '</div>',
    '<div style="font-size:11.5px;font-weight:700;color:#555;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;">Treatment Behavior</div>',
    '<div style="display:grid;grid-template-columns:1fr 1fr;gap:5px 16px;margin-bottom:14px;">',
    resp_row("Drinks Inside Tap", yn(get_val(row, "wtr_inside"))),
    resp_row("No Treatment", yn(get_val(row, "no_trt"))),
    resp_row("Portable Filter", yn(get_val(row, "portable_trt"))),
    resp_row("Single Faucet", yn(get_val(row, "single_faucet_trt"))),
    resp_row("Whole Home", yn(get_val(row, "whole_home_trt"))),
    '</div></div></div>'
  )
}

message("Building respondent popups...")
respondents$popup_html <- apply(respondents, 1, make_respondent_popup)
message(sprintf("  %d respondent popups built.", nrow(respondents)))


# ── 6.  Polygon popup HTML ────────────────────────────────────────────────────

cell <- function(label, value) {
  paste0('<div><div style="color:#888;font-size:10px;text-transform:uppercase;letter-spacing:.5px;">', label, '</div>',
         '<div style="color:#1a1a2e;font-weight:600;">', value, '</div></div>')
}

make_sparkline <- function(pwsid, trend) {
  yt <- yearly_totals |> filter(pws_id == pwsid) |> arrange(calendar_year)
  if (nrow(yt) < 2) return("")
  max_val <- max(yt$year_total, 1); w <- 60; h <- 20
  pts <- sapply(seq_len(nrow(yt)), function(i) {
    paste0(round((i-1)/(nrow(yt)-1)*w,1), ",", round(h-(yt$year_total[i]/max_val*h),1))
  })
  arrow_col <- if (trend=="increasing") "#c0392b" else if (trend=="decreasing") "#27ae60" else "#888"
  arrow     <- if (trend=="increasing") " &#8599;" else if (trend=="decreasing") " &#8600;" else " &#8594;"
  paste0('<div style="display:flex;align-items:center;margin-top:4px;">',
         '<svg width="',w,'" height="',h,'" style="vertical-align:middle;margin-right:6px;" xmlns="http://www.w3.org/2000/svg">',
         '<polyline points="',paste(pts,collapse=" "),'" fill="none" stroke="',arrow_col,'" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"/></svg>',
         '<span style="color:',arrow_col,';font-size:11px;font-weight:700;">',str_to_title(trend),arrow,'</span></div>')
}

make_polygon_popup <- function(pws_id_val, pws_name, pop_served, svc_conn, svc_type,
                               sys_size_val, region, total_viol, percentile, n_resp,
                               avg_concern, trend, viol_per_cap, dom_type) {
  chart_html <- make_html_chart(pws_id_val, pws_name)
  spark_html <- make_sparkline(pws_id_val, trend)
  fmt    <- function(x) { v <- suppressWarnings(as.integer(x)); if (is.na(v)) "\u2014" else format(v, big.mark=",") }
  na_str <- function(x) { if (is.null(x)||is.na(x)||x==""||x=="Na") "\u2014" else x }
  badge_col   <- if (total_viol==0) "#27ae60" else if (total_viol<=10) "#f1c40f" else
    if (total_viol<=30) "#e67e22" else if (total_viol<=75) "#e74c3c" else "#7b241c"
  pct_col     <- if (is.na(percentile)) "#888" else if (percentile>=75) "#c0392b" else if (percentile>=50) "#e67e22" else "#27ae60"
  pct_str     <- if (is.na(percentile)) "\u2014" else paste0(percentile, "th percentile among all systems")
  resp_str    <- if (n_resp==0) "No survey respondents" else paste0(n_resp, " respondent", if(n_resp>1) "s" else "")
  concern_str <- if (is.na(avg_concern)) "\u2014" else paste0(round(avg_concern,1), " / 5")
  per_cap_str <- if (is.na(viol_per_cap)) "\u2014" else paste0(viol_per_cap, " per 1,000 served")
  dom_str     <- if (is.na(dom_type)||dom_type=="") "\u2014" else htmlEscape(dom_type)
  
  meta_html <- paste0(
    '<div style="font-family:Arial,sans-serif;font-size:11.5px;background:#f4f6f9;border-top:1px solid #dde2ea;padding:9px 16px 11px;display:grid;grid-template-columns:1fr 1fr;gap:6px 20px;">',
    cell("System ID", toupper(pws_id_val)), cell("Region", na_str(region)),
    cell("System Size", na_str(sys_size_val)), cell("Service Area", na_str(svc_type)),
    cell("Population Served", fmt(pop_served)), cell("Connections", fmt(svc_conn)),
    cell("Per Capita (per 1k)", per_cap_str), cell("Dominant Type", dom_str),
    paste0('<div style="grid-column:1/-1;border-top:1px solid #dde2ea;padding-top:7px;margin-top:2px;">',
           '<div style="color:#888;font-size:10px;text-transform:uppercase;letter-spacing:.5px;">Total Violations (2015\u20132025)</div>',
           '<div style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;">',
           '<span style="color:',badge_col,';font-weight:700;font-size:15px;">',total_viol,'</span>',
           spark_html,'<span style="color:',pct_col,';font-size:11px;font-weight:600;">',pct_str,'</span></div></div>'),
    paste0('<div style="grid-column:1/-1;border-top:1px solid #dde2ea;padding-top:7px;margin-top:2px;">',
           '<div style="color:#888;font-size:10px;text-transform:uppercase;letter-spacing:.5px;">Survey Respondents</div>',
           '<div style="display:flex;align-items:baseline;gap:10px;">',
           '<span style="color:#1a1a2e;font-weight:700;font-size:13px;">',resp_str,'</span>',
           if(n_resp>0) paste0('<span style="color:#555;font-size:11px;">avg. water quality concern: <b>',concern_str,'</b></span>') else "",
           '</div></div>'),
    '</div>'
  )
  paste0('<div style="border-radius:8px;overflow:hidden;box-shadow:0 4px 18px rgba(0,0,0,.14);">', chart_html, meta_html, '</div>')
}

message("Building polygon popups...")
boundaries$popup_html <- unlist(mapply(
  make_polygon_popup,
  boundaries$pws_id, boundaries$PWS_Name,
  boundaries$Population_Served_Count, boundaries$Service_Connections_Count,
  boundaries$Service_Area_Type, boundaries$system_size, boundaries$region,
  boundaries$total_violations, boundaries$viol_percentile, boundaries$n_respondents,
  boundaries$avg_concern, boundaries$trend, boundaries$viol_per_capita,
  boundaries$dominant_viol_type, SIMPLIFY = FALSE
))
message(sprintf("  %d polygon popups built.", nrow(boundaries)))


# ── 7.  Color palette ─────────────────────────────────────────────────────────

THRESHOLD_PALETTE <- c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#7b241c")

threshold_color <- function(x, breaks) {
  case_when(
    is.na(x)       ~ "#bdc3c7",
    x <= breaks[1] ~ THRESHOLD_PALETTE[1],
    x <= breaks[2] ~ THRESHOLD_PALETTE[2],
    x <= breaks[3] ~ THRESHOLD_PALETTE[3],
    x <= breaks[4] ~ THRESHOLD_PALETTE[4],
    TRUE           ~ THRESHOLD_PALETTE[5]
  )
}

boundaries <- boundaries |>
  mutate(fill_color = threshold_color(total_violations, c(0, 10, 30, 75)))


# ── 8.  JS data payloads ──────────────────────────────────────────────────────

zip_js_entries <- zip_to_pws |>
  mutate(pws_id_upper = toupper(pws_id)) |>
  group_by(current_zip) |>
  summarise(ids = paste0('"', pws_id_upper, '"', collapse = ", "), .groups = "drop") |>
  mutate(entry = paste0('"', current_zip, '": [', ids, ']')) |>
  pull(entry) |> paste(collapse = ",\n  ")

zip_js_object <- paste0("{\n  ", zip_js_entries, "\n}")
zip_labels_js <- paste0('["', paste(sort(unique(zip_to_pws$current_zip)), collapse = '","'), '"]')

total_js   <- paste0("[", paste(boundaries$total_violations, collapse = ","), "]")
percap_js  <- paste0("[", paste(ifelse(is.na(boundaries$viol_per_capita), "null", boundaries$viol_per_capita), collapse = ","), "]")
recent_js  <- paste0("[", paste(boundaries$recent_violations, collapse = ","), "]")
domtype_js <- paste0('["', paste(ifelse(is.na(boundaries$dominant_viol_type), "", boundaries$dominant_viol_type), collapse = '","'), '"]')

labels_total_js  <- '["None (0)", "Low (1\u201310)", "Moderate (11\u201330)", "High (31\u201375)", "Very High (75+)"]'
labels_percap_js <- '["None (0)", "Low (0\u20135)", "Moderate (5\u201325)", "High (25\u2013100)", "Very High (100+)"]'
labels_recent_js <- '["None (0)", "Low (1\u20132)", "Moderate (3\u20135)", "High (6\u201315)", "Very High (15+)"]'

# Filter data arrays (one value per polygon row, matching boundary order)
filter_region_js  <- paste0('["', paste(ifelse(is.na(boundaries$region), "", boundaries$region), collapse = '","'), '"]')
filter_size_js    <- paste0('["', paste(ifelse(is.na(boundaries$system_size), "", boundaries$system_size), collapse = '","'), '"]')
filter_domtype_js <- paste0('["', paste(ifelse(is.na(boundaries$dominant_viol_type), "", boundaries$dominant_viol_type), collapse = '","'), '"]')

# Dropdown option lists (with "All" option prepended)
region_opts_js  <- paste0('["All Regions","',  paste(filter_regions, collapse = '","'), '"]')
size_opts_js    <- paste0('["All Sizes","',    paste(filter_sizes,   collapse = '","'), '"]')
domtype_opts_js <- paste0('["All Types","',    paste(filter_types,   collapse = '","'), '"]')


# ── 9.  Build the Leaflet map ─────────────────────────────────────────────────

message("Building Leaflet map...")

map <- leaflet(options = leafletOptions(minZoom = 5, maxZoom = 16)) |>
  addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.88)) |>
  setView(lng = -97.5, lat = 35.5, zoom = 7) |>
  
  addPolygons(
    data             = boundaries,
    group            = "Water Systems",
    layerId          = ~toupper(pws_id),
    fillColor        = ~fill_color,
    fillOpacity      = 0.50,
    color            = "#2c3e50",
    weight           = 1.2,
    opacity          = 0.8,
    highlightOptions = highlightOptions(
      color = "#f39c12", weight = 3.5, fillOpacity = 0.80, bringToFront = TRUE
    ),
    popup        = ~popup_html,
    popupOptions = popupOptions(maxWidth = 480, minWidth = 360, className = "viol-popup"),
    label = ~paste0(PWS_Name, " [", toupper(pws_id), "] \u2014 ",
                    total_violations, " violation", ifelse(total_violations == 1, "", "s")),
    labelOptions = labelOptions(
      style = list("font-size" = "12px", "font-family" = "Arial, sans-serif",
                   "background-color" = "rgba(255,255,255,0.92)",
                   "border" = "1px solid #dde2ea", "border-radius" = "5px",
                   "padding" = "4px 8px"),
      direction = "auto"
    )
  ) |>
  
  addMarkers(
    data         = respondents,
    group        = "Respondents",
    lng          = ~lon, lat = ~lat,
    icon         = makeIcon(
      iconUrl     = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12'%3E%3Ccircle cx='6' cy='6' r='5' fill='%232980b9' stroke='%231a5276' stroke-width='1.5'/%3E%3C/svg%3E",
      iconWidth = 12, iconHeight = 12, iconAnchorX = 6, iconAnchorY = 6
    ),
    popup        = ~popup_html,
    popupOptions = popupOptions(maxWidth = 440, minWidth = 320, className = "resp-popup"),
    label        = ~paste0("Respondent: ", p_id),
    labelOptions = labelOptions(
      style = list("font-size" = "11px", "font-family" = "Arial, sans-serif"),
      direction = "auto"
    ),
    clusterOptions = markerClusterOptions(
      showCoverageOnHover = FALSE, zoomToBoundsOnClick = TRUE, maxClusterRadius = 40,
      iconCreateFunction = JS("
        function(cluster) {
          var count = cluster.getChildCount();
          var size  = count < 10 ? 30 : count < 50 ? 36 : 42;
          return new L.DivIcon({
            html: '<div style=\"background:#2980b9;color:#fff;border-radius:50%;width:'+size+'px;height:'+size+'px;display:flex;align-items:center;justify-content:center;font-size:12px;font-weight:700;font-family:Arial,sans-serif;border:2px solid #1a5276;box-shadow:0 2px 6px rgba(0,0,0,.2);\">'+count+'</div>',
            className: '', iconSize: new L.Point(size, size)
          });
        }
      ")
    )
  ) |>
  
  addLayersControl(
    overlayGroups = c("Water Systems", "Respondents"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  
  addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE,
             minimized = FALSE, width = 150, height = 120, position = "bottomleft") |>
  
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = TRUE, metric = TRUE)) |>
  
  addControl(
    html     = '<div id="map-legend" style="background:rgba(255,255,255,0.92);border-radius:8px;box-shadow:0 2px 10px rgba(0,0,0,.15);padding:10px 14px;font-family:Arial,sans-serif;font-size:11.5px;min-width:160px;"></div>',
    position = "bottomright"
  )


# ── 10.  Control panel ────────────────────────────────────────────────────────
# Built with writeLines() — one <script> tag, no duplicate functions,
# all sections inside one #ctrl-panel div.

control_html_file <- tempfile(fileext = ".html")

writeLines(c(
  
  # ── CSS ────────────────────────────────────────────────────────────────────
  '<style>',
  '  #ctrl-panel { position:absolute; top:12px; left:52px; z-index:1000; background:#fff; border-radius:10px; box-shadow:0 3px 16px rgba(0,0,0,.20); padding:13px 15px 12px; width:280px; font-family:Arial,sans-serif; max-height:90vh; overflow-y:auto; }',
  '  #ctrl-panel h3 { margin:0 0 10px; font-size:13.5px; color:#1a1a2e; font-weight:700; }',
  '  .ctrl-label { font-size:10.5px; color:#888; text-transform:uppercase; letter-spacing:.5px; margin-bottom:4px; margin-top:10px; display:block; }',
  '  #zip-wrap { position:relative; }',
  '  #zip-input { width:100%; box-sizing:border-box; padding:8px 11px; border:1.5px solid #ced4da; border-radius:6px; font-size:13px; outline:none; transition:border-color .18s; }',
  '  #zip-input:focus { border-color:#2980b9; box-shadow:0 0 0 2px rgba(41,128,185,.15); }',
  '  #zip-autocomplete { position:absolute; top:100%; left:0; right:0; z-index:2000; background:#fff; border:1.5px solid #ced4da; border-top:none; border-radius:0 0 6px 6px; max-height:140px; overflow-y:auto; display:none; box-shadow:0 4px 12px rgba(0,0,0,.12); }',
  '  .zip-ac-item { padding:7px 11px; font-size:13px; cursor:pointer; color:#1a1a2e; }',
  '  .zip-ac-item:hover { background:#eaf2fb; }',
  '  .ctrl-btn-primary { margin-top:7px; width:100%; padding:8px; background:#2c3e50; color:#fff; border:none; border-radius:6px; font-size:13px; cursor:pointer; transition:background .18s; display:block; }',
  '  .ctrl-btn-primary:hover { background:#1a252f; }',
  '  .ctrl-btn-secondary { margin-top:5px; width:100%; padding:6px; background:#f4f6f9; color:#555; border:1px solid #dde2ea; border-radius:6px; font-size:12px; cursor:pointer; transition:background .18s; display:block; }',
  '  .ctrl-btn-secondary:hover { background:#dde2ea; }',
  '  #zip-result { margin-top:8px; font-size:11.5px; color:#555; min-height:16px; line-height:1.55; }',
  '  #color-toggle { display:grid; grid-template-columns:1fr 1fr; gap:5px; margin-top:4px; }',
  '  .col-btn { padding:5px 4px; font-size:11px; text-align:center; border:1.5px solid #ced4da; border-radius:5px; cursor:pointer; background:#fff; color:#1a1a2e; transition:all .15s; line-height:1.3; }',
  '  .col-btn:hover { border-color:#2980b9; color:#2980b9; }',
  '  .col-btn.active { background:#2c3e50; color:#fff; border-color:#2c3e50; }',
  '  .ctrl-divider { border:none; border-top:1px solid #eee; margin:10px 0 0; }',
  '  #instr-header { display:flex; justify-content:space-between; align-items:center; cursor:pointer; padding:6px 0; }',
  '  #instr-header:hover span:first-child { color:#2980b9; }',
  '  .filter-select { width:100%; box-sizing:border-box; padding:6px 8px; border:1.5px solid #ced4da; border-radius:6px; font-size:12px; color:#1a1a2e; background:#fff; outline:none; cursor:pointer; transition:border-color .18s; }',
  '  .filter-select:focus { border-color:#2980b9; }',
  '  #filter-status { margin-top:6px; font-size:11px; color:#888; min-height:14px; }',
  '  #footnote-bar { position:absolute; bottom:0; left:0; right:0; z-index:1000; background:rgba(255,255,255,0.92); border-top:1px solid #dde2ea; padding:5px 16px; font-family:Arial,sans-serif; font-size:10.5px; color:#666; display:flex; align-items:center; justify-content:center; gap:20px; flex-wrap:wrap; }',
  '  #footnote-bar a { color:#2980b9; text-decoration:none; }',
  '  #footnote-bar a:hover { text-decoration:underline; }',
  '  .viol-popup .leaflet-popup-content-wrapper { border-radius:8px !important; padding:0 !important; overflow:hidden; box-shadow:0 6px 24px rgba(0,0,0,.18) !important; }',
  '  .viol-popup .leaflet-popup-content { margin:0 !important; width:auto !important; }',
  '  .viol-popup .leaflet-popup-tip-container { display:none !important; }',
  '  .resp-popup .leaflet-popup-content-wrapper { border-radius:8px !important; padding:0 !important; overflow:hidden; box-shadow:0 6px 24px rgba(0,0,0,.18) !important; }',
  '  .resp-popup .leaflet-popup-content { margin:0 !important; width:auto !important; }',
  '  .resp-popup .leaflet-popup-tip-container { display:none !important; }',
  '</style>',
  
  # ── ONE ctrl-panel div containing ALL sections ──────────────────────────────
  '<div id="ctrl-panel">',
  
  '  <h3>Water System Explorer</h3>',
  
  # Instructions (starts open)
  '  <div style="margin-bottom:10px;border-bottom:1px solid #eee;padding-bottom:8px;">',
  '    <div id="instr-header" onclick="toggleInstructions()">',
  '      <span style="font-size:11px;font-weight:700;color:#2c3e50;text-transform:uppercase;letter-spacing:.4px;">How to Use</span>',
  '      <span id="instr-arrow" style="font-size:10px;color:#aaa;">&#9650;</span>',
  '    </div>',
  '    <div id="instr-body" style="padding-top:4px;">',
  '      <ul style="margin:0;padding-left:16px;font-size:11px;color:#555;line-height:1.9;">',
  '        <li>Search a respondent zipcode to zoom to their water system and highlight it in orange.</li>',
  '        <li>Click any highlighted polygon to see a violation chart and system details.</li>',
  '        <li>Click a blue dot or numbered cluster to see that respondent\'s survey responses.</li>',
  '        <li>Use <b>Color Polygons By</b> to switch between violation views.</li>',
  '        <li>Use <b>Filter By</b> to narrow polygons by region, size, or violation type.</li>',
  '        <li>Toggle layers on and off using the control in the top-right corner.</li>',
  '      </ul>',
  '    </div>',
  '  </div>',
  
  # Zipcode search
  '  <span class="ctrl-label">Search by Zipcode</span>',
  '  <div id="zip-wrap">',
  '    <input id="zip-input" type="text" maxlength="5" placeholder="e.g. 74103" autocomplete="off" inputmode="numeric">',
  '    <div id="zip-autocomplete"></div>',
  '  </div>',
  '  <button class="ctrl-btn-primary" onclick="doZipSearch()">Find Water System(s)</button>',
  '  <button class="ctrl-btn-secondary" onclick="doReset()">Reset View</button>',
  '  <div id="zip-result"></div>',
  
  '  <hr class="ctrl-divider">',
  
  # Color toggle
  '  <span class="ctrl-label">Color Polygons By</span>',
  '  <div id="color-toggle">',
  '    <div class="col-btn active" onclick="setColor(this,\'total\')">Total<br>Violations</div>',
  '    <div class="col-btn" onclick="setColor(this,\'percap\')">Per Capita<br>(per 1k)</div>',
  '    <div class="col-btn" onclick="setColor(this,\'recent\')">Most Recent<br>Year</div>',
  '    <div class="col-btn" onclick="setColor(this,\'type\')">Dominant<br>Viol. Type</div>',
  '  </div>',
  
  '  <hr class="ctrl-divider">',
  
  # Filter dropdowns
  '  <span class="ctrl-label">Filter By</span>',
  '  <span class="ctrl-label" style="margin-top:6px;">Region</span>',
  '  <select class="filter-select" id="filter-region" onchange="applyFilters()"></select>',
  '  <span class="ctrl-label" style="margin-top:6px;">System Size</span>',
  '  <select class="filter-select" id="filter-size" onchange="applyFilters()"></select>',
  '  <span class="ctrl-label" style="margin-top:6px;">Dominant Violation Type</span>',
  '  <select class="filter-select" id="filter-type" onchange="applyFilters()"></select>',
  '  <div id="filter-status"></div>',
  '  <button class="ctrl-btn-secondary" onclick="clearFilters()" style="margin-top:6px;">Clear Filters</button>',
  
  '</div>',  # end ctrl-panel
  
  # Footnote bar (outside ctrl-panel, anchored to bottom of map)
  '<div id="footnote-bar">',
  '  <span><b>Violation data:</b> <a href="https://echo.epa.gov/trends/comparative-maps-dashboards/drinking-water-dashboard" target="_blank">EPA ECHO Drinking Water Dashboard</a></span>',
  '  <span style="color:#dde2ea;">|</span>',
  '  <span><b>Survey data:</b> <a href="https://dataverse.harvard.edu/dataverse/msisnet" target="_blank">MSISNET Dataverse, Harvard Dataverse</a></span>',
  '  <span style="color:#dde2ea;">|</span>',
  '  <span>Violations 2015\u20132025 &middot; Oklahoma community water systems</span>',
  '</div>',
  
  # ── ONE <script> block with ALL functions ───────────────────────────────────
  '<script>',
  
  # R data injected as JS variables
  paste0('var ZIP_MAP       = ', zip_js_object, ';'),
  paste0('var ZIP_LABELS    = ', zip_labels_js, ';'),
  paste0('var COLOR_DATA    = { total:', total_js, ', percap:', percap_js, ', recent:', recent_js, ', type:', domtype_js, ' };'),
  paste0('var LABELS        = { total:', labels_total_js, ', percap:', labels_percap_js, ', recent:', labels_recent_js, ' };'),
  paste0('var FILTER_DATA   = { region:', filter_region_js, ', size:', filter_size_js, ', domtype:', filter_domtype_js, ' };'),
  paste0('var REGION_OPTS   = ', region_opts_js,  ';'),
  paste0('var SIZE_OPTS     = ', size_opts_js,    ';'),
  paste0('var DOMTYPE_OPTS  = ', domtype_opts_js, ';'),
  
  # Pure JS state variables
  'var PALETTES = {',
  '  total:  ["#2ecc71","#f1c40f","#e67e22","#e74c3c","#7b241c"],',
  '  percap: ["#2ecc71","#f1c40f","#e67e22","#e74c3c","#7b241c"],',
  '  recent: ["#2ecc71","#f1c40f","#e67e22","#e74c3c","#7b241c"]',
  '};',
  'var BREAKS = { total:[0,10,30,75,Infinity], percap:[0,5,25,100,Infinity], recent:[0,2,5,15,Infinity] };',
  'var TYPE_COLORS = { "Acute Health-Based":"#c0392b","Health-Based":"#e67e22","Monitoring & Reporting":"#2980b9","Public Notification & Other":"#7f8c8d" };',
  'var LEGEND_TITLES = { total:"Total Violations (2015\u20132025)", percap:"Violations per 1,000 Served", recent:"Most Recent Year Violations", type:"Dominant Violation Type" };',
  'var _hl = [];',
  'var _colorMode = "total";',
  'var _activeFilters = { region:"All Regions", size:"All Sizes", domtype:"All Types" };',
  
  # Populate dropdowns once on load
  'function populateDropdowns() {',
  '  function fill(id, opts) {',
  '    var sel = document.getElementById(id);',
  '    opts.forEach(function(o) {',
  '      var el = document.createElement("option");',
  '      el.value = o; el.textContent = o;',
  '      sel.appendChild(el);',
  '    });',
  '  }',
  '  fill("filter-region",  REGION_OPTS);',
  '  fill("filter-size",    SIZE_OPTS);',
  '  fill("filter-type",    DOMTYPE_OPTS);',
  '}',
  
  # Instructions toggle
  'function toggleInstructions() {',
  '  var body = document.getElementById("instr-body");',
  '  var arrow = document.getElementById("instr-arrow");',
  '  if (body.style.display === "none") { body.style.display = "block"; arrow.innerHTML = "&#9650;"; }',
  '  else { body.style.display = "none"; arrow.innerHTML = "&#9660;"; }',
  '}',
  
  # Legend
  'function renderLegend(mode) {',
  '  var box = document.getElementById("map-legend"); if (!box) return;',
  '  var html = "<b style=\'font-size:12px;color:#1a1a2e;display:block;margin-bottom:8px;line-height:1.4;\'>" + LEGEND_TITLES[mode] + "</b>";',
  '  if (mode === "type") {',
  '    Object.keys(TYPE_COLORS).forEach(function(t) {',
  '      html += "<div style=\'display:flex;align-items:center;margin-bottom:5px;\'>";',
  '      html += "<div style=\'width:16px;height:16px;border-radius:3px;margin-right:8px;flex-shrink:0;border:1px solid rgba(0,0,0,.1);background:" + TYPE_COLORS[t] + ";\'></div>";',
  '      html += "<span style=\'font-size:11px;color:#333;\'>" + t + "</span></div>";',
  '    });',
  '  } else {',
  '    var pal = PALETTES[mode], labels = LABELS[mode];',
  '    for (var i = 0; i < pal.length; i++) {',
  '      html += "<div style=\'display:flex;align-items:center;margin-bottom:5px;\'>";',
  '      html += "<div style=\'width:16px;height:16px;border-radius:3px;margin-right:8px;flex-shrink:0;border:1px solid rgba(0,0,0,.1);background:" + pal[i] + ";\'></div>";',
  '      html += "<span style=\'font-size:11px;color:#333;\'>" + labels[i] + "</span></div>";',
  '    }',
  '  }',
  '  box.innerHTML = html;',
  '}',
  
  # Threshold color
  'function thresholdColor(mode, val) {',
  '  if (val === null || isNaN(val)) return "#bdc3c7";',
  '  var breaks = BREAKS[mode], pal = PALETTES[mode];',
  '  for (var i = breaks.length - 2; i >= 0; i--) { if (val > breaks[i]) return pal[i+1]; }',
  '  return pal[0];',
  '}',
  
  # Color toggle — respects active filter (greyed layers stay grey)
  'function setColor(btn, mode) {',
  '  _colorMode = mode;',
  '  document.querySelectorAll(".col-btn").forEach(function(b) { b.classList.remove("active"); });',
  '  btn.classList.add("active");',
  '  var lmap = window.HTMLWidgets.find(".leaflet").getMap(); var i = 0;',
  '  lmap.eachLayer(function(layer) {',
  '    if (!layer.options || !layer.options.layerId) return;',
  '    var isGreyed = (layer.options.fillOpacity < 0.1);',
  '    if (!isGreyed) {',
  '      var val = COLOR_DATA[mode][i];',
  '      var col = (mode === "type") ? (TYPE_COLORS[val] || "#bdc3c7") : thresholdColor(mode, val);',
  '      layer.setStyle({ fillColor: col });',
  '    }',
  '    i++;',
  '  });',
  '  renderLegend(mode);',
  '}',
  
  # Apply filters
  'function applyFilters() {',
  '  _activeFilters.region  = document.getElementById("filter-region").value;',
  '  _activeFilters.size    = document.getElementById("filter-size").value;',
  '  _activeFilters.domtype = document.getElementById("filter-type").value;',
  '  var lmap = window.HTMLWidgets.find(".leaflet").getMap(); var i = 0; var matchCount = 0;',
  '  lmap.eachLayer(function(layer) {',
  '    if (!layer.options || !layer.options.layerId) return;',
  '    var regionMatch  = (_activeFilters.region  === "All Regions") || (FILTER_DATA.region[i]  === _activeFilters.region);',
  '    var sizeMatch    = (_activeFilters.size    === "All Sizes")   || (FILTER_DATA.size[i]    === _activeFilters.size);',
  '    var domtypeMatch = (_activeFilters.domtype === "All Types")   || (FILTER_DATA.domtype[i] === _activeFilters.domtype);',
  '    if (regionMatch && sizeMatch && domtypeMatch) {',
  '      var val = COLOR_DATA[_colorMode][i];',
  '      var col = (_colorMode === "type") ? (TYPE_COLORS[val] || "#bdc3c7") : thresholdColor(_colorMode, val);',
  '      layer.setStyle({ fillColor: col, fillOpacity: 0.55, color: "#2c3e50", weight: 1.2, opacity: 0.8 });',
  '      matchCount++;',
  '    } else {',
  '      layer.setStyle({ fillColor: "#cccccc", fillOpacity: 0.08, color: "#cccccc", weight: 0.5, opacity: 0.3 });',
  '    }',
  '    i++;',
  '  });',
  '  var allDefault = (_activeFilters.region === "All Regions" && _activeFilters.size === "All Sizes" && _activeFilters.domtype === "All Types");',
  '  document.getElementById("filter-status").innerHTML = allDefault ? "" :',
  '    "<span style=\'color:#2980b9;font-weight:600;\'>" + matchCount + " system" + (matchCount !== 1 ? "s" : "") + " match active filters</span>";',
  '}',
  
  # Clear filters
  'function clearFilters() {',
  '  document.getElementById("filter-region").value  = "All Regions";',
  '  document.getElementById("filter-size").value    = "All Sizes";',
  '  document.getElementById("filter-type").value    = "All Types";',
  '  _activeFilters = { region:"All Regions", size:"All Sizes", domtype:"All Types" };',
  '  document.getElementById("filter-status").innerHTML = "";',
  '  var lmap = window.HTMLWidgets.find(".leaflet").getMap(); var i = 0;',
  '  lmap.eachLayer(function(layer) {',
  '    if (!layer.options || !layer.options.layerId) return;',
  '    var val = COLOR_DATA[_colorMode][i];',
  '    var col = (_colorMode === "type") ? (TYPE_COLORS[val] || "#bdc3c7") : thresholdColor(_colorMode, val);',
  '    layer.setStyle({ fillColor: col, fillOpacity: 0.55, color: "#2c3e50", weight: 1.2, opacity: 0.8 });',
  '    i++;',
  '  });',
  '}',
  
  # Autocomplete
  'var acBox = document.getElementById("zip-autocomplete");',
  'function showAutocomplete(val) {',
  '  if (!val || val.length < 2) { acBox.style.display = "none"; return; }',
  '  var matches = ZIP_LABELS.filter(function(z) { return z.startsWith(val); }).slice(0,8);',
  '  if (!matches.length) { acBox.style.display = "none"; return; }',
  '  acBox.innerHTML = matches.map(function(z) { return "<div class=\'zip-ac-item\' onclick=\'selectZip(\\\""+z+"\\\")\'>"+z+"</div>"; }).join("");',
  '  acBox.style.display = "block";',
  '}',
  'function selectZip(zip) { document.getElementById("zip-input").value = zip; acBox.style.display = "none"; doZipSearch(); }',
  
  # DOMContentLoaded — populate dropdowns and render legend
  'document.addEventListener("DOMContentLoaded", function() {',
  '  populateDropdowns();',
  '  renderLegend("total");',
  '  var inp = document.getElementById("zip-input"); if (!inp) return;',
  '  inp.addEventListener("input",    function() { showAutocomplete(this.value); });',
  '  inp.addEventListener("keypress", function(e) { if (e.key==="Enter") { acBox.style.display="none"; doZipSearch(); } });',
  '  inp.addEventListener("blur",     function() { setTimeout(function() { acBox.style.display="none"; }, 150); });',
  '});',
  
  # Search
  'function _resetHighlights() {',
  '  _hl.forEach(function(l) { l.setStyle({ color:"#2c3e50", weight:1.2, fillOpacity:0.55 }); });',
  '  _hl = [];',
  '}',
  'function doZipSearch() {',
  '  var raw = document.getElementById("zip-input").value.trim();',
  '  var zip = raw.replace(/\\D/g,"").slice(-5).padStart(5,"0");',
  '  var out = document.getElementById("zip-result");',
  '  _resetHighlights();',
  '  if (!zip||zip==="00000") { out.innerHTML="<span style=\'color:#c0392b\'>Please enter a valid 5-digit zipcode.</span>"; return; }',
  '  var systems = ZIP_MAP[zip];',
  '  if (!systems||!systems.length) { out.innerHTML="<span style=\'color:#c0392b\'>No systems found for ZIP <b>"+zip+"</b>.</span>"; return; }',
  '  var bounds=[], lmap=window.HTMLWidgets.find(".leaflet").getMap();',
  '  lmap.eachLayer(function(layer) {',
  '    if (!layer.options||!layer.options.layerId) return;',
  '    if (systems.indexOf(String(layer.options.layerId).toUpperCase())===-1) return;',
  '    layer.setStyle({color:"#f39c12",weight:3.5,fillOpacity:0.78}); layer.bringToFront(); _hl.push(layer);',
  '    try { bounds.push(layer.getBounds()); } catch(e) {}',
  '  });',
  '  if (!bounds.length) { out.innerHTML="<span style=\'color:#e67e22\'>Found in data but not rendered \u2014 zoom in and try again.</span>"; return; }',
  '  var combined=bounds[0]; bounds.forEach(function(b){combined.extend(b);});',
  '  lmap.flyToBounds(combined,{padding:[50,50],maxZoom:13,duration:1.1});',
  '  out.innerHTML="<b style=\'color:#27ae60\'>"+systems.length+" system"+(systems.length>1?"s":"")+" found</b> for ZIP <b>"+zip+"</b><br><span style=\'color:#888;font-size:11px\'>Hover to preview \u2014 click for details.</span>";',
  '}',
  
  # Reset — clears search AND filters
  'function doReset() {',
  '  _resetHighlights();',
  '  document.getElementById("zip-input").value="";',
  '  document.getElementById("zip-result").innerHTML="";',
  '  clearFilters();',
  '  var lmap=window.HTMLWidgets.find(".leaflet").getMap();',
  '  lmap.flyTo([35.5,-97.5],7,{duration:1.0}); lmap.closePopup();',
  '}',
  
  '</script>'   # ONE closing script tag
  
), control_html_file)

control_ui <- HTML(paste(readLines(control_html_file), collapse = "\n"))
map <- map |> htmlwidgets::appendContent(control_ui)


# ── 11.  Save ─────────────────────────────────────────────────────────────────

message(sprintf('\nSaving to "%s" ...', OUT_HTML))
saveWidget(map, file = OUT_HTML, selfcontained = TRUE,
           title = "MSISNET Water System Violation Map")
message(sprintf('Done! Open "%s" in any browser.', OUT_HTML))
