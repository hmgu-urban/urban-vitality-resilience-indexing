
# =============================================================================
# Urban Vitality Resilience Indexing (UVRI) method
# Demonstration + validation script for a MethodsX method article.
#
# Calculates resistance (RSTN), recovery capacity (RCVY), and adaptability (ADPT)
# from longitudinal activity data, and reproduces the validation tables
# (Tables 7-10) reported in the method article.
#
# Required input columns:
#   unit_id  : spatial unit identifier
#   month    : monthly time stamp, either "YYYY-MM" or numeric YYYYMM
#   vitality : activity level or activity density
#
# --- Changes vs. the previous package version --------------------------------
# 1. DATE-BASED TIME INDEX (gap-safe). The integer time index t is now derived
#    from the calendar month itself (months since the panel start), NOT from
#    row position. base_lag and the (t_{k+1}-t_k) slope spans are therefore
#    correct even if a unit has missing months. On a complete monthly panel
#    the results are identical to the row-based version.
# 2. mean_stat ARGUMENT added ("mean" or "median") so the median variant used
#    in the adaptation-window sensitivity table can be produced by the function
#    itself. value_col now defaults to "vitality" to match the shipped data and
#    the article's column naming (use one consistent name in Table 1 / text).
# 3. VALIDATION REPRODUCTION. run_full_validation() regenerates Table 7
#    (descriptives), Table 8 (correlation vs. simple pre-post change), Table 9
#    (adaptation-window sensitivity), and Table 10 (base-lag sensitivity).
#
# Note: the RSTN/RCVY/ADPT indicators are invariant to positive, unit-specific
# scaling, so dividing each unit's counts by its area (to obtain density) does
# not change the indicators. Density is still preferable for plotting absolute
# levels or comparing raw magnitudes across units.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

normalize_month <- function(x) {
  x <- as.character(x)
  x <- ifelse(grepl("^\\d{6}$", x),
              paste0(substr(x, 1, 4), "-", substr(x, 5, 6)),
              x)
  x <- substr(x, 1, 7)
  as.Date(paste0(x, "-01"))
}

# Calendar-month ordinal of a Date (first-of-month). Used to build a
# date-derived, gap-safe integer time index. Consecutive months differ by 1.
month_ordinal <- function(d) {
  as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
}

# Return "YYYY-MM" shifted n months from a Date d (n may be negative).
add_months_str <- function(d, n) {
  o <- as.integer(format(d, "%Y")) * 12L + (as.integer(format(d, "%m")) - 1L) + n
  sprintf("%04d-%02d", o %/% 12L, o %% 12L + 1L)
}

# Bounded actual-to-expected ratio: ac / (|ac| + |ec|); zero when denominator 0.
safe_ratio <- function(ac, ec) {
  den <- abs(ac) + abs(ec)
  ifelse(den == 0 | is.na(den), 0, ac / den)
}

# Correlation that ignores non-finite pairs.
safe_cor <- function(x, y, method) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  stats::cor(x[ok], y[ok], method = method)
}

recode_seoul_admin_codes <- function(data, id_col = "adm_cd") {
  # Administrative-code harmonization used in the Seoul application.
  # Replace with a study-specific table when applying to other cities/units.
  data %>%
    mutate(
      "{id_col}" := case_when(
        .data[[id_col]] == 11680740 ~ 11680675,
        .data[[id_col]] == 11305590 ~ 11305595,
        .data[[id_col]] == 11305600 ~ 11305603,
        .data[[id_col]] == 11305606 ~ 11305608,
        .data[[id_col]] == 11305610 ~ 11305615,
        .data[[id_col]] == 11305620 ~ 11305625,
        .data[[id_col]] == 11305630 ~ 11305635,
        TRUE ~ .data[[id_col]]
      )
    )
}

# ---------------------------------------------------------------------------
# Core method
# ---------------------------------------------------------------------------

index_vitality_resilience <- function(
    data,
    id_col         = "unit_id",
    time_col       = "month",
    value_col      = "vitality",
    peak_window    = c("2019-06", "2020-02"),
    minimum_window = c("2020-03", "2022-03"),
    maximum_window = c("2022-04", "2023-04"),
    mean_window    = c("2023-05", "2024-02"),
    base_lag       = 12,
    mean_stat      = c("mean", "median")
) {
  stopifnot(all(c(id_col, time_col, value_col) %in% names(data)))
  mean_stat <- match.arg(mean_stat)
  stat_fun  <- if (mean_stat == "median") stats::median else base::mean

  peak_window    <- normalize_month(peak_window)
  minimum_window <- normalize_month(minimum_window)
  maximum_window <- normalize_month(maximum_window)
  mean_window    <- normalize_month(mean_window)

  d <- data %>%
    transmute(
      unit_id__  = .data[[id_col]],
      month__    = normalize_month(.data[[time_col]]),
      vitality__ = as.numeric(.data[[value_col]])
    ) %>%
    arrange(unit_id__, month__)

  # Date-derived, gap-safe integer time index (months since panel start).
  anchor__ <- min(month_ordinal(d$month__), na.rm = TRUE)
  d <- d %>% mutate(t__ = month_ordinal(month__) - anchor__ + 1L)

  na_row <- function(t0 = NA_integer_, t1 = NA_integer_, t2 = NA_integer_,
                     t3 = NA_integer_, t4 = NA_integer_,
                     y0 = NA_real_, y1 = NA_real_, y2 = NA_real_,
                     y3 = NA_real_, y4 = NA_real_) {
    tibble(t0 = t0, t1 = t1, t2 = t2, t3 = t3, t4 = t4,
           y0 = y0, y1 = y1, y2 = y2, y3 = y3, y4 = y4,
           r = NA_real_, y2hat = NA_real_, y3hat = NA_real_, y4hat = NA_real_,
           RSTN = NA_real_, RCVY = NA_real_, ADPT = NA_real_)
  }

  out <- d %>%
    group_by(unit_id__) %>%
    group_modify(~{
      g <- .x %>% arrange(month__)

      peak <- g %>%
        filter(month__ >= peak_window[1], month__ <= peak_window[2]) %>%
        arrange(desc(vitality__), month__) %>% slice(1)

      local_min <- g %>%
        filter(month__ >= minimum_window[1], month__ <= minimum_window[2]) %>%
        arrange(vitality__, month__) %>% slice(1)

      local_max <- g %>%
        filter(month__ >= maximum_window[1], month__ <= maximum_window[2]) %>%
        arrange(desc(vitality__), month__) %>% slice(1)

      local_mean_window <- g %>%
        filter(month__ >= mean_window[1], month__ <= mean_window[2])

      if (nrow(peak) == 0 || nrow(local_min) == 0 ||
          nrow(local_max) == 0 || nrow(local_mean_window) == 0) {
        return(na_row())
      }

      t1 <- peak$t__[1];  t2 <- local_min$t__[1]
      t3 <- local_max$t__[1]; t4 <- max(local_mean_window$t__)
      y1 <- peak$vitality__[1]; y2 <- local_min$vitality__[1]
      y3 <- local_max$vitality__[1]
      y4 <- stat_fun(local_mean_window$vitality__, na.rm = TRUE)

      # Base = same seasonal point base_lag months before the peak (date-anchored).
      t0   <- t1 - base_lag
      base <- g %>% filter(t__ == t0)
      if (nrow(base) == 0) {
        return(na_row(t0, t1, t2, t3, t4, NA_real_, y1, y2, y3, y4))
      }

      y0 <- base$vitality__[1]
      r  <- (y1 - y0) / (t1 - t0)

      y2hat <- y1 + r * (t2 - t1)
      y3hat <- y2 + r * (t3 - t2)
      y4hat <- y3 + r * (t4 - t3)

      ec1 <- y2hat - y1; ec2 <- y3hat - y2; ec3 <- y4hat - y3
      ac1 <- y2 - y2hat; ac2 <- y3 - y3hat; ac3 <- y4 - y4hat

      tibble(
        t0 = t0, t1 = t1, t2 = t2, t3 = t3, t4 = t4,
        y0 = y0, y1 = y1, y2 = y2, y3 = y3, y4 = y4,
        r = r, y2hat = y2hat, y3hat = y3hat, y4hat = y4hat,
        RSTN = safe_ratio(ac1, ec1),
        RCVY = safe_ratio(ac2, ec2),
        ADPT = safe_ratio(ac3, ec3)
      )
    }) %>%
    ungroup() %>%
    rename("{id_col}" := unit_id__)

  out
}

# ---------------------------------------------------------------------------
# Validation reproduction (Tables 7-10)
# ---------------------------------------------------------------------------

# Table 7: descriptive statistics of RSTN, RCVY, ADPT.
summarize_resilience <- function(result) {
  result %>%
    summarise(across(
      c(RSTN, RCVY, ADPT),
      list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm = TRUE),
           sd = ~sd(.x, na.rm = TRUE), min = ~min(.x, na.rm = TRUE),
           max = ~max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}")) %>%
    tidyr::pivot_longer(everything(),
      names_to = c("indicator", ".value"),
      names_pattern = "(RSTN|RCVY|ADPT)_(.*)")
}

# Table 8: correlation between proposed indicators and simple pre-post change.
validate_table8 <- function(result) {
  d <- result %>%
    mutate(simple_resistance = (y2 - y1) / y1,
           simple_recovery   = (y3 - y2) / y2,
           simple_adaptation = (y4 - y3) / y3)
  tibble(
    proposed_indicator = c("RSTN", "RCVY", "ADPT"),
    simple_indicator   = c("Simple resistance", "Simple recovery", "Simple adaptation"),
    pearson  = c(safe_cor(d$RSTN, d$simple_resistance, "pearson"),
                 safe_cor(d$RCVY, d$simple_recovery,   "pearson"),
                 safe_cor(d$ADPT, d$simple_adaptation, "pearson")),
    spearman = c(safe_cor(d$RSTN, d$simple_resistance, "spearman"),
                 safe_cor(d$RCVY, d$simple_recovery,   "spearman"),
                 safe_cor(d$ADPT, d$simple_adaptation, "spearman"))
  )
}

# Compare one indicator from an alternative specification against the default.
cmp_indicator <- function(alt_result, default_result, id_col, ind, label) {
  m <- merge(
    data.frame(.id = alt_result[[id_col]],     alt = alt_result[[ind]]),
    data.frame(.id = default_result[[id_col]], def = default_result[[ind]]),
    by = ".id")
  tibble(
    alternative = label, indicator = ind,
    pearson  = safe_cor(m$alt, m$def, "pearson"),
    spearman = safe_cor(m$alt, m$def, "spearman"),
    alt_mean = mean(alt_result[[ind]], na.rm = TRUE)
  )
}

# Tables 7-10 in one call.
run_full_validation <- function(
    data, id_col = "unit_id", time_col = "month", value_col = "vitality",
    peak_window    = c("2019-06", "2020-02"),
    minimum_window = c("2020-03", "2022-03"),
    maximum_window = c("2022-04", "2023-04"),
    mean_window    = c("2023-05", "2024-02"),
    base_lag       = 12) {

  run <- function(mw = mean_window, lag = base_lag, stat = "mean") {
    index_vitality_resilience(data, id_col, time_col, value_col,
      peak_window, minimum_window, maximum_window, mw, lag, stat)
  }

  default_result <- run()

  end_date <- normalize_month(mean_window[2])
  win8 <- c(add_months_str(end_date, -7), mean_window[2])  # 8-month local mean
  win6 <- c(add_months_str(end_date, -5), mean_window[2])  # 6-month local mean

  r_adpt8  <- run(mw = win8)
  r_adpt6  <- run(mw = win6)
  r_median <- run(stat = "median")
  r_lag6   <- run(lag = 6)

  table7  <- summarize_resilience(default_result)
  table8  <- validate_table8(default_result)
  table9  <- bind_rows(
    cmp_indicator(r_adpt8,  default_result, id_col, "ADPT", "8-month local mean"),
    cmp_indicator(r_adpt6,  default_result, id_col, "ADPT", "6-month local mean"),
    cmp_indicator(r_median, default_result, id_col, "ADPT", "Local median instead of mean"))
  table10 <- bind_rows(
    cmp_indicator(r_lag6, default_result, id_col, "RSTN", "6-month base lag"),
    cmp_indicator(r_lag6, default_result, id_col, "RCVY", "6-month base lag"),
    cmp_indicator(r_lag6, default_result, id_col, "ADPT", "6-month base lag"))

  list(result = default_result,
       table7 = table7, table8 = table8, table9 = table9, table10 = table10)
}

# ===========================================================================
# Example 1. Synthetic data supplied with the package.
# ===========================================================================
if (file.exists("synthetic_vitality_monthly.csv")) {
  synthetic <- read_csv("synthetic_vitality_monthly.csv", show_col_types = FALSE)

  v <- run_full_validation(synthetic, id_col = "unit_id",
                           time_col = "month", value_col = "vitality")

  write_csv(v$result,  "synthetic_resilience_outputs_from_R.csv")
  write_csv(v$table7,  "synthetic_table7_descriptives_from_R.csv")
  write_csv(v$table8,  "synthetic_table8_vs_simple_from_R.csv")
  write_csv(v$table9,  "synthetic_table9_adaptation_window_from_R.csv")
  write_csv(v$table10, "synthetic_table10_base_lag_from_R.csv")
}

# ===========================================================================
# Example 2. Seoul de facto population file (place pop.csv in this folder).
# ===========================================================================
if (file.exists("pop.csv")) {
  seoul_pop <- read_csv("pop.csv", show_col_types = FALSE) %>%
    rename(adm_cd = adm_cd, month = ym, vitality = pop) %>%
    recode_seoul_admin_codes(id_col = "adm_cd")

  v <- run_full_validation(seoul_pop, id_col = "adm_cd",
                           time_col = "month", value_col = "vitality")

  write_csv(v$result,  "actual_resilience_outputs_from_R.csv")
  write_csv(v$table7,  "actual_table7_descriptives_from_R.csv")   # -> -0.65 / 0.34 / -0.45
  write_csv(v$table8,  "actual_table8_vs_simple_from_R.csv")      # -> 0.361/0.346, 0.377/0.464, 0.436/0.496
  write_csv(v$table9,  "actual_table9_adaptation_window_from_R.csv")
  write_csv(v$table10, "actual_table10_base_lag_from_R.csv")
}
