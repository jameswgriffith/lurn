summary_stats <- function(x) {
  c(
    "Mean" = mean(x$lurn_si_10_score, na.rm = TRUE),
    "SD" = stats::sd(x$lurn_si_10_score, na.rm = TRUE),
    "Median" = stats::median(x$lurn_si_10_score, na.rm = TRUE),
    "Minimum" = min(x$lurn_si_10_score, na.rm = TRUE),
    "Maximum" = max(x$lurn_si_10_score, na.rm = TRUE),
    "N (non-missing)" = sum(!is.na(x$lurn_si_10_score), na.rm = TRUE),
    "Number missing" = sum(is.na(x$lurn_si_10_score), na.rm = TRUE),
    "Proportion missing" = sum(is.na(x$lurn_si_10_score), na.rm = TRUE) /
      length(x$lurn_si_10_score),
    "Min. number of valid responses" = min(x$lurn_si_10_count_valid, na.rm = TRUE),
    "Max. number of valid responses" = max(x$lurn_si_10_count_valid, na.rm = TRUE),
    "Mean number of valid responses" = mean(x$lurn_si_10_count_valid, na.rm = TRUE),
    "Median number of valid responses" = stats::median(x$lurn_si_10_count_valid, na.rm = TRUE)
  )
}
