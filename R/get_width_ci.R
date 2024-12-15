get_width_ci <- function(est, iso_code, CI = "95") {
  est2 <- est %>%
    filter(iso == iso_code)
  if (is.na(CI)) {
    CI <- "none"
  }
  CI_def <- if (coverage == 95) {
    low <- "L95"
    upp <- "U95"
  }
  else if (coverage == 80) {
    low <- "L80"
    upp <- "U80"
  }
