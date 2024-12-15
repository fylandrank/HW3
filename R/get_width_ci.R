#' get_width_ci
#'
#' this function will give the width of the confidence interval for each year for a given country or iso code
#'
#' @param est tibble from est.csv
#' @param iso_code column title for country defining identifier code
#' @param coverage confidence interval to define width by
#'
#' @return tibble with column "year" and "width"
#'
get_width_ci <- function(est, iso_code, coverage) {
  est2 <- est %>%
    filter(iso == iso_code)
  if (coverage == 95) {
    low <- "L95"
    upp <- "U95"
  }
  else if (coverage == 80) {
    low <- "L80"
    upp <- "U80"
  }
  CI_width <- est2[[upp]] - est2[[low]]
  ci_width_tibble <- tibble(
    year = est2$Year,
    width = CI_width
  )
  print(ci_width_tibble)
}

