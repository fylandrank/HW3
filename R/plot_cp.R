#plot_cp function

#function for plot_cp
#' Documentation for plot_cp function
#'
#' @param dat tibble corresponding to contraceptive_use dataset, lists mCPR, key columns: iso, obs_mCPR, mar_status, year
#' @param est tibble corresponding to est dataset, lists mCPR, key columns: iso, year, L95, L80, U95, U80
#' @param iso_code corresponds to iso/country code to plot
#' @param CI corresponds to confidence interval to plot as a geom_smooth outline - values are either 80, 95, or NA
#'
#' @return ggplot graph that displays selected observations for a chosen country
#' @export
#'
#' @examples
#' (dat, est, 4, CI = "95")

plot_cp <- function(dat, est, iso_code, CI = "95") {
  dat <- cp_use %>%
    select(division_numeric_code, contraceptive_use_modern, is_in_union, start_date , end_date) %>%
    rename(iso = division_numeric_code, obs_mCPR = contraceptive_use_modern, mar_status = is_in_union) %>%
    mutate(year = (start_date + end_date)/2, cp = (obs_mCPR*100)) %>%
    filter(mar_status == "Y")

  est2 <- est %>%
    filter(iso == iso_code)

  dat2 <- dat %>%
    filter(iso == iso_code)
  est2 <- est %>%
    filter(iso == iso_code)
  if (is.na(CI)) {
    CI <- "none"
  }
  combo_plot <- ggplot(est2, aes(x = Year, y = Median)) +
    geom_line()
  if (CI == "95") {
    combo_plot <- combo_plot +
      geom_smooth(
        stat = "identity",
        aes(ymax = U95, ymin = L95)
      )}
  else if(CI == "80") {
    combo_plot <- combo_plot +
      geom_smooth(
        stat = "identity",
        aes(ymax = U80, ymin = L80)
      )}
  combo_plot <- combo_plot +
    geom_point(data = dat2, aes(year, cp)) +
    labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])
  return(combo_plot)
}

