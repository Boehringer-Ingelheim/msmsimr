#' @export
kaplan_meier <- function(data, starting_state, events, tidy = TRUE) {
  f <- function(to, Tstart, Tstop, duration, status) {
    idx <- which(to %in% events & status == 1)
    if (length(idx) == 0) {
      idx <- length(to)
      status <- 0
    } else {
      idx <- idx[1]
      status <- 1
    }
    return(tibble::tibble(
      Tstart = min(Tstart),
      Tstop = Tstop[idx],
      duration = sum(duration[1:idx]),
      status = status
    ))
  }
  km_data <- data %>%
    dplyr::filter(as.integer(.data$from) >= which(get_state_labels(msm) == starting_state)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(
      res = f(to, Tstart, Tstop, duration, status)
    ) %>%
    tidyr::unnest(res)
  res <- survival::survfit(survival::Surv(duration, status) ~ 1, data = km_data)
  if (tidy)
    res <- broom::tidy(res)
  return(res)
}
