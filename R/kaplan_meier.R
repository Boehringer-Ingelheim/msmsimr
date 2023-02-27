#' @title Kaplan-Meier estimator of time-to-first-event
#'
#' @description ...
#'
#' @template msm
#' @template from
#' @template to
#' @template data
#' @param tidy logical, if true data is returned as `[tibble::tibble]`
#'
#' @export
kaplan_meier <- function(msm, from, to, data = simulate(msm), tidy = TRUE) {
  f <- function(to_, Tstart, Tstop, duration, status) {
    idx <- which(to_ %in% to & status == 1)
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
    dplyr::filter(as.integer(.data$from) >= which(get_state_labels(msm) == !!from)) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarize(
      res = f(.data$to, .data$Tstart, .data$Tstop, .data$duration, .data$status)
    ) %>%
    tidyr::unnest(.data$res)
  res <- survival::survfit(survival::Surv(duration, status) ~ 1, data = km_data)
  if (tidy) {
    res <- broom::tidy(res)
  }
  return(res)
}
