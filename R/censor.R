#' @title Censor multi-state data set
#'
#' @description Restrict multi-state data set to given time point.
#'
#' @template data
#' @param t positive numerical, global maximal censoring time point
#'
#' @export
censor <- function(data, t = Inf) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  data %>%
    dplyr::filter(.data$Tstart <= t) %>%
    dplyr::mutate(
      status = dplyr::if_else(.data$Tstop > t, 0, .data$status),
      Tstop = pmin(.data$t, .data$Tstop),
      duration = .data$Tstop - .data$Tstart,
      to = dplyr::if_else(.data$status == 0, NA, .data$to),
      trans = dplyr::if_else(.data$status == 0, NA, .data$trans)
    )
}
