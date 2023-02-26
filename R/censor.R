#' @export
censor <- function(data, t = Inf) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  data %>%
    dplyr::filter(Tstart <= t) %>%
    dplyr::mutate(
      status = dplyr::if_else(Tstop > t, 0, status), # censored?
      Tstop = pmin(t, Tstop),
      duration = Tstop - Tstart,
      to = dplyr::if_else(status == 0, NA, to),
      trans = dplyr::if_else(status == 0, NA, trans)
    )
}
