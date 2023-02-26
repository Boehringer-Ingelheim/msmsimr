mstate_mssample_wrapper <- function(id, Haz, tmat) {
  m <- length(id)
  res <- mstate::mssample(Haz, tmat, M = m, output = "data",
    clock = "reset", tvec = NULL)
  class(res) <- "data.frame"
  res$id <- factor(res$id, levels = 1:m, labels = id)
  return(tibble::as_tibble(res))
}

#' @export
calibration_probability <- function(from, to, probability, standard_error) {
  structure(list(
      from = from,
      to = to,
      probability = probability,
      standard_error = standard_error
    ),
    class = c("CalibrationProbability", "list")
  )
}

#' @export
calibration_survival_curve <- function(from, events, time, survival, standard_error) {
  structure(tibble::tibble(
      time = time,
      survival = survival,
      standard_error = standard_error
    ),
    class = c("CalibrationSurvivalCurve", class(tibble::tibble())),
    from = from,
    events = events
  )
}
