mstate_mssample_wrapper <- function(id, Haz, tmat) { #nolint
  m <- length(id)
  res <- mstate::mssample(Haz, tmat, M = m, output = "data",
    clock = "reset", tvec = NULL)
  class(res) <- "data.frame"
  res$id <- factor(res$id, levels = 1:m, labels = id)
  return(tibble::as_tibble(res))
}

#' @title Transition probability for approximate likelihood inference
#'
#' @template from
#' @param to character, target state
#' @param probability numeric vector of transition probability
#' @param standard_error numeric vector of standard errors for transition
#'   probabilities
#'
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

#' @title Survival curve for approximate likelihood inference
#'
#' @template from
#' @template to
#' @param time numeric vector of times
#' @param survival numeric vector of survival proportions at `time`
#' @param standard_error numeric vector of standard errors for survival
#'   proportions
#'
#' @export
calibration_survival_curve <- function(
    from, to, time, survival, standard_error
) {
  structure(tibble::tibble(
      time = time,
      survival = survival,
      standard_error = standard_error
    ),
    class = c("CalibrationSurvivalCurve", class(tibble::tibble())),
    from = from,
    to = to
  )
}
