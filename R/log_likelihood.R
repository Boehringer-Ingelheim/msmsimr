#' @title Sample-based log-likelihood for summary data
#'
#' @description ...
#'
#' @template msm
#' @template tmax
#' @template grid_size
#' @template nsim
#' @template hazard_ratios
#' @template seed
#' @param ... a list of calibration objects TODO: explain
#'
#' @export
log_likelihood <- function(
  msm,
  ...,
  nsim = attr(msm, "m"),
  tmax = get_tmax(msm),
  grid_size = attr(msm, "grid_size"),
  hazard_ratios = rep(1, length(msm)),
  seed = NULL
) {
  probs <- list()
  curves <- list()
  args <- list(...)
  # sample from model
  data <- simulate(msm, nsim = nsim, seed = seed, tmax = tmax,
    grid_size = grid_size, hazard_ratios = hazard_ratios)
  for (i in seq_along(args)) {
    if (inherits(args[[i]], "CalibrationProbability")) {
      probs <- c(probs, args[i])
    }
    if (inherits(args[[i]], "CalibrationSurvivalCurve")) {
      curves <- c(curves, args[i])
    }
  }
  if (length(probs) > 0) { # compare probabilities
    tbl_probs <- next_state_probabilities(msm, data) %>%
      dplyr::left_join(
        purrr::map_df(probs, tibble::as_tibble),
        by = c("from", "to")
      ) %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      dplyr::mutate(
        z = (.data$probability.x - .data$probability.y) / .data$standard_error
      )
  }
  if (length(curves) > 0) { # compare survival curves
    tbl_curves <- tibble::tibble()
    for (i in seq_along(curves)) {
      curve <- curves[[i]]
      from <- attr(curve, "from")
      to <- attr(curve, "to")
      tbl_curves <- dplyr::bind_rows(
        tbl_curves,
        tibble::tibble(
          to = paste(to, collapse = ", "),
          survfit = survival(msm, curve$time, from = from, to = to,
            data = data),
          surv = curve$survival,
          se = curve$standard_error,
          z = (.data$survfit - .data$surv) / .data$se,
          weight = 1 / length(.data$z)
        )
      )
    }
  }
  loglikelihood <- sum(stats::dt(tbl_probs$z, df = 1, log = TRUE)) +
    sum(stats::dt(tbl_curves$z, df = 1, log = TRUE) * tbl_curves$weight)
  return(loglikelihood)
}
