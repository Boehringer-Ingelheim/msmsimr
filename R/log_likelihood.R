#'@export
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
      left_join(
        purrr::map_df(probs, tibble::as_tibble),
        by = c("from", "to")
      ) %>%
      filter(complete.cases(.)) %>%
      mutate(z = (probability.x - probability.y) / standard_error)
  }
  if (length(curves) > 0) { # compare survival curves
    tbl_curves <- tibble::tibble()
    for (i in seq_along(curves)) {
      curve <- curves[[i]]
      from <- attr(curve, "from")
      events <- attr(curve, "events")
      tbl_curves <- bind_rows(
        tbl_curves,
        tibble::tibble(
          events = paste(events, collapse = ", "),
          survfit = survival(data, curve$time, from = from, events = events),
          surv = curve$survival,
          se = curve$standard_error,
          z = (survfit - surv) / se,
          weight = 1 / length(z)
        )
      )
    }
  }
  loglikelihood <- sum(dt(tbl_probs$z, df = 1, log = TRUE)) +
    sum(dt(tbl_curves$z, df = 1, log = TRUE) * tbl_curves$weight)
  return(loglikelihood)
}
