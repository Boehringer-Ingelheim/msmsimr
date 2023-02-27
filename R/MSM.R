# class description ===========================================================

#' @title Multi-State Model
#'
#' @description ...
#'
#' @param ... list of [survival distributions][survival-distribution];
#'  distributions are mapped to transitions using the indices specified
#'  in the transition matrix `tmat`
#' @param tmat transition matrix for the multi-state model
#' @param tmax maximal time when simulating from model
#'
#' @seealso [as_TransitionMatrix()] [survival-distribution]
#'
#' @export
MSM <- function(..., tmat, tmax = NULL, m = 2^11, grid_size = 500) {
  transition_distributions <- list(...)
  for (i in seq_along(transition_distributions)) {
    checkmate::assert_class(transition_distributions[[i]], "SurvivalDistribution")
  }
  checkmate::assert_class(tmat, "TransitionMatrix")
  res <- structure(
    list(...),
    tmat = tmat,
    tmax = tmax,
    m = m,
    grid_size = grid_size,
    class = "MSM"
  )
  return(res)
}

#' @description `get_tmat()` returns the transition matrix of an `MSM` object.
#'
#' @param msm a `MSM` multi-state object
#'
#' @rdname MSM
#'
#' @export
get_tmat <- function(msm) {
  checkmate::assert_class(msm, "MSM")
  return(attr(msm, "tmat"))
}

#' @description `get_tmax()` returns the maximal simulation horizon of a
#' `MSM` object.
#'
#' @param msm a `MSM` multi-state object
#'
#' @rdname MSM
#'
#' @export
get_tmax <- function(msm) {
  checkmate::assert_class(msm, "MSM")
  return(attr(msm, "tmax"))
}

#' @description `get_state_labels()` ...
#'
#' @param msm a `MSM` multi-state object
#'
#' @rdname MSM
#'
#' @export
get_state_labels <- function(msm) {
  checkmate::assert_class(msm, "MSM")
  return(colnames(attr(msm, "tmat")))
}

#' @description `get_transition_distribution()` returns a single distribution
#'
#' @param msm a `MSM` multi-state object
#' @param index the integer index of the distribution to return
#'
#' @rdname MSM
#'
#' @export
get_transition_distribution <- function(msm, index) {
  checkmate::assert_class(msm, "MSM")
  checkmate::assert_integerish(index, lower = 1L, upper = length(msm), len = 1,
    null.ok = FALSE, any.missing = FALSE)
  return(msm[[index]])
}


#' @export
as_vector.MSM <- function(x) {
  do.call(c, lapply(x, as_vector))
}


#' @description update model parameters
#'
#' @param msm a `MSM` multi-state object
#' @param params numeric vector with new paramter values
#'
#' @rdname MSM
#'
#' @export
update_parameters <- function(msm, params) {
  checkmate::assert_class(msm, "MSM")
  assertthat::assert_that(length(params) == length(as_vector(msm)))
  idx <- 0L
  for (i in seq_along(msm)) {
    if (!attr(msm[[i]], "optimize")) {
      next
    }
    for (j in seq_along(msm[[i]])) {
      idx <- idx + 1L
      msm[[i]][[j]] <- as.numeric(params[idx])
    }
  }
  return(msm)
}




#' @importFrom stats simulate
#' @export
simulate.MSM <- function(
  object,
  nsim = attr(object, "m"),
  seed = NULL,
  tmax = get_tmax(object),
  grid_size = attr(object, "grid_size"),
  hazard_ratios = rep(1, length(object)),
  batches = parallel::detectCores(),
  ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  time_grid <- seq(0, tmax, length.out = grid_size)
  # create table of cumulative hazards on time grid
  tbl_cumhaz <- tidyr::expand_grid(
      time = time_grid,
      trans = seq_along(object)
    ) %>%
    dplyr::group_by(.data$trans) %>%
    dplyr::mutate(
        Haz = cumulative_hazard(
          t = .data$time,
          dst = get_transition_distribution(object, .data$trans[1])
        ) * hazard_ratios[.data$trans[1]]
    ) %>%
    dplyr::ungroup()
  # sample; c++ implementation of https://doi.org/10.1002/sim.3305 might be quicker
  res <- tibble::tibble(
      id = 1:nsim
    ) %>%
    dplyr::mutate(
      batch = dplyr::row_number() %% batches
    ) %>%
    dplyr::group_by(.data$batch) %>%
    dplyr::summarize(
      ids = list(.data$id)
    ) %>%
    dplyr::mutate(
      res = furrr::future_map(
          .data$ids,
          mstate_mssample_wrapper,
          tbl_cumhaz,
          get_tmat(object),
          .options = furrr::furrr_options(seed = TRUE)
        )
    ) %>%
    tidyr::unnest(.data$res) %>%
    dplyr::select(-.data$batch, -.data$ids)
  res %>%
    dplyr::filter(.data$status == 1) %>%
    dplyr::mutate(
      dplyr::across(
        c(.data$from, .data$to),
        ~factor(., levels = seq_along(object), labels = get_state_labels(object))
      )
    )
}
