# class description ===========================================================

#' @title Survival Distributions
#'
#' @description ...
#'
#' @name survival-distribution
#' @seealso [LogNormal], [cumulative_hazard()]
NULL



# generic functions ===========================================================

#' @title Cumulative Hazard Function
#'
#' @description Cumulative hazard function of a survival distribution.
#'
#' @template time
#' @template distribution
#'
#' @return numeric vector of cumulative hazards evaluated at `t`
#'
#' @seealso [survival-distribution]
#' @family survival distribution methods
#'
#' @export
cumulative_hazard <- function(t, dst) {
  UseMethod("cumulative_hazard", object = dst)
}

#' @export
as_vector <- function(x) {
  UseMethod("as_vector", object = x)
}

#' @export
as_vector.default <- function(x) {
  res <- if (attr(x, "optimize")) {
    unlist(x)
  } else {
    numeric(0L)
  }
  return(res)
}



# exponential distribution ====================================================

#' @title Exponential Distribution
#'
#' @description description
#'
#' @param lambda rate
#'
#' @return A [survival distribution](survival-distribution) of class `Exponential`.
#'
#' @seealso [survival-distribution]
#' @family survival distributions
#'
#' @export
Exponential <- function(lambda, optimize = FALSE) {
  checkmate::assert_numeric(
    lambda, finite = TRUE, any.missing = FALSE, len = 1L, null.ok = FALSE,
    lower = sqrt(.Machine$double.eps)
  )
  structure(list(
      lambda = lambda
    ),
    optimize = optimize,
    class = c("Exponential", "SurvivalDistribution")
  )
}

#' @export
cumulative_hazard.Exponential <- function(t, dst) {
  checkmate::assert_numeric(
    t, finite = TRUE, any.missing = FALSE, min.len = 1L, null.ok = FALSE,
    lower = 0
  )
  return(dst$lambda*t)
}