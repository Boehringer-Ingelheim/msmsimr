#' @title Gradient descent
#'
#' @param fn function to optimize
#' @param gr gradient
#' @param init initial parameter variables
#' @param rate learning rate
#' @param niter number of iteration
#'
#' @export
gradient_descent <- function(fn, gr, init, rate, niter) {
  x <- matrix(NA_real_, nrow = niter + 1, ncol = length(init), byrow = TRUE)
  vals <- numeric(niter + 1)
  x[1, ] <- init
  vals[1] <- fn(init) # make this more efficient by reusing function evaluations
  for (i in 1:niter) {
    cat("current location:\n\r")
    cat(round(x[i, ], 3), vals[i])
    cat("\n\r")
    cat("calculating current gradient:\n\r")
    grad <- gr(x[i, ])
    direction <- -grad
    cat(round(grad, 3))
    cat("\n\r")
    cat("current scaled direction:\n\r")
    cat(round(rate * direction, 3))
    cat("\n\n\r")
    x[i + 1, ] <- x[i, ] + rate * direction
    vals[i + 1] <- fn(x[i + 1, ]) # reuse f evals!
  }
  return(list(x, vals))
}
