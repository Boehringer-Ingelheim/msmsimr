gradient_descent <- function(fn, gr, init, lower, upper, rate, niter, momentum) {
  x <- matrix(NA_real_, nrow = niter + 1, ncol = length(init), byrow = TRUE)
  vals <- numeric(niter + 1)
  x[1, ] <- init
  vals[1] <- fn(init) # can make this more efficient by reusing function evaluations
  old_direction <- numeric(length(init)) # buffer for momentum
  for (i in 1:niter) {
    cat("current location:\n\r")
    cat(round(x[i, ], 3), vals[i])
    cat("\n\r")
    cat("calculating current gradient:\n\r")
    direction <- -(1 - momentum) * gr(x[i, ]) + momentum * old_direction
    old_direction <- direction
    # adjust rate to bounds - ideally we do this un unbounded space...
    rr <- rate
    while (any(!dplyr::between(x[i, ] + rr*direction, lower, upper))) {
      cat("rate reduction\n\r")
      rr <- rr/2
    }
    cat(round(direction, 3))
    cat("\n\r")
    cat("current scaled direction:\n\r")
    cat(round(rr*direction, 3))
    cat("\n\n\r")
    x[i + 1, ] <- x[i, ] + rr * direction
    vals[i + 1] <- fn(x[i + 1, ]) # reuse f evals!
  }
  return(list(x, vals))
}
