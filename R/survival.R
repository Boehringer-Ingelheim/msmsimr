#' @export
survival <- function(msm, t, from, to, data = simulate(msm))
{
  km <- kaplan_meier(msm, from, to, data = data, tidy = TRUE)
  res <- with(km,
    approx(x = time, y = estimate, xout = t, yleft = 1, rule = 2,
      method = "constant")
  )
  return(res$y)
}
