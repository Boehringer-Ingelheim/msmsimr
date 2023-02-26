#' @export
survival <- function(data, t, from, events)
{
  km <- kaplan_meier(data, from, events, tidy = TRUE)
  res <- with(km,
    approx(x = time, y = estimate, xout = t, yleft = 1, rule = 2,
      method = "constant")
  )
  return(res$y)
}
