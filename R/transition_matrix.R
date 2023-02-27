#' @title Transition Matrix
#'
#' @description Check a multi-state transition matrix for consistency and
#'  turn it into a matrix of class `TransitionMatrix`
#'
#' @param tmat a k-by-k integer-ish matrix where `tmat[i,j]` is missing if the
#'  transition is impossible and an integer otherwise. The integers must be
#'  unique and sequential since they are used to index into the list of
#'  transition distributions.
#' @param state_labels character vector of length k with labels for the states;
#'  populated from the column names of `tmat` if available.
#'
#' @return A matrix of class `TransitionMatrix`.
#'
#' @export
as_TransitionMatrix <- function(tmat, state_labels = colnames(tmat)) { # nolint
  checkmate::assert_matrix(tmat, mode = "integerish", all.missing = FALSE,
    nrows = ncol(tmat), null.ok = FALSE)
  t_ids <- as.vector(tmat)
  t_ids <- t_ids[!is.na(t_ids)]
  checkmate::assert_integerish(t_ids, lower = 1, upper = sum(!is.na(tmat)),
    unique = TRUE)
  if (is.null(state_labels)) {
    stop("argument 'state_labels' must be provided")
  } else {
    colnames(tmat) <- state_labels
    rownames(tmat) <- state_labels
  }
  return(structure(tmat, class = "TransitionMatrix"))
}
