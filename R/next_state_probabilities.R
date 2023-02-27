#' @title Compute sample-based next-state probabilities
#'
#' @description ...
#'
#' @template msm
#' @template data
#' @param as_matrix logical, return as matrix?
#'
#' @export
next_state_probabilities <- function(msm, data = simulate(msm),
  as_matrix = FALSE
) {
  state_labels <- get_state_labels(msm)
  res <- data %>%
    # add one artificial transition to make all levels appear
    dplyr::bind_rows(
      tidyr::expand_grid(
        from = state_labels,
        to = state_labels
      )
    ) %>%
    dplyr::group_by(.data$from, .data$to) %>%
    dplyr::count() %>%
    dplyr::group_by(.data$from) %>%
    dplyr::mutate(
      probability = if (sum(.data$n) == dplyr::n()) {
          rep(0, dplyr::n())
        } else {
          (.data$n - 1) / (sum(.data$n) - dplyr::n())
        }
    ) %>%
    dplyr::select(-"n") %>%
    dplyr::ungroup()
  if (as_matrix) {
    res <- res %>%
      tidyr::pivot_wider(
        names_from = .data$to, values_from = .data$probability
      ) %>%
      tibble::column_to_rownames("from") %>%
      as.matrix()
    # rearrange
    idx <- order(state_labels)
    res <- res[idx, idx]
  }
  return(res)
}
