#' @export
next_state_probabilities <- function(msm, data = simulate(msm), as_matrix = FALSE) {
  state_labels <- get_state_labels(msm)
  res <- data %>%
    # add one artificial transition to make all levels appear
    dplyr::bind_rows(
      tidyr::expand_grid(
        from = state_labels,
        to = state_labels
      )
    ) %>%
    dplyr::group_by(from, to) %>%
    dplyr::count() %>%
    dplyr::group_by(from) %>%
    dplyr::mutate(
      probability = if (sum(n) == dplyr::n()) rep(0, dplyr::n()) else (n - 1) / (sum(n) - dplyr::n())
    ) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup()
  if (as_matrix) {
    res <- res %>%
      tidyr::pivot_wider(names_from = to, values_from = probability) %>%
      tibble::column_to_rownames("from") %>%
      as.matrix()
    # rearrange
    idx <- order(state_labels)
    res <- res[idx, idx]
  }
  return(res)
}