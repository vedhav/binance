#' @keywords internal
list_to_tibble <- function(list_to_convert) {
  list_to_convert
  for (i in seq_along(list_to_convert)) {
    if (is.vector(list_to_convert[[i]]) && length(list_to_convert[[i]]) > 1) {
      list_to_convert[[i]] <- list(list_to_convert[[i]])
    } else if (length(list_to_convert[[i]]) == 0) {
      list_to_convert[[i]] <- NA
    }
  }
  tibble::as_tibble(list_to_convert)
}
