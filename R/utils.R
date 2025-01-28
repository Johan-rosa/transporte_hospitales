#' @export
summarise_time <- function(seconds) {
  if (!is.numeric(seconds) || any(seconds < 0)) {
    stop("Input must be a non-negative numeric vector.")
  }
  
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  remaining_seconds <- round(seconds %% 60)
  
  pad <- \(x) stringr::str_pad(x, width = 2, pad = "0")
  glue::glue("{pad(hours)}:{pad(minutes)}:{pad(remaining_seconds)}")
}
