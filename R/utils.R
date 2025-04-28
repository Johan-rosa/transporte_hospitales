#' Summarize Seconds into Time Format
#'
#' Converts a numeric vector of seconds into a formatted time string (`HH:MM:SS`).
#'
#' @param seconds A non-negative numeric vector representing durations in seconds.
#'
#' @return A character vector with the time formatted as `HH:MM:SS`.
#'
#' @details 
#' - Each element in `seconds` is converted separately.
#' - Seconds are rounded to the nearest whole number.
#' - Hours, minutes, and seconds are zero-padded to two digits.
#'
#' @examples
#' summarise_time(3661)
#' # Returns "01:01:01"
#'
#' summarise_time(c(45, 125, 3675))
#' # Returns "00:00:45", "00:02:05", "01:01:15"
#'
#' @importFrom stringr str_pad
#' @importFrom glue glue
#' 
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
