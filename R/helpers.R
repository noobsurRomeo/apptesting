time_to_seconds <- function(minutes, seconds) {
  minutes * 60 + seconds
}

valid_constant_effort_time <- function(seconds) {
  !is.na(seconds) && seconds >= 300 && seconds <= 360
}

format_time <- function(seconds) {
  sprintf("%02d:%02d", floor(seconds / 60), round(seconds %% 60))
}

parse_mm_ss <- function(value) {
  if (is.null(value) || is.na(value)) return(NA_real_)
  x <- strsplit(value, ":", fixed = TRUE)[[1]]
  if (length(x) != 2) return(NA_real_)
  minutes <- suppressWarnings(as.numeric(x[1]))
  seconds <- suppressWarnings(as.numeric(x[2]))
  if (is.na(minutes) || is.na(seconds)) return(NA_real_)
  if (seconds < 0 || seconds > 59) return(NA_real_)
  minutes * 60 + seconds
}

validate_heart_rate <- function(mean_hr, max_hr) {
  
  if (is.na(mean_hr) || mean_hr <= 0) {
    stop("Invalid mean heart rate")
  }
  
  if (is.na(max_hr) || max_hr <= 0) {
    stop("Invalid maximum heart rate")
  }
  
  if (max_hr < mean_hr) {
    stop("Maximum heart rate cannot be lower than mean heart rate")
  }
  
  TRUE
}