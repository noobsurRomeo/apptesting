calculate_vma <- function(distance_m, duration_seconds) {
  distance_m / duration_seconds * 3.6
}

calculate_pma <- function(power_watts) {
  power_watts
}

calculate_linear_speed <- function(distances, times) {
  model <- lm(distances ~ times)
  list(
    value = unname(coef(model)[["times"]]) * 3.6,
    r_squared = summary(model)$r.squared
  )
}

calculate_linear_power <- function(powers, times) {
  inverse_time <- 1 / times
  model <- lm(powers ~ inverse_time)
  list(
    value = unname(coef(model)[["(Intercept)"]]),
    w_prime = unname(coef(model)[["inverse_time"]]),
    r_squared = summary(model)$r.squared
  )
}