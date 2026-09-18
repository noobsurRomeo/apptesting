critical_power_ui <- function(lang) {
  tagList(
    h4(lang$critical_power_test),
    p(lang$critical_power_protocol),
    numericInput("cp_power_1", paste(lang$power, "1", "(", lang$watts, ")"), value = 300, min = 1),
    textInput("cp_time_1", paste(lang$time, "1"), value = "03:00"),
    numericInput("cp_power_2", paste(lang$power, "2", "(", lang$watts, ")"), value = 280, min = 1),
    textInput("cp_time_2", paste(lang$time, "2"), value = "06:00"),
    numericInput("cp_power_3", paste(lang$power, "3", "(", lang$watts, ")"), value = 250, min = 1),
    textInput("cp_time_3", paste(lang$time, "3"), value = "12:00"),
    numericInput("mean_hr", lang$mean_hr, value = NA, min = 40, max = 250),
    numericInput("max_hr", lang$max_hr, value = NA, min = 80, max = 250)
  )
}

calculate_critical_power_protocol <- function(input) {
  powers <- c(input$cp_power_1, input$cp_power_2, input$cp_power_3)
  times <- c(
    parse_mm_ss(input$cp_time_1),
    parse_mm_ss(input$cp_time_2),
    parse_mm_ss(input$cp_time_3)
  )
  
  if (any(is.na(powers)) || any(is.na(times)) || any(powers <= 0) || any(times <= 0)) {
    stop("invalid_critical_power_data")
  }
  
  validate_heart_rate(input$mean_hr, input$max_hr)
  
  out <- calculate_linear_power(powers, times)
  
  list(
    value = out$value,
    unit = "W",
    w_prime = out$w_prime,
    r_squared = out$r_squared,
    mean_hr = input$mean_hr,
    max_hr = input$max_hr,
    protocol = "critical_power"
  )
}