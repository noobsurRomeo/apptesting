critical_speed_ui <- function(lang) {
  tagList(
    h4(lang$critical_speed_test),
    p(lang$critical_speed_protocol),
    numericInput("cs_distance_1", paste(lang$distance, "1"), value = 2000, min = 1),
    textInput("cs_time_1", paste(lang$time, "1"), value = "06:00"),
    numericInput("cs_distance_2", paste(lang$distance, "2"), value = 3000, min = 1),
    textInput("cs_time_2", paste(lang$time, "2"), value = "09:00"),
    numericInput("cs_distance_3", paste(lang$distance, "3"), value = 5000, min = 1),
    textInput("cs_time_3", paste(lang$time, "3"), value = "17:00"),
    numericInput("mean_hr", lang$mean_hr, value = NA, min = 40, max = 250),
    numericInput("max_hr", lang$max_hr, value = NA, min = 80, max = 250)
  )
}

calculate_critical_speed_protocol <- function(input) {
  distances <- c(input$cs_distance_1, input$cs_distance_2, input$cs_distance_3)
  times <- c(
    parse_mm_ss(input$cs_time_1),
    parse_mm_ss(input$cs_time_2),
    parse_mm_ss(input$cs_time_3)
  )
  
  if (any(is.na(distances)) || any(is.na(times)) || any(distances <= 0) || any(times <= 0)) {
    stop("invalid_critical_speed_data")
  }
  
  validate_heart_rate(input$mean_hr, input$max_hr)
  
  out <- calculate_linear_speed(distances, times)
  
  list(
    value = out$value,
    unit = "km/h",
    r_squared = out$r_squared,
    mean_hr = input$mean_hr,
    max_hr = input$max_hr,
    protocol = "critical_speed"
  )
}