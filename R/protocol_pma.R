pma_ui <- function(lang) {
  tagList(
    h4(lang$pma_test),
    p(lang$pma_protocol),
    textInput("pma_time", lang$time, value = "05:00", placeholder = "mm:ss"),
    numericInput("pma_power", paste(lang$average_power, "(", lang$watts, ")"), value = 250, min = 1),
    numericInput("mean_hr", lang$mean_hr, value = NA, min = 40, max = 250),
    numericInput("max_hr", lang$max_hr, value = NA, min = 80, max = 250)
  )
}

calculate_pma_protocol <- function(input) {
  duration <- parse_mm_ss(input$pma_time)

  if (is.na(duration)) {
    stop("invalid_time")
  }

  if (!valid_constant_effort_time(duration)) {
    stop("invalid_constant_effort_time")
  }

  if (is.na(input$pma_power) || input$pma_power <= 0) {
    stop("invalid_pma_power")
  }

  validate_heart_rate(input$mean_hr, input$max_hr)

  list(
    value = calculate_pma(input$pma_power),
    unit = "W",
    duration = duration,
    power = input$pma_power,
    mean_hr = input$mean_hr,
    max_hr = input$max_hr,
    protocol = "pma"
  )
}