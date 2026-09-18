pma_ui <- function(lang) {
  tagList(
    h4(lang$pma_test),
    p(lang$pma_protocol),
    numericInput("pma_minutes", lang$minutes, value = 5, min = 5, max = 6),
    numericInput("pma_seconds", lang$seconds, value = 0, min = 0, max = 59),
    numericInput("pma_power", paste(lang$average_power, "(", lang$watts, ")"), value = 250, min = 1),
    numericInput("max_hr", lang$max_hr, value = NA, min = 80, max = 250)
  )
}

calculate_pma_protocol <- function(input) {
  duration <- time_to_seconds(input$pma_minutes, input$pma_seconds)
  if (!valid_constant_effort_time(duration)) stop("Invalid PMA time")
  if (is.na(input$pma_power) || input$pma_power <= 0) stop("Invalid PMA power")
  
  list(
    value = input$pma_power,
    unit = "W",
    duration = duration,
    power = input$pma_power,
    max_hr = input$max_hr,
    mean_hr = NA_real_,
    protocol = "pma"
  )
}