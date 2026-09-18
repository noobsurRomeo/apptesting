vertical_speed_ui <- function(lang) {

  tagList(

    h4(lang$vertical_speed_test),

    p(
      lang$vertical_speed_protocol
    ),

    numericInput(
      inputId = "vertical_distance_1",
      label = paste(
        lang$elevation,
        "1"
      ),
      value = 300,
      min = 1
    ),

    textInput(
      inputId = "vertical_time_1",
      label = paste(lang$time, "1"),
      value = "05:00"
    ),

    numericInput(
      inputId = "mean_hr",
      label = lang$mean_hr,
      value = NA,
      min = 40,
      max = 250
    ),

    numericInput(
      inputId = "max_hr",
      label = lang$max_hr,
      value = NA,
      min = 80,
      max = 250
    )
  )
}

calculate_vertical_speed_protocol <- function(input) {
  duration <- parse_mm_ss(input$vertical_time_1)

  if (is.na(duration) || duration <= 0) {
    stop("invalid_time")
  }

  if (is.na(input$vertical_distance_1) || input$vertical_distance_1 <= 0) {
    stop("invalid_vertical_speed_data")
  }

  validate_heart_rate(input$mean_hr, input$max_hr)

  list(
    value = input$vertical_distance_1 / duration * 3600,
    unit = "m/h",
    duration = duration,
    distance = input$vertical_distance_1,
    mean_hr = input$mean_hr,
    max_hr = input$max_hr,
    protocol = "vertical_speed"
  )
}