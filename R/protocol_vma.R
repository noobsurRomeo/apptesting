vma_ui <- function(lang) {
  tagList(
    h4(lang$vma_test),

    p(lang$vma_protocol),

    textInput(
      inputId = "vma_time",
      label = lang$time,
      value = "05:00",
      placeholder = "mm:ss"
    ),

    numericInput(
      "vma_distance",
      paste(lang$distance, "(", lang$metres, ")"),
      value = 1500,
      min = 1
    ),

    numericInput(
      "mean_hr",
      lang$mean_hr,
      value = NA,
      min = 40,
      max = 250
    ),

    numericInput(
      "max_hr",
      lang$max_hr,
      value = NA,
      min = 80,
      max = 250
    )
  )
}

calculate_vma_protocol <- function(input) {
  duration <- parse_mm_ss(input$vma_time)

  if (is.na(duration)) {
    stop("invalid_time")
  }

  if (!valid_constant_effort_time(duration)) {
    stop("invalid_constant_effort_time")
  }

  if (is.na(input$vma_distance) || input$vma_distance <= 0) {
    stop("invalid_vma_distance")
  }

  validate_heart_rate(input$mean_hr, input$max_hr)

  list(
    value = calculate_vma(input$vma_distance, duration),
    unit = "km/h",
    duration = duration,
    distance = input$vma_distance,
    mean_hr = input$mean_hr,
    max_hr = input$max_hr,
    protocol = "vma"
  )
}
