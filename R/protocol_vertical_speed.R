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
      label = paste(
        lang$duration,
        "1 (mm:ss)"
      ),
      value = "05:00"
    )
  )
}