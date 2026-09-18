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
      "max_hr",
      lang$max_hr,
      value = NA,
      min = 80,
      max = 250
    )
  )
}
