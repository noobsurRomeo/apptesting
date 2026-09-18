library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)

source_files <- list.files(
  path = "R",
  pattern = "\\.R$",
  full.names = TRUE
)

for (file in source_files) {
  source(file)
}

initialize_database()

shinyApp(
  ui = app_ui(),
  server = app_server
)