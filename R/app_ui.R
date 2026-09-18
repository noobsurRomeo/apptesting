app_ui <- function() {
  
  fluidPage(
    
    # ========================================================
    # Application title
    # ========================================================
    
    titlePanel(
      uiOutput("app_title")
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        
        # ====================================================
        # Language selector
        # ====================================================
        
        selectInput(
          inputId = "language",
          label = "Language / Langue",
          choices = c(
            "Français" = "fr",
            "English" = "en"
          ),
          selected = "fr"
        ),
        
        hr(),
        
        # ====================================================
        # Athlete information
        # ====================================================
        
        uiOutput("athlete_information_title"),
        
        textInput(
          inputId = "athlete_name",
          label = NULL,
          placeholder = NULL
        ),
        
        textInput(
          inputId = "athlete_email",
          label = NULL,
          placeholder = NULL
        ),
        
        dateInput(
          inputId = "test_date",
          label = NULL,
          value = Sys.Date()
        ),
        
        hr(),
        
        # ====================================================
        # Sport and test selection
        # ====================================================
        
        uiOutput("select_test_title"),
        
        selectInput(
          inputId = "sport",
          label = NULL,
          choices = NULL,
          selected = "running"
        ),
        
        selectInput(
          inputId = "test",
          label = NULL,
          choices = NULL
        ),
        
        uiOutput("protocol_ui"),
        
        hr(),
        
        # ====================================================
        # Selected protocol inputs
        # ====================================================
        
        uiOutput("enter_data_title"),
        
        uiOutput("protocol_ui"),
        
        hr(),
        
        # ====================================================
        # Actions
        # ====================================================
        
        uiOutput("calculate_button"),
        
        br(),
        br(),
        
        uiOutput("download_button"),
        
        
        br(),
        br(),
        
        uiOutput("share_button")
      ),
      
      # ======================================================
      # Report
      # ======================================================
      
      mainPanel(
        
        uiOutput("report_ui")
      )
    )
  )
}
