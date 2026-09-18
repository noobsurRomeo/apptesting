app_server <- function(input, output, session) {

  # ==========================================================
  # Language
  # ==========================================================

  current_lang <- reactive({

    selected_language <- input$language

    if (is.null(selected_language) ||
        !selected_language %in% names(translations)) {
      selected_language <- "fr"
    }

    translations[[selected_language]]
  })


  # ==========================================================
  # Application translations
  # ==========================================================

  output$app_title <- renderUI({
    current_lang()$app_title
  })


  output$athlete_information_title <- renderUI({
    h4(current_lang()$athlete_information)
  })


  output$select_test_title <- renderUI({
    h4(current_lang()$select_test)
  })


  output$enter_data_title <- renderUI({
    h4(current_lang()$enter_data)
  })


  # ==========================================================
  # Dynamic labels for standard inputs
  # ==========================================================

  observe({

    lang <- current_lang()

    updateTextInput(
      session = session,
      inputId = "athlete_name",
      label = lang$name,
      placeholder = lang$name_placeholder
    )

    updateTextInput(
      session = session,
      inputId = "athlete_email",
      label = lang$email,
      placeholder = lang$email_placeholder
    )

    updateDateInput(
      session = session,
      inputId = "test_date",
      label = lang$test_date
    )


  })
  # ==========================================================
  # Button outputs
  # ==========================================================
  output$calculate_button <- renderUI({

    actionButton(
      inputId = "calculate",
      label = current_lang()$calculate,
      class = "btn-primary"
    )
  })

  output$download_button <- renderUI({

    downloadButton(
      outputId = "download_report",
      label = current_lang()$download
    )
  })

  output$share_button <- renderUI({

    actionButton(
      inputId = "share_report",
      label = current_lang()$share,
      class = "btn-success"
    )
  })
  # ==========================================================
  # Sport selector
  # ==========================================================

  observe({

    lang <- current_lang()

    sport_values <- c(
      "running",
      "trail",
      "cycling",
      "ergometer"
    )

    sport_labels <- c(
      lang$running,
      lang$trail,
      lang$cycling,
      lang$ergometer
    )

    sport_choices <- setNames(
      object = sport_values,
      nm = sport_labels
    )

    selected_sport <- input$sport

    if (is.null(selected_sport) ||
        !selected_sport %in% sport_values) {
      selected_sport <- "running"
    }

    updateSelectInput(
      session = session,
      inputId = "sport",
      label = lang$sport,
      choices = sport_choices,
      selected = selected_sport
    )
  })
  # ==========================================================
  # Get protocols for the selected sport
  # ==========================================================

  available_protocols <- reactive({

    sport <- input$sport

    if (is.null(sport) ||
        is.null(protocol_registry[[sport]])) {
      return(list())
    }

    protocol_registry[[sport]]
  })


  # ==========================================================
  # Test selector
  # ==========================================================

  observe({

    lang <- current_lang()
    protocols <- available_protocols()

    if (length(protocols) == 0) {
      return()
    }

    test_values <- vapply(
      protocols,
      function(protocol) {
        protocol$id
      },
      character(1)
    )

    test_labels <- vapply(
      protocols,
      function(protocol) {
        lang[[protocol$label_key]]
      },
      character(1)
    )

    test_choices <- setNames(
      object = test_values,
      nm = test_labels
    )

    selected_test <- input$test

    if (is.null(selected_test) ||
        !selected_test %in% test_values) {
      selected_test <- test_values[1]
    }

    updateSelectInput(
      session = session,
      inputId = "test",
      label = lang$test_protocol,
      choices = test_choices,
      selected = selected_test
    )
  })

  # ==========================================================
  # Selected protocol
  # ==========================================================

  selected_protocol <- reactive({

    protocols <- available_protocols()

    if (is.null(protocols) ||
        is.null(input$test)) {
      return(NULL)
    }

    matching_protocol <- vapply(
      protocols,
      function(protocol) {
        identical(protocol$id, input$test)
      },
      logical(1)
    )

    if (!any(matching_protocol)) {
      return(NULL)
    }

    protocols[[which(matching_protocol)[1]]]
  })


  # ==========================================================
  # Selected protocol inputs
  # ==========================================================

  output$protocol_ui <- renderUI({

    protocol <- selected_protocol()
    lang <- current_lang()

    if (is.null(protocol)) {
      return(
        div(
          class = "alert alert-warning",
          lang$no_protocol_selected
        )
      )
    }

    switch(

      protocol$type,

      vma = vma_ui(lang),

      pma = pma_ui(lang),

      critical_speed = critical_speed_ui(lang),

      critical_power = critical_power_ui(lang),

      vertical_speed = vertical_speed_ui(lang),

      div(
        class = "alert alert-danger",
        paste(
          "Unknown protocol type:",
          protocol$type
        )
      )
    )
  })

  # ==========================================================
  # Report storage
  # ==========================================================

  report <- reactiveValues(
    calculated = FALSE,
    status = "not_shared",
    saved_to_database = FALSE,
    result = NULL,
    protocol = NULL,
    sport = NULL,
    athlete_name = NULL,
    athlete_email = NULL,
    test_date = NULL
  )

  # ==========================================================
  # Calculate selected protocol
  # ==========================================================

  observeEvent(input$calculate, {

    lang <- current_lang()
    protocol <- selected_protocol()

    if (is.null(protocol)) {
      showNotification(
        lang$no_protocol_selected,
        type = "error"
      )
      return()
    }

    calculation <- tryCatch(

      switch(
        protocol$type,

        vma = calculate_vma_protocol(input),

        pma = calculate_pma_protocol(input),

        critical_speed = calculate_critical_speed_protocol(input),

        critical_power = calculate_critical_power_protocol(input),

        vertical_speed = calculate_vertical_speed_protocol(input),

        stop(
          paste(
            "Unknown protocol:",
            protocol$type
          )
        )
      ),

      error = function(error) {
        error_key <- error$message
        translated_message <- lang[[error_key]]

        if (is.null(translated_message)) {
          translated_message <- error$message
        }

        showNotification(
          translated_message,
          type = "error"
        )

        NULL
      }
    )

    if (is.null(calculation)) {
      return()
    }

    report$calculated <- TRUE
    report$status <- "not_shared"
    report$saved_to_database <- FALSE
    report$result <- calculation
    report$protocol <- protocol$type
    report$sport <- input$sport
    report$athlete_name <- input$athlete_name
    report$athlete_email <- input$athlete_email
    report$test_date <- input$test_date

    showNotification(
      lang$calc_success,
      type = "message"
    )
  })
  # ==========================================================
  # Share report
  # ==========================================================

  observeEvent(input$share_report, {

    lang <- current_lang()
    protocol <- selected_protocol()

    if (!report$calculated) {

      showNotification(
        lang$calculate_first,
        type = "error"
      )

      return()
    }

    if (report$saved_to_database) {

      showNotification(
        "This report has already been shared.",
        type = "warning"
      )

      return()
    }

    if (is.null(protocol)) {

      showNotification(
        lang$no_protocol_selected,
        type = "error"
      )

      return()
    }

    # Generate the same HTML used by the download button
    html <- build_report_html(
      athlete_name = report$athlete_name,
      athlete_email = report$athlete_email,
      sport = report$sport,
      protocol = protocol,
      test_date = report$test_date,
      language = input$language,
      result = report$result,
      report_status = "shared",
      lang = lang
    )

    saved_id <- tryCatch(

      save_shared_report(
        athlete_name = report$athlete_name,
        athlete_email = report$athlete_email,
        sport = report$sport,
        protocol = report$protocol,
        test_date = report$test_date,
        language = input$language,
        report_html = html,
        result = report$result
      ),

      error = function(error) {

        showNotification(
          paste(
            "Database error:",
            error$message
          ),
          type = "error"
        )

        NULL
      }
    )

    if (is.null(saved_id)) {
      return()
    }

    report$saved_to_database <- TRUE
    report$status <- "shared"

    showNotification(
      paste(
        lang$shared_success,
        "Report ID:",
        saved_id
      ),
      type = "message"
    )
  })


  # ==========================================================
  # Display report
  # ==========================================================

  output$report_ui <- renderUI({

    lang <- current_lang()

    if (!report$calculated) {

      return(
        div(
          class = "alert alert-secondary",
          lang$calculate_first
        )
      )
    }

    protocol <- selected_protocol()
    result <- report$result

    if (is.null(protocol) ||
        is.null(result)) {
      return(NULL)
    }

    sport_label <- switch(

      report$sport,

      running = lang$running,

      trail = lang$trail,

      cycling = lang$cycling,

      ergometer = lang$ergometer,

      report$sport
    )

    protocol_label <- lang[[protocol$label_key]]

    status_label <- if (
      report$status == "private"
    ) {
      lang$private
    } else {
      lang$shared
    }

    result_label <- switch(

      protocol$type,

      vma = lang$vma,

      pma = lang$pma,

      critical_speed = lang$critical_speed,

      critical_power = lang$critical_power,

      vertical_speed = lang$critical_vertical_speed,

      lang$result
    )

    result_unit <- result$unit

    additional_rows <- list()

    if (!is.null(result$duration)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th(lang$duration),
            tags$td(format_time(result$duration))
          )
        )
      )
    }

    if (!is.null(result$distance)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th(lang$distance),
            tags$td(
              paste(
                result$distance,
                lang$metres
              )
            )
          )
        )
      )
    }

    if (!is.null(result$power)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th(lang$average_power),
            tags$td(
              paste(
                result$power,
                lang$watts
              )
            )
          )
        )
      )
    }

    if (!is.null(result$mean_hr) &&
        !is.na(result$mean_hr)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th(lang$mean_hr),
            tags$td(
              paste(
                result$mean_hr,
                lang$bpm
              )
            )
          )
        )
      )
    }

    if (!is.null(result$max_hr) &&
        !is.na(result$max_hr)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th(lang$max_hr),
            tags$td(
              paste(
                result$max_hr,
                lang$bpm
              )
            )
          )
        )
      )
    }

    if (!is.null(result$r_squared) &&
        !is.na(result$r_squared)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th("R²"),
            tags$td(round(result$r_squared, 3))
          )
        )
      )
    }

    if (!is.null(result$w_prime) &&
        !is.na(result$w_prime)) {

      additional_rows <- c(
        additional_rows,

        list(
          tags$tr(
            tags$th("W-prime"),
            tags$td(
              paste(
                round(result$w_prime, 1),
                "J"
              )
            )
          )
        )
      )
    }

    tagList(

      h2(lang$report),

      div(
        class = "alert alert-info",

        strong(
          paste(
            lang$status,
            ":"
          )
        ),

        paste(status_label)
      ),

      tags$table(
        class = "table table-bordered",

        tags$tr(
          tags$th(lang$name),
          tags$td(report$athlete_name)
        ),

        tags$tr(
          tags$th(lang$email),
          tags$td(report$athlete_email)
        ),

        tags$tr(
          tags$th(lang$sport),
          tags$td(sport_label)
        ),

        tags$tr(
          tags$th(lang$test_protocol),
          tags$td(protocol_label)
        ),

        tags$tr(
          tags$th(lang$test_date),
          tags$td(as.character(report$test_date))
        )
      ),

      h3(lang$result),

      tags$table(
        class = "table table-bordered",

        tags$tr(
          tags$th(result_label),
          tags$td(
            paste(
              round(result$value, 2),
              result_unit
            )
          )
        ),

        additional_rows
      )
    )
  })


  # ==========================================================
  # Download HTML report
  # ==========================================================

  output$download_report <- downloadHandler(

    filename = function() {

      athlete_name <- gsub(
        pattern = "[^A-Za-z0-9]+",
        replacement = "_",
        x = input$athlete_name
      )

      paste0(
        "report_",
        athlete_name,
        "_",
        Sys.Date(),
        ".html"
      )
    },

    content = function(file) {

      req(report$calculated)

      html <- build_report_html(
        athlete_name = report$athlete_name,
        athlete_email = report$athlete_email,
        sport = report$sport,
        protocol = selected_protocol(),
        test_date = report$test_date,
        language = input$language,
        result = report$result,
        report_status = "private",
        lang = current_lang()
      )

      writeLines(
        text = html,
        con = file,
        useBytes = TRUE
      )
    }
  )
}