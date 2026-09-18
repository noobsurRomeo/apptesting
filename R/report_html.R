build_report_html <- function(
    athlete_name,
    athlete_email,
    sport,
    protocol,
    test_date,
    language,
    result,
    report_status,
    lang
) {
  
  protocol_label <- lang[[protocol$label_key]]
  
  status_label <- if (
    report_status == "shared"
  ) {
    lang$shared
  } else {
    lang$private
  }
  
  paste0(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<title>",
    htmltools::htmlEscape(lang$report),
    "</title>",
    "</head>",
    "<body>",
    
    "<h1>",
    htmltools::htmlEscape(lang$report),
    "</h1>",
    
    "<p><strong>",
    htmltools::htmlEscape(lang$name),
    ":</strong> ",
    htmltools::htmlEscape(athlete_name),
    "</p>",
    
    "<p><strong>",
    htmltools::htmlEscape(lang$email),
    ":</strong> ",
    htmltools::htmlEscape(athlete_email),
    "</p>",
    
    "<p><strong>",
    htmltools::htmlEscape(lang$test_protocol),
    ":</strong> ",
    htmltools::htmlEscape(protocol_label),
    "</p>",
    
    "<p><strong>",
    htmltools::htmlEscape(lang$status),
    ":</strong> ",
    htmltools::htmlEscape(status_label),
    "</p>",
    
    "<h2>",
    htmltools::htmlEscape(lang$result),
    "</h2>",
    
    "<p>",
    round(result$value, 2),
    " ",
    htmltools::htmlEscape(result$unit),
    "</p>",
    
    if (!is.null(result$duration)) {
      paste0(
        "<p><strong>",
        htmltools::htmlEscape(lang$duration),
        ":</strong> ",
        htmltools::htmlEscape(format_time(result$duration)),
        "</p>"
      )
    } else {
      ""
    },
    
    if (!is.null(result$mean_hr) && !is.na(result$mean_hr)) {
      paste0(
        "<p><strong>",
        htmltools::htmlEscape(lang$mean_hr),
        ":</strong> ",
        result$mean_hr,
        " ",
        htmltools::htmlEscape(lang$bpm),
        "</p>"
      )
    } else {
      ""
    },
    
    if (!is.null(result$max_hr) && !is.na(result$max_hr)) {
      paste0(
        "<p><strong>",
        htmltools::htmlEscape(lang$max_hr),
        ":</strong> ",
        result$max_hr,
        " ",
        htmltools::htmlEscape(lang$bpm),
        "</p>"
      )
    } else {
      ""
    },
    
    "</body>",
    "</html>"
  )
}