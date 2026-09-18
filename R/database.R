database_path <- file.path(
  "data",
  "athlete_testing.sqlite"
)


initialize_database <- function() {
  
  if (!dir.exists("data")) {
    dir.create(
      "data",
      recursive = TRUE
    )
  }
  
  connection <- DBI::dbConnect(
    RSQLite::SQLite(),
    database_path
  )
  
  DBI::dbExecute(
    connection,
    "
    CREATE TABLE IF NOT EXISTS shared_reports (
      id INTEGER PRIMARY KEY AUTOINCREMENT,

      athlete_name TEXT NOT NULL,
      athlete_email TEXT,

      sport TEXT NOT NULL,
      protocol TEXT NOT NULL,
      test_date TEXT NOT NULL,
      language TEXT NOT NULL,

      report_html TEXT NOT NULL,
      result_json TEXT NOT NULL,

      created_at TEXT NOT NULL,
      shared_at TEXT NOT NULL
    )
    "
  )
  
  DBI::dbDisconnect(connection)
}


save_shared_report <- function(
    athlete_name,
    athlete_email,
    sport,
    protocol,
    test_date,
    language,
    report_html,
    result
) {
  
  connection <- DBI::dbConnect(
    RSQLite::SQLite(),
    database_path
  )
  
  now <- as.character(Sys.time())
  
  DBI::dbExecute(
    connection,
    "
    INSERT INTO shared_reports (
      athlete_name,
      athlete_email,
      sport,
      protocol,
      test_date,
      language,
      report_html,
      result_json,
      created_at,
      shared_at
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      athlete_name,
      athlete_email,
      sport,
      protocol,
      as.character(test_date),
      language,
      report_html,
      jsonlite::toJSON(
        result,
        auto_unbox = TRUE,
        null = "null"
      ),
      now,
      now
    )
  )
  
  report_id <- DBI::dbGetQuery(
    connection,
    "SELECT last_insert_rowid() AS id"
  )$id[[1]]
  
  DBI::dbDisconnect(connection)
  
  report_id
}


get_shared_reports <- function() {
  
  connection <- DBI::dbConnect(
    RSQLite::SQLite(),
    database_path
  )
  
  reports <- DBI::dbGetQuery(
    connection,
    "
    SELECT
      id,
      athlete_name,
      athlete_email,
      sport,
      protocol,
      test_date,
      language,
      created_at,
      shared_at
    FROM shared_reports
    ORDER BY shared_at DESC
    "
  )
  
  DBI::dbDisconnect(connection)
  
  reports
}


get_shared_report <- function(report_id) {
  
  connection <- DBI::dbConnect(
    RSQLite::SQLite(),
    database_path
  )
  
  report <- DBI::dbGetQuery(
    connection,
    "
    SELECT *
    FROM shared_reports
    WHERE id = ?
    ",
    params = list(report_id)
  )
  
  DBI::dbDisconnect(connection)
  
  report
}