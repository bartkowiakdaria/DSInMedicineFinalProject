library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(stringr)
library(tibble)
library(scales)
library(bsicons)
library(rmarkdown)
library(knitr)

DATA_FILE <- "Hospital_Admissions_ULTIMATE.csv"
DB_FILE <- "hospital_admissions.sqlite"

init_db <- function() {
  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  if (!file.exists(DATA_FILE)) {
    stop(paste("CSV file not found:", DATA_FILE))
  }
  
  if (!dbExistsTable(con, "admissions")) {
    data <- read_csv(DATA_FILE, show_col_types = FALSE)
    
    names(data) <- names(data) |>
      str_replace_all("\\s+", "_") |>
      str_replace_all("[^A-Za-z0-9_]", "") |>
      tolower()
    
    if (!"record_id" %in% names(data)) {
      data$record_id <- seq_len(nrow(data))
    }
    
    dbWriteTable(con, "admissions", data, overwrite = TRUE)
  }
  
  if (!dbExistsTable(con, "audit_log")) {
    dbExecute(con, "
      CREATE TABLE audit_log (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp TEXT,
        action TEXT,
        record_id TEXT,
        user TEXT,
        details TEXT
      )
    ")
  }
}

get_data <- function() {
  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  data <- dbReadTable(con, "admissions")
  
  if (!"record_id" %in% names(data)) {
    data$record_id <- seq_len(nrow(data))
  }
  
  data
}

save_data <- function(data) {
  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, "admissions", data, overwrite = TRUE)
}

add_log <- function(action, record_id = NA, user = "unknown_user", details = "") {
  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  record_id <- as.character(record_id[1])
  if (is.na(record_id) || record_id == "") record_id <- "NA"
  
  dbExecute(
    con,
    "INSERT INTO audit_log(timestamp, action, record_id, user, details)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      as.character(Sys.time()),
      as.character(action),
      record_id,
      as.character(user),
      as.character(details)
    )
  )
}

get_logs <- function() {
  con <- dbConnect(SQLite(), DB_FILE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  if (!dbExistsTable(con, "audit_log")) {
    return(data.frame())
  }
  
  dbReadTable(con, "audit_log")
}

calculate_quality <- function(data) {
  total_cells <- nrow(data) * ncol(data)
  null_cells <- sum(is.na(data) | data == "", na.rm = TRUE)
  
  completeness <- ifelse(
    total_cells == 0,
    0,
    round(100 * (1 - null_cells / total_cells), 2)
  )
  
  issues <- tibble(issue = character(), rows = integer(), category = character())
  
  cols <- names(data)
  
  age_col <- cols[str_detect(cols, "age") & !str_detect(cols, "computed")][1]
  computed_age_col <- cols[str_detect(cols, "computed_age")][1]
  sex_col <- cols[str_detect(cols, "sex|gender")][1]
  weight_col <- cols[str_detect(cols, "weight")][1]
  height_col <- cols[str_detect(cols, "height")][1]
  dosage_col <- cols[str_detect(cols, "dosage")][1]
  smoker_col <- cols[str_detect(cols, "smoker")][1]
  patient_col <- cols[str_detect(cols, "patient_id")][1]
  
  if (!is.na(patient_col)) {
    bad_patient <- sum(
      !is.na(data[[patient_col]]) &
        data[[patient_col]] != "" &
        !grepl("^P[0-9]{4,}$", as.character(data[[patient_col]])),
      na.rm = TRUE
    )
    issues <- add_row(issues, issue = "Invalid patient_id format", rows = bad_patient, category = "consistency")
  }
  
  if (!is.na(age_col)) {
    age <- suppressWarnings(as.numeric(data[[age_col]]))
    bad_age <- sum(age < 0 | age > 120, na.rm = TRUE)
    issues <- add_row(issues, issue = "Age outside 0-120", rows = bad_age, category = "accuracy/outliers")
  }
  
  if (!is.na(computed_age_col) && !is.na(age_col)) {
    age <- suppressWarnings(as.numeric(data[[age_col]]))
    computed_age <- suppressWarnings(as.numeric(data[[computed_age_col]]))
    bad_computed_age <- sum(abs(age - computed_age) > 1, na.rm = TRUE)
    issues <- add_row(issues, issue = "Age and computed_age mismatch", rows = bad_computed_age, category = "consistency")
  }
  
  if (!is.na(sex_col)) {
    valid_sex <- c("Male", "Female", "Other", "M", "F", "O", "male", "female", "other", "m", "f", "o")
    bad_sex <- sum(
      !is.na(data[[sex_col]]) &
        data[[sex_col]] != "" &
        !as.character(data[[sex_col]]) %in% valid_sex,
      na.rm = TRUE
    )
    issues <- add_row(issues, issue = "Invalid sex values", rows = bad_sex, category = "consistency")
  }
  
  if (!is.na(weight_col)) {
    weight <- suppressWarnings(as.numeric(data[[weight_col]]))
    bad_weight <- sum(weight < 2 | weight > 300, na.rm = TRUE)
    issues <- add_row(issues, issue = "Unrealistic weight", rows = bad_weight, category = "accuracy/outliers")
  }
  
  if (!is.na(height_col)) {
    height <- suppressWarnings(as.numeric(data[[height_col]]))
    bad_height <- sum(height < 40 | height > 250, na.rm = TRUE)
    issues <- add_row(issues, issue = "Unrealistic height", rows = bad_height, category = "accuracy/outliers")
  }
  
  if (!is.na(dosage_col)) {
    dosage <- suppressWarnings(as.numeric(data[[dosage_col]]))
    bad_dosage <- sum(dosage < 0, na.rm = TRUE)
    issues <- add_row(issues, issue = "Negative dosage", rows = bad_dosage, category = "accuracy/outliers")
  }
  
  if (!is.na(smoker_col)) {
    valid_smoker <- c("Yes", "No", "Y", "N", "yes", "no", "y", "n", "TRUE", "FALSE", "True", "False", "0", "1")
    bad_smoker <- sum(
      !is.na(data[[smoker_col]]) &
        data[[smoker_col]] != "" &
        !as.character(data[[smoker_col]]) %in% valid_smoker,
      na.rm = TRUE
    )
    issues <- add_row(issues, issue = "Invalid smoker values", rows = bad_smoker, category = "consistency")
  }
  
  total_issues <- sum(issues$rows, na.rm = TRUE)
  
  number_of_checks <- nrow(issues)
  total_possible_checks <- max(nrow(data), 1) * max(number_of_checks, 1)
  
  consistency <- round(100 * (1 - total_issues / total_possible_checks), 2)
  consistency <- max(min(consistency, 100), 0)
  
  list(
    completeness = completeness,
    missing_cells = null_cells,
    consistency = consistency,
    consistency_issues = issues
  )
}

dashboard_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    layout_columns(
      value_box(
        title = "Completeness",
        value = textOutput(ns("completeness")),
        showcase = bs_icon("check-circle")
      ),
      value_box(
        title = "Missing values",
        value = textOutput(ns("missing")),
        showcase = bs_icon("exclamation-triangle")
      ),
      value_box(
        title = "Consistency / accuracy",
        value = textOutput(ns("consistency")),
        showcase = bs_icon("shield-check")
      )
    ),
    layout_columns(
      card(
        card_header("Missing values by variable"),
        plotlyOutput(ns("missing_plot"))
      ),
      card(
        card_header("Consistency and accuracy issues"),
        DTOutput(ns("issues_table"))
      )
    )
  )
}

dashboard_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    quality <- reactive({
      calculate_quality(data_reactive())
    })
    
    output$completeness <- renderText({
      paste0(quality()$completeness, "%")
    })
    
    output$missing <- renderText({
      quality()$missing_cells
    })
    
    output$consistency <- renderText({
      paste0(quality()$consistency, "%")
    })
    
    output$missing_plot <- renderPlotly({
      data <- data_reactive()
      
      missing_df <- tibble(
        variable = names(data),
        missing = sapply(data, function(x) sum(is.na(x) | x == "", na.rm = TRUE))
      ) |>
        arrange(desc(missing)) |>
        head(15)
      
      p <- ggplot(missing_df, aes(x = reorder(variable, missing), y = missing)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "Missing values") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    output$issues_table <- renderDT({
      datatable(
        quality()$consistency_issues,
        options = list(pageLength = 8),
        rownames = FALSE
      )
    })
  })
}

visualization_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("x"), "X variable", choices = NULL),
        selectInput(ns("y"), "Y variable", choices = NULL),
        selectInput(ns("type"), "Chart type", choices = c("Histogram", "Bar chart", "Scatter plot"))
      ),
      card(
        card_header("Interactive visualization"),
        plotlyOutput(ns("plot"), height = "600px")
      )
    )
  )
}

visualization_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      data <- data_reactive()
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      
      updateSelectInput(
        session,
        "x",
        choices = names(data),
        selected = if ("age" %in% names(data)) "age" else names(data)[1]
      )
      
      updateSelectInput(
        session,
        "y",
        choices = names(data),
        selected = if ("weight_kg" %in% names(data)) "weight_kg" else numeric_cols[1]
      )
    })
    
    output$plot <- renderPlotly({
      req(input$x)
      
      data <- data_reactive()
      
      x_values <- data[[input$x]]
      x_numeric <- suppressWarnings(as.numeric(x_values))
      x_is_numeric <- any(!is.na(x_numeric))
      
      if (input$type == "Histogram") {
        
        if (x_is_numeric) {
          temp <- data.frame(x = x_numeric)
          
          p <- ggplot(temp, aes(x = x)) +
            geom_histogram(bins = 30) +
            theme_minimal() +
            labs(
              title = paste("Histogram of", input$x),
              x = input$x,
              y = "Count"
            )
        } else {
          temp <- data.frame(x = as.character(x_values))
          
          p <- ggplot(temp, aes(x = x)) +
            geom_bar() +
            coord_flip() +
            theme_minimal() +
            labs(
              title = paste(input$x, "is not numeric — bar chart shown instead"),
              x = input$x,
              y = "Count"
            )
        }
        
      } else if (input$type == "Bar chart") {
        
        temp <- data.frame(x = as.character(x_values))
        
        p <- ggplot(temp, aes(x = x)) +
          geom_bar() +
          coord_flip() +
          theme_minimal() +
          labs(
            title = paste("Bar chart of", input$x),
            x = input$x,
            y = "Count"
          )
        
      } else {
        req(input$y)
        
        y_values <- data[[input$y]]
        y_numeric <- suppressWarnings(as.numeric(y_values))
        
        temp <- data.frame(
          x = x_numeric,
          y = y_numeric
        )
        
        temp <- temp[complete.cases(temp), , drop = FALSE]
        
        validate(
          need(nrow(temp) > 0, "Choose numeric variables for scatter plot.")
        )
        
        p <- ggplot(temp, aes(x = x, y = y)) +
          geom_point(alpha = 0.6) +
          theme_minimal() +
          labs(
            title = paste("Scatter plot:", input$x, "vs", input$y),
            x = input$x,
            y = input$y
          )
      }
      
      ggplotly(p)
    })
  })
}

records_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    card(
      card_header("Record management"),
      layout_columns(
        textInput(ns("search"), "Search records", placeholder = "Type to filter..."),
        actionButton(ns("delete"), "Delete selected record", class = "btn-danger"),
        actionButton(ns("refresh"), "Refresh", class = "btn-secondary")
      ),
      DTOutput(ns("table"))
    )
  )
}

records_server <- function(id, data_reactive, refresh_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      data <- data_reactive()
      
      if (!is.null(input$search) && input$search != "") {
        keep <- apply(data, 1, function(row) {
          any(str_detect(tolower(as.character(row)), tolower(input$search)))
        })
        data <- data[keep, , drop = FALSE]
      }
      
      data
    })
    
    output$table <- renderDT({
      datatable(
        filtered_data(),
        selection = "single",
        filter = "top",
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
    
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      req(selected)
      
      displayed <- filtered_data()
      
      if (!"record_id" %in% names(displayed)) {
        showNotification("No record_id column found", type = "error")
        return()
      }
      
      record_id <- displayed$record_id[selected]
      
      data <- data_reactive()
      user <- "admin"
      
      if ("updated_by" %in% names(data)) {
        possible_user <- data$updated_by[data$record_id == record_id][1]
        if (!is.na(possible_user) && possible_user != "") {
          user <- possible_user
        }
      }
      
      data <- data |> filter(record_id != !!record_id)
      
      save_data(data)
      add_log("DELETE", record_id, user, "Record deleted")
      refresh_data()
      
      showNotification("Record deleted successfully", type = "message")
    })
    
    observeEvent(input$refresh, {
      refresh_data()
    })
  })
}

add_record_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    card(
      card_header("Add new admission record with defensive validation"),
      p("Required fields and logical ranges are checked before saving."),
      uiOutput(ns("form")),
      actionButton(ns("add"), "Add record", class = "btn-primary")
    )
  )
}

add_record_server <- function(id, data_reactive, refresh_data) {
  moduleServer(id, function(input, output, session) {
    
    output$form <- renderUI({
      data <- data_reactive()
      
      if (!"record_id" %in% names(data)) {
        data$record_id <- seq_len(nrow(data))
      }
      
      cols <- setdiff(names(data), "record_id")
      
      tagList(
        lapply(cols, function(col) {
          if (col %in% c("sex")) {
            selectInput(
              session$ns(paste0("field_", col)),
              label = col,
              choices = c("", "Male", "Female", "Other", "M", "F", "O")
            )
          } else if (col %in% c("smoker")) {
            selectInput(
              session$ns(paste0("field_", col)),
              label = col,
              choices = c("", "Yes", "No", "Y", "N", "0", "1")
            )
          } else {
            textInput(session$ns(paste0("field_", col)), label = col, value = "")
          }
        })
      )
    })
    
    observeEvent(input$add, {
      data <- data_reactive()
      
      if (!"record_id" %in% names(data)) {
        data$record_id <- seq_len(nrow(data))
      }
      
      cols <- setdiff(names(data), "record_id")
      
      get_val <- function(col) {
        input[[paste0("field_", col)]]
      }
      
      errors <- c()
      
      if ("patient_id" %in% cols) {
        patient_id <- get_val("patient_id")
        if (is.null(patient_id) || patient_id == "") {
          errors <- c(errors, "patient_id is mandatory.")
        } else if (!grepl("^P[0-9]{4,}$", patient_id)) {
          errors <- c(errors, "patient_id must have format like P1001.")
        }
      }
      
      if ("age" %in% cols) {
        age <- suppressWarnings(as.numeric(get_val("age")))
        if (is.na(age)) {
          errors <- c(errors, "age is mandatory and must be numeric.")
        } else if (age < 0 || age > 120) {
          errors <- c(errors, "age must be between 0 and 120.")
        }
      }
      
      if ("computed_age" %in% cols && "age" %in% cols) {
        age <- suppressWarnings(as.numeric(get_val("age")))
        computed_age <- suppressWarnings(as.numeric(get_val("computed_age")))
        
        if (!is.na(age) && !is.na(computed_age) && abs(age - computed_age) > 1) {
          errors <- c(errors, "computed_age must be coherent with age.")
        }
      }
      
      if ("sex" %in% cols) {
        sex <- get_val("sex")
        if (is.null(sex) || sex == "") {
          errors <- c(errors, "sex is mandatory.")
        }
      }
      
      if ("weight_kg" %in% cols) {
        weight <- suppressWarnings(as.numeric(get_val("weight_kg")))
        if (!is.na(weight) && (weight < 2 || weight > 300)) {
          errors <- c(errors, "weight_kg must be between 2 and 300.")
        }
      }
      
      if ("height_cm" %in% cols) {
        height <- suppressWarnings(as.numeric(get_val("height_cm")))
        if (!is.na(height) && (height < 40 || height > 250)) {
          errors <- c(errors, "height_cm must be between 40 and 250.")
        }
      }
      
      if ("dosage_mg" %in% cols) {
        dosage <- suppressWarnings(as.numeric(get_val("dosage_mg")))
        if (!is.na(dosage) && dosage < 0) {
          errors <- c(errors, "dosage_mg cannot be negative.")
        }
      }
      
      if ("doctor_name" %in% cols) {
        doctor <- get_val("doctor_name")
        if (is.null(doctor) || doctor == "") {
          errors <- c(errors, "doctor_name is mandatory.")
        }
      }
      
      if ("created_by" %in% cols) {
        created_by <- get_val("created_by")
        if (is.null(created_by) || created_by == "") {
          errors <- c(errors, "created_by is mandatory.")
        }
      }
      
      if (length(errors) > 0) {
        showNotification(
          paste(errors, collapse = "\n"),
          type = "error",
          duration = 12
        )
        return()
      }
      
      new_record <- data[1, , drop = FALSE]
      new_record[1, ] <- NA
      
      current_max <- suppressWarnings(max(as.numeric(data$record_id), na.rm = TRUE))
      if (!is.finite(current_max)) current_max <- 0
      
      new_id <- current_max + 1
      new_record$record_id <- new_id
      
      for (col in cols) {
        value <- get_val(col)
        
        if (col %in% c("created_at", "updated_at")) {
          new_record[[col]] <- as.character(Sys.time())
        } else if (is.null(value) || value == "") {
          new_record[[col]] <- NA
        } else if (is.numeric(data[[col]])) {
          new_record[[col]] <- suppressWarnings(as.numeric(value))
        } else {
          new_record[[col]] <- as.character(value)
        }
      }
      
      combined <- bind_rows(data, new_record)
      
      save_data(combined)
      
      user <- "unknown_user"
      if ("created_by" %in% cols) {
        user <- get_val("created_by")
      }
      
      add_log("CREATE", new_id, user, paste("New record added by", user))
      refresh_data()
      
      showNotification("Record added successfully", type = "message")
    })
  })
}

reports_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    card(
      card_header("Generate summary report"),
      p("Download an HTML report describing the current state of the database, completeness, consistency, and accuracy metrics."),
      downloadButton(ns("download_report"), "Download HTML report", class = "btn-primary")
    )
  )
}

reports_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("database_quality_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        data <- data_reactive()
        quality <- calculate_quality(data)
        
        temp_report <- tempfile(fileext = ".Rmd")
        
        writeLines(c(
          "---",
          "title: 'Biomedical Database Quality Report'",
          "output: html_document",
          "---",
          "",
          "```{r setup, include=FALSE}",
          "library(dplyr)",
          "library(knitr)",
          "```",
          "",
          "## Database overview",
          "",
          "```{r echo=FALSE}",
          "cat('Number of records:', nrow(data), '\\n')",
          "cat('Number of variables:', ncol(data), '\\n')",
          "```",
          "",
          "## Completeness metrics",
          "",
          "```{r echo=FALSE}",
          "missing_table <- data.frame(",
          "  variable = names(data),",
          "  missing_values = sapply(data, function(x) sum(is.na(x) | x == '', na.rm = TRUE))",
          ")",
          "missing_table <- missing_table[order(-missing_table$missing_values), ]",
          "kable(missing_table)",
          "```",
          "",
          "## Consistency and accuracy metrics",
          "",
          "```{r echo=FALSE}",
          "kable(quality$consistency_issues)",
          "```",
          "",
          "## Summary indicators",
          "",
          "```{r echo=FALSE}",
          "cat('Completeness:', quality$completeness, '%\\n')",
          "cat('Missing cells:', quality$missing_cells, '\\n')",
          "cat('Consistency / accuracy score:', quality$consistency, '%\\n')",
          "```",
          "",
          "## Audit logs",
          "",
          "```{r echo=FALSE}",
          "if (exists('logs') && nrow(logs) > 0) {",
          "  kable(logs)",
          "} else {",
          "  cat('No audit logs available.')",
          "}",
          "```"
        ), temp_report)
        
        report_env <- new.env(parent = globalenv())
        report_env$data <- data
        report_env$quality <- quality
        report_env$logs <- get_logs()
        
        rmarkdown::render(
          input = temp_report,
          output_file = file,
          envir = report_env,
          quiet = TRUE
        )
      }
    )
  })
}

logs_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    card(
      card_header("Audit logs"),
      DTOutput(ns("logs"))
    )
  )
}

logs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$logs <- renderDT({
      datatable(
        get_logs(),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
  })
}

init_db()

ui <- page_navbar(
  title = "Biomedical Data Quality System",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C7FB8"
  ),
  
  nav_panel("Dashboard", dashboard_ui("dashboard")),
  nav_panel("Data visualization", visualization_ui("visualization")),
  nav_panel("Record management", records_ui("records")),
  nav_panel("Add record", add_record_ui("add_record")),
  nav_panel("Reports", reports_ui("reports")),
  nav_panel("Audit logs", logs_ui("logs"))
)

server <- function(input, output, session) {
  data_store <- reactiveVal(get_data())
  
  refresh_data <- function() {
    data_store(get_data())
  }
  
  dashboard_server("dashboard", data_store)
  visualization_server("visualization", data_store)
  records_server("records", data_store, refresh_data)
  add_record_server("add_record", data_store, refresh_data)
  reports_server("reports", data_store)
  logs_server("logs")
}

shinyApp(ui, server)