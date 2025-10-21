#' Internal REDCap Project Creation Function
#'
#' @description
#' Internal function that creates a REDCap project object. Users should use 
#' the main redcap_project() function instead of calling this directly.
#'
#' @param url Character string. The REDCap API URL (e.g., "https://redcap.example.edu/api/")
#' @param token Character string. Your REDCap API token
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#'
#' @return A redcap_project object with cached data and methods
#' @keywords internal
.redcap_project_internal <- function(url, token, ssl_verify = TRUE, timeout = 30) {
  # Validate inputs
  if (missing(url) || is.null(url) || url == "") {
    stop("REDCap URL is required")
  }
  if (missing(token) || is.null(token) || token == "") {
    stop("REDCap API token is required")
  }

  # Validate REDCap-specific URL format
  if (!stringr::str_detect(url, "/api/?$")) {
    cli::cli_alert_warning("URL should end with '/api/' for REDCap projects")
    if (!stringr::str_detect(url, "/api")) {
      url <- paste0(stringr::str_remove(url, "/$"), "/api/")
      cli::cli_alert_info("Auto-corrected URL to: {url}")
    }
  }

  cli::cli_alert_info("Creating REDCap project connection...")

  # Create base connection for API calls
  connection <- list(
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout
  )

  # Test connection and get project info
  cli::cli_alert_info("Testing connection and fetching project information...")
  tryCatch({
    project_info <- .redcap_get_project_info(connection)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to connect to REDCap project")
    cli::cli_alert_danger("Error: {e$message}")
    stop("REDCap project creation failed. Check your URL and token.", call. = FALSE)
  })

  cli::cli_alert_success("Connected to REDCap project: {project_info$project_title}")

  # Get metadata
  cli::cli_alert_info("Fetching project metadata...")
  metadata <- tryCatch({
    .redcap_get_metadata(connection)
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch metadata: {e$message}")
    NULL
  })

  # Get full dataset
  cli::cli_alert_info("Caching full dataset...")
  data <- tryCatch({
    dat <- .redcap_get_all_records(connection)
    cli::cli_alert_success("Cached {nrow(dat)} records with {ncol(dat)} fields")
    dat
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch data: {e$message}")
    NULL
  })

  # Create project object
  # Determine id field from data (first column) or metadata, fallback to 'record_id'
  id_field <- if (!is.null(data) && ncol(data) > 0) {
    names(data)[1]
  } else if (!is.null(metadata) && 'field_name' %in% names(metadata)) {
    metadata$field_name[1]
  } else {
    'record_id'
  }
  project <- list(
    .connection = connection,
    .created_at = Sys.time(),
    data = data,
    metadata = metadata,
    dictionary = metadata,  # Alias for compatibility with validation functions
    project_info = project_info,
    id_field = id_field,
    refresh = function() {
      cli::cli_alert_info("Refreshing data from REDCap...")
      tryCatch({
        project$data <<- .redcap_get_all_records(connection)
        cli::cli_alert_success("Data refreshed: {nrow(project$data)} records")
        project$.created_at <<- Sys.time()
        invisible(project$data)
      }, error = function(e) {
        cli::cli_alert_danger("Failed to refresh data: {e$message}")
        invisible(NULL)
      })
    },
      import = function(data, overwrite = FALSE, force = FALSE, save_changes = NULL) {
        # Always refresh cache before analyzing changes
        project$refresh()

        # Analyze changes
        changes <- .analyze_import_changes(project$data, data, overwrite)
      
        # If no changes, return early
        if (changes$n_records_affected == 0 && changes$n_new_records == 0) {
          cli::cli_alert_info("No changes detected. Nothing to import.")
          return(invisible(NULL))
        }
      
        # Display change summary
        cli::cli_h2("Import Change Summary")
        cli::cli_alert_info("Records to add: {changes$n_new_records}")
        cli::cli_alert_info("Records to update: {changes$n_records_affected}")
        cli::cli_alert_info("Fields affected: {changes$n_fields_affected}")
      
        if (changes$n_overwrites > 0 && !overwrite) {
          cli::cli_alert_danger("Warning: {changes$n_overwrites} existing value(s) would be overwritten")
          cli::cli_alert_info("Set overwrite = TRUE to allow overwriting existing values")
          return(invisible(NULL))
        }
      
        if (changes$n_overwrites > 0) {
          cli::cli_alert_warning("{changes$n_overwrites} existing value(s) will be overwritten")
        }
      
        # Show preview of changes
        if (nrow(changes$preview) > 0) {
          cli::cli_h3("Preview of changes (first 5)")
          print(changes$preview, n = 5)
        }
      
        # Ask for confirmation unless force = TRUE
        if (!force && interactive()) {
          if (!is.null(save_changes)) {
            cli::cli_alert_info("Detailed change report will be saved to: {save_changes}")
          }
        
          if (!.ask_yes_no("Proceed with import?")) {
            cli::cli_alert_info("Import cancelled by user")
            return(invisible(NULL))
          }
        }
      
        # Save detailed change report if requested
        if (!is.null(save_changes)) {
          .save_change_report(changes$detailed, save_changes)
          cli::cli_alert_success("Change report saved to: {save_changes}")
        }
      
        # Perform import
        cli::cli_alert_info("Importing {nrow(data)} records to REDCap...")
        tryCatch({
          overwrite_behavior <- if (overwrite) "overwrite" else "normal"
          result <- .redcap_import_records(connection, data, overwrite_behavior)
          cli::cli_alert_success("Successfully imported {result$count} records")
          # Refresh cache after import for consistency
          project$refresh()
          invisible(result)
        }, error = function(e) {
          cli::cli_alert_danger("Failed to import data: {e$message}")
          stop("Import failed", call. = FALSE)
        })
    },
      piping = function(source_project, source_fields, target_fields, by = "record_id", overwrite = FALSE, force = FALSE, save_changes = NULL) {
      if (!inherits(source_project, "redcap_project")) {
        stop("source_project must be a redcap_project object")
      }
      
      if (length(source_fields) != length(target_fields)) {
        stop("source_fields and target_fields must have the same length")
      }
      
      cli::cli_alert_info("Piping data from source project to target project...")
      
      # Extract data from source project
      source_data <- source_project$data
      
      # Respect dynamic id field names
      src_id <- source_project$id_field %||% names(source_data)[1]
      tgt_id <- project$id_field %||% names(project$data)[1]
      
      if (missing(by) || is.null(by)) by <- tgt_id
      
      # Check if source fields exist
      missing_fields <- setdiff(source_fields, names(source_data))
      if (length(missing_fields) > 0) {
        stop("Source fields not found: ", paste(missing_fields, collapse = ", "))
      }
      
      # Check if by field exists in both
      if (!by %in% names(source_data)) {
        stop("Join field '", by, "' not found in source project (source id field is '", src_id, "')")
      }
      if (!by %in% names(project$data)) {
        stop("Join field '", by, "' not found in target project (target id field is '", tgt_id, "')")
      }
      
      # Select and rename fields
      pipe_data <- source_data[, c(by, source_fields), drop = FALSE]
      names(pipe_data) <- c(project$id_field, target_fields)
      # Ensure join id uses target's id field name
      names(pipe_data)[1] <- project$id_field
      
      cli::cli_alert_info("Piping {length(source_fields)} field(s) for {nrow(pipe_data)} record(s)")
      
    # Import to target project with change detection
    result <- project$import(pipe_data, overwrite = overwrite, force = force, save_changes = save_changes)
      
      cli::cli_alert_success("Successfully piped data from source to target project")
      invisible(result)
    },
    info = function() {
      cat("REDCap Project\n")
      cat("==============\n\n")
      cat("Title:", project_info$project_title, "\n")
      cat("URL:", stringr::str_remove(url, "/api/?$"), "\n")
    cat("Created:", format(project$.created_at, "%Y-%m-%d %H:%M:%S"), "\n")
      if (!is.null(data)) cat("Cached Data:", nrow(data), "records,", ncol(data), "fields\n") else cat("Cached Data: None\n")
      if (!is.null(metadata)) cat("Metadata:", nrow(metadata), "fields defined\n")
      cat("\nAccess data with: project$data\n")
      cat("Refresh data with: project$refresh()\n")
      cat("Import data with: project$import(data)\n")
      cat("Pipe data with: project$piping(source_project, source_fields, target_fields)\n")
      invisible(project)
    }
  )
  class(project) <- c("redcap_project", "sardine_project")
  project
}

# Internal helper functions --------------------------------------------------
.redcap_get_project_info <- function(connection) {
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "project",
      format = "json"
    ) %>%
    httr2::req_perform()
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response), ": ", httr2::resp_body_string(response))
  }
  httr2::resp_body_json(response)
}

.redcap_get_metadata <- function(connection) {
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "metadata",
      format = "json"
    ) %>%
    httr2::req_perform()
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response))
  }
  metadata <- httr2::resp_body_json(response, simplifyVector = TRUE)
  tibble::as_tibble(metadata)
}

.redcap_get_all_records <- function(connection) {
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "record",
      format = "json",
      type = "flat"
    ) %>%
    httr2::req_perform()
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response))
  }
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  tibble::as_tibble(data)
}

.redcap_import_records <- function(connection, data, overwrite = "normal") {
  # Convert data to JSON
  data_json <- jsonlite::toJSON(data, auto_unbox = TRUE, na = "null")
  
  response <- httr2::request(connection$url) %>%
    httr2::req_timeout(connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = connection$token,
      content = "record",
      format = "json",
      type = "flat",
      overwriteBehavior = overwrite,
      data = data_json
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("HTTP error ", httr2::resp_status(response), ": ", httr2::resp_body_string(response))
  }
  
  result <- httr2::resp_body_json(response)
  list(count = result$count)
}

# Helper function to analyze changes between current and import data
.analyze_import_changes <- function(current_data, import_data, allow_overwrite) {
  # Get record ID field from project convention: first column in current_data
  id_field <- if (ncol(current_data) > 0) names(current_data)[1] else "record_id"
  
  # Identify new vs existing records
  import_ids <- import_data[[id_field]]
  current_ids <- current_data[[id_field]]
  
  new_records <- setdiff(import_ids, current_ids)
  existing_records <- intersect(import_ids, current_ids)
  
  # Analyze changes for existing records
  changes_list <- list()
  n_overwrites <- 0
  
  for (record_id in existing_records) {
    import_row <- import_data[import_data[[id_field]] == record_id, , drop = FALSE]
    current_row <- current_data[current_data[[id_field]] == record_id, , drop = FALSE]
    
    # Compare each field
    for (field in names(import_row)) {
      if (field == id_field) next
      if (!field %in% names(current_row)) next
      
      import_val <- import_row[[field]]
      current_val <- current_row[[field]]
      
      # Check if value changed
      if (!identical(import_val, current_val)) {
        # Check if we're overwriting a non-NA value
        is_overwrite <- !is.na(current_val) && !is.na(import_val)
        if (is_overwrite) n_overwrites <- n_overwrites + 1
        
        changes_list[[length(changes_list) + 1]] <- data.frame(
          record_id = record_id,
          field = field,
          current_value = as.character(current_val),
          new_value = as.character(import_val),
          change_type = if (is_overwrite) "overwrite" else "fill",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Combine changes into a dataframe
  detailed <- if (length(changes_list) > 0) {
    do.call(rbind, changes_list)
  } else {
    data.frame(record_id = character(), field = character(), 
               current_value = character(), new_value = character(),
               change_type = character(), stringsAsFactors = FALSE)
  }
  
  # Create preview (show first few changes)
  preview <- if (nrow(detailed) > 0) {
    head(detailed, 5)
  } else {
    data.frame()
  }
  
  # Count affected fields
  affected_fields <- if (nrow(detailed) > 0) unique(detailed$field) else character()
  
  list(
    n_new_records = length(new_records),
    n_records_affected = length(existing_records),
    n_fields_affected = length(affected_fields),
    n_overwrites = n_overwrites,
    new_record_ids = new_records,
    affected_record_ids = existing_records,
    detailed = detailed,
    preview = preview
  )
}

# Helper function to ask yes/no questions
.ask_yes_no <- function(prompt) {
  if (!interactive()) return(FALSE)
  
  response <- readline(prompt = paste0(prompt, " (yes/no): "))
  tolower(trimws(response)) %in% c("y", "yes")
}

# Helper function to save change report
.save_change_report <- function(changes_df, filename) {
  if (nrow(changes_df) == 0) {
    cli::cli_alert_info("No changes to save")
    return(invisible(NULL))
  }
  
  # Determine format from filename
  if (grepl("\\.csv$", filename, ignore.case = TRUE)) {
    utils::write.csv(changes_df, filename, row.names = FALSE)
  } else if (grepl("\\.rds$", filename, ignore.case = TRUE)) {
    saveRDS(changes_df, filename)
  } else {
    # Default to CSV
    if (!grepl("\\.", filename)) {
      filename <- paste0(filename, ".csv")
    }
    utils::write.csv(changes_df, filename, row.names = FALSE)
  }
  
  invisible(filename)
}

#' Print Method for REDCap Projects
#'
#' @param x A redcap_project object
#' @param ... Additional arguments (unused)
#'
#' @export
print.redcap_project <- function(x, ...) {
  x$info()
  invisible(x)
}
