#' Export REDCap Arms
#'
#' @description
#' Exports the arms for a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character vector of specific arm numbers to export (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing arm information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all arms
#' arms <- redcap_export_arms(conn)
#' }
#'
#' @export
redcap_export_arms <- function(connection, arms = NULL, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "arm",
    format = format
  )
  
  if (!is.null(arms)) {
    if (!is.character(arms) && !is.numeric(arms)) {
      rlang::abort("arms must be a character or numeric vector")
    }
    params$arms <- paste(arms, collapse = ",")
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting arms from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response)
      if (length(result_raw) == 0) {
        return(tibble::tibble())
      }
      result <- purrr::map_dfr(result_raw, ~ tibble::as_tibble(lapply(.x, as.character)))
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported arms")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export arms: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import REDCap Arms
#'
#' @description
#' Imports arms to a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing arm information
#' @param override Logical. Whether to override existing arms (default: FALSE)
#'
#' @return Number of arms imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import arms
#' arm_data <- data.frame(
#'   arm_num = c(1, 2),
#'   name = c("Arm 1", "Arm 2")
#' )
#' result <- redcap_import_arms(conn, arm_data)
#' }
#'
#' @export
redcap_import_arms <- function(connection, data, override = FALSE) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "arm",
    action = "import",
    format = "json",
    data = data_string,
    override = as.integer(override)
  )
  
  tryCatch({
    cli::cli_alert_info("Importing arms to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported arms")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import arms: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete REDCap Arms
#'
#' @description
#' Deletes arms from a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character or numeric vector of arm numbers to delete
#'
#' @return Number of arms deleted
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete specific arms
#' result <- redcap_delete_arms(conn, c(2, 3))
#' }
#'
#' @export
redcap_delete_arms <- function(connection, arms) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(arms) || is.null(arms)) {
    rlang::abort("arms is required")
  }
  
  if (!is.character(arms) && !is.numeric(arms)) {
    rlang::abort("arms must be a character or numeric vector")
  }
  
  params <- list(
    content = "arm",
    action = "delete",
    arms = paste(arms, collapse = ",")
  )
  
  tryCatch({
    cli::cli_alert_info("Deleting arms from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted arms")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete arms: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export REDCap Events
#'
#' @description
#' Exports the events for a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character vector of arm numbers to export events for (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing event information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all events
#' events <- redcap_export_events(conn)
#' }
#'
#' @export
redcap_export_events <- function(connection, arms = NULL, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "event",
    format = format
  )
  
  if (!is.null(arms)) {
    if (!is.character(arms) && !is.numeric(arms)) {
      rlang::abort("arms must be a character or numeric vector")
    }
    params$arms <- paste(arms, collapse = ",")
  }
  
  tryCatch({
    cli::cli_alert_info("Exporting events from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response)
      if (length(result_raw) == 0) {
        return(tibble::tibble())
      }
      result <- purrr::map_dfr(result_raw, ~ tibble::as_tibble(lapply(.x, as.character)))
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported events")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export events: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import REDCap Events
#'
#' @description
#' Imports events to a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing event information
#' @param override Logical. Whether to override existing events (default: FALSE)
#'
#' @return Number of events imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import events
#' event_data <- data.frame(
#'   event_name = c("baseline", "followup"),
#'   arm_num = c(1, 1),
#'   day_offset = c(0, 30),
#'   offset_min = c(0, 0),
#'   offset_max = c(0, 0),
#'   unique_event_name = c("baseline_arm_1", "followup_arm_1")
#' )
#' result <- redcap_import_events(conn, event_data)
#' }
#'
#' @export
redcap_import_events <- function(connection, data, override = FALSE) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "event",
    action = "import",
    format = "json",
    data = data_string,
    override = as.integer(override)
  )
  
  tryCatch({
    cli::cli_alert_info("Importing events to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported events")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import events: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete REDCap Events
#'
#' @description
#' Deletes events from a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param events Character vector of unique event names to delete
#'
#' @return Number of events deleted
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete specific events
#' result <- redcap_delete_events(conn, c("followup_arm_1", "month6_arm_1"))
#' }
#'
#' @export
redcap_delete_events <- function(connection, events) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(events) || is.null(events)) {
    rlang::abort("events is required")
  }
  
  if (!is.character(events)) {
    rlang::abort("events must be a character vector")
  }
  
  params <- list(
    content = "event",
    action = "delete",
    events = paste(events, collapse = ",")
  )
  
  tryCatch({
    cli::cli_alert_info("Deleting events from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted events")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete events: {e$message}")
    rlang::abort(e$message)
  })
}