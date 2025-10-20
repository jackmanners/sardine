#' Export Records from REDCap Project
#'
#' @description
#' Export specific records or fields from a REDCap project object.
#' This provides more targeted data access than the cached full dataset.
#'
#' @param project A redcap_project object created by \code{\link{redcap_project}}
#' @param records Character vector. Specific record IDs to export (optional)
#' @param fields Character vector. Specific fields to export (optional)
#' @param events Character vector. Specific events to export for longitudinal projects (optional)
#' @param forms Character vector. Specific forms/instruments to export (optional)
#' @param return_format Character. Format for returned data ("json", "csv", "xml")
#' @param type Character. Type of records ("flat", "eav")
#'
#' @return A tibble with the requested data
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#'
#' # Export specific fields
#' demographics <- export_records(project, fields = c("record_id", "age", "gender"))
#'
#' # Export specific records
#' subset <- export_records(project, records = c("001", "002", "003"))
#'
#' # Export specific form
#' baseline <- export_records(project, forms = "baseline_survey")
#' }
#'
#' @export
export_records <- function(project, records = NULL, fields = NULL, events = NULL, 
                          forms = NULL, return_format = "json", type = "flat") {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  # Build request parameters
  params <- list(
    token = project$.connection$token,
    content = "record",
    format = return_format,
    type = type
  )
  
  if (!is.null(records)) {
    params$records <- paste(records, collapse = ",")
  }
  
  if (!is.null(fields)) {
    params$fields <- paste(fields, collapse = ",")
  }
  
  if (!is.null(events)) {
    params$events <- paste(events, collapse = ",")
  }
  
  if (!is.null(forms)) {
    params$forms <- paste(forms, collapse = ",")
  }
  
  # Make request
  response <- httr2::request(project$.connection$url) %>%
    httr2::req_timeout(project$.connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(!!!params) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("Failed to export records: HTTP ", httr2::resp_status(response))
  }
  
  # Parse response
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  
  if (length(data) == 0) {
    cli::cli_alert_warning("No records found matching the criteria")
    return(tibble::tibble())
  }
  
  result <- tibble::as_tibble(data)
  cli::cli_alert_success("Exported {nrow(result)} records with {ncol(result)} fields")
  
  return(result)
}

#' Import Records to REDCap Project
#'
#' @description
#' Import data records to a REDCap project. This function warns about
#' cached data potentially being outdated after import.
#'
#' @param project A redcap_project object created by \code{\link{redcap_project}}
#' @param data Data frame or tibble with records to import
#' @param overwrite_behavior Character. How to handle existing data ("normal", "overwrite")
#' @param return_content Character. What to return ("count", "ids", "nothing")
#' @param return_format Character. Format for returned data ("json", "csv", "xml")
#' @param date_format Character. Format for dates ("YMD", "MDY", "DMY")
#'
#' @return Response from REDCap API (depends on return_content parameter)
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#'
#' # Import new records
#' new_data <- tibble::tibble(
#'   record_id = c("001", "002"),
#'   age = c(25, 30),
#'   gender = c("M", "F")
#' )
#'
#' result <- import_records(project, new_data)
#' }
#'
#' @export
import_records <- function(project, data, overwrite_behavior = "normal", 
                          return_content = "count", return_format = "json",
                          date_format = "YMD") {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  if (missing(data) || is.null(data) || nrow(data) == 0) {
    stop("data is required and cannot be empty")
  }
  
  # Warning about cached data
  cli::cli_alert_warning("Importing data to REDCap will make cached project data outdated")
  cli::cli_alert_info("Consider running project$refresh() after import to update cached data")
  
  # Convert data to JSON
  data_json <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  # Make request
  response <- httr2::request(project$.connection$url) %>%
    httr2::req_timeout(project$.connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = project$.connection$token,
      content = "record",
      format = return_format,
      type = "flat",
      overwriteBehavior = overwrite_behavior,
      data = data_json,
      returnContent = return_content,
      dateFormat = date_format
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("Failed to import records: HTTP ", httr2::resp_status(response))
  }
  
  # Parse response
  result <- httr2::resp_body_json(response, simplifyVector = TRUE)
  
  cli::cli_alert_success("Successfully imported {nrow(data)} records")
  cli::cli_alert_info("Remember to refresh cached data: project$refresh()")
  
  return(result)
}

#' Export Instruments from REDCap Project
#'
#' @description
#' Export information about data collection instruments (forms) in the project.
#'
#' @param project A redcap_project object created by \code{\link{redcap_project}}
#' @param return_format Character. Format for returned data ("json", "csv", "xml")
#'
#' @return A tibble with instrument information
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' instruments <- export_instruments(project)
#' }
#'
#' @export
export_instruments <- function(project, return_format = "json") {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  response <- httr2::request(project$.connection$url) %>%
    httr2::req_timeout(project$.connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = project$.connection$token,
      content = "instrument",
      format = return_format
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("Failed to export instruments: HTTP ", httr2::resp_status(response))
  }
  
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  return(tibble::as_tibble(data))
}

#' Export Events from REDCap Project
#'
#' @description
#' Export event information for longitudinal REDCap projects.
#'
#' @param project A redcap_project object created by \code{\link{redcap_project}}
#' @param arms Character vector. Specific arms to export events for (optional)
#' @param return_format Character. Format for returned data ("json", "csv", "xml")
#'
#' @return A tibble with event information
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' events <- export_events(project)
#' }
#'
#' @export
export_events <- function(project, arms = NULL, return_format = "json") {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  params <- list(
    token = project$.connection$token,
    content = "event",
    format = return_format
  )
  
  if (!is.null(arms)) {
    params$arms <- paste(arms, collapse = ",")
  }
  
  response <- httr2::request(project$.connection$url) %>%
    httr2::req_timeout(project$.connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(!!!params) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("Failed to export events: HTTP ", httr2::resp_status(response))
  }
  
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  return(tibble::as_tibble(data))
}

#' Export Project Users
#'
#' @description
#' Export user information from a REDCap project.
#'
#' @param project A redcap_project object created by \code{\link{redcap_project}}
#' @param return_format Character. Format for returned data ("json", "csv", "xml")
#'
#' @return A tibble with user information
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' users <- export_users(project)
#' }
#'
#' @export
export_users <- function(project, return_format = "json") {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  response <- httr2::request(project$.connection$url) %>%
    httr2::req_timeout(project$.connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(
      token = project$.connection$token,
      content = "user",
      format = return_format
    ) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("Failed to export users: HTTP ", httr2::resp_status(response))
  }
  
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  return(tibble::as_tibble(data))
}

#' Export Instrument Event Mappings
#'
#' @description
#' Export the mapping between instruments and events for longitudinal projects.
#'
#' @param project A redcap_project object created by \code{\link{redcap_project}}
#' @param arms Character vector. Specific arms to export mappings for (optional)
#' @param return_format Character. Format for returned data ("json", "csv", "xml")
#'
#' @return A tibble with instrument-event mappings
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' mappings <- export_instrument_event_mappings(project)
#' }
#'
#' @export
export_instrument_event_mappings <- function(project, arms = NULL, return_format = "json") {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  params <- list(
    token = project$.connection$token,
    content = "formEventMapping",
    format = return_format
  )
  
  if (!is.null(arms)) {
    params$arms <- paste(arms, collapse = ",")
  }
  
  response <- httr2::request(project$.connection$url) %>%
    httr2::req_timeout(project$.connection$timeout) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_body_form(!!!params) %>%
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    stop("Failed to export instrument event mappings: HTTP ", httr2::resp_status(response))
  }
  
  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  return(tibble::as_tibble(data))
}