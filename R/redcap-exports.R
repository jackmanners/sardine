# Provide top-level implementations of the REDCap export functions so they are
# available during devtools::load_all(), even if subdirectories under R/ are
# not sourced recursively by the loader.

#' Export REDCap Arms
#'
#' Exports the list of arms in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character or numeric vector of specific arm numbers to export (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing arm information (when format = "json"); otherwise a character string
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
      result_raw <- httr2::resp_body_json(response, simplifyVector = TRUE)
      if (length(result_raw) == 0) return(tibble::tibble())
      tibble::as_tibble(result_raw)
    } else {
      httr2::resp_body_string(response)
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export arms: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export REDCap Events
#'
#' Exports the list of events in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character or numeric vector of arm numbers to export events for (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing event information (when format = "json"); otherwise a character string
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
      result_raw <- httr2::resp_body_json(response, simplifyVector = TRUE)
      if (length(result_raw) == 0) return(tibble::tibble())
      tibble::as_tibble(result_raw)
    } else {
      httr2::resp_body_string(response)
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export events: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export REDCap Instruments (Forms)
#'
#' Exports the list of instruments (forms) in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing instrument information (when format = "json"); otherwise a character string
#' @export
redcap_export_instruments <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  params <- list(
    content = "instrument",
    format = format
  )
  tryCatch({
    cli::cli_alert_info("Exporting instruments from REDCap...")
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response, simplifyVector = TRUE)
      if (length(result_raw) == 0) return(tibble::tibble())
      tibble::as_tibble(result_raw)
    } else {
      httr2::resp_body_string(response)
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export instruments: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export Instrument Event Mappings
#'
#' Exports the instrument-event mappings for a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param arms Character or numeric vector of arm numbers to filter by (optional)
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing instrument-event mappings (when format = "json"); otherwise a character string
#' @export
redcap_export_instrument_event_mappings <- function(connection, arms = NULL, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  params <- list(
    content = "formEventMapping",
    format = format
  )
  if (!is.null(arms)) {
    if (!is.character(arms) && !is.numeric(arms)) {
      rlang::abort("arms must be a character or numeric vector")
    }
    params$arms <- paste(arms, collapse = ",")
  }
  tryCatch({
    cli::cli_alert_info("Exporting instrument-event mappings from REDCap...")
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response, simplifyVector = TRUE)
      if (length(result_raw) == 0) return(tibble::tibble())
      tibble::as_tibble(result_raw)
    } else {
      httr2::resp_body_string(response)
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export instrument-event mappings: {e$message}")
    rlang::abort(e$message)
  })
}
