#' Export REDCap Events
#'
#' @description
#' Exports the list of events in a REDCap project.
#'
#' @param connection A redcap_connection object
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
#' events <- redcap_export_events(conn)
#' }
#'
#' @export
redcap_export_events <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  params <- list(
    content = "event",
    format = format
  )
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
