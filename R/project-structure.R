#' Summarise REDCap Project Structure
#'
#' Provides a clear summary of events, arms, instruments, and fields in a REDCap project.
#' Outputs to console and optionally as interactive tables.
#'
#' @param project A redcap_project object
#' @param interactive Logical. If TRUE, shows interactive DT tables (default: FALSE)
#' @return Invisibly returns a list with events, arms, instruments, and fields data frames
#' @export
summarise_project_structure <- function(project, interactive = FALSE) {
  # Events
  events <- if (!is.null(project$events)) project$events else data.frame()
  arms <- if (!is.null(project$arms)) project$arms else data.frame()
  instruments <- if (!is.null(project$instruments)) project$instruments else data.frame()
  fields <- if (!is.null(project$fields)) project$fields else data.frame()
  metadata <- if (!is.null(project$metadata)) project$metadata else data.frame()

  cat("\nREDCap Project Structure\n========================\n")

  # Arms
  if (nrow(arms) > 0) {
    cat("\nArms:\n")
    print(arms)
  } else {
    cat("\nNo arms found.\n")
  }

  # Events
  if (nrow(events) > 0) {
    cat("\nEvents:\n")
    print(events)
  } else {
    cat("\nNo events found.\n")
  }

  # Instruments
  if (nrow(instruments) > 0) {
    cat("\nInstruments (Forms):\n")
    print(instruments)
  } else {
    cat("\nNo instruments found.\n")
  }

  # Fields
  if (nrow(fields) > 0) {
    cat("\nFields:\n")
    print(head(fields, 20))
    if (nrow(fields) > 20) cat("... (showing first 20 of", nrow(fields), "fields)\n")
  } else if (nrow(metadata) > 0) {
    cat("\nFields (from metadata):\n")
    print(head(metadata, 20))
    if (nrow(metadata) > 20) cat("... (showing first 20 of", nrow(metadata), "fields)\n")
  } else {
    cat("\nNo fields found.\n")
  }

  # Interactive tables
  if (interactive) {
    if (requireNamespace("DT", quietly = TRUE)) {
      cat("\nInteractive tables in Viewer:\n")
      if (nrow(events) > 0) DT::datatable(events)
      if (nrow(arms) > 0) DT::datatable(arms)
      if (nrow(instruments) > 0) DT::datatable(instruments)
      if (nrow(fields) > 0) DT::datatable(fields)
      if (nrow(metadata) > 0) DT::datatable(metadata)
    } else {
      cat("\nInstall the DT package for interactive tables.\n")
    }
  }

  invisible(list(
    arms = arms,
    events = events,
    instruments = instruments,
    fields = fields,
    metadata = metadata
  ))
}
