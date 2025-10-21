#' Display an object as a DT datatable
#'
#' Generic helper to show any supported sardine object as an interactive DT table.
#' Works for data frames, table_one, reports, metadata, etc.
#'
#' @param x Object to display (data frame, table_one, report, etc.)
#' @param ... Additional arguments passed to DT::datatable
#' @return DT datatable (in Viewer)
#' @export
as_datatable <- function(x, ...) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT package is required for interactive tables. Please install.packages('DT')")
  }
  if (is.data.frame(x)) {
    DT::datatable(x, ...)
  } else if (inherits(x, "table_one") && !is.null(attr(x, "table"))) {
    DT::datatable(attr(x, "table"), ...)
  } else if (inherits(x, "event_completion_summary") && !is.null(x$summary)) {
    DT::datatable(x$summary, ...)
  } else if (inherits(x, "retention_summary") && !is.null(x$summary)) {
    DT::datatable(x$summary, ...)
  } else {
    stop("Object type not supported for DT display.")
  }
}
