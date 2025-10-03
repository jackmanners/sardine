#' Deprecated Functions
#'
#' @description
#' These functions are deprecated in favor of the new object-oriented
#' approach using redcap_project objects.
#'
#' @name sardine-deprecated
NULL

#' @rdname sardine-deprecated
#' @export
redcap_connection <- function(...) {
  .Deprecated("redcap_project", package = "sardine", 
              msg = "redcap_connection() is deprecated. Use redcap_project() instead for better caching and object-oriented interface.")
  
  # Still provide the old functionality for backward compatibility
  sardine::source_connection(source_type = "redcap", ...)
}

#' @rdname sardine-deprecated
#' @export  
redcap_export_records <- function(connection, ...) {
  .Deprecated("export_records", package = "sardine",
              msg = "redcap_export_records() is deprecated. Create a redcap_project object and use export_records() instead.")
  
  # Convert old connection to project-like object temporarily
  if (inherits(connection, "source_connection")) {
    # This is a basic fallback - users should migrate to new approach
    stop("Please migrate to the new redcap_project() approach. See package documentation for examples.")
  }
}

#' @rdname sardine-deprecated
#' @export
redcap_import_records <- function(connection, ...) {
  .Deprecated("import_records", package = "sardine",
              msg = "redcap_import_records() is deprecated. Create a redcap_project object and use import_records() instead.")
  
  stop("Please migrate to the new redcap_project() approach. See package documentation for examples.")
}

#' @rdname sardine-deprecated
#' @export
redcap_export_instruments <- function(connection, ...) {
  .Deprecated("export_instruments", package = "sardine",
              msg = "redcap_export_instruments() is deprecated. Create a redcap_project object and use export_instruments() instead.")
  
  stop("Please migrate to the new redcap_project() approach. See package documentation for examples.")
}

#' @rdname sardine-deprecated
#' @export
redcap_export_events <- function(connection, ...) {
  .Deprecated("export_events", package = "sardine",
              msg = "redcap_export_events() is deprecated. Create a redcap_project object and use export_events() instead.")
  
  stop("Please migrate to the new redcap_project() approach. See package documentation for examples.")
}

#' @rdname sardine-deprecated
#' @export
redcap_export_users <- function(connection, ...) {
  .Deprecated("export_users", package = "sardine",
              msg = "redcap_export_users() is deprecated. Create a redcap_project object and use export_users() instead.")
  
  stop("Please migrate to the new redcap_project() approach. See package documentation for examples.")
}

#' @rdname sardine-deprecated
#' @export
test_connection <- function(...) {
  .Deprecated("redcap_project", package = "sardine",
              msg = "test_connection() is deprecated. Connection testing is now automatic during redcap_project() creation.")
  
  stop("Connection testing is now automatic. Use redcap_project() which will fail fast if connection issues exist.")
}