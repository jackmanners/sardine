#' Test Connection to Data Source
#'
#' @description
#' Generic function to test connections to various data sources. Dispatches
#' to source-specific testing methods.
#'
#' @param connection A sardine_connection object
#' @param ... Additional arguments passed to source-specific methods
#'
#' @return Logical. TRUE if connection is successful, FALSE otherwise
#'
#' @export
#' @examples
#' \dontrun{
#'   conn <- redcap_connection(url = "https://example/api/", token = "TOKEN")
#'   ok <- test_connection(conn)
#' }
test_connection <- function(connection, ...) {
  UseMethod("test_connection")
}

#' @export
test_connection.default <- function(connection, ...) {
  rlang::abort("No test_connection method for objects of this class")
}
