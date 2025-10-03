#' Create a REDCap API Connection
#'
#' @description
#' Creates a connection object for REDCap API interactions. This object stores
#' the necessary authentication information and can be used with other sardine
#' functions to interact with your REDCap project.
#'
#' @param url Character string. The REDCap API URL (typically ends with '/api/')
#' @param token Character string. Your REDCap API token
#' @param ssl_verify Logical. Whether to verify SSL certificates (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#'
#' @return A redcap_connection object (subclass of sardine_connection)
#'
#' @examples
#' \dontrun{
#' # Create connection using direct parameters
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Create connection using environment variables
#' conn <- redcap_connection(
#'   url = Sys.getenv("REDCAP_URL"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#' }
#'
#' @export
redcap_connection <- function(url, token, ssl_verify = TRUE, timeout = 30) {
  connection <- sardine_connection(
    source = "redcap",
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout
  )
  
  cli::cli_alert_success("REDCap connection created successfully")
  
  return(connection)
}