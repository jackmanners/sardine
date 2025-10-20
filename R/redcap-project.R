#' Create REDCap Project
#'
#' @description
#' Creates a REDCap project object. Can accept parameters directly or 
#' fall back to environment variables if not provided. This is the main
#' function for creating REDCap projects in sardine.
#'
#' @param url Character string. REDCap API URL. If NULL, uses REDCAP_URL environment variable
#' @param token Character string. REDCap API token. If NULL, uses REDCAP_TOKEN environment variable  
#' @param ssl_verify Logical. SSL verification (default: TRUE)
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#' @param env_prefix Character string. Environment variable prefix (default: "REDCAP")
#'
#' @return A redcap_project object
#' @export
#'
#' @examples
#' \dontrun{
#' # Direct parameters
#' project <- redcap_project(
#'   url = "https://redcap.example.edu/api/",
#'   token = "your_token_here"
#' )
#' 
#' # Environment variables (REDCAP_URL and REDCAP_TOKEN)
#' project <- redcap_project()
#' 
#' # Mix of direct and environment
#' project <- redcap_project(url = "https://redcap.example.edu/api/")
#' }
redcap_project <- function(url = NULL, token = NULL, ssl_verify = TRUE, 
                           timeout = 30, env_prefix = "REDCAP") {
  # Get environment variable names
  url_var <- paste0(env_prefix, "_URL")
  token_var <- paste0(env_prefix, "_TOKEN")

  # Use provided parameters or fall back to environment variables
  if (is.null(url)) {
    url <- Sys.getenv(url_var)
    if (url == "") {
      stop(paste("URL must be provided directly or set in", url_var, "environment variable"))
    }
    cli::cli_alert_info("Using URL from {url_var} environment variable")
  }

  if (is.null(token)) {
    token <- Sys.getenv(token_var)
    if (token == "") {
      stop(paste("Token must be provided directly or set in", token_var, "environment variable"))
    }
    cli::cli_alert_info("Using token from {token_var} environment variable")
  }

  # Resolve internal function robustly
  internal <- get0(".redcap_project_internal", mode = "function")
  if (is.null(internal) && "sardine" %in% loadedNamespaces()) {
    internal <- get0(".redcap_project_internal", envir = asNamespace("sardine"), inherits = FALSE)
  }
  if (is.null(internal)) {
    stop("Internal function not available. Ensure the package is loaded (devtools::load_all) or source 'R/sources/redcap/project.R'.", call. = FALSE)
  }

  # Create the project using the internal function
  internal(
    url = url,
    token = token, 
    ssl_verify = ssl_verify,
    timeout = timeout
  )
}
