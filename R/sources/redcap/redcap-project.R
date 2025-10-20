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
NULL