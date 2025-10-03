#' REDCap Reporting Functions
#'
#' @description
#' This file provides access to all REDCap-specific reporting and
#' analysis functions in the sardine package.
#'
#' @section Available Reports:
#' \describe{
#'   \item{Completion Reports}{Participant completion status analysis}
#'   \item{Quick Reports}{Fast summary reports for project overview}
#'   \item{Simple Functions}{Basic data extraction and analysis utilities}
#'   \item{Test Reports}{Connection testing and validation reports}
#' }
#'
#' @section Usage:
#' All reporting functions require a valid REDCap connection object.
#' Create one using \code{\link{redcap_connection}} or 
#' \code{\link{redcap_connection_from_env}}.
#'
#' @examples
#' \dontrun{
#' # Create connection
#' conn <- redcap_connection_from_env()
#' 
#' # Generate quick completion report
#' report <- quick_completion_report(conn)
#' 
#' # Run comprehensive completion analysis
#' analysis <- generate_completion_report(conn)
#' }
#'
#' @name redcap-reports
NULL

# Note: The actual function exports will be handled by the individual
# report files. This file serves as documentation and organization.