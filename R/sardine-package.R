#' sardine: Structured Architecture for Research Data Integration and Evaluation
#'
#' @description
#' The sardine package provides a modular, extensible interface for research data
#' integration and evaluation across multiple platforms. Built with a source-agnostic
#' architecture, it currently supports REDCap with plans for additional research
#' data platforms including Qualtrics, SurveyMonkey, and database connections.
#'
#' @section Package Architecture:
#' \describe{
#'   \item{Sources}{Platform-specific API integrations (REDCap, Qualtrics, etc.)}
#'   \item{Features}{Advanced cross-platform functionality and analytics}
#'   \item{Base Classes}{Common interfaces and utilities for all data sources}
#' }
#'
#' @section Core Functions:
#' \itemize{
#'   \item{\code{\link{redcap_project}}: Create a REDCap project object (tests connection, caches data)}

#'   \item{\code{\link{load_env}}: Load environment configuration from .env file}
#'   \item{\code{\link{create_env_template}}: Create .env template file}
#' }
#'
#' @section Data Access Functions:
#' \itemize{
#'   \item{\code{\link{export_records}}: Export specific records or fields}
#'   \item{\code{\link{import_records}}: Import data to REDCap (warns about cache)}
#'   \item{\code{\link{export_instruments}}: Export form/instrument information}
#'   \item{\code{\link{export_events}}: Export events for longitudinal projects}
#'   \item{\code{\link{export_users}}: Export project user information}
#'   \item{\code{\link{export_instrument_event_mappings}}: Export instrument-event mappings}
#' }
#'
#' @section Usage Pattern:
#' \preformatted{
#' # Create project (connects and caches data)
#' project <- redcap_project()
#' 
#' # Access full cached data
#' all_data <- project$data
#' 
#' # Export specific fields
#' demographics <- export_records(project, fields = c("age", "gender"))
#' 
#' # View project information
#' project$info()
#' }
#'
#' @keywords internal
"_PACKAGE"
#'
#' @importFrom httr2 request req_url_path_append req_headers req_body_form req_perform req_timeout
#' @importFrom httr2 resp_body_json resp_body_string resp_body_raw resp_status resp_is_error
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_info cli_alert_warning cli_ul
#' @importFrom rlang abort warn inform
#' @importFrom glue glue
#' @importFrom dplyr bind_rows mutate select filter arrange
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map_chr map_dfr
#' @importFrom stringr str_detect str_extract str_replace_all
#' @importFrom dotenv load_dot_env
NULL

# Suppress R CMD check notes about global variables
utils::globalVariables(c(".", "field_name", "field_type", "record_id"))