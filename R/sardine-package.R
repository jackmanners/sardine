#' sardine: Structured Architecture for Research Data Integration and Evaluation
#'
#' @description
#' The sardine package provides a structured interface for research data
#' integration and evaluation, with initial focus on REDCap API integration.
#' The package facilitates secure and efficient data exchange between R and
#' various research data platforms.
#'
#' @section Main functions:
#' \itemize{
#'   \item{\code{\link{redcap_connection}}: Create a REDCap API connection}
#'   \item{\code{\link{test_connection}}: Test API connection}
#'   \item{\code{\link{redcap_export_records}}: Export records from REDCap}
#'   \item{\code{\link{redcap_import_records}}: Import records to REDCap}
#'   \item{\code{\link{redcap_metadata}}: Get project metadata}
#'   \item{\code{\link{redcap_export_arms}}: Export arms for longitudinal projects}
#'   \item{\code{\link{redcap_export_events}}: Export events for longitudinal projects}
#'   \item{\code{\link{redcap_export_instruments}}: Export instruments (forms)}
#' }
#'
#' @docType package
#' @name sardine
#' @aliases sardine-package
#'
#' @importFrom httr2 request req_url_path_append req_headers req_body_form req_perform
#' @importFrom httr2 resp_body_json resp_body_string resp_body_raw resp_status resp_is_error
#' @importFrom httr2 curl_file
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_info cli_alert_warning
#' @importFrom rlang abort warn inform
#' @importFrom glue glue
#' @importFrom dplyr bind_rows mutate select filter arrange
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map_chr map_dfr
#' @importFrom stringr str_detect str_extract str_replace_all
NULL

# Suppress R CMD check notes about global variables
utils::globalVariables(c(".", "field_name", "field_type", "record_id"))