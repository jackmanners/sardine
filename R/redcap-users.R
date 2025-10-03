#' Export REDCap Users
#'
#' @description
#' Exports the list of users in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing user information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all users
#' users <- redcap_export_users(conn)
#' }
#'
#' @export
redcap_export_users <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "user",
    format = format
  )
  
  tryCatch({
    cli::cli_alert_info("Exporting users from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    if (format == "json") {
      result_raw <- httr2::resp_body_json(response)
      if (length(result_raw) == 0) {
        return(tibble::tibble())
      }
      result <- purrr::map_dfr(result_raw, ~ tibble::as_tibble(lapply(.x, as.character)))
    } else {
      result <- httr2::resp_body_string(response)
    }
    
    cli::cli_alert_success("Successfully exported users")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export users: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import REDCap Users
#'
#' @description
#' Imports users to a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing user information
#'
#' @return Number of users imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import users
#' user_data <- data.frame(
#'   username = c("user1", "user2"),
#'   email = c("user1@example.com", "user2@example.com"),
#'   firstname = c("John", "Jane"),
#'   lastname = c("Doe", "Smith"),
#'   expiration = c("", ""),
#'   data_access_group = c("", ""),
#'   design = c(0, 1),
#'   alerts = c(1, 1),
#'   user_rights = c(1, 0),
#'   data_access_groups = c(0, 1),
#'   reports = c(1, 1),
#'   stats_and_charts = c(1, 0),
#'   manage_survey_participants = c(0, 0),
#'   calendar = c(1, 1),
#'   data_import_tool = c(0, 1),
#'   data_comparison_tool = c(0, 0),
#'   logging = c(1, 1),
#'   file_repository = c(1, 0),
#'   data_quality_create = c(0, 1),
#'   data_quality_execute = c(1, 1),
#'   api_export = c(1, 0),
#'   api_import = c(0, 1),
#'   mobile_app = c(0, 0),
#'   mobile_app_download_data = c(0, 0),
#'   record_create = c(1, 1),
#'   record_rename = c(0, 0),
#'   record_delete = c(0, 0),
#'   lock_records_customization = c(0, 0),
#'   lock_records = c(0, 0),
#'   lock_records_all_forms = c(0, 0),
#'   forms = c("", "")
 )
 result <- redcap_import_users(conn, user_data)
 }

 @export\nredcap_import_users <- function(connection, data) {\n  if (!inherits(connection, \"redcap_connection\")) {\n    rlang::abort(\"connection must be a redcap_connection object\")\n  }\n  \n  if (missing(data) || is.null(data)) {\n    rlang::abort(\"data is required\")\n  }\n  \n  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)\n  \n  params <- list(\n    content = \"user\",\n    action = \"import\",\n    format = \"json\",\n    data = data_string\n  )\n  \n  tryCatch({\n    cli::cli_alert_info(\"Importing users to REDCap...\")\n    \n    response <- do.call(.redcap_request, c(list(connection = connection), params))\n    \n    if (httr2::resp_is_error(response)) {\n      error_msg <- httr2::resp_body_string(response)\n      rlang::abort(glue::glue(\"REDCap API error: {error_msg}\"))\n    }\n    \n    result <- httr2::resp_body_string(response)\n    \n    cli::cli_alert_success(\"Successfully imported users\")\n    return(result)\n    \n  }, error = function(e) {\n    cli::cli_alert_danger(\"Failed to import users: {e$message}\")\n    rlang::abort(e$message)\n  })\n}\n\n#' Delete REDCap Users\n#'\n#' @description\n#' Deletes users from a REDCap project.\n#'\n#' @param connection A redcap_connection object\n#' @param users Character vector of usernames to delete\n#'\n#' @return Number of users deleted\n#'\n#' @examples\n#' \\dontrun{\n#' conn <- redcap_connection(\n#'   url = \"https://redcap.example.edu/api/\",\n#'   token = \"YOUR_API_TOKEN\"\n#' )\n#'\n#' # Delete specific users\n#' result <- redcap_delete_users(conn, c(\"user1\", \"user2\"))\n#' }\n#'\n#' @export\nredcap_delete_users <- function(connection, users) {\n  if (!inherits(connection, \"redcap_connection\")) {\n    rlang::abort(\"connection must be a redcap_connection object\")\n  }\n  \n  if (missing(users) || is.null(users)) {\n    rlang::abort(\"users is required\")\n  }\n  \n  if (!is.character(users)) {\n    rlang::abort(\"users must be a character vector\")\n  }\n  \n  params <- list(\n    content = \"user\",\n    action = \"delete\",\n    users = paste(users, collapse = \",\")\n  )\n  \n  tryCatch({\n    cli::cli_alert_info(\"Deleting users from REDCap...\")\n    \n    response <- do.call(.redcap_request, c(list(connection = connection), params))\n    \n    if (httr2::resp_is_error(response)) {\n      error_msg <- httr2::resp_body_string(response)\n      rlang::abort(glue::glue(\"REDCap API error: {error_msg}\"))\n    }\n    \n    result <- httr2::resp_body_string(response)\n    \n    cli::cli_alert_success(\"Successfully deleted users\")\n    return(result)\n    \n  }, error = function(e) {\n    cli::cli_alert_danger(\"Failed to delete users: {e$message}\")\n    rlang::abort(e$message)\n  })\n}\n\n#' Export REDCap User Roles\n#'\n#' @description\n#' Exports the user roles defined in a REDCap project.\n#'\n#' @param connection A redcap_connection object\n#' @param format Character string. Format for returned data: \"json\" (default), \"csv\", \"xml\"\n#'\n#' @return A tibble containing user role information\n#'\n#' @examples\n#' \\dontrun{\n#' conn <- redcap_connection(\n#'   url = \"https://redcap.example.edu/api/\",\n#'   token = \"YOUR_API_TOKEN\"\n#' )\n#'\n#' # Export all user roles\n#' roles <- redcap_export_user_roles(conn)\n#' }\n#'\n#' @export\nredcap_export_user_roles <- function(connection, format = \"json\") {\n  if (!inherits(connection, \"redcap_connection\")) {\n    rlang::abort(\"connection must be a redcap_connection object\")\n  }\n  \n  params <- list(\n    content = \"userRole\",\n    format = format\n  )\n  \n  tryCatch({\n    cli::cli_alert_info(\"Exporting user roles from REDCap...\")\n    \n    response <- do.call(.redcap_request, c(list(connection = connection), params))\n    \n    if (httr2::resp_is_error(response)) {\n      error_msg <- httr2::resp_body_string(response)\n      rlang::abort(glue::glue(\"REDCap API error: {error_msg}\"))\n    }\n    \n    if (format == \"json\") {\n      result_raw <- httr2::resp_body_json(response)\n      if (length(result_raw) == 0) {\n        return(tibble::tibble())\n      }\n      result <- purrr::map_dfr(result_raw, ~ tibble::as_tibble(lapply(.x, as.character)))\n    } else {\n      result <- httr2::resp_body_string(response)\n    }\n    \n    cli::cli_alert_success(\"Successfully exported user roles\")\n    return(result)\n    \n  }, error = function(e) {\n    cli::cli_alert_danger(\"Failed to export user roles: {e$message}\")\n    rlang::abort(e$message)\n  })\n}\n\n#' Import REDCap User Roles\n#'\n#' @description\n#' Imports user roles to a REDCap project.\n#'\n#' @param connection A redcap_connection object\n#' @param data A data.frame containing user role information\n#'\n#' @return Number of user roles imported\n#'\n#' @examples\n#' \\dontrun{\n#' conn <- redcap_connection(\n#'   url = \"https://redcap.example.edu/api/\",\n#'   token = \"YOUR_API_TOKEN\"\n#' )\n#'\n#' # Import user roles\n#' role_data <- data.frame(\n#'   unique_role_name = c(\"coordinator\", \"data_entry\"),\n#'   role_label = c(\"Study Coordinator\", \"Data Entry Staff\"),\n#'   design = c(1, 0),\n#'   alerts = c(1, 0),\n#'   user_rights = c(1, 0),\n#'   data_access_groups = c(1, 0),\n#'   reports = c(1, 1),\n#'   stats_and_charts = c(1, 0),\n#'   manage_survey_participants = c(1, 0),\n#'   calendar = c(1, 1),\n#'   data_import_tool = c(1, 0),\n#'   data_comparison_tool = c(1, 0),\n#'   logging = c(1, 0),\n#'   file_repository = c(1, 1),\n#'   data_quality_create = c(1, 0),\n#'   data_quality_execute = c(1, 1),\n#'   api_export = c(1, 0),\n#'   api_import = c(1, 0),\n#'   mobile_app = c(0, 0),\n#'   mobile_app_download_data = c(0, 0),\n#'   record_create = c(1, 1),\n#'   record_rename = c(1, 0),\n#'   record_delete = c(1, 0),\n#'   lock_records_customization = c(1, 0),\n#'   lock_records = c(1, 0),\n#'   lock_records_all_forms = c(1, 0),\n#'   forms = c(\"\", \"\")\n#' )\n#' result <- redcap_import_user_roles(conn, role_data)\n#' }\n#'\n#' @export\nredcap_import_user_roles <- function(connection, data) {\n  if (!inherits(connection, \"redcap_connection\")) {\n    rlang::abort(\"connection must be a redcap_connection object\")\n  }\n  \n  if (missing(data) || is.null(data)) {\n    rlang::abort(\"data is required\")\n  }\n  \n  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)\n  \n  params <- list(\n    content = \"userRole\",\n    action = \"import\",\n    format = \"json\",\n    data = data_string\n  )\n  \n  tryCatch({\n    cli::cli_alert_info(\"Importing user roles to REDCap...\")\n    \n    response <- do.call(.redcap_request, c(list(connection = connection), params))\n    \n    if (httr2::resp_is_error(response)) {\n      error_msg <- httr2::resp_body_string(response)\n      rlang::abort(glue::glue(\"REDCap API error: {error_msg}\"))\n    }\n    \n    result <- httr2::resp_body_string(response)\n    \n    cli::cli_alert_success(\"Successfully imported user roles\")\n    return(result)\n    \n  }, error = function(e) {\n    cli::cli_alert_danger(\"Failed to import user roles: {e$message}\")\n    rlang::abort(e$message)\n  })\n}\n\n#' Delete REDCap User Roles\n#'\n#' @description\n#' Deletes user roles from a REDCap project.\n#'\n#' @param connection A redcap_connection object\n#' @param roles Character vector of unique role names to delete\n#'\n#' @return Number of user roles deleted\n#'\n#' @examples\n#' \\dontrun{\n#' conn <- redcap_connection(\n#'   url = \"https://redcap.example.edu/api/\",\n#'   token = \"YOUR_API_TOKEN\"\n#' )\n#'\n#' # Delete specific user roles\n#' result <- redcap_delete_user_roles(conn, c(\"old_role1\", \"old_role2\"))\n#' }\n#'\n#' @export\nredcap_delete_user_roles <- function(connection, roles) {\n  if (!inherits(connection, \"redcap_connection\")) {\n    rlang::abort(\"connection must be a redcap_connection object\")\n  }\n  \n  if (missing(roles) || is.null(roles)) {\n    rlang::abort(\"roles is required\")\n  }\n  \n  if (!is.character(roles)) {\n    rlang::abort(\"roles must be a character vector\")\n  }\n  \n  params <- list(\n    content = \"userRole\",\n    action = \"delete\",\n    roles = paste(roles, collapse = \",\")\n  )\n  \n  tryCatch({\n    cli::cli_alert_info(\"Deleting user roles from REDCap...\")\n    \n    response <- do.call(.redcap_request, c(list(connection = connection), params))\n    \n    if (httr2::resp_is_error(response)) {\n      error_msg <- httr2::resp_body_string(response)\n      rlang::abort(glue::glue(\"REDCap API error: {error_msg}\"))\n    }\n    \n    result <- httr2::resp_body_string(response)\n    \n    cli::cli_alert_success(\"Successfully deleted user roles\")\n    return(result)\n    \n  }, error = function(e) {\n    cli::cli_alert_danger(\"Failed to delete user roles: {e$message}\")\n    rlang::abort(e$message)\n  })\n}"