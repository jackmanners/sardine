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
#' )
#' result <- redcap_import_users(conn, user_data)
#' }
#'
#' @export
redcap_import_users <- function(connection, data) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "user",
    action = "import",
    format = "json",
    data = data_string
  )
  
  tryCatch({
    cli::cli_alert_info("Importing users to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported users")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import users: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete REDCap Users
#'
#' @description
#' Deletes users from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param users Character vector of usernames to delete
#'
#' @return Number of users deleted
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete specific users
#' result <- redcap_delete_users(conn, c("user1", "user2"))
#' }
#'
#' @export
redcap_delete_users <- function(connection, users) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(users) || is.null(users)) {
    rlang::abort("users is required")
  }
  
  if (!is.character(users)) {
    rlang::abort("users must be a character vector")
  }
  
  params <- list(
    content = "user",
    action = "delete",
    users = paste(users, collapse = ",")
  )
  
  tryCatch({
    cli::cli_alert_info("Deleting users from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted users")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete users: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export REDCap User Roles
#'
#' @description
#' Exports the user roles defined in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing user role information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all user roles
#' roles <- redcap_export_user_roles(conn)
#' }
#'
#' @export
redcap_export_user_roles <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "userRole",
    format = format
  )
  
  tryCatch({
    cli::cli_alert_info("Exporting user roles from REDCap...")
    
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
    
    cli::cli_alert_success("Successfully exported user roles")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export user roles: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import REDCap User Roles
#'
#' @description
#' Imports user roles to a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing user role information
#'
#' @return Number of user roles imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import user roles
#' role_data <- data.frame(
#'   unique_role_name = c("coordinator", "data_entry"),
#'   role_label = c("Study Coordinator", "Data Entry Staff"),
#'   design = c(1, 0),
#'   alerts = c(1, 0),
#'   user_rights = c(1, 0),
#'   data_access_groups = c(1, 0),
#'   reports = c(1, 1),
#'   stats_and_charts = c(1, 0),
#'   manage_survey_participants = c(1, 0),
#'   calendar = c(1, 1),
#'   data_import_tool = c(1, 0),
#'   data_comparison_tool = c(1, 0),
#'   logging = c(1, 0),
#'   file_repository = c(1, 1),
#'   data_quality_create = c(1, 0),
#'   data_quality_execute = c(1, 1),
#'   api_export = c(1, 0),
#'   api_import = c(1, 0),
#'   mobile_app = c(0, 0),
#'   mobile_app_download_data = c(0, 0),
#'   record_create = c(1, 1),
#'   record_rename = c(1, 0),
#'   record_delete = c(1, 0),
#'   lock_records_customization = c(1, 0),
#'   lock_records = c(1, 0),
#'   lock_records_all_forms = c(1, 0),
#'   forms = c("", "")
#' )
#' result <- redcap_import_user_roles(conn, role_data)
#' }
#'
#' @export
redcap_import_user_roles <- function(connection, data) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "userRole",
    action = "import",
    format = "json",
    data = data_string
  )
  
  tryCatch({
    cli::cli_alert_info("Importing user roles to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported user roles")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import user roles: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete REDCap User Roles
#'
#' @description
#' Deletes user roles from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param roles Character vector of unique role names to delete
#'
#' @return Number of user roles deleted
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete specific user roles
#' result <- redcap_delete_user_roles(conn, c("old_role1", "old_role2"))
#' }
#'
#' @export
redcap_delete_user_roles <- function(connection, roles) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(roles) || is.null(roles)) {
    rlang::abort("roles is required")
  }
  
  if (!is.character(roles)) {
    rlang::abort("roles must be a character vector")
  }
  
  params <- list(
    content = "userRole",
    action = "delete",
    roles = paste(roles, collapse = ",")
  )
  
  tryCatch({
    cli::cli_alert_info("Deleting user roles from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted user roles")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete user roles: {e$message}")
    rlang::abort(e$message)
  })
}