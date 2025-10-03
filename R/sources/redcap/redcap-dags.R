#' Export REDCap Data Access Groups (DAGs)
#'
#' @description
#' Exports the Data Access Groups defined in a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing DAG information
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export all DAGs
#' dags <- redcap_export_dags(conn)
#' }
#'
#' @export
redcap_export_dags <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "dag",
    format = format
  )
  
  tryCatch({
    cli::cli_alert_info("Exporting DAGs from REDCap...")
    
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
    
    cli::cli_alert_success("Successfully exported DAGs")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export DAGs: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import REDCap Data Access Groups (DAGs)
#'
#' @description
#' Imports Data Access Groups to a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing DAG information with columns: data_access_group_name, unique_group_name
#'
#' @return Number of DAGs imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import DAGs
#' dag_data <- data.frame(
#'   data_access_group_name = c("Site A", "Site B"),
#'   unique_group_name = c("site_a", "site_b")
#' )
#' result <- redcap_import_dags(conn, dag_data)
#' }
#'
#' @export
redcap_import_dags <- function(connection, data) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "dag",
    action = "import",
    format = "json",
    data = data_string
  )
  
  tryCatch({
    cli::cli_alert_info("Importing DAGs to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported DAGs")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import DAGs: {e$message}")
    rlang::abort(e$message)
  })
}

#' Delete REDCap Data Access Groups (DAGs)
#'
#' @description
#' Deletes Data Access Groups from a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param dags Character vector of unique DAG names to delete
#'
#' @return Number of DAGs deleted
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Delete specific DAGs
#' result <- redcap_delete_dags(conn, c("old_site_a", "old_site_b"))
#' }
#'
#' @export
redcap_delete_dags <- function(connection, dags) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(dags) || is.null(dags)) {
    rlang::abort("dags is required")
  }
  
  if (!is.character(dags)) {
    rlang::abort("dags must be a character vector")
  }
  
  params <- list(
    content = "dag",
    action = "delete",
    dags = paste(dags, collapse = ",")
  )
  
  tryCatch({
    cli::cli_alert_info("Deleting DAGs from REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully deleted DAGs")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to delete DAGs: {e$message}")
    rlang::abort(e$message)
  })
}

#' Export User-DAG Mappings
#'
#' @description
#' Exports the mapping between users and Data Access Groups.
#'
#' @param connection A redcap_connection object
#' @param format Character string. Format for returned data: "json" (default), "csv", "xml"
#'
#' @return A tibble containing user-DAG mappings
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Export user-DAG mappings
#' mappings <- redcap_export_user_dag_mappings(conn)
#' }
#'
#' @export
redcap_export_user_dag_mappings <- function(connection, format = "json") {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  params <- list(
    content = "userDagMapping",
    format = format
  )
  
  tryCatch({
    cli::cli_alert_info("Exporting user-DAG mappings from REDCap...")
    
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
    
    cli::cli_alert_success("Successfully exported user-DAG mappings")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export user-DAG mappings: {e$message}")
    rlang::abort(e$message)
  })
}

#' Import User-DAG Mappings
#'
#' @description
#' Imports user-DAG mappings to a REDCap project.
#'
#' @param connection A redcap_connection object
#' @param data A data.frame containing user-DAG mappings with columns: username, redcap_data_access_group
#'
#' @return Number of mappings imported
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Import user-DAG mappings
#' mapping_data <- data.frame(
#'   username = c("user1", "user2", "user3"),
#'   redcap_data_access_group = c("site_a", "site_a", "site_b")
#' )
#' result <- redcap_import_user_dag_mappings(conn, mapping_data)
#' }
#'
#' @export
redcap_import_user_dag_mappings <- function(connection, data) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(data) || is.null(data)) {
    rlang::abort("data is required")
  }
  
  data_string <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  params <- list(
    content = "userDagMapping",
    action = "import",
    format = "json",
    data = data_string
  )
  
  tryCatch({
    cli::cli_alert_info("Importing user-DAG mappings to REDCap...")
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully imported user-DAG mappings")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to import user-DAG mappings: {e$message}")
    rlang::abort(e$message)
  })
}

#' Switch User's Data Access Group
#'
#' @description
#' Switches a user from one Data Access Group to another.
#'
#' @param connection A redcap_connection object
#' @param username Character string. Username to switch
#' @param dag Character string. New DAG unique name (use "" to remove from all DAGs)
#'
#' @return Success message
#'
#' @examples
#' \dontrun{
#' conn <- redcap_connection(
#'   url = "https://redcap.example.edu/api/",
#'   token = "YOUR_API_TOKEN"
#' )
#'
#' # Switch user to a different DAG
#' result <- redcap_switch_dag(conn, "john.doe", "site_b")
#' 
#' # Remove user from all DAGs
#' result <- redcap_switch_dag(conn, "john.doe", "")
#' }
#'
#' @export
redcap_switch_dag <- function(connection, username, dag) {
  if (!inherits(connection, "redcap_connection")) {
    rlang::abort("connection must be a redcap_connection object")
  }
  
  if (missing(username) || missing(dag)) {
    rlang::abort("Both username and dag are required")
  }
  
  if (!is.character(username) || length(username) != 1) {
    rlang::abort("username must be a single character string")
  }
  
  if (!is.character(dag) || length(dag) != 1) {
    rlang::abort("dag must be a single character string")
  }
  
  params <- list(
    content = "userDagMapping",
    action = "switch",
    username = username,
    dag = dag
  )
  
  tryCatch({
    if (dag == "") {
      cli::cli_alert_info("Removing user '{username}' from all DAGs...")
    } else {
      cli::cli_alert_info("Switching user '{username}' to DAG '{dag}'...")
    }
    
    response <- do.call(.redcap_request, c(list(connection = connection), params))
    
    if (httr2::resp_is_error(response)) {
      error_msg <- httr2::resp_body_string(response)
      rlang::abort(glue::glue("REDCap API error: {error_msg}"))
    }
    
    result <- httr2::resp_body_string(response)
    
    cli::cli_alert_success("Successfully switched user DAG")
    return(result)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to switch user DAG: {e$message}")
    rlang::abort(e$message)
  })
}