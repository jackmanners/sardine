#' Create .env File Template
#'
#' @description
#' Creates a template .env file in your project root with example
#' configuration for REDCap connections. This helps with secure
#' storage of API credentials.
#'
#' @param path Character string. Path where to create the .env file (default: current directory)
#' @param force Logical. Whether to overwrite existing .env file (default: FALSE)
#'
#' @return Invisibly returns the path to the created file
#'
#' @examples
#' \dontrun{
#' # Create .env template in current directory
#' create_env_template()
#'
#' # Create in specific location
#' create_env_template("~/my_project/")
#' }
#'
#' @export
create_env_template <- function(path = ".", force = FALSE) {
  
  env_path <- file.path(path, ".env")
  
  if (file.exists(env_path) && !force) {
    cli::cli_alert_warning(".env file already exists. Use force = TRUE to overwrite.")
    return(invisible(env_path))
  }
  
  env_content <- "# REDCap Configuration
# Replace the example values below with your actual REDCap details

# Primary REDCap Instance
REDCAP_URL=https://redcap.example.edu/api/
REDCAP_TOKEN=YOUR_API_TOKEN_HERE

# Alternative REDCap Instance (if needed)
REDCAP_URL_2=https://redcap2.example.edu/api/
REDCAP_TOKEN_2=YOUR_SECOND_API_TOKEN_HERE

# Optional: SSL and Timeout Settings
REDCAP_SSL_VERIFY=TRUE
REDCAP_TIMEOUT=30

# Database Configuration (for future use)
DATABASE_URL=
DATABASE_USER=
DATABASE_PASSWORD=

# Other API Keys (for future integrations)
# QUALTRICS_TOKEN=
# SURVEY_MONKEY_TOKEN=
# GOOGLE_SHEETS_KEY=

# Note: 
# - Never commit this file to version control
# - Add .env to your .gitignore file
# - Keep your tokens secure and rotate them regularly
"
  
  writeLines(env_content, env_path)
  
  cli::cli_alert_success("Created .env template at: {env_path}")
  cli::cli_alert_info("Remember to:")
  cli::cli_ul(c(
    "Replace example values with your actual credentials",
    "Add .env to your .gitignore file",
    "Never commit .env files to version control"
  ))
  
  return(invisible(env_path))
}

#' Load Environment Configuration
#'
#' @description  
#' Loads environment variables from a .env file. This is a wrapper
#' around dotenv::load_dot_env() with additional validation for
#' REDCap-specific variables.
#'
#' @param file Character string. Path to the .env file (default: ".env")
#' @param validate_redcap Logical. Whether to validate REDCap variables (default: TRUE)
#'
#' @return Invisibly returns TRUE if successful
#'
#' @examples
#' \dontrun{
#' # Load from default .env file
#' load_env()
#'
#' # Load from specific file
#' load_env("config/.env")
#'
#' # Load without validation
#' load_env(validate_redcap = FALSE)
#' }
#'
#' @export
load_env <- function(file = ".env", validate_redcap = TRUE) {
  
  if (!file.exists(file)) {
    cli::cli_alert_danger(".env file not found at: {file}")
    cli::cli_alert_info("Create one using create_env_template()")
    return(invisible(FALSE))
  }
  
  # Load the environment file
  dotenv::load_dot_env(file)
  
  cli::cli_alert_success("Loaded environment from: {file}")
  
  if (validate_redcap) {
    .validate_redcap_env()
  }
  
  return(invisible(TRUE))
}

#' Validate REDCap Environment Variables
#'
#' @description
#' Internal function to validate that required REDCap environment
#' variables are properly set.
#'
#' @return Logical. TRUE if validation passes
#' @keywords internal
.validate_redcap_env <- function() {
  
  required_vars <- c("REDCAP_URL", "REDCAP_TOKEN")
  missing_vars <- character(0)
  
  for (var in required_vars) {
    value <- Sys.getenv(var)
    if (value == "" || value == "YOUR_API_TOKEN_HERE" || value == "https://redcap.example.edu/api/") {
      missing_vars <- c(missing_vars, var)
    }
  }
  
  if (length(missing_vars) > 0) {
    cli::cli_alert_warning("The following REDCap environment variables need to be configured:")
    cli::cli_ul(missing_vars)
    return(FALSE)
  }
  
  cli::cli_alert_success("REDCap environment variables validated successfully")
  return(TRUE)
}

#' Create REDCap Connection from Environment
#'
#' @description
#' Creates a REDCap connection using environment variables. This is
#' a convenience function that reads REDCAP_URL and REDCAP_TOKEN
#' from environment variables.
#'
#' @param env_prefix Character string. Prefix for environment variables (default: "REDCAP")
#' @param ssl_verify Logical. Override SSL verification setting from environment
#' @param timeout Numeric. Override timeout setting from environment
#'
#' @return A redcap_connection object
#'
#' @examples
#' \dontrun{
#' # Using default environment variables
#' conn <- redcap_connection_from_env()
#'
#' # Using alternative environment variables
#' conn <- redcap_connection_from_env(env_prefix = "REDCAP_2")
#' }
#'
#' @export
redcap_connection_from_env <- function(env_prefix = "REDCAP", ssl_verify = NULL, timeout = NULL) {
  
  url_var <- paste0(env_prefix, "_URL")
  token_var <- paste0(env_prefix, "_TOKEN")
  ssl_var <- paste0(env_prefix, "_SSL_VERIFY")
  timeout_var <- paste0(env_prefix, "_TIMEOUT")
  
  url <- Sys.getenv(url_var)
  token <- Sys.getenv(token_var)
  
  if (url == "" || token == "") {
    cli::cli_alert_danger("Environment variables {url_var} and {token_var} must be set")
    cli::cli_alert_info("Use load_env() to load from .env file or set variables manually")
    stop("Missing required environment variables")
  }
  
  # Use environment settings or provided overrides
  if (is.null(ssl_verify)) {
    ssl_env <- Sys.getenv(ssl_var, "TRUE")
    ssl_verify <- as.logical(ssl_env)
  }
  
  if (is.null(timeout)) {
    timeout_env <- Sys.getenv(timeout_var, "30")
    timeout <- as.numeric(timeout_env)
  }
  
  return(redcap_connection(
    url = url,
    token = token,
    ssl_verify = ssl_verify,
    timeout = timeout
  ))
}