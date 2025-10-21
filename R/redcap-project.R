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
      # Interactive fallback: ask user to choose URL
      if (interactive()) {
        cli::cli_h2("REDCap URL not found")
        cli::cli_alert_info("Select your REDCap instance or choose Other to enter manually")

        known <- tibble::tibble(
          label = c("Flinders University", "Vanderbilt University", "Other"),
          url = c(
            "https://researchsurvey.flinders.edu.au/api/",
            "https://redcap.vanderbilt.edu/api/",
            NA_character_
          )
        )

        for (i in seq_len(nrow(known))) {
          cat(sprintf("%d) %s%s\n", i, known$label[i], if (!is.na(known$url[i])) paste0(" (", known$url[i], ")") else ""))
        }
        choice <- suppressWarnings(as.integer(readline(prompt = "Select (1-3): ")))
        if (!is.na(choice) && choice >= 1 && choice <= nrow(known)) {
          if (!is.na(known$url[choice])) {
            url <- known$url[choice]
          } else {
            manual <- readline(prompt = "Enter REDCap API URL (ends with /api/): ")
            url <- trimws(manual)
          }
        }

        if (is.null(url) || url == "") {
          stop(paste("URL must be provided directly or set in", url_var, "environment variable"))
        }
      } else {
        stop(paste("URL must be provided directly or set in", url_var, "environment variable"))
      }
    } else {
      cli::cli_alert_info("Using URL from {url_var} environment variable")
    }
  }

  if (is.null(token)) {
    token <- Sys.getenv(token_var)
    if (token == "") {
      # Interactive fallback: ask user for token
      if (interactive()) {
        cli::cli_h2("REDCap API token not found")
        token <- readline(prompt = "Enter REDCap API token: ")
        token <- trimws(token)
        if (token == "") {
          stop(paste("Token must be provided directly or set in", token_var, "environment variable"))
        }
      } else {
        stop(paste("Token must be provided directly or set in", token_var, "environment variable"))
      }
    } else {
      cli::cli_alert_info("Using token from {token_var} environment variable")
    }
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
  proj <- internal(
    url = url,
    token = token, 
    ssl_verify = ssl_verify,
    timeout = timeout
  )

  # Offer to save credentials to environment and .env for future sessions
  if (interactive()) {
    # If env variables not set, offer to save
    if (Sys.getenv(url_var) == "" || Sys.getenv(token_var) == "") {
      cat(sprintf("\nWould you like to save these credentials to environment variables (%s/%s) and .env for future sessions?\n", url_var, token_var))
      save_ans <- tolower(trimws(readline(prompt = "Save credentials? [y/N]: ")))
      if (save_ans %in% c("y", "yes")) {
        Sys.setenv(structure(list(url), names = url_var))
        Sys.setenv(structure(list(token), names = token_var))
        # Append or create .env seamlessly
        env_path <- ".env"
        line <- function(k, v) paste0(k, "=", v)
        lines <- c()
        if (file.exists(env_path)) {
          existing <- readLines(env_path, warn = FALSE)
          # replace or append
          existing <- existing[!grepl(paste0("^", url_var, "="), existing)]
          existing <- existing[!grepl(paste0("^", token_var, "="), existing)]
          lines <- c(existing, line(url_var, url), line(token_var, token))
        } else {
          lines <- c(line(url_var, url), line(token_var, token))
        }
        writeLines(lines, env_path)
        cli::cli_alert_success("Saved credentials to environment and .env")
      }
    }
  }

  proj
}
