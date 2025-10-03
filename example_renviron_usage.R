# Alternative: Using .Renviron file for R environment variables
# This approach uses R's built-in environment variable system

# To set up .Renviron file:
# 1. Run this command to edit your user .Renviron file:
# usethis::edit_r_environ()

# 2. Add these lines to the .Renviron file:
# REDCAP_URL=https://redcap.example.edu/api/
# REDCAP_TOKEN=your_actual_api_token_here

# 3. Restart R session to load the variables

library(sardine)

# Create connection using environment variables (no need to load anything)
redcap_config <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("REDCAP_TOKEN")
)

# Test the connection
test_connection(redcap_config)

# Alternative: Check if environment variables are set
check_env_vars <- function() {
  url <- Sys.getenv("REDCAP_URL")
  token <- Sys.getenv("REDCAP_TOKEN")
  
  if (url == "" || token == "") {
    cat("❌ Environment variables not set properly.\n")
    cat("Please ensure REDCAP_URL and REDCAP_TOKEN are defined.\n")
    cat("Current values:\n")
    cat("  REDCAP_URL:", ifelse(url == "", "(not set)", "✓ set"), "\n")
    cat("  REDCAP_TOKEN:", ifelse(token == "", "(not set)", "✓ set"), "\n")
    return(FALSE)
  } else {
    cat("✅ Environment variables are set correctly.\n")
    return(TRUE)
  }
}

# Check if variables are properly set
check_env_vars()