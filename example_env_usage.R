# Example: Using .env file for REDCap API credentials
# This script shows how to load environment variables from a .env file

# Method 1: Using dotenv package (recommended)
# Install dotenv if you don't have it: install.packages("dotenv")
library(dotenv)
library(sardine)

# Load environment variables from .env file
load_dot_env()

# Create connection using environment variables
redcap_config <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("REDCAP_TOKEN")
)

# Test the connection
test_connection(redcap_config)

# Export some data
data <- redcap_export_records(redcap_config)
print(paste("Exported", nrow(data), "records"))

# ---------------------------------------------------

# Method 2: Manual loading without dotenv package
# If you prefer not to use the dotenv package, you can manually read the .env file

load_env_file <- function(file_path = ".env") {
  if (!file.exists(file_path)) {
    stop("Environment file not found: ", file_path)
  }
  
  lines <- readLines(file_path)
  lines <- lines[!grepl("^#", lines) & nchar(lines) > 0]  # Remove comments and empty lines
  
  for (line in lines) {
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        key <- trimws(parts[1])
        value <- trimws(parts[2])
        Sys.setenv(setNames(value, key))
      }
    }
  }
}

# Use the manual method
# load_env_file(".env")
# redcap_config <- redcap_connection(
#   url = Sys.getenv("REDCAP_URL"),
#   token = Sys.getenv("REDCAP_TOKEN")
# )