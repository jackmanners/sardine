# Example Script: Comprehensive REDCap API Usage with sardine
# This script demonstrates the major functionality of the sardine package

library(sardine)

# Setup connection ----
# It's recommended to store these in environment variables
# In your .Renviron file:
# REDCAP_URL=https://redcap.your-institution.edu/api/
# REDCAP_TOKEN=your_api_token_here

conn <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("REDCAP_TOKEN")
)

# Test the connection
if (!test_connection(conn)) {
  stop("Connection failed. Check your URL and token.")
}

# Project Information ----
project_info <- redcap_project_info(conn)
cat("Project:", project_info$project_title, "\n")
cat("In Production:", project_info$in_production, "\n")

# Get REDCap version
redcap_version <- redcap_export_version(conn)
cat("REDCap Version:", redcap_version, "\n")

# Metadata and Structure ----
# Get field names
field_names <- redcap_field_names(conn)
cat("Number of fields:", length(field_names), "\n")

# Get complete metadata
metadata <- redcap_metadata(conn)
cat("Metadata rows:", nrow(metadata), "\n")

# Get instruments
instruments <- redcap_export_instruments(conn)
cat("Number of instruments:", nrow(instruments), "\n")

# For longitudinal projects
if (project_info$is_longitudinal == "1") {
  # Get arms
  arms <- redcap_export_arms(conn)
  cat("Number of arms:", nrow(arms), "\n")
  
  # Get events
  events <- redcap_export_events(conn)
  cat("Number of events:", nrow(events), "\n")
  
  # Get instrument-event mappings
  mappings <- redcap_export_instrument_event_mappings(conn)
  cat("Number of instrument-event mappings:", nrow(mappings), "\n")
}

# Data Export ----
# Export all records
all_data <- redcap_export_records(conn)
cat("Total records:", nrow(all_data), "\n")
cat("Total fields in data:", ncol(all_data), "\n")

# Export specific fields
demographic_fields <- c("record_id", "age", "gender", "race", "ethnicity")
demo_data <- redcap_export_records(
  conn,
  fields = demographic_fields[demographic_fields %in% field_names]
)

# Export with labels
labeled_data <- redcap_export_records(
  conn,
  raw_or_label = "label",
  fields = demographic_fields[demographic_fields %in% field_names]
)

# Export with filtering (if applicable)
# filtered_data <- redcap_export_records(
#   conn,
#   filter_logic = "[age] > 18"
# )

# Data Import Example ----
# Create a new record (be careful with this in production!)
# new_record <- data.frame(
#   record_id = paste0("test_", format(Sys.time(), "%Y%m%d_%H%M%S")),
#   first_name = "Test",
#   last_name = "User",
#   stringsAsFactors = FALSE
# )
# 
# # Import the record
# import_result <- redcap_import_records(conn, new_record)
# cat("Import result:", import_result, "\n")

# User Management ----
# Export users (requires appropriate permissions)
# users <- redcap_export_users(conn)
# cat("Number of users:", nrow(users), "\n")

# Export user roles
# user_roles <- redcap_export_user_roles(conn)
# cat("Number of user roles:", nrow(user_roles), "\n")

# Data Access Groups ----
# Export DAGs (if applicable)
# dags <- redcap_export_dags(conn)
# if (nrow(dags) > 0) {
#   cat("Number of DAGs:", nrow(dags), "\n")
#   
#   # Export user-DAG mappings
#   user_dag_mappings <- redcap_export_user_dag_mappings(conn)
#   cat("Number of user-DAG mappings:", nrow(user_dag_mappings), "\n")
# }

# Survey Functions ----
# Generate survey link (if surveys are enabled)
# survey_url <- redcap_export_survey_link(
#   conn, 
#   record = all_data$record_id[1], 
#   instrument = instruments$instrument_name[1]
# )
# cat("Survey URL generated\n")

# Reports ----
# Export reports (if you have report IDs)
# report_data <- redcap_export_reports(conn, report_id = "12345")

# Logging ----
# Export recent logging activity
recent_logs <- redcap_export_logging(
  conn,
  begin_time = format(Sys.Date() - 30, "%Y-%m-%d 00:00"),
  end_time = format(Sys.Date(), "%Y-%m-%d 23:59")
)
cat("Log entries in last 30 days:", nrow(recent_logs), "\n")

# File Operations ----
# Export/import files (if your project has file upload fields)
# file_data <- redcap_export_file(conn, "001", "file_upload_field")
# redcap_import_file(conn, "001", "file_upload_field", "path/to/file.pdf")

# Project Backup ----
# Export complete project as XML for backup
# project_xml <- redcap_export_project_xml(conn)
# writeLines(project_xml, "project_backup.xml")

# Export instruments as PDF
# pdf_data <- redcap_export_instruments_pdf(conn)
# writeBin(pdf_data, "instruments.pdf")

cat("\nScript completed successfully!\n")
cat("Remember to handle API tokens securely and never commit them to version control.\n")