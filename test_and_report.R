# Simple test and completion report script
# Load environment and test basic functionality

library(dotenv)
library(sardine)

# Load environment variables
load_dot_env()

# Check that environment variables are set
cat("🔍 Checking environment variables...\n")
cat("REDCAP_URL:", if(Sys.getenv("REDCAP_URL") != "") "✅ Set" else "❌ Not set", "\n")
cat("REDCAP_TOKEN:", if(Sys.getenv("REDCAP_TOKEN") != "") "✅ Set" else "❌ Not set", "\n\n")

if(Sys.getenv("REDCAP_URL") == "" || Sys.getenv("REDCAP_TOKEN") == "") {
  stop("Please set REDCAP_URL and REDCAP_TOKEN in your .env file")
}

# Create connection
cat("🔗 Creating REDCap connection...\n")
redcap_config <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"), 
  token = Sys.getenv("REDCAP_TOKEN")
)

# Test connection
cat("🧪 Testing connection...\n")
test_result <- test_connection(redcap_config)

if (test_result) {
  cat("✅ Connection successful!\n\n")
} else {
  stop("❌ Connection failed. Please check your URL and token.")
}

# Basic data export
cat("📊 Exporting records...\n")
records <- redcap_export_records(redcap_config)
cat("Found", nrow(records), "records\n\n")

# Get project structure
cat("🔧 Getting project structure...\n")
instruments <- redcap_export_instruments(redcap_config)
cat("Found", nrow(instruments), "instruments:\n")
for(i in 1:nrow(instruments)) {
  cat(" -", instruments$instrument_name[i], "(", instruments$instrument_label[i], ")\n")
}
cat("\n")

# Check if longitudinal
cat("📅 Checking if longitudinal project...\n")
events <- tryCatch({
  redcap_export_events(redcap_config)
}, error = function(e) {
  cat("Not a longitudinal project (classic design)\n")
  NULL
})

if (!is.null(events)) {
  cat("Longitudinal project with", nrow(events), "events:\n")
  for(i in 1:nrow(events)) {
    cat(" -", events$unique_event_name[i], "(", events$event_name[i], ")\n")
  }
  cat("\n")
}

# Simple completion analysis
cat("📈 Analyzing completion status...\n")
record_id_field <- names(records)[1]
participants <- unique(records[[record_id_field]])

cat("Record ID field:", record_id_field, "\n")
cat("Participants:", length(participants), "\n")

# Look for completion fields
completion_fields <- names(records)[grepl("_complete$", names(records))]
cat("Found", length(completion_fields), "completion fields:\n")
if (length(completion_fields) > 0) {
  for(field in completion_fields) {
    cat(" -", field, "\n")
  }
}

cat("\n📋 Sample completion data:\n")
if (length(completion_fields) > 0 && nrow(records) > 0) {
  # Show completion status for first few participants
  sample_participants <- head(participants, 3)
  
  for (participant in sample_participants) {
    cat("Participant", participant, ":\n")
    participant_data <- records[records[[record_id_field]] == participant, ]
    
    for (field in completion_fields) {
      if (field %in% names(participant_data)) {
        status <- participant_data[[field]][1]
        status_text <- switch(as.character(status),
                            "0" = "Incomplete", 
                            "1" = "Unverified",
                            "2" = "Complete",
                            "Not Started")
        cat("  ", field, ":", status_text, "\n")
      }
    }
    cat("\n")
  }
} else {
  cat("No completion data found.\n")
}

cat("🎉 Basic analysis complete!\n")
cat("\n💡 To run full completion reports, use:\n")
cat("   source('simple_completion_functions.R')\n")
cat("   completion_result <- get_participant_completion(redcap_config)\n")
cat("   print_completion_report(completion_result)\n")