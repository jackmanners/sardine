# Simple REDCap Completion Report
# Quick and easy participant completion analysis

library(dotenv)
library(sardine)
library(dplyr)

# Load environment and connect
load_dot_env()
project <- redcap_project_from_env()

#' Quick Participant Completion Summary
#' 
#' @description
#' Generates a simple completion report showing what each participant has completed
#' 
#' @param project REDCap project object
#' @return Data frame with completion summary
quick_completion_report <- function(project) {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  cat("📊 Generating quick completion report...\n")
  
  # Get basic project info (use cached data and methods)
  records <- project$data
  instruments <- export_instruments(project)
  
  # Check if longitudinal
  events <- tryCatch({
    export_events(project)
  }, error = function(e) NULL)
  
  is_longitudinal <- !is.null(events) && nrow(events) > 0
  
  # Get record ID field (first field)
  record_id_field <- names(records)[1]
  participants <- unique(records[[record_id_field]])
  
  cat("🆔 Record ID field:", record_id_field, "\n")
  cat("👥 Found", length(participants), "participants\n")
  cat("🔧 Found", nrow(instruments), "instruments\n")
  
  if (is_longitudinal) {
    cat("📅 Longitudinal project with", nrow(events), "events\n")
    
    # For longitudinal projects, we need event-instrument mapping
    event_mapping <- tryCatch({
      export_instrument_event_mappings(project)  
    }, error = function(e) {
      # If mapping fails, assume all instruments in all events
      expand.grid(
        unique_event_name = events$unique_event_name,
        form = instruments$instrument_name,
        stringsAsFactors = FALSE
      )
    })
    
    # Create completion summary for each participant
    completion_summary <- data.frame()
    
    for (participant in participants) {
      participant_data <- records[records[[record_id_field]] == participant, ]
      
      for (event in events$unique_event_name) {
        event_instruments <- event_mapping[event_mapping$unique_event_name == event, "form"]
        
        for (instrument in event_instruments) {
          # Look for completion field
          completion_field <- paste0(instrument, "_complete")
          
          if (completion_field %in% names(records)) {
            # Get completion status
            completion_status <- NA
            
            # In longitudinal projects, we need to check redcap_event_name
            if ("redcap_event_name" %in% names(participant_data)) {
              event_data <- participant_data[participant_data$redcap_event_name == event, ]
              if (nrow(event_data) > 0 && !is.na(event_data[[completion_field]])) {
                completion_status <- event_data[[completion_field]]
              }
            } else {
              # Fallback for classic projects treated as longitudinal
              if (!is.na(participant_data[[completion_field]][1])) {
                completion_status <- participant_data[[completion_field]][1]
              }
            }
            
            # Convert to human-readable status
            status_text <- case_when(
              is.na(completion_status) | completion_status == "" ~ "Not Started",
              completion_status == "0" ~ "Incomplete",
              completion_status == "1" ~ "Unverified", 
              completion_status == "2" ~ "Complete",
              TRUE ~ "Unknown"
            )
            
            completion_summary <- rbind(completion_summary, data.frame(
              participant_id = participant,
              event_name = event,
              instrument = instrument,
              completion_status = status_text,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
    
  } else {
    cat("📝 Classic project\n")
    
    # For classic projects
    completion_summary <- data.frame()
    
    for (participant in participants) {
      participant_data <- records[records[[record_id_field]] == participant, ]
      
      for (instrument in instruments$instrument_name) {
        completion_field <- paste0(instrument, "_complete")
        
        if (completion_field %in% names(records)) {
          completion_status <- participant_data[[completion_field]][1]
          
          status_text <- case_when(
            is.na(completion_status) | completion_status == "" ~ "Not Started",
            completion_status == "0" ~ "Incomplete",
            completion_status == "1" ~ "Unverified",
            completion_status == "2" ~ "Complete", 
            TRUE ~ "Unknown"
          )
          
          completion_summary <- rbind(completion_summary, data.frame(
            participant_id = participant,
            instrument = instrument,
            completion_status = status_text,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  cat("✅ Report generated!\n\n")
  return(completion_summary)
}

#' Print a nice summary of completion data
#' 
#' @param completion_data Output from quick_completion_report()
print_completion_summary <- function(completion_data) {
  
  if ("event_name" %in% names(completion_data)) {
    # Longitudinal project
    cat("📊 COMPLETION SUMMARY BY PARTICIPANT AND EVENT\n")
    cat("=" %+% paste(rep("=", 50), collapse="") %+% "\n\n")
    
    # Summary by participant
    participant_summary <- completion_data %>%
      group_by(participant_id) %>%
      summarise(
        total_items = n(),
        completed = sum(completion_status == "Complete"),
        incomplete = sum(completion_status == "Incomplete"), 
        unverified = sum(completion_status == "Unverified"),
        not_started = sum(completion_status == "Not Started"),
        completion_rate = round(completed / total_items * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    
    cat("👥 BY PARTICIPANT:\n")
    print(participant_summary)
    
    cat("\n📅 BY EVENT:\n")
    event_summary <- completion_data %>%
      group_by(event_name) %>%
      summarise(
        total_items = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_items * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    print(event_summary)
    
    cat("\n🔧 BY INSTRUMENT:\n")
    instrument_summary <- completion_data %>%
      group_by(instrument) %>%
      summarise(
        total_items = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_items * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    print(instrument_summary)
    
  } else {
    # Classic project
    cat("📊 COMPLETION SUMMARY BY PARTICIPANT\n")
    cat("=" %+% paste(rep("=", 40), collapse="") %+% "\n\n")
    
    participant_summary <- completion_data %>%
      group_by(participant_id) %>%
      summarise(
        total_instruments = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_instruments * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    
    cat("👥 BY PARTICIPANT:\n")
    print(participant_summary)
    
    cat("\n🔧 BY INSTRUMENT:\n")
    instrument_summary <- completion_data %>%
      group_by(instrument) %>%
      summarise(
        total_participants = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_participants * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    print(instrument_summary)
  }
}

# Helper for string concatenation
`%+%` <- function(x, y) paste0(x, y)

# Example usage:
cat("🚀 Quick completion report functions loaded!\n")
cat("📖 Usage:\n")
cat("   completion_data <- quick_completion_report(project)\n")
cat("   print_completion_summary(completion_data)\n")
cat("   View(completion_data)  # To see raw data\n\n")

# Run the report automatically if project is available
if (exists("project") && inherits(project, "redcap_project")) {
  cat("🔄 Running quick completion report...\n")
  completion_data <- quick_completion_report(project)
  print_completion_summary(completion_data)
}