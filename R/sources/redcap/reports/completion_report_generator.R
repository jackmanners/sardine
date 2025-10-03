# REDCap Participant Completion Report Generator
# This script generates comprehensive completion reports for longitudinal REDCap projects

library(dotenv)
library(sardine)
library(dplyr)
library(tidyr)
library(knitr)
library(DT)  # For interactive tables

# Load environment variables
load_dot_env()

# Create connection
redcap_config <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"), 
  token = Sys.getenv("REDCAP_TOKEN")
)

#' Generate Participant Completion Report
#'
#' @description
#' Creates a comprehensive report showing which events and instruments 
#' each participant has completed in a longitudinal REDCap project.
#'
#' @param connection A redcap_connection object
#' @param include_incomplete Logical. Include incomplete records in the report (default: TRUE)
#' @param export_format Character. Export format: "console", "html", "csv" (default: "console")
#' @param output_file Character. Output file path (required for html/csv formats)
#'
#' @return A list containing summary data and detailed completion matrix
#'
#' @examples
#' \dontrun{
#' # Basic console report
#' report <- generate_completion_report(redcap_config)
#' 
#' # Export to HTML
#' generate_completion_report(redcap_config, 
#'                           export_format = "html", 
#'                           output_file = "completion_report.html")
#' }
#'
#' @export
generate_completion_report <- function(connection, 
                                     include_incomplete = TRUE,
                                     export_format = "console",
                                     output_file = NULL) {
  
  cat("🔄 Generating participant completion report...\n\n")
  
  # 1. Get project metadata to understand the structure
  cat("📋 Fetching project metadata...\n")
  metadata <- redcap_export_metadata(connection)
  instruments <- redcap_export_instruments(connection)
  
  # Check if this is a longitudinal project
  events <- tryCatch({
    redcap_export_events(connection)
  }, error = function(e) {
    NULL  # Not a longitudinal project
  })
  
  is_longitudinal <- !is.null(events) && nrow(events) > 0
  
  # 2. Get all records
  cat("📊 Fetching all records...\n")
  records <- redcap_export_records(connection, return_format = "json")
  
  if (nrow(records) == 0) {
    stop("No records found in the project.")
  }
  
  # Get the record ID field (first field)
  record_id_field <- metadata$field_name[1]
  cat("🆔 Record ID field:", record_id_field, "\n")
  
  # 3. Generate completion report
  if (is_longitudinal) {
    cat("📅 Longitudinal project detected - analyzing events and instruments...\n")
    completion_data <- analyze_longitudinal_completion(records, metadata, instruments, events, record_id_field)
  } else {
    cat("📝 Classic project detected - analyzing instruments...\n")
    completion_data <- analyze_classic_completion(records, metadata, instruments, record_id_field)
  }
  
  # 4. Generate summary statistics
  summary_stats <- generate_summary_stats(completion_data, is_longitudinal)
  
  # 5. Output the report
  report_data <- list(
    summary = summary_stats,
    completion_matrix = completion_data$completion_matrix,
    detailed_data = completion_data$detailed_data,
    metadata = list(
      is_longitudinal = is_longitudinal,
      total_participants = length(unique(records[[record_id_field]])),
      total_instruments = nrow(instruments),
      total_events = if(is_longitudinal) nrow(events) else 1
    )
  )
  
  # Output based on format
  if (export_format == "console") {
    print_console_report(report_data)
  } else if (export_format == "html") {
    if (is.null(output_file)) stop("output_file required for HTML format")
    export_html_report(report_data, output_file)
  } else if (export_format == "csv") {
    if (is.null(output_file)) stop("output_file required for CSV format")
    export_csv_report(report_data, output_file)
  }
  
  cat("✅ Report generation complete!\n")
  invisible(report_data)
}

#' Analyze completion for longitudinal projects
analyze_longitudinal_completion <- function(records, metadata, instruments, events, record_id_field) {
  
  # Create completion matrix: participant x event x instrument
  participants <- unique(records[[record_id_field]])
  
  # Get instrument-event mapping
  event_instrument_mapping <- redcap_export_instrument_event_mappings(redcap_config)
  
  completion_matrix <- expand_grid(
    participant_id = participants,
    event_name = events$unique_event_name,
    instrument = instruments$instrument_name
  ) %>%
    left_join(event_instrument_mapping, by = c("event_name" = "unique_event_name", "instrument" = "form")) %>%
    filter(!is.na(form)) %>%  # Only include valid instrument-event combinations
    mutate(
      completion_status = map2_chr(participant_id, paste(event_name, instrument, sep = "_"), 
                                  ~get_completion_status(.x, .y, records))
    )
  
  # Create summary by participant and event
  participant_event_summary <- completion_matrix %>%
    group_by(participant_id, event_name) %>%
    summarise(
      total_instruments = n(),
      completed = sum(completion_status == "Complete"),
      incomplete = sum(completion_status == "Incomplete"),
      unverified = sum(completion_status == "Unverified"),
      not_started = sum(completion_status == "Not Started"),
      completion_rate = round(completed / total_instruments * 100, 1),
      .groups = "drop"
    )
  
  # Create wide format for easy viewing
  completion_wide <- completion_matrix %>%
    select(participant_id, event_name, instrument, completion_status) %>%
    pivot_wider(
      names_from = c(event_name, instrument),
      values_from = completion_status,
      names_sep = " | "
    )
  
  list(
    completion_matrix = completion_matrix,
    participant_summary = participant_event_summary,
    completion_wide = completion_wide,
    detailed_data = completion_matrix
  )
}

#' Analyze completion for classic projects
analyze_classic_completion <- function(records, metadata, instruments, record_id_field) {
  
  participants <- unique(records[[record_id_field]])
  
  completion_matrix <- expand_grid(
    participant_id = participants,
    instrument = instruments$instrument_name
  ) %>%
    mutate(
      completion_status = map2_chr(participant_id, instrument, 
                                  ~get_completion_status(.x, .y, records))
    )
  
  # Create summary by participant
  participant_summary <- completion_matrix %>%
    group_by(participant_id) %>%
    summarise(
      total_instruments = n(),
      completed = sum(completion_status == "Complete"),
      incomplete = sum(completion_status == "Incomplete"),
      unverified = sum(completion_status == "Unverified"),
      not_started = sum(completion_status == "Not Started"),
      completion_rate = round(completed / total_instruments * 100, 1),
      .groups = "drop"
    )
  
  # Create wide format
  completion_wide <- completion_matrix %>%
    select(participant_id, instrument, completion_status) %>%
    pivot_wider(
      names_from = instrument,
      values_from = completion_status
    )
  
  list(
    completion_matrix = completion_matrix,
    participant_summary = participant_summary,
    completion_wide = completion_wide,
    detailed_data = completion_matrix
  )
}

#' Get completion status for a specific participant and instrument/event
get_completion_status <- function(participant_id, instrument_key, records) {
  # Look for the completion field
  completion_field <- paste0(instrument_key, "_complete")
  
  if (completion_field %in% names(records)) {
    participant_data <- records[records[[names(records)[1]]] == participant_id, ]
    if (nrow(participant_data) > 0) {
      status_value <- participant_data[[completion_field]][1]
      if (is.na(status_value) || status_value == "") {
        return("Not Started")
      } else if (status_value == "0") {
        return("Incomplete")
      } else if (status_value == "1") {
        return("Unverified")
      } else if (status_value == "2") {
        return("Complete")
      }
    }
  }
  return("Not Started")
}

#' Generate summary statistics
generate_summary_stats <- function(completion_data, is_longitudinal) {
  
  if (is_longitudinal) {
    overall_stats <- completion_data$completion_matrix %>%
      summarise(
        total_records = n(),
        completed = sum(completion_status == "Complete"),
        incomplete = sum(completion_status == "Incomplete"),
        unverified = sum(completion_status == "Unverified"),
        not_started = sum(completion_status == "Not Started"),
        overall_completion_rate = round(completed / total_records * 100, 1)
      )
    
    by_event <- completion_data$completion_matrix %>%
      group_by(event_name) %>%
      summarise(
        total_records = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_records * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
      
    by_instrument <- completion_data$completion_matrix %>%
      group_by(instrument) %>%
      summarise(
        total_records = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_records * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    
    list(
      overall = overall_stats,
      by_event = by_event,
      by_instrument = by_instrument,
      by_participant = completion_data$participant_summary
    )
    
  } else {
    overall_stats <- completion_data$completion_matrix %>%
      summarise(
        total_records = n(),
        completed = sum(completion_status == "Complete"),
        incomplete = sum(completion_status == "Incomplete"),
        unverified = sum(completion_status == "Unverified"),
        not_started = sum(completion_status == "Not Started"),
        overall_completion_rate = round(completed / total_records * 100, 1)
      )
    
    by_instrument <- completion_data$completion_matrix %>%
      group_by(instrument) %>%
      summarise(
        total_participants = n(),
        completed = sum(completion_status == "Complete"),
        completion_rate = round(completed / total_participants * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completion_rate))
    
    list(
      overall = overall_stats,
      by_instrument = by_instrument,
      by_participant = completion_data$participant_summary
    )
  }
}

#' Print console report
print_console_report <- function(report_data) {
  cat("\n" %+% "="*60 %+% "\n")
  cat("📊 REDCAP PARTICIPANT COMPLETION REPORT\n")
  cat("="*60 %+% "\n\n")
  
  # Overall statistics
  cat("📈 OVERALL STATISTICS\n")
  cat("-"*30 %+% "\n")
  print(report_data$summary$overall)
  cat("\n")
  
  # By instrument
  if (!is.null(report_data$summary$by_instrument)) {
    cat("🔧 COMPLETION BY INSTRUMENT\n")
    cat("-"*30 %+% "\n")
    print(report_data$summary$by_instrument)
    cat("\n")
  }
  
  # By event (if longitudinal)
  if (!is.null(report_data$summary$by_event)) {
    cat("📅 COMPLETION BY EVENT\n")
    cat("-"*30 %+% "\n")
    print(report_data$summary$by_event)
    cat("\n")
  }
  
  # Participant summary (top 10)
  cat("👥 PARTICIPANT COMPLETION SUMMARY (Top 10)\n")
  cat("-"*30 %+% "\n")
  top_participants <- head(report_data$summary$by_participant %>% 
                          arrange(desc(completion_rate)), 10)
  print(top_participants)
  
  cat("\n💡 Use export_format='html' or 'csv' for detailed reports\n")
}

# Helper function for string repetition (if not available)
`%+%` <- function(x, y) paste0(x, y)

# Example usage
cat("🚀 REDCap Completion Report Generator Loaded!\n")
cat("📖 Usage examples:\n")
cat("   report <- generate_completion_report(redcap_config)\n")
cat("   generate_completion_report(redcap_config, export_format='html', output_file='report.html')\n")
cat("   generate_completion_report(redcap_config, export_format='csv', output_file='completion_data.csv')\n\n")