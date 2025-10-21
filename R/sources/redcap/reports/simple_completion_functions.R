# Simple function to add to your existing test_script.r
# This creates a basic completion report

#' Get participant completion status
#' @param project REDCap project object
#' @return List with completion data and summaries
get_participant_completion <- function(project) {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  # Get all the data we need (use cached data and methods)
  records <- project$data
  instruments <- export_instruments(project)
  
  # Check if longitudinal
  events <- tryCatch(export_events(project), error = function(e) NULL)
  is_longitudinal <- !is.null(events) && nrow(events) > 0
  
  # Get record ID field
  record_id_field <- names(records)[1]
  participants <- unique(records[[record_id_field]])
  
  cat("Project Info:\n")
  cat("- Record ID field:", record_id_field, "\n")
  cat("- Participants:", length(participants), "\n") 
  cat("- Instruments:", nrow(instruments), "\n")
  cat("- Type:", ifelse(is_longitudinal, "Longitudinal", "Classic"), "\n")
  if (is_longitudinal) cat("- Events:", nrow(events), "\n")
  cat("\n")
  
  # Create completion matrix
  completion_list <- list()
  
  for (participant in participants) {
    participant_data <- records[records[[record_id_field]] == participant, ]
    participant_completion <- list()
    
    if (is_longitudinal) {
      # For longitudinal projects
      for (event in events$unique_event_name) {
        event_completion <- list()
        
        for (instrument in instruments$instrument_name) {
          completion_field <- paste0(instrument, "_complete")
          
          if (completion_field %in% names(records)) {
            # Get the data for this event
            if ("redcap_event_name" %in% names(participant_data)) {
              event_data <- participant_data[participant_data$redcap_event_name == event, ]
              if (nrow(event_data) > 0) {
                status_code <- event_data[[completion_field]][1]
              } else {
                status_code <- NA
              }
            } else {
              status_code <- participant_data[[completion_field]][1]
            }
            
            # Convert to text
            status_text <- switch(as.character(status_code),
                                "0" = "Incomplete",
                                "1" = "Unverified", 
                                "2" = "Complete",
                                "Not Started")
            
            event_completion[[instrument]] <- status_text
          }
        }
        participant_completion[[event]] <- event_completion
      }
    } else {
      # For classic projects
      for (instrument in instruments$instrument_name) {
        completion_field <- paste0(instrument, "_complete")
        
        if (completion_field %in% names(records)) {
          status_code <- participant_data[[completion_field]][1]
          
          status_text <- switch(as.character(status_code),
                              "0" = "Incomplete",
                              "1" = "Unverified",
                              "2" = "Complete", 
                              "Not Started")
          
          participant_completion[[instrument]] <- status_text
        }
      }
    }
    
    completion_list[[participant]] <- participant_completion
  }
  
  return(list(
    completion_data = completion_list,
    is_longitudinal = is_longitudinal,
    participants = participants,
    instruments = instruments$instrument_name,
    events = if(is_longitudinal) events$unique_event_name else NULL
  ))
}

#' Print completion report in a readable format
#' @param completion_result Output from get_participant_completion()
print_completion_report <- function(completion_result) {
  
  cat("PARTICIPANT COMPLETION REPORT\n")
  cat("============================\n\n")
  
  for (participant in completion_result$participants) {
    cat("Participant:", participant, "\n")
    cat(paste(rep("-", 20), collapse=""), "\n")
    
    if (completion_result$is_longitudinal) {
      # Longitudinal format
      for (event in completion_result$events) {
        cat("  Event:", event, "\n")
        
        for (instrument in completion_result$instruments) {
          status <- completion_result$completion_data[[participant]][[event]][[instrument]]
          status_symbol <- switch(status,
                                "Complete" = "✅",
                                "Incomplete" = "🟡", 
                                "Unverified" = "🔵",
                                "❌")
          cat("    ", status_symbol, instrument, ":", status, "\n")
        }
        cat("\n")
      }
    } else {
      # Classic format
      for (instrument in completion_result$instruments) {
        status <- completion_result$completion_data[[participant]][[instrument]]
        status_symbol <- switch(status,
                              "Complete" = "✅",
                              "Incomplete" = "🟡",
                              "Unverified" = "🔵", 
                              "❌")
        cat("  ", status_symbol, instrument, ":", status, "\n")
      }
    }
    cat("\n")
  }
}

#' Create a summary table of completion rates
#' @param completion_result Output from get_participant_completion()
create_completion_summary <- function(completion_result) {
  
  summary_data <- data.frame()
  
  for (participant in completion_result$participants) {
    if (completion_result$is_longitudinal) {
      for (event in completion_result$events) {
        statuses <- unlist(completion_result$completion_data[[participant]][[event]])
        total <- length(statuses)
        completed <- sum(statuses == "Complete", na.rm = TRUE)
        
        summary_data <- rbind(summary_data, data.frame(
          participant_id = participant,
          event_name = event,
          total_instruments = total,
          completed = completed,
          completion_rate = round(completed/total*100, 1),
          stringsAsFactors = FALSE
        ))
      }
    } else {
      statuses <- unlist(completion_result$completion_data[[participant]])
      total <- length(statuses)
      completed <- sum(statuses == "Complete", na.rm = TRUE)
      
      summary_data <- rbind(summary_data, data.frame(
        participant_id = participant,
        total_instruments = total,
        completed = completed,
        completion_rate = round(completed/total*100, 1),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_data)
}

# Example usage (uncomment to run):
# project <- redcap_project()
# completion_result <- get_participant_completion(project)
# print_completion_report(completion_result)
# summary_table <- create_completion_summary(completion_result)