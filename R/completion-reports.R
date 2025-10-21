#' Get Event Completion Summary
#'
#' @description
#' Generates a summary of form completion status across events for longitudinal
#' REDCap projects. Shows how many participants have completed each form at each
#' event, useful for tracking study progress and identifying completion gaps.
#'
#' @param project A redcap_project object
#' @param forms Character vector of form names to include. If NULL, includes all forms (default: NULL)
#' @param events Character vector of event names to include. If NULL, includes all events (default: NULL)
#' @param format Character. Output format: "wide" (default) or "long"
#' @param datatable Logical. If TRUE, returns a datatable output (default: FALSE)
#'
#' @return A data frame with completion counts by event and form
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' 
#' # Get completion summary for all events and forms
#' completion <- get_event_completion_summary(project)
#' 
#' # Specific forms
#' completion <- get_event_completion_summary(
#'   project,
#'   forms = c("demographics", "baseline_survey", "follow_up")
#' )
#' }
get_event_completion_summary <- function(project, 
                                        forms = NULL, 
                                        events = NULL,
                                        format = c("wide", "long"),
                                        datatable = FALSE) {
  
  format <- match.arg(format)
  data <- project$data
  
  # Check if project is longitudinal
  if (!"redcap_event_name" %in% names(data)) {
    cli::cli_alert_warning("Project does not appear to be longitudinal (no redcap_event_name field)")
    return(NULL)
  }
  
  # Get metadata for forms
  metadata <- project$metadata
  
  # Get list of forms and their completion fields
  form_list <- metadata %>%
    dplyr::select(form_name, field_name) %>%
    dplyr::distinct(form_name) %>%
    dplyr::pull(form_name)
  
  # Filter to requested forms
  if (!is.null(forms)) {
    form_list <- intersect(form_list, forms)
    if (length(form_list) == 0) {
      cli::cli_alert_danger("None of the specified forms found in project")
      return(NULL)
    }
  }
  
  # Get unique events
  all_events <- unique(data$redcap_event_name)
  all_events <- all_events[!is.na(all_events)]
  
  # Filter to requested events
  if (!is.null(events)) {
    all_events <- intersect(all_events, events)
    if (length(all_events) == 0) {
      cli::cli_alert_danger("None of the specified events found in project")
      return(NULL)
    }
  }
  
  # Build completion summary
  completion_list <- list()
  
  for (event in all_events) {
    event_data <- data %>% dplyr::filter(redcap_event_name == event)
    
    if (nrow(event_data) == 0) next
    
    event_row <- data.frame(
      event = event,
      n_participants = nrow(event_data),
      stringsAsFactors = FALSE
    )
    
    for (form in form_list) {
      complete_field <- paste0(form, "_complete")
      
      if (complete_field %in% names(event_data)) {
        # Count complete (status = 2)
        n_complete <- sum(event_data[[complete_field]] == 2, na.rm = TRUE)
        # Count incomplete (status = 1)
        n_incomplete <- sum(event_data[[complete_field]] == 1, na.rm = TRUE)
        # Count unverified (status = 3, if exists)
        n_unverified <- sum(event_data[[complete_field]] == 3, na.rm = TRUE)
        # Not started (NA or 0)
        n_not_started <- sum(is.na(event_data[[complete_field]]) | 
                              event_data[[complete_field]] == 0, na.rm = TRUE)
        
        event_row[[paste0(form, "_complete")]] <- n_complete
        event_row[[paste0(form, "_incomplete")]] <- n_incomplete
        event_row[[paste0(form, "_unverified")]] <- n_unverified
        event_row[[paste0(form, "_not_started")]] <- n_not_started
        event_row[[paste0(form, "_pct_complete")]] <- round(n_complete / nrow(event_data) * 100, 1)
      }
    }
    
    completion_list[[event]] <- event_row
  }
  
  if (length(completion_list) == 0) {
    cli::cli_alert_warning("No completion data found")
    return(NULL)
  }
  
  # Combine results
  result <- dplyr::bind_rows(completion_list)
  
  # Convert to long format if requested
  if (format == "long") {
    result <- result %>%
      tidyr::pivot_longer(
        cols = -c(event, n_participants),
        names_to = c("form", "status"),
        names_pattern = "(.+)_(complete|incomplete|unverified|not_started|pct_complete)",
        values_to = "value"
      ) %>%
      tidyr::pivot_wider(
        names_from = status,
        values_from = value
      )
  }
  
  if (datatable) {
    DT::datatable(result)
    invisible(result)
  } else {
    class(result) <- c("event_completion_summary", class(result))
    result
  }
}


#' Print Event Completion Summary
#'
#' @param x An event_completion_summary object
#' @param ... Additional arguments (not used)
#'
#' @export
print.event_completion_summary <- function(x, ...) {
  cli::cli_h2("Event Completion Summary")
  
  cat("\n")
  cli::cli_text("Events: {.val {nrow(x)}}")
  cli::cli_text("Total participants tracked: {.val {sum(x$n_participants)}}")
  
  # Calculate overall completion rate
  pct_cols <- grep("_pct_complete$", names(x), value = TRUE)
  if (length(pct_cols) > 0) {
    avg_completion <- mean(unlist(x[, pct_cols]), na.rm = TRUE)
    cli::cli_text("Average completion rate: {.val {round(avg_completion, 1)}%}")
  }
  
  cat("\n")
  print(as.data.frame(x))
  invisible(x)
}


#' Get Retention Summary
#'
#' @description
#' Calculates participant retention across events in a longitudinal study.
#' Shows the number and percentage of participants retained at each event
#' compared to the baseline/first event.
#'
#' @param project A redcap_project object
#' @param baseline_event Character. Name of the baseline event. If NULL, uses first event (default: NULL)
#' @param definition Character. How to define retention: "any_data" (default) or "complete_form"
#' @param form Character. If definition = "complete_form", which form to check (default: NULL)
#' @param datatable Logical. If TRUE, returns a datatable output (default: FALSE)
#'
#' @return A data frame with retention statistics by event
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' 
#' # Get retention summary
#' retention <- get_retention_summary(project)
#' 
#' # Based on specific form completion
#' retention <- get_retention_summary(
#'   project,
#'   definition = "complete_form",
#'   form = "follow_up_survey"
#' )
#' }
get_retention_summary <- function(project, 
                                 baseline_event = NULL,
                                 definition = c("any_data", "complete_form"),
                                 form = NULL,
                                 datatable = FALSE) {
  
  definition <- match.arg(definition)
  data <- project$data
  id_field <- project$id_field
  
  # Check if project is longitudinal
  if (!"redcap_event_name" %in% names(data)) {
    cli::cli_alert_warning("Project does not appear to be longitudinal (no redcap_event_name field)")
    return(NULL)
  }
  
  # Get unique events
  events <- unique(data$redcap_event_name)
  events <- events[!is.na(events)]
  
  # Determine baseline event
  if (is.null(baseline_event)) {
    # Try to get from project metadata if available
    event_info <- tryCatch({
      project$export_events()
    }, error = function(e) NULL)
    
    if (!is.null(event_info) && nrow(event_info) > 0) {
      baseline_event <- event_info %>%
        dplyr::arrange(day_offset) %>%
        dplyr::slice(1) %>%
        dplyr::pull(unique_event_name)
    } else {
      # Use first event in data
      baseline_event <- events[1]
    }
    
    cli::cli_alert_info("Using {.val {baseline_event}} as baseline event")
  }
  
  # Get baseline participants
  baseline_ids <- data %>%
    dplyr::filter(redcap_event_name == baseline_event) %>%
    dplyr::pull(!!rlang::sym(id_field)) %>%
    unique()
  
  n_baseline <- length(baseline_ids)
  
  if (n_baseline == 0) {
    cli::cli_alert_danger("No participants found at baseline event")
    return(NULL)
  }
  
  # Calculate retention for each event
  retention_list <- list()
  
  for (event in events) {
    event_data <- data %>%
      dplyr::filter(redcap_event_name == event)
    
    if (nrow(event_data) == 0) {
      retained_ids <- character(0)
    } else if (definition == "any_data") {
      # Count participants with any data at this event
      retained_ids <- event_data %>%
        dplyr::pull(!!rlang::sym(id_field)) %>%
        unique()
    } else if (definition == "complete_form") {
      # Count participants with completed form
      if (is.null(form)) {
        cli::cli_alert_danger("Must specify form when definition = 'complete_form'")
        return(NULL)
      }
      
      complete_field <- paste0(form, "_complete")
      if (!complete_field %in% names(event_data)) {
        cli::cli_alert_warning("Form {.val {form}} not found at event {.val {event}}")
        retained_ids <- character(0)
      } else {
        retained_ids <- event_data %>%
          dplyr::filter(!!rlang::sym(complete_field) == 2) %>%
          dplyr::pull(!!rlang::sym(id_field)) %>%
          unique()
      }
    }
    
    # Calculate retention among baseline participants
    n_retained <- sum(retained_ids %in% baseline_ids)
    retention_rate <- round(n_retained / n_baseline * 100, 1)
    
    # Calculate attrition
    n_lost <- n_baseline - n_retained
    attrition_rate <- round(n_lost / n_baseline * 100, 1)
    
    retention_list[[event]] <- data.frame(
      event = event,
      n_baseline = n_baseline,
      n_retained = n_retained,
      n_lost = n_lost,
      retention_rate = retention_rate,
      attrition_rate = attrition_rate,
      stringsAsFactors = FALSE
    )
  }
  
  result <- dplyr::bind_rows(retention_list)
  
  if (datatable) {
    DT::datatable(result)
    invisible(result)
  } else {
    class(result) <- c("retention_summary", class(result))
    result
  }
}


#' Print Retention Summary
#'
#' @param x A retention_summary object
#' @param ... Additional arguments (not used)
#'
#' @export
print.retention_summary <- function(x, ...) {
  cli::cli_h2("Retention Summary")
  
  cat("\n")
  cli::cli_text("Baseline participants: {.val {unique(x$n_baseline)}}")
  cli::cli_text("Events tracked: {.val {nrow(x)}}")
  
  # Final retention
  final_retention <- x %>% 
    dplyr::slice(dplyr::n()) %>% 
    dplyr::pull(retention_rate)
  
  final_n <- x %>% 
    dplyr::slice(dplyr::n()) %>% 
    dplyr::pull(n_retained)
  
  cli::cli_text("Final retention: {.val {final_n}} ({.val {final_retention}%})")
  
  cat("\n")
  print(as.data.frame(x))
  invisible(x)
}


#' Plot Attrition Curve
#'
#' @description
#' Creates a visualization of participant retention/attrition over time using
#' event day_offset values. Requires ggplot2 to be installed.
#'
#' @param project A redcap_project object, or an attrition_over_time data frame
#' @param baseline_event Character. Name of the baseline event (if project is provided)
#' @param definition Character. How to define retention (if project is provided)
#' @param form Character. Form to check if definition = "complete_form" (if project is provided)
#' @param metric Character. Which metric to plot: "retention_rate" (default), "attrition_rate", 
#'   "n_retained", or "n_lost"
#' @param show_points Logical. Whether to show points at each event (default: TRUE)
#' @param show_labels Logical. Whether to show event labels (default: FALSE)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' 
#' # Quick plot
#' plot_attrition_curve(project)
#' 
#' # Customize
#' plot_attrition_curve(project, metric = "n_retained", show_labels = TRUE)
#' 
#' # Or pass pre-calculated attrition data
#' attrition <- get_attrition_over_time(project)
#' plot_attrition_curve(attrition)
#' }
plot_attrition_curve <- function(project,
                                baseline_event = NULL,
                                definition = c("any_data", "complete_form"),
                                form = NULL,
                                metric = c("retention_rate", "attrition_rate", "n_retained", "n_lost"),
                                show_points = TRUE,
                                show_labels = FALSE) {
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.", call. = FALSE)
  }
  
  metric <- match.arg(metric)
  
  # Get attrition data if project object provided
  if (inherits(project, "redcap_project")) {
    definition <- match.arg(definition)
    attrition_data <- get_attrition_over_time(
      project = project,
      baseline_event = baseline_event,
      definition = definition,
      form = form
    )
  } else if (inherits(project, "attrition_over_time") || is.data.frame(project)) {
    attrition_data <- project
  } else {
    stop("First argument must be a redcap_project or attrition_over_time object", call. = FALSE)
  }
  
  if (is.null(attrition_data) || nrow(attrition_data) == 0) {
    cli::cli_alert_warning("No attrition data available to plot")
    return(NULL)
  }
  
  # Set up labels based on metric
  y_label <- switch(metric,
    "retention_rate" = "Retention Rate (%)",
    "attrition_rate" = "Attrition Rate (%)",
    "n_retained" = "Number of Participants Retained",
    "n_lost" = "Number of Participants Lost"
  )
  
  title <- switch(metric,
    "retention_rate" = "Participant Retention Over Time",
    "attrition_rate" = "Participant Attrition Over Time",
    "n_retained" = "Participants Retained Over Time",
    "n_lost" = "Participants Lost Over Time"
  )
  
  # Create plot
  p <- ggplot2::ggplot(attrition_data, ggplot2::aes(x = day_offset, y = !!rlang::sym(metric))) +
    ggplot2::geom_line(linewidth = 1, color = "#2C3E50") +
    ggplot2::labs(
      title = title,
      x = "Days from Baseline",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add points if requested
  if (show_points) {
    p <- p + ggplot2::geom_point(size = 3, color = "#E74C3C")
  }
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = event_name),
      vjust = -0.5,
      hjust = 0.5,
      size = 3,
      angle = 45
    )
  }
  
  # Add percentage formatting for rate metrics
  if (metric %in% c("retention_rate", "attrition_rate")) {
    p <- p + ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20)
    )
  }
  
  return(p)
}

#' Get Form Completion Status
#'
#' @description
#' Gets the completion status for specific forms across all participants,
#' useful for identifying which participants need follow-up.
#'
#' @param project A redcap_project object
#' @param forms Character vector of form names to check
#' @param event Character. Event name for longitudinal projects (default: NULL)
#' @param status Character vector. Which statuses to include: "complete", "incomplete", 
#'   "not_started", "unverified". Default is all statuses.
#'
#' @return A data frame with participant IDs and their completion status for each form
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' 
#' # Get all completion statuses
#' completion <- get_form_completion_status(
#'   project,
#'   forms = c("demographics", "baseline_survey")
#' )
#' 
#' # Only incomplete forms (for follow-up)
#' incomplete <- get_form_completion_status(
#'   project,
#'   forms = c("follow_up_1", "follow_up_2"),
#'   status = c("incomplete", "not_started")
#' )
#' }
get_form_completion_status <- function(project, 
                                       forms,
                                       event = NULL,
                                       status = c("complete", "incomplete", "not_started", "unverified")) {
  
  data <- project$data
  id_field <- project$id_field
  
  # Filter to event if specified
  if (!is.null(event)) {
    if (!"redcap_event_name" %in% names(data)) {
      cli::cli_alert_warning("Project does not have redcap_event_name field")
      return(NULL)
    }
    data <- data %>% dplyr::filter(redcap_event_name == event)
  }
  
  # Build status data
  status_data <- data %>%
    dplyr::select(!!rlang::sym(id_field))
  
  # Map status codes
  status_map <- c(
    "0" = "not_started",
    "1" = "incomplete", 
    "2" = "complete",
    "3" = "unverified"
  )
  
  for (form in forms) {
    complete_field <- paste0(form, "_complete")
    
    if (complete_field %in% names(data)) {
      status_data[[form]] <- dplyr::case_when(
        is.na(data[[complete_field]]) ~ "not_started",
        data[[complete_field]] == 0 ~ "not_started",
        data[[complete_field]] == 1 ~ "incomplete",
        data[[complete_field]] == 2 ~ "complete",
        data[[complete_field]] == 3 ~ "unverified",
        TRUE ~ "unknown"
      )
    } else {
      cli::cli_alert_warning("Form {.val {form}} not found in data")
      status_data[[form]] <- NA_character_
    }
  }
  
  # Filter to requested statuses
  if (!all(c("complete", "incomplete", "not_started", "unverified") %in% status)) {
    # Filter rows where at least one form matches requested status
    status_cols <- setdiff(names(status_data), id_field)
    
    status_data <- status_data %>%
      dplyr::filter(
        dplyr::if_any(
          dplyr::all_of(status_cols),
          ~ . %in% status
        )
      )
  }
  
  return(status_data)
}


#' Get Attrition Over Time
#'
#' @description
#' Calculates participant retention/attrition across events in a longitudinal study,
#' incorporating the day_offset from event metadata. This provides a time-based view
#' of participant retention for plotting attrition curves.
#'
#' @param project A redcap_project object
#' @param baseline_event Character. Name of the baseline event. If NULL, uses first event by day_offset (default: NULL)
#' @param definition Character. How to define retention: "any_data" (default) or "complete_form"
#' @param form Character. If definition = "complete_form", which form to check (default: NULL)
#'
#' @return A data frame with retention statistics by event including day_offset for time-series plotting
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' 
#' # Get attrition over time
#' attrition <- get_attrition_over_time(project)
#' 
#' # Plot with ggplot2
#' library(ggplot2)
#' ggplot(attrition, aes(x = day_offset, y = retention_rate)) +
#'   geom_line() +
#'   geom_point() +
#'   labs(title = "Participant Retention Over Time",
#'        x = "Days from Baseline",
#'        y = "Retention Rate (%)")
#' }
get_attrition_over_time <- function(project, 
                                    baseline_event = NULL,
                                    definition = c("any_data", "complete_form"),
                                    form = NULL) {
  
  definition <- match.arg(definition)
  data <- project$data
  id_field <- project$id_field
  
  # Check if project is longitudinal
  if (!"redcap_event_name" %in% names(data)) {
    cli::cli_alert_warning("Project does not appear to be longitudinal (no redcap_event_name field)")
    return(NULL)
  }
  
  # Get event information with day_offset
  events_info <- project$events
  
  if (is.null(events_info) || nrow(events_info) == 0) {
    cli::cli_alert_warning("No event information available in project")
    return(NULL)
  }
  
  # Ensure day_offset is numeric
  if (!"day_offset" %in% names(events_info)) {
    cli::cli_alert_warning("day_offset not found in event information")
    return(NULL)
  }
  
  events_info <- events_info %>%
    dplyr::mutate(day_offset = as.numeric(day_offset))
  
  # Get unique events from data
  data_events <- unique(data$redcap_event_name)
  data_events <- data_events[!is.na(data_events)]
  
  # Match events between data and event info
  events_info <- events_info %>%
    dplyr::filter(unique_event_name %in% data_events) %>%
    dplyr::arrange(day_offset)
  
  if (nrow(events_info) == 0) {
    cli::cli_alert_warning("No matching events found between data and event information")
    return(NULL)
  }
  
  # Determine baseline event
  if (is.null(baseline_event)) {
    baseline_event <- events_info %>%
      dplyr::slice(1) %>%
      dplyr::pull(unique_event_name)
    
    cli::cli_alert_info("Using {.val {baseline_event}} as baseline event")
  }
  
  # Get baseline participants
  baseline_ids <- data %>%
    dplyr::filter(redcap_event_name == baseline_event) %>%
    dplyr::pull(!!rlang::sym(id_field)) %>%
    unique()
  
  n_baseline <- length(baseline_ids)
  
  if (n_baseline == 0) {
    cli::cli_alert_danger("No participants found at baseline event")
    return(NULL)
  }
  
  # Calculate retention for each event
  retention_list <- list()
  
  for (i in seq_len(nrow(events_info))) {
    event <- events_info$unique_event_name[i]
    event_name <- events_info$event_name[i]
    day_offset <- events_info$day_offset[i]
    
    event_data <- data %>%
      dplyr::filter(redcap_event_name == event)
    
    if (nrow(event_data) == 0) {
      retained_ids <- character(0)
    } else if (definition == "any_data") {
      # Count participants with any data at this event
      retained_ids <- event_data %>%
        dplyr::pull(!!rlang::sym(id_field)) %>%
        unique()
    } else if (definition == "complete_form") {
      # Count participants with completed form
      if (is.null(form)) {
        cli::cli_alert_danger("Must specify form when definition = 'complete_form'")
        return(NULL)
      }
      
      complete_field <- paste0(form, "_complete")
      if (!complete_field %in% names(event_data)) {
        cli::cli_alert_warning("Form {.val {form}} not found at event {.val {event}}")
        retained_ids <- character(0)
      } else {
        retained_ids <- event_data %>%
          dplyr::filter(!!rlang::sym(complete_field) == 2) %>%
          dplyr::pull(!!rlang::sym(id_field)) %>%
          unique()
      }
    }
    
    # Calculate retention among baseline participants
    n_retained <- sum(retained_ids %in% baseline_ids)
    retention_rate <- round(n_retained / n_baseline * 100, 1)
    
    # Calculate attrition
    n_lost <- n_baseline - n_retained
    attrition_rate <- round(n_lost / n_baseline * 100, 1)
    
    retention_list[[event]] <- data.frame(
      event_name = event_name,
      unique_event_name = event,
      day_offset = day_offset,
      n_baseline = n_baseline,
      n_retained = n_retained,
      n_lost = n_lost,
      retention_rate = retention_rate,
      attrition_rate = attrition_rate,
      stringsAsFactors = FALSE
    )
  }
  
  result <- dplyr::bind_rows(retention_list) %>%
    dplyr::arrange(day_offset)
  
  class(result) <- c("attrition_over_time", "data.frame")
  result
}


#' Print Attrition Over Time
#'
#' @param x An attrition_over_time object
#' @param ... Additional arguments (not used)
#'
#' @export
print.attrition_over_time <- function(x, ...) {
  cli::cli_h2("Attrition Over Time")
  
  cat("\n")
  cli::cli_text("Baseline participants: {.val {unique(x$n_baseline)}}")
  cli::cli_text("Events tracked: {.val {nrow(x)}}")
  cli::cli_text("Time span: {.val {min(x$day_offset)}} to {.val {max(x$day_offset)}} days")
  
  # Final retention
  final_retention <- x %>% 
    dplyr::slice(dplyr::n()) %>% 
    dplyr::pull(retention_rate)
  
  final_n <- x %>% 
    dplyr::slice(dplyr::n()) %>% 
    dplyr::pull(n_retained)
  
  cli::cli_text("Final retention: {.val {final_n}} ({.val {final_retention}%})")
  
  cat("\n")
  print(as.data.frame(x))
  invisible(x)
}

