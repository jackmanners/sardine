#' Data Qua#' @name data-validation
NULL and Validation Functions
#'
#' @description
#' Functions for assessing and validating data quality in REDCap projects,
#' including missing data analysis, data type validation, and quality metrics.
#'
#' @importFrom dplyr select summarise_all mutate arrange filter left_join group_by summarise n n_distinct slice_head rowwise c_across slice all_of bind_rows pull
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent
#' @importFrom stringr str_extract_all str_remove str_trim
#'
#' @name data-validation
NULLortFrom stringr str_extract_all str_remove str_trimand Validatio#' @examples
#' \dontrun{
#' project <- redcap_project()
#' analysis <- analyze_missing_data(project)nctions
#'
#' @description
#' Functions for assessing and validating data quality in REDCap projects,
#' including missing data analysis, data type validation, and quality metrics.
#'
#' @importFrom dplyr select summarise_all mutate arrange filter left_join group_by summarise n n_distinct slice_head rowwise c_across slice all_of bind_rows pull
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent
#' @importFrom stringr str_ext#' @examples
#' \dontrun{
#' project <- redcap_project()
#' report <- generate_data_quality_report(project)_all str_remove str_trim
#'
#' @name data-validation
NULL

#' Analyze Missing Data Patterns
#'
#' @description
#' Provides comprehensive analysis of missing data patterns in a REDCap project,
#' including missing rates by field, form, and record.
#'
#' @param project A redcap_project object
#' @param by_form Logical. If TRUE, summarize missing data by form (default: TRUE)
#' @param by_field Logical. If TRUE, summarize missing data by field (default: TRUE)
#' @param by_record Logical. If TRUE, summarize missing data by record (default: FALSE)
#' @param threshold Numeric. Highlight fields/forms with missing rates above this threshold (default: 0.20)
#'
#' @return A list containing missing data analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project_from_env()
#' missing_analysis <- analyze_missing_data(project)
#' print(missing_analysis$summary)
#' }
analyze_missing_data <- function(project, by_form = TRUE, by_field = TRUE, by_record = FALSE, threshold = 0.20) {
  
  # Validate inputs
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  if (!is.logical(by_form) || !is.logical(by_field) || !is.logical(by_record)) {
    stop("by_form, by_field, and by_record must be logical")
  }
  
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be a number between 0 and 1")
  }
  
  # Get data and metadata
  data <- project$data
  dictionary <- project$dictionary
  
  if (is.null(data) || nrow(data) == 0) {
    stop("No data available in project")
  }
  
  if (is.null(dictionary) || nrow(dictionary) == 0) {
    stop("No data dictionary available in project")
  }
  
  # Calculate total records
  total_records <- nrow(data)
  
  # Initialize results list
  results <- list(
    project_name = project$short_name,
    total_records = total_records,
    analysis_timestamp = Sys.time(),
    threshold = threshold
  )
  
  # Overall summary
  total_cells <- total_records * (ncol(data) - 1)  # Exclude record_id
  missing_cells <- sum(is.na(data[, -1]))  # Exclude record_id from missing count
  overall_missing_rate <- missing_cells / total_cells
  
  results$summary <- list(
    total_records = total_records,
    total_fields = ncol(data) - 1,
    total_cells = total_cells,
    missing_cells = missing_cells,
    overall_missing_rate = overall_missing_rate
  )
  
  # By field analysis
  if (by_field) {
    field_missing <- data %>%
      dplyr::select(-record_id) %>%
      dplyr::summarise_all(~sum(is.na(.))) %>%
      tidyr::pivot_longer(everything(), names_to = "field_name", values_to = "missing_count") %>%
      dplyr::mutate(
        missing_rate = missing_count / total_records,
        above_threshold = missing_rate > threshold
      ) %>%
      dplyr::arrange(desc(missing_rate))
    
    # Add field labels from dictionary
    field_missing <- field_missing %>%
      dplyr::left_join(
        dictionary %>% dplyr::select(field_name, field_label, field_type, form_name),
        by = "field_name"
      )
    
    results$by_field <- field_missing
    results$high_missing_fields <- field_missing %>%
      dplyr::filter(above_threshold) %>%
      nrow()
  }
  
  # By form analysis
  if (by_form) {
    # Get form assignments for each field
    form_fields <- dictionary %>%
      dplyr::select(field_name, form_name) %>%
      dplyr::filter(field_name %in% names(data))
    
    # Calculate missing by form without using pivot_longer on mixed types
    form_missing_list <- list()
    
    for (form in unique(form_fields$form_name)) {
      form_field_names <- form_fields %>%
        dplyr::filter(form_name == form) %>%
        dplyr::pull(field_name)
      
      # Get only fields that exist in both data and this form
      existing_form_fields <- intersect(form_field_names, names(data))
      
      if (length(existing_form_fields) > 0) {
        form_data <- data %>% dplyr::select(dplyr::all_of(existing_form_fields))
        total_cells <- nrow(form_data) * ncol(form_data)
        missing_cells <- sum(is.na(form_data))
        
        form_missing_list[[form]] <- data.frame(
          form_name = form,
          total_cells = total_cells,
          missing_cells = missing_cells,
          missing_rate = missing_cells / total_cells,
          fields_in_form = length(existing_form_fields),
          above_threshold = (missing_cells / total_cells) > threshold
        )
      }
    }
    
    form_missing <- dplyr::bind_rows(form_missing_list) %>%
      dplyr::arrange(desc(missing_rate))
    
    results$by_form <- form_missing
    results$high_missing_forms <- form_missing %>%
      dplyr::filter(above_threshold) %>%
      nrow()
  }
  
  # By record analysis
  if (by_record) {
    record_missing <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        missing_count = sum(is.na(dplyr::c_across(-record_id))),
        missing_rate = missing_count / (ncol(data) - 1),
        above_threshold = missing_rate > threshold
      ) %>%
      dplyr::select(record_id, missing_count, missing_rate, above_threshold) %>%
      dplyr::arrange(desc(missing_rate))
    
    results$by_record <- record_missing
    results$high_missing_records <- record_missing %>%
      dplyr::filter(above_threshold) %>%
      nrow()
  }
  
  class(results) <- c("missing_data_analysis", "list")
  return(results)
}

#' Print Missing Data Analysis
#'
#' @description
#' Print method for missing data analysis results
#'
#' @param x A missing_data_analysis object
#' @param ... Additional arguments (unused)
#'
#' @export
print.missing_data_analysis <- function(x, ...) {
  cat("Missing Data Analysis for Project:", x$project_name, "\n")
  cat("Analysis Date:", format(x$analysis_timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Threshold for flagging:", scales::percent(x$threshold), "\n\n")
  
  # Overall summary
  cat("=== OVERALL SUMMARY ===\n")
  cat("Total Records:", x$summary$total_records, "\n")
  cat("Total Fields:", x$summary$total_fields, "\n")
  cat("Overall Missing Rate:", scales::percent(x$summary$overall_missing_rate, accuracy = 0.1), "\n")
  
  # High missing counts
  if (!is.null(x$high_missing_fields)) {
    cat("Fields above threshold:", x$high_missing_fields, "\n")
  }
  if (!is.null(x$high_missing_forms)) {
    cat("Forms above threshold:", x$high_missing_forms, "\n")
  }
  if (!is.null(x$high_missing_records)) {
    cat("Records above threshold:", x$high_missing_records, "\n")
  }
  
  cat("\n")
  
  # Field-level summary (top 10)
  if (!is.null(x$by_field)) {
    cat("=== TOP 10 FIELDS BY MISSING RATE ===\n")
    top_fields <- x$by_field %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(
        field_display = paste0(field_name, " (", form_name, ")"),
        missing_display = paste0(missing_count, "/", x$total_records, " (", scales::percent(missing_rate, accuracy = 0.1), ")")
      )
    
    for (i in seq_len(nrow(top_fields))) {
      flag <- if (top_fields$above_threshold[i]) " *" else ""
      cat(sprintf("%-40s %s%s\n", 
                  top_fields$field_display[i], 
                  top_fields$missing_display[i], 
                  flag))
    }
    cat("* Above threshold\n\n")
  }
  
  # Form-level summary
  if (!is.null(x$by_form)) {
    cat("=== FORMS BY MISSING RATE ===\n")
    for (i in seq_len(nrow(x$by_form))) {
      flag <- if (x$by_form$above_threshold[i]) " *" else ""
      cat(sprintf("%-30s %s (%d fields)%s\n",
                  x$by_form$form_name[i],
                  scales::percent(x$by_form$missing_rate[i], accuracy = 0.1),
                  x$by_form$fields_in_form[i],
                  flag))
    }
    cat("* Above threshold\n")
  }
}

#' Validate Data Types Against REDCap Dictionary
#'
#' @description
#' Validates that data in REDCap fields matches the expected data types
#' defined in the data dictionary.
#'
#' @param project A redcap_project object
#' @param strict Logical. If TRUE, applies strict validation rules (default: TRUE)
#'
#' @return A list containing validation results
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' validation <- validate_data_types(project)
#' print(validation)
#' }
validate_data_types <- function(project, strict = TRUE) {
  
  # Validate inputs
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  # Get data and metadata
  data <- project$data
  dictionary <- project$dictionary
  
  if (is.null(data) || nrow(data) == 0) {
    stop("No data available in project")
  }
  
  if (is.null(dictionary) || nrow(dictionary) == 0) {
    stop("No data dictionary available in project")
  }
  
  # Initialize results
  results <- list(
    project_name = project$short_name,
    validation_timestamp = Sys.time(),
    strict_mode = strict,
    issues = list(),
    summary = list()
  )
  
  # Get fields that exist in both data and dictionary
  common_fields <- intersect(names(data), dictionary$field_name)
  
  validation_issues <- list()
  
  for (field in common_fields) {
    if (field == "record_id") next  # Skip record_id
    
    field_info <- dictionary %>% 
      dplyr::filter(field_name == field) %>% 
      dplyr::slice(1)
    
    if (nrow(field_info) == 0) next
    
    field_data <- data[[field]]
    field_type <- field_info$field_type
    validation_min <- field_info$text_validation_min
    validation_max <- field_info$text_validation_max
    validation_type <- field_info$text_validation_type_or_show_slider_number
    
    field_issues <- list()
    
    # Skip validation for completely missing fields
    if (all(is.na(field_data))) {
      next
    }
    
    # Type-specific validation
    if (field_type == "text") {
      # Check for numeric validations on text fields
      if (!is.null(validation_type) && !is.na(validation_type)) {
        if (validation_type %in% c("number", "integer")) {
          non_numeric <- field_data[!is.na(field_data) & !grepl("^-?[0-9]+(\\.[0-9]+)?$", field_data)]
          if (length(non_numeric) > 0) {
            field_issues$non_numeric_values <- list(
              count = length(non_numeric),
              examples = head(non_numeric, 5)
            )
          }
        }
        
        # Range validation for numeric text fields
        if (validation_type %in% c("number", "integer") && (!is.null(validation_min) || !is.null(validation_max))) {
          numeric_values <- suppressWarnings(as.numeric(field_data[!is.na(field_data)]))
          if (!is.null(validation_min) && !is.na(validation_min)) {
            below_min <- numeric_values[!is.na(numeric_values) & numeric_values < as.numeric(validation_min)]
            if (length(below_min) > 0) {
              field_issues$below_minimum <- list(
                count = length(below_min),
                minimum = validation_min,
                examples = head(below_min, 5)
              )
            }
          }
          if (!is.null(validation_max) && !is.na(validation_max)) {
            above_max <- numeric_values[!is.na(numeric_values) & numeric_values > as.numeric(validation_max)]
            if (length(above_max) > 0) {
              field_issues$above_maximum <- list(
                count = length(above_max),
                maximum = validation_max,
                examples = head(above_max, 5)
              )
            }
          }
        }
      }
    } else if (field_type %in% c("radio", "dropdown", "checkbox")) {
      # Validate choice fields
      if (!is.null(field_info$select_choices_or_calculations) && !is.na(field_info$select_choices_or_calculations)) {
        # Parse valid choices
        choices_raw <- field_info$select_choices_or_calculations
        if (field_type == "checkbox") {
          # For checkboxes, we need to handle the special format
          # This is simplified - full implementation would need to handle checkbox naming
          next
        } else {
          # For radio/dropdown, parse choices
          valid_codes <- stringr::str_extract_all(choices_raw, "^([^,]+),")[[1]]
          valid_codes <- stringr::str_remove(valid_codes, ",$")
          valid_codes <- stringr::str_trim(valid_codes)
          
          if (length(valid_codes) > 0) {
            invalid_values <- field_data[!is.na(field_data) & !field_data %in% valid_codes]
            if (length(invalid_values) > 0) {
              field_issues$invalid_choice_values <- list(
                count = length(invalid_values),
                valid_choices = valid_codes,
                examples = head(unique(invalid_values), 5)
              )
            }
          }
        }
      }
    } else if (field_type == "yesno") {
      # Validate yes/no fields
      valid_yesno <- c("0", "1", 0, 1)
      invalid_yesno <- field_data[!is.na(field_data) & !field_data %in% valid_yesno]
      if (length(invalid_yesno) > 0) {
        field_issues$invalid_yesno_values <- list(
          count = length(invalid_yesno),
          examples = head(unique(invalid_yesno), 5)
        )
      }
    }
    
    # Add field issues to results if any found
    if (length(field_issues) > 0) {
      validation_issues[[field]] <- list(
        field_name = field,
        field_label = field_info$field_label,
        field_type = field_type,
        form_name = field_info$form_name,
        issues = field_issues
      )
    }
  }
  
  # Compile summary
  results$issues <- validation_issues
  results$summary <- list(
    total_fields_checked = length(common_fields) - 1,  # Exclude record_id
    fields_with_issues = length(validation_issues),
    validation_pass_rate = 1 - (length(validation_issues) / (length(common_fields) - 1))
  )
  
  class(results) <- c("data_type_validation", "list")
  return(results)
}

#' Print Data Type Validation Results
#'
#' @description
#' Print method for data type validation results
#'
#' @param x A data_type_validation object
#' @param ... Additional arguments (unused)
#'
#' @export
print.data_type_validation <- function(x, ...) {
  cat("Data Type Validation for Project:", x$project_name, "\n")
  cat("Validation Date:", format(x$validation_timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Strict Mode:", x$strict_mode, "\n\n")
  
  # Summary
  cat("=== VALIDATION SUMMARY ===\n")
  cat("Fields Checked:", x$summary$total_fields_checked, "\n")
  cat("Fields with Issues:", x$summary$fields_with_issues, "\n")
  cat("Validation Pass Rate:", scales::percent(x$summary$validation_pass_rate, accuracy = 0.1), "\n\n")
  
  # Details of issues
  if (length(x$issues) > 0) {
    cat("=== VALIDATION ISSUES ===\n")
    for (field_name in names(x$issues)) {
      field_info <- x$issues[[field_name]]
      cat("Field:", field_info$field_name, "(", field_info$field_type, ")\n")
      cat("Form:", field_info$form_name, "\n")
      cat("Label:", field_info$field_label, "\n")
      
      for (issue_type in names(field_info$issues)) {
        issue <- field_info$issues[[issue_type]]
        cat("  Issue:", issue_type, "- Count:", issue$count, "\n")
        if (!is.null(issue$examples)) {
          cat("  Examples:", paste(issue$examples, collapse = ", "), "\n")
        }
        if (!is.null(issue$minimum)) {
          cat("  Minimum expected:", issue$minimum, "\n")
        }
        if (!is.null(issue$maximum)) {
          cat("  Maximum expected:", issue$maximum, "\n")
        }
      }
      cat("\n")
    }
  } else {
    cat("No validation issues found!\n")
  }
}

#' Generate Data Quality Report
#'
#' @description
#' Generates a comprehensive data quality report combining missing data analysis
#' and data type validation.
#'
#' @param project A redcap_project object
#' @param missing_threshold Numeric. Threshold for flagging high missing rates (default: 0.20)
#' @param include_validation Logical. Include data type validation (default: TRUE)
#' @param output_file Character. Optional file path to save report (default: NULL)
#'
#' @return A list containing the complete data quality assessment
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project_from_env()
#' quality_report <- generate_data_quality_report(project)
#' print(quality_report)
#' }
generate_data_quality_report <- function(project, missing_threshold = 0.20, include_validation = TRUE, output_file = NULL) {
  
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  # Generate missing data analysis
  missing_analysis <- analyze_missing_data(
    project, 
    by_form = TRUE, 
    by_field = TRUE, 
    by_record = FALSE, 
    threshold = missing_threshold
  )
  
  # Generate data type validation if requested
  validation_results <- NULL
  if (include_validation) {
    validation_results <- validate_data_types(project, strict = TRUE)
  }
  
  # Compile report
  report <- list(
    project_name = project$short_name,
    report_timestamp = Sys.time(),
    missing_analysis = missing_analysis,
    validation_results = validation_results,
    summary = list(
      total_records = missing_analysis$total_records,
      overall_missing_rate = missing_analysis$summary$overall_missing_rate,
      high_missing_fields = missing_analysis$high_missing_fields %||% 0,
      high_missing_forms = missing_analysis$high_missing_forms %||% 0,
      validation_issues = if (!is.null(validation_results)) validation_results$summary$fields_with_issues else 0
    )
  )
  
  # Save to file if requested
  if (!is.null(output_file)) {
    # Generate markdown report
    report_md <- generate_quality_report_markdown(report)
    writeLines(report_md, output_file)
    cat("Data quality report saved to:", output_file, "\n")
  }
  
  class(report) <- c("data_quality_report", "list")
  return(report)
}

#' Print Data Quality Report
#'
#' @description
#' Print method for data quality reports
#'
#' @param x A data_quality_report object
#' @param ... Additional arguments (unused)
#'
#' @export
print.data_quality_report <- function(x, ...) {
  cat("==============================================\n")
  cat("DATA QUALITY REPORT\n")
  cat("==============================================\n")
  cat("Project:", x$project_name, "\n")
  cat("Report Date:", format(x$report_timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Executive summary
  cat("=== EXECUTIVE SUMMARY ===\n")
  cat("Total Records:", x$summary$total_records, "\n")
  cat("Overall Missing Rate:", scales::percent(x$summary$overall_missing_rate, accuracy = 0.1), "\n")
  cat("Fields with High Missing:", x$summary$high_missing_fields, "\n")
  cat("Forms with High Missing:", x$summary$high_missing_forms, "\n")
  if (!is.null(x$validation_results)) {
    cat("Fields with Validation Issues:", x$summary$validation_issues, "\n")
  }
  cat("\n")
  
  # Print detailed missing analysis
  print(x$missing_analysis)
  
  # Print validation results if available
  if (!is.null(x$validation_results)) {
    cat("\n")
    print(x$validation_results)
  }
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Generate Quality Report Markdown
#'
#' @description
#' Internal function to generate markdown version of quality report
#'
#' @param report A data_quality_report object
#'
#' @return Character vector of markdown lines
#' @keywords internal
generate_quality_report_markdown <- function(report) {
  lines <- c(
    paste("# Data Quality Report:", report$project_name),
    "",
    paste("**Report Generated:** ", format(report$report_timestamp, "%Y-%m-%d %H:%M:%S")),
    "",
    "## Executive Summary",
    "",
    paste("- **Total Records:** ", report$summary$total_records),
    paste("- **Overall Missing Rate:** ", scales::percent(report$summary$overall_missing_rate, accuracy = 0.1)),
    paste("- **Fields with High Missing:** ", report$summary$high_missing_fields),
    paste("- **Forms with High Missing:** ", report$summary$high_missing_forms)
  )
  
  if (!is.null(report$validation_results)) {
    lines <- c(lines, paste("- **Fields with Validation Issues:** ", report$summary$validation_issues))
  }
  
  lines <- c(lines, "", "## Missing Data Analysis")
  
  # Add missing data tables
  if (!is.null(report$missing_analysis$by_field)) {
    lines <- c(lines, "", "### Fields by Missing Rate", "")
    
    # Create table
    field_table <- report$missing_analysis$by_field %>%
      dplyr::slice_head(n = 20) %>%
      dplyr::mutate(
        missing_rate_pct = scales::percent(missing_rate, accuracy = 0.1),
        flag = ifelse(above_threshold, "⚠️", "")
      ) %>%
      dplyr::select(field_name, form_name, missing_count, missing_rate_pct, flag)
    
    lines <- c(lines, "| Field | Form | Missing Count | Missing Rate | Flag |")
    lines <- c(lines, "|-------|------|---------------|--------------|------|")
    
    for (i in seq_len(nrow(field_table))) {
      lines <- c(lines, sprintf("| %s | %s | %d | %s | %s |",
                                field_table$field_name[i],
                                field_table$form_name[i],
                                field_table$missing_count[i],
                                field_table$missing_rate_pct[i],
                                field_table$flag[i]))
    }
  }
  
  return(lines)
}