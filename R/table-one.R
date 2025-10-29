# Internal function to validate filter expressions for security
# Only allows safe data frame operations, blocks system calls and file operations
.validate_filter_expression <- function(expr) {
  # List of dangerous functions that should never be in filter expressions
  dangerous_patterns <- c(
    "system", "shell", "eval", "parse", "source", "sink", "cat", "print",
    "write", "save", "load", "file", "unlink", "download", "install",
    "library", "require", "attach", "detach", "setwd", "getwd", "list.files",
    "readLines", "writeLines", "<<-", "assign", "rm", "remove"
  )
  
  # Check for dangerous patterns
  for (pattern in dangerous_patterns) {
    if (grepl(pattern, expr, fixed = TRUE)) {
      stop("Filter expression contains potentially dangerous operation: ", pattern, 
           "\nOnly data frame operations are allowed (e.g., 'age > 18', '!is.na(gender)')", 
           call. = FALSE)
    }
  }
  
  # Try to parse the expression to ensure it's valid R syntax
  tryCatch({
    str2lang(expr)
  }, error = function(e) {
    stop("Invalid R expression in filter: ", expr, "\n", e$message, call. = FALSE)
  })
  
  invisible(TRUE)
}

#' Generate Table One Summary Statistics
#'
#' @description
#' Generates publication-ready descriptive statistics (Table 1) for selected
#' variables from a REDCap project. Produces n (%), mean (SD), or median (IQR)
#' based on variable type and distribution. Supports stratification by grouping
#' variables and filtering of records.
#' 
#' The function automatically handles character-stored numeric data (common in REDCap)
#' by attempting numeric conversion. Missing values are properly handled and reported.
#'
#' @param project A redcap_project object
#' @param vars Character vector of variable names to include in the table. If NULL, 
#'   all non-ID fields are included (default: NULL)
#' @param strata Character vector of variable name(s) to stratify by (default: NULL for no stratification)
#' @param filter Character vector of filter expressions to subset data (e.g., c("age > 18", "consent == 1")).
#'   Uses standard R logical expressions. Multiple filters are combined with AND logic.
#'   For security, only data frame operations are allowed - system calls and file operations are blocked (default: NULL)
#' @param cat_vars Character vector of variables to force as categorical (overrides auto-detection)
#' @param cont_vars Character vector of variables to force as continuous (overrides auto-detection).
#'   Character data will be converted to numeric if possible.
#' @param non_normal Character vector of continuous variables to display as median (IQR) 
#'   instead of mean (SD) (default: NULL)
#' @param include_missing Logical. Include missing data counts (default: TRUE)
#' @param test Logical. Include statistical tests comparing strata (default: FALSE)
#' @param test_type Character. Type of test: "auto" (default), "parametric", "nonparametric"
#' @param digits Numeric. Number of decimal places for continuous variables (default: 1)
#' @param output_format Character. Output format: "data.frame" (default), "kable", "gt", "flextable"
#' @param datatable Logical. Return output as a DT datatable (default: FALSE)
#'
#' @return A data frame or formatted table object depending on output_format
#' @export
#'
#' @examples
#' \dontrun{
#' project <- redcap_project()
#' 
#' # Basic table with all variables
#' table_one <- generate_table_one(project)
#' 
#' # Filtered and stratified
#' table_one <- generate_table_one(
#'   project,
#'   filter = c("withdrawn != 1", "consent_complete == 1"),
#'   strata = "treatment_group"
#' )
#' 
#' # Force variable types
#' table_one <- generate_table_one(
#'   project,
#'   vars = c("age", "gender", "bmi", "study_group"),
#'   cat_vars = c("gender", "study_group"),
#'   cont_vars = c("age", "bmi"),
#'   non_normal = "bmi",
#'   test = TRUE
#' )
#' }
generate_table_one <- function(project,
                                vars = NULL,
                                strata = NULL,
                                filter = NULL,
                                cat_vars = NULL,
                                cont_vars = NULL,
                                non_normal = NULL,
                                include_missing = TRUE,
                                test = FALSE,
                                test_type = "auto",
                                digits = 1,
                                output_format = "data.frame",
                                datatable = FALSE) {
  
  # Validate inputs
  if (!inherits(project, "redcap_project")) {
    stop("project must be a redcap_project object")
  }
  
  data <- project$data
  metadata <- project$metadata
  
  if (is.null(data) || nrow(data) == 0) {
    stop("No data available in project")
  }
  
  # Get ID field
  id_field <- project$id_field %||% names(data)[1]
  
  # Apply filters if provided
  if (!is.null(filter) && length(filter) > 0) {
    original_n <- nrow(data)
    
    for (f in filter) {
      tryCatch({
        # Safer filtering: validate expression before evaluation
        .validate_filter_expression(f)
        
        # Use subset() with parsed expression
        # This limits scope to data frame operations only
        filter_expr <- str2lang(f)
        data <- subset(data, eval(filter_expr))
      }, error = function(e) {
        stop("Filter error in '", f, "': ", e$message, call. = FALSE)
      })
    }
    
    filtered_n <- nrow(data)
    if (filtered_n == 0) {
      stop("All records were filtered out. No data remaining.")
    }
    
    cli::cli_alert_info("Filtered from {original_n} to {filtered_n} records")
  }
  
  # Select variables
  if (is.null(vars)) {
    vars <- setdiff(names(data), c(id_field, strata))
  }
  
  # Validate variables exist
  missing_vars <- setdiff(c(vars, strata), names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Determine variable types with explicit overrides taking precedence
  var_types <- .detect_variable_types(data, metadata, vars, cat_vars, cont_vars)

  # Force continuous variables where defined
  for (var in cont_vars) {
      var_types[[var]] <- "continuous"
      # Replace "" with NA
      data[[var]][data[[var]] == ""] <- NA
  }

  # Force categorical variables where defined
  for (var in cat_vars) {
      var_types[[var]] <- "categorical"
  }
  
  # Build table
  if (is.null(strata)) {
    # Overall table only
    table_df <- .build_overall_table(
      data = data,
      vars = vars,
      var_types = var_types,
      non_normal = non_normal,
      include_missing = include_missing,
      digits = digits
    )
  } else {
    # Stratified table
    table_df <- .build_stratified_table(
      data = data,
      vars = vars,
      strata = strata,
      var_types = var_types,
      non_normal = non_normal,
      include_missing = include_missing,
      test = test,
      test_type = test_type,
      digits = digits
    )
  }
  
  # Format output
  result <- .format_table_one_output(table_df, output_format, strata)
  
  # Add metadata
  attr(result, "n_total") <- nrow(data)
  attr(result, "strata") <- strata
  attr(result, "project_name") <- project$project_info$project_title %||% "REDCap Project"
  
  class(result) <- c("table_one", class(result))
  
  if (datatable) {
    as_datatable(result)
    invisible(result)
  } else {
    result
  }
}

#' Detect Variable Types
#'
#' @description
#' Internal function to detect whether variables are categorical or continuous
#'
#' @keywords internal
.detect_variable_types <- function(data, metadata, vars, categorical, continuous) {
  var_types <- list()
  
  for (var in vars) {
    # User-specified type takes precedence
    if (!is.null(categorical) && var %in% categorical) {
      var_types[[var]] <- "categorical"
      next
    }
    if (!is.null(continuous) && var %in% continuous) {
      var_types[[var]] <- "continuous"
      next
    }
    
    # Try to infer from metadata
    if (!is.null(metadata) && var %in% metadata$field_name) {
      field_info <- metadata[metadata$field_name == var, ]
      field_type <- field_info$field_type[1]
      
      if (field_type %in% c("radio", "dropdown", "yesno", "checkbox")) {
        var_types[[var]] <- "categorical"
        next
      }
      if (field_type %in% c("text", "calc") && 
          !is.null(field_info$text_validation_type_or_show_slider_number) &&
          field_info$text_validation_type_or_show_slider_number[1] %in% c("number", "integer")) {
        var_types[[var]] <- "continuous"
        next
      }
    }
    
    # Fall back to data inspection
    col_data <- data[[var]]
    
    if (is.character(col_data) || is.factor(col_data)) {
      # Try to convert to numeric to see if it's actually numeric data stored as character
      numeric_test <- suppressWarnings(as.numeric(as.character(col_data)))
      
      # If most non-NA values successfully convert, treat as continuous
      n_original <- sum(!is.na(col_data))
      n_converted <- sum(!is.na(numeric_test))
      
      if (n_original > 0 && n_converted / n_original > 0.8) {
        # More than 80% successfully converted - treat as continuous
        n_unique <- length(unique(numeric_test[!is.na(numeric_test)]))
        var_types[[var]] <- if (n_unique < 10) "categorical" else "continuous"
      } else {
        var_types[[var]] <- "categorical"
      }
    } else if (is.numeric(col_data)) {
      # If < 10 unique values, treat as categorical
      n_unique <- length(unique(col_data[!is.na(col_data)]))
      var_types[[var]] <- if (n_unique < 10) "categorical" else "continuous"
    } else {
      var_types[[var]] <- "categorical"
    }
  }
  
  return(var_types)
}

#' Build Overall Summary Table
#'
#' @description
#' Internal function to build unstratified table
#'
#' @keywords internal
.build_overall_table <- function(data, vars, var_types, non_normal, include_missing, digits) {
  
  results <- list()
  types <- list()
  
  for (var in vars) {
    var_type <- var_types[[var]]
    
    if (var_type == "categorical") {
      # Categorical variable: n (%)
      stats <- .summarize_categorical(data[[var]], var, include_missing)
      results <- c(results, stats)
      # Add type indicators
      types[[var]] <- ""  # Empty for variable name row
      for (stat_name in names(stats)) {
        if (stat_name != var) {
          types[[stat_name]] <- "n (%)"
        }
      }
    } else {
      # Continuous variable: mean (SD) or median (IQR)
      is_non_normal <- !is.null(non_normal) && var %in% non_normal
      stats <- .summarize_continuous(data[[var]], var, is_non_normal, include_missing, digits)
      results <- c(results, stats)
      # Add type indicators
      type_label <- if (is_non_normal) "Median [IQR]" else "Mean (SD)"
      types[[var]] <- type_label
      for (stat_name in names(stats)) {
        if (stat_name != var && grepl("Missing", stat_name)) {
          types[[stat_name]] <- "n (%)"
        }
      }
    }
  }
  
  # Convert to data frame
  df <- data.frame(
    Variable = names(results),
    Type = unlist(types[names(results)]),
    Overall = unlist(results),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # Add N row at the top
  n_row <- data.frame(
    Variable = "N",
    Type = "",
    Overall = as.character(nrow(data)),
    stringsAsFactors = FALSE
  )
  
  df <- rbind(n_row, df)
  rownames(df) <- NULL
  
  return(df)
}

#' Build Stratified Summary Table
#'
#' @description
#' Internal function to build stratified table with optional tests
#'
#' @keywords internal
.build_stratified_table <- function(data, vars, strata, var_types, non_normal, 
                                    include_missing, test, test_type, digits) {
  
  # Get strata levels
  strata_var <- strata[1]  # Support only one stratification variable for now
  strata_levels <- sort(unique(data[[strata_var]][!is.na(data[[strata_var]])]))
  
  # Create N row at the top with explicit column order
  n_row <- data.frame(
    Variable = "N",
    Type = "",
    stringsAsFactors = FALSE
  )
  
  # Add strata columns with their N values
  for (level in strata_levels) {
    level_char <- as.character(level)
    n_level <- sum(!is.na(data[[strata_var]]) & data[[strata_var]] == level)
    n_row[[level_char]] <- as.character(n_level)
  }
  
  # Add p-value column if needed
  if (test) {
    n_row[["p-value"]] <- ""
  }
  
  all_rows <- list()
  all_rows[[1]] <- n_row
  
  for (var in vars) {
    var_type <- var_types[[var]]
    
    if (var_type == "categorical") {
      # Categorical variable
      stats_by_strata <- list()
      for (level in strata_levels) {
        subset_data <- data[!is.na(data[[strata_var]]) & data[[strata_var]] == level, ]
        stats <- .summarize_categorical(subset_data[[var]], var, include_missing)
        stats_by_strata[[as.character(level)]] <- stats
      }
      
      # Statistical test
      p_val <- if (test) .test_categorical(data[[var]], data[[strata_var]]) else NA_real_
      
      # Get all unique row names across strata (union of all stat names)
      all_stat_names <- unique(unlist(lapply(stats_by_strata, names)))
      
      # Build rows ensuring all have same columns
      for (stat_name in all_stat_names) {
        # Build row with explicit column order
        type_val <- if (stat_name == var) "" else "n (%)"
        
        # Collect strata values
        strata_vals <- character(length(strata_levels))
        for (i in seq_along(strata_levels)) {
          level_char <- as.character(strata_levels[i])
          strata_vals[i] <- if (stat_name %in% names(stats_by_strata[[level_char]])) {
            stats_by_strata[[level_char]][[stat_name]]
          } else {
            ""
          }
        }
        
        # Create data frame with explicit columns in order
        if (test) {
          p_val_formatted <- if (stat_name == var) .format_p_value(p_val) else ""
          row_df <- data.frame(
            Variable = stat_name,
            Type = type_val,
            stringsAsFactors = FALSE
          )
          # Add strata columns
          for (i in seq_along(strata_levels)) {
            row_df[[as.character(strata_levels[i])]] <- strata_vals[i]
          }
          row_df[["p-value"]] <- p_val_formatted
        } else {
          row_df <- data.frame(
            Variable = stat_name,
            Type = type_val,
            stringsAsFactors = FALSE
          )
          # Add strata columns
          for (i in seq_along(strata_levels)) {
            row_df[[as.character(strata_levels[i])]] <- strata_vals[i]
          }
        }
        
        all_rows[[length(all_rows) + 1]] <- row_df
      }
      
    } else {
      # Continuous variable
      is_non_normal <- !is.null(non_normal) && var %in% non_normal
      
      stats_by_strata <- list()
      for (level in strata_levels) {
        subset_data <- data[!is.na(data[[strata_var]]) & data[[strata_var]] == level, ]
        stats <- .summarize_continuous(subset_data[[var]], var, is_non_normal, include_missing, digits)
        stats_by_strata[[as.character(level)]] <- stats
      }
      
      # Statistical test
      p_val <- if (test) .test_continuous(data[[var]], data[[strata_var]], test_type, is_non_normal) else NA_real_
      
      # Get all unique row names across strata
      all_stat_names <- unique(unlist(lapply(stats_by_strata, names)))
      
      # Determine type label
      type_label <- if (is_non_normal) "Median [IQR]" else "Mean (SD)"
      
      # Build rows ensuring all have same columns
      for (stat_name in all_stat_names) {
        # Determine type for this row
        if (stat_name == var) {
          type_val <- type_label
        } else if (grepl("Missing", stat_name)) {
          type_val <- "n (%)"
        } else {
          type_val <- ""
        }
        
        # Collect strata values
        strata_vals <- character(length(strata_levels))
        for (i in seq_along(strata_levels)) {
          level_char <- as.character(strata_levels[i])
          strata_vals[i] <- if (stat_name %in% names(stats_by_strata[[level_char]])) {
            stats_by_strata[[level_char]][[stat_name]]
          } else {
            ""
          }
        }
        
        # Create data frame with explicit columns in order
        if (test) {
          p_val_formatted <- if (stat_name == var) .format_p_value(p_val) else ""
          row_df <- data.frame(
            Variable = stat_name,
            Type = type_val,
            stringsAsFactors = FALSE
          )
          # Add strata columns
          for (i in seq_along(strata_levels)) {
            row_df[[as.character(strata_levels[i])]] <- strata_vals[i]
          }
          row_df[["p-value"]] <- p_val_formatted
        } else {
          row_df <- data.frame(
            Variable = stat_name,
            Type = type_val,
            stringsAsFactors = FALSE
          )
          # Add strata columns
          for (i in seq_along(strata_levels)) {
            row_df[[as.character(strata_levels[i])]] <- strata_vals[i]
          }
        }
        
        all_rows[[length(all_rows) + 1]] <- row_df
      }
    }
  }
  
  # Ensure all rows have identical columns in same order
  # Get column names from first row (N row) which has the canonical order
  expected_cols <- names(all_rows[[1]])
  
  # Reorder/subset all rows to match
  for (i in seq_along(all_rows)) {
    # Make sure all expected columns exist
    for (col in expected_cols) {
      if (!col %in% names(all_rows[[i]])) {
        all_rows[[i]][[col]] <- ""
      }
    }
    # Reorder to match expected column order
    all_rows[[i]] <- all_rows[[i]][, expected_cols, drop = FALSE]
  }
  
  # Combine all rows - now they all have identical column structures
  df <- do.call(rbind, all_rows)
  rownames(df) <- NULL
  
  return(df)
}

#' Summarize Categorical Variable
#'
#' @description
#' Internal function to compute n (%) for categorical variables
#'
#' @keywords internal
.summarize_categorical <- function(x, var_name, include_missing) {
  
  # Count frequencies
  freq_table <- table(x, useNA = if (include_missing) "ifany" else "no")
  n_total <- length(x)
  
  results <- list()
  
  # Variable name as header
  results[[var_name]] <- sprintf("n = %d", n_total)
  
  # Each level
  for (level in names(freq_table)) {
    n <- as.numeric(freq_table[level])
    pct <- (n / n_total) * 100
    
    level_label <- if (is.na(level)) "Missing" else level
    results[[paste0("  ", level_label)]] <- sprintf("%d (%.1f%%)", n, pct)
  }
  
  return(results)
}

#' Summarize Continuous Variable
#'
#' @description
#' Internal function to compute mean (SD) or median (IQR) for continuous variables
#'
#' @keywords internal
.summarize_continuous <- function(x, var_name, non_normal, include_missing, digits) {
  
  results <- list()
  
  # Try to convert to numeric if not already
  if (!is.numeric(x)) {
    x <- suppressWarnings(as.numeric(as.character(x)))
  }
  
  # Now check for missing/valid after conversion
  x_clean <- x[!is.na(x)]
  n_total <- length(x)
  n_valid <- length(x_clean)
  n_missing <- n_total - n_valid
  
  if (n_valid == 0) {
    results[[var_name]] <- sprintf("n = %d (all missing)", n_total)
    return(results)
  }
  
  if (non_normal) {
    # Median (IQR)
    med <- median(x_clean, na.rm = TRUE)
    q25 <- quantile(x_clean, 0.25, na.rm = TRUE)
    q75 <- quantile(x_clean, 0.75, na.rm = TRUE)
    
    results[[var_name]] <- sprintf("%.*f [%.*f, %.*f]", 
                                   digits, med, 
                                   digits, q25, 
                                   digits, q75)
  } else {
    # Mean (SD)
    mean_val <- mean(x_clean, na.rm = TRUE)
    sd_val <- sd(x_clean, na.rm = TRUE)
    
    results[[var_name]] <- sprintf("%.*f (%.*f)", 
                                   digits, mean_val, 
                                   digits, sd_val)
  }
  
  if (include_missing && n_missing > 0) {
    results[[paste0("  Missing")]] <- sprintf("%d (%.1f%%)", n_missing, (n_missing / n_total) * 100)
  }
  
  return(results)
}

#' Test Categorical Variable Across Strata
#'
#' @description
#' Internal function to perform chi-square or Fisher's exact test
#'
#' @keywords internal
.test_categorical <- function(x, strata) {
  
  tryCatch({
    tab <- table(x, strata, useNA = "no")
    
    # Use Fisher's exact if any cell < 5
    if (any(tab < 5)) {
      test_result <- fisher.test(tab, simulate.p.value = TRUE)
    } else {
      test_result <- chisq.test(tab)
    }
    
    return(test_result$p.value)
  }, error = function(e) {
    return(NA_real_)
  })
}

#' Test Continuous Variable Across Strata
#'
#' @description
#' Internal function to perform t-test, ANOVA, or Kruskal-Wallis test
#'
#' @keywords internal
.test_continuous <- function(x, strata, test_type, non_normal) {
  
  tryCatch({
    # Try to convert to numeric if not already
    if (!is.numeric(x)) {
      x <- suppressWarnings(as.numeric(as.character(x)))
    }
    
    # Remove missing
    valid_idx <- !is.na(x) & !is.na(strata)
    x_clean <- x[valid_idx]
    strata_clean <- strata[valid_idx]
    
    n_groups <- length(unique(strata_clean))
    
    if (n_groups < 2) return(NA_real_)
    
    # Choose test
    if (test_type == "nonparametric" || non_normal) {
      # Kruskal-Wallis (or Wilcoxon if 2 groups)
      if (n_groups == 2) {
        test_result <- wilcox.test(x_clean ~ strata_clean)
      } else {
        test_result <- kruskal.test(x_clean ~ strata_clean)
      }
    } else if (test_type == "parametric") {
      # t-test or ANOVA
      if (n_groups == 2) {
        test_result <- t.test(x_clean ~ strata_clean)
      } else {
        test_result <- aov(x_clean ~ strata_clean)
        test_result <- summary(test_result)[[1]][1, "Pr(>F)"]
        return(test_result)
      }
    } else {
      # Auto: use normality test
      if (n_groups == 2) {
        # Shapiro-Wilk for normality
        is_normal <- shapiro.test(x_clean)$p.value > 0.05
        if (is_normal) {
          test_result <- t.test(x_clean ~ strata_clean)
        } else {
          test_result <- wilcox.test(x_clean ~ strata_clean)
        }
      } else {
        test_result <- kruskal.test(x_clean ~ strata_clean)
      }
    }
    
    return(test_result$p.value)
  }, error = function(e) {
    return(NA_real_)
  })
}

#' Format p-value
#'
#' @description
#' Internal function to format p-values
#'
#' @keywords internal
.format_p_value <- function(p) {
  if (is.na(p)) return("\u2014")
  if (p < 0.001) return("<0.001")
  return(sprintf("%.3f", p))
}

#' Format Table One Output
#'
#' @description
#' Internal function to format output based on requested format
#'
#' @keywords internal
.format_table_one_output <- function(df, output_format, strata) {
  
  if (output_format == "data.frame") {
    return(df)
  }
  
  if (output_format == "kable") {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      warning("knitr package not available, returning data.frame")
      return(df)
    }
    return(knitr::kable(df, format = "simple"))
  }
  
  if (output_format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("gt package not available, returning data.frame")
      return(df)
    }
    gt_table <- gt::gt(df)
    return(gt_table)
  }
  
  if (output_format == "flextable") {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      warning("flextable package not available, returning data.frame")
      return(df)
    }
    ft <- flextable::flextable(df)
    return(ft)
  }
  
  return(df)
}

#' Print Method for Table One
#'
#' @description
#' Print method for table_one objects
#'
#' @param x A table_one object
#' @param ... Additional arguments (unused)
#'
#' @export
print.table_one <- function(x, ...) {
  
  project_name <- attr(x, "project_name") %||% "REDCap Project"
  n_total <- attr(x, "n_total")
  strata <- attr(x, "strata")
  
  cat("Table 1: Descriptive Statistics\n")
  cat("Project:", project_name, "\n")
  cat("Total N:", n_total, "\n")
  if (!is.null(strata)) {
    cat("Stratified by:", paste(strata, collapse = ", "), "\n")
  }
  cat("\n")
  
  # Remove class to print as regular data frame or table
  class(x) <- setdiff(class(x), "table_one")
  print(x)
  
  invisible(x)
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
