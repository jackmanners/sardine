# sardine <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/1068834156.svg)](https://doi.org/10.5281/zenodo.17255208)
[![R-CMD-check](https://github.com/jackmanners/sardine/workflows/R-CMD-check/badge.svg)](https://github.com/jackmanners/sardine/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/sardine)](https://CRAN.R-project.org/package=sardine)
<!-- badges: end -->

## Overview

**S**tructured **A**rchitecture for **R**esearch **D**ata **I**ntegration and **E**valuation

Unlike the acronym suggests, `sardine` is designed to help access research data without excessive ceremony. It currently has a focus on REDCap, but the architecture is designed to be extensible to other research data platforms in the future. 

## Key Features

**Project objects**: Cache data, test connections, and generally mind their own business.
**Data quality & validation**: Find missing data and type mismatches.
**Table One generation**: Publication-ready descriptive statistics with stratification and statistical tests.
**Error handling**: If something goes wrong, you should know. If it doesn't, you probably won't notice (and that's okay).
**Data caching**: Data is cached when you create a project, and can be refreshed manually. 
**Quality reporting**: Get summaries and breakdowns.
**Safe data import**: Cache staleness detection, change analysis, and confirmation prompts protect your data.
**Import change reports**: Save detailed before/after reports of what will change during import.
**REDCap coverage**: Most major API endpoints are supported. If you find one that's missing, let me know. Or don't. I won't be offended.
**Modular-ish**: The architecture is ready for other APIs, should I get the time to implement.
**Security**: Use environment variables. Or don't. But you probably should.

## Installation

You can install the development version of sardine from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("jackmanners/sardine")
```

## Quick Start

### 1. Create REDCap Project

Using Environment variables for your API token is optional but recommended if working with a single REDCap project regularly. This also helps avoid hardcoding the API credentials in your scripts (if using GitHub or other version control).
Specifically, you'll need `REDCAP_URL` and `REDCAP_TOKEN` set as environment variables.

```r
# Create project (tests connection & caches data automatically)
project <- redcap_project()

# Alternatively, specify URL and token directly
project <- redcap_project(
  url = "https://redcap.your-institution.edu/api/",
  token = "YOUR_API_TOKEN"
)

# View project information
project$info()
```

### 2. Access Data

```r
# Access all cached data
all_data <- project$data

# Use dplyr for filtering and selecting
library(dplyr)

# Select specific fields
demographics <- project$data %>%
  select(record_id, age, gender)

# Filter specific records
subset <- project$data %>%
  filter(record_id %in% c("001", "002", "003"))

# Filter by form (all fields from baseline_survey)
baseline <- project$data %>%
  select(record_id, starts_with("baseline_"))

# Complex filtering
adults_complete <- project$data %>%
  filter(age >= 18, baseline_survey_complete == 2)
```

### 3. Working with Metadata

```r
# Access metadata (cached automatically)
metadata <- project$metadata
head(metadata)

# Get field information
field_info <- metadata %>%
  select(field_name, field_label, field_type, text_validation_type_or_show_slider_number)

# Find all choice fields
choice_fields <- metadata %>%
  filter(field_type %in% c("radio", "dropdown", "checkbox"))

# View project information
project$info()

# Access detailed project info
project_details <- project$project_info
print(project_details$project_title)
print(project_details$is_longitudinal)
```

### 4. Refresh Data

```r
# After external changes to REDCap, refresh your cached data
project$refresh()

# Access the updated data
updated_data <- project$data
```

### 5. Data Quality Assessment

```r
# Comprehensive data quality report
quality_report <- generate_data_quality_report(
  project,
  missing_threshold = 0.15,  # Flag fields with >15% missing data
  include_validation = TRUE
)
print(quality_report)

# Save report to file
generate_data_quality_report(
  project,
  output_file = "quality_report.md"
)

# Detailed missing data analysis
missing_analysis <- analyze_missing_data(
  project, 
  threshold = 0.20,  # Flag fields with >20% missing
  by_form = TRUE,    # Group by REDCap forms
  by_field = TRUE,   # Individual field analysis
  by_record = FALSE  # Optional: analyze by record
)
print(missing_analysis)

# Access specific components
missing_analysis$summary              # Overall statistics
missing_analysis$by_field            # Field-level details
missing_analysis$by_form             # Form-level summary
missing_analysis$high_missing_fields # Count of flagged fields

# Data type validation against REDCap dictionary
validation <- validate_data_types(project, strict = TRUE)
print(validation)

# Access validation details
validation$summary           # Overview of issues
validation$invalid_fields   # Fields with type mismatches
validation$warnings         # Warning-level issues
```

### 6. Table One Generation

```r
# Basic Table One (all variables)
table_one <- generate_table_one(project)
print(table_one)

# With filtering
table_one_filtered <- generate_table_one(
  project,
  filter = c("withdrawn != 1", "consent_complete == 1", "age >= 18")
)
print(table_one_filtered)

# Force variable types (override auto-detection)
table_one_typed <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi", "study_site"),
  cat_vars = c("gender", "study_site"),  # Force categorical
  cont_vars = c("age", "bmi")            # Force continuous
)

# Stratified by treatment group
table_one_stratified <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi", "baseline_score"),
  strata = "treatment_group",
  cat_vars = c("gender"),
  cont_vars = c("age", "bmi", "baseline_score")
)
print(table_one_stratified)

# With statistical tests
table_one_with_tests <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi"),
  filter = c("withdrawn != 1"),
  strata = "study_group",
  cat_vars = c("gender"),
  cont_vars = c("age", "bmi"),
  test = TRUE,  # Include p-values
  test_type = "auto"  # or "parametric", "nonparametric"
)
print(table_one_with_tests)

# Custom formatting for non-normal distributions
table_one_custom <- generate_table_one(
  project,
  vars = c("age", "bmi", "cholesterol", "gender"),
  filter = c("age >= 18", "consent_complete == 1"),
  strata = "treatment_group",
  cont_vars = c("age", "bmi", "cholesterol"),
  cat_vars = c("gender"),
  non_normal = c("bmi", "cholesterol"),  # Use median [IQR]
  digits = 2,  # Decimal places
  include_missing = TRUE
)

# Export to different formats
table_kable <- generate_table_one(project, output_format = "kable")  # For R Markdown
table_gt <- generate_table_one(project, output_format = "gt")        # For gt package
```

### 7. Longitudinal Studies and Retention Tracking

```r
# For longitudinal REDCap projects with events

# Get retention summary across events
retention <- get_retention_summary(project)
print(retention)

# Get attrition over time (requires events with day_offset field)
attrition <- get_attrition_over_time(project)
print(attrition)

# Plot attrition curve
plot_attrition_curve(project, 
                     metric = "retention_rate",  # or "n_retained", "n_lost", "attrition_rate"
                     show_labels = TRUE)

# Customize the plot
plot_attrition_curve(project, 
                     metric = "n_retained",
                     show_labels = TRUE) +
  ggplot2::labs(title = "Participant Retention Over Time",
                subtitle = "My Longitudinal Study")

# Get event completion summary
completion <- get_event_completion_summary(project)
print(completion)
```

### 8. Project Object Methods and Properties

```r
# Project object provides several methods and properties

# Methods
project$info()      # Display project information
project$refresh()   # Refresh data from REDCap
project$import(data, overwrite = FALSE, force = FALSE, save_changes = NULL)  # Import data with safety checks
project$piping(source_project, source_fields, target_fields, by = "record_id", 
              overwrite = FALSE, force = FALSE, save_changes = NULL)  # Pipe data between projects

# Data properties (cached at creation, updated with refresh())
project$data          # Full dataset (tibble)
project$metadata      # Field metadata/data dictionary
project$project_info  # Project settings and information
project$events        # Event information (longitudinal projects)
project$arms          # Arm information (longitudinal projects)
project$instruments   # Instrument/form information

# Internal properties (advanced users)
project$.connection   # Connection details
project$.created_at   # When the project object was created
```

### 9. Cross-Project Piping

```r
# Create multiple project connections
project_a <- redcap_project(env_prefix = "PROJECTA")
project_b <- redcap_project(env_prefix = "PROJECTB")

# Select source field label(s) from project A
source_fields <- c("age", "sex")

# Select target field label(s) in project B
target_fields <- c("age_at_visit", "sex_at_visit")

# Pipe data with safety checks (same as import)
project_b$piping(
  source_project = project_a,
  source_fields = source_fields,
  target_fields = target_fields,
  by = "record_id",  # Join by record_id
  overwrite = FALSE  # Safe by default
)

# Pipe with overwrite and change report
project_b$piping(
  source_project = project_a,
  source_fields = source_fields,
  target_fields = target_fields,
  by = "record_id",
  overwrite = TRUE,
  save_changes = "piping_changes.csv"
)
```

### 10. Data Import with Safety Checks

```r
# Basic import (safe by default - won't overwrite existing values)
new_data <- data.frame(
  record_id = c("001", "002"),
  age = c(25, 30),
  stringsAsFactors = FALSE
)

# Import with automatic change detection and confirmation
project$import(new_data)
# This will:
# - Check if cache is out of date (warns if > 5 minutes old)
# - Analyze what will change
# - Show preview of changes
# - Ask for confirmation before proceeding
# - Warn if trying to overwrite existing values (blocked by default)

# Allow overwriting existing values
project$import(new_data, overwrite = TRUE)
# This will show which values will be overwritten and ask for confirmation

# Save a detailed change report before importing
project$import(new_data, 
               overwrite = TRUE,
               save_changes = "import_changes.csv")
# Saves a CSV with: record_id, field, current_value, new_value, change_type

# Skip confirmation prompts (useful for scripts)
project$import(new_data, 
               overwrite = TRUE,
               force = TRUE)
# Use with caution - bypasses all safety checks and confirmations
```

**Import Safety Features:**
- Cache staleness detection (warns if cache is >5 minutes old)
- Automatic change analysis showing:
  - Number of new records to add
  - Number of existing records to update
  - Number of fields affected
  - Number of values that would be overwritten
- Interactive confirmation with preview of first 5 changes
- `overwrite = FALSE` by default (protects existing data)
- Detailed change reports (CSV format) showing before/after values
- Option to force import without confirmations for automated workflows

## Documentation

For detailed guides and examples, see the package vignettes:

- [Getting Started with REDCap Data Extraction](vignettes/redcap-data-extraction.Rmd)
- [Importing Data into REDCap](vignettes/redcap-data-import.Rmd)  
- [Participant Completion Reporting](vignettes/redcap-reporting.Rmd)
- [Data Quality and Validation](vignettes/data-quality-validation.Rmd)


## Security Considerations

- **Never commit API tokens** to version control
- Use environment variables or secure configuration files
- Implement proper access controls in your REDCap projects
- Regularly rotate API tokens
- Use HTTPS connections only

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- REDCap (Research Electronic Data Capture) is developed and supported by Vanderbilt University
- Thanks to the R community for the excellent HTTP and data manipulation packages
