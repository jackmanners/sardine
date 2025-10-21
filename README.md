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

``` r
# install.packages("devtools")
devtools::install_github("jackmanners/sardine")
```

## Quick Start

### 1. Environment Setup

This is optional, but recommended if working with a single REDCap project regularly. This also helps avoid hardcoding the API credentials in your scripts (if using GitHub or other version control).

Specifically, you'll need `REDCAP_URL` and `REDCAP_TOKEN` set as environment variables.

```r
library(sardine)

# Create environment template (one-time)
create_env_template()
# Edit .env file with your REDCap credentials

# Load environment
load_env()
```

### 2. Create REDCap Project

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

### 3. Access Data

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

### 4. Working with Metadata

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

### 5. Refresh Data

```r
# After external changes to REDCap, refresh your cached data
project$refresh()

# Access the updated data
updated_data <- project$data
```

### 6. Data Quality Assessment

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

### 7. Project Object Methods and Properties

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

# Internal properties (advanced users)
project$.connection   # Connection details
project$.created_at   # When the project object was created
```

### 8. Environment Management

```r
# Create .env template for secure credential storage
create_env_template()

# This creates a .env file with:
# REDCAP_URL=your_redcap_url_here
# REDCAP_TOKEN=your_api_token_here

# Load environment variables from .env file
load_env()

# Now create project without parameters
project <- redcap_project()  # Uses REDCAP_URL and REDCAP_TOKEN

# Use alternate environment variable prefixes
project <- redcap_project(env_prefix = "MYPROJECT")
# Looks for MYPROJECT_URL and MYPROJECT_TOKEN
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

## Features

- **Automatic data caching**: You get your data, and it's cached. No need to thank anyone.
- **Error handling**: If something fails, you'll know. If it works, you probably won't notice.
- **Data quality assessment**: Find missing data, type mismatches, and other things you may or may not care about.
- **Reporting**: Executive summaries, breakdowns, and markdown reports. For those who like reports.
- **Environment management**: Use `.env` files if you want to be responsible.
- **Simple data access**: Use `project$data` with dplyr. Or base R. No judgment.
- **Metadata access**: It's there if you want it.
- **Refresh capability**: For when you suspect the world has changed.
- **Security**: Don't hardcode credentials. Unless you want to live dangerously.
- **Clean API**: Intuitive, object-oriented, and not trying too hard.

## Future Development Roadmap

The following features are planned for future releases, prioritized by user feedback:

### ✅ Priority 1: Data Quality & Validation - COMPLETED

- ✅ Missing data analysis and reporting
- ✅ Data type validation against REDCap dictionary
- ✅ Range and logic checks
- ✅ Data completeness metrics
- ✅ Quality control dashboards

### Priority 2: Cross-Project Integration

- Record matching between projects
- Data piping from one REDCap project to another
- Cross-project analytics and reporting
- Automated data synchronization

### Priority 3: Analytics & Reporting

- Table 1 generation for descriptive statistics
- Automated codebook creation
- Statistical summaries and visualizations
- Export-ready analysis datasets

### Later Priorities

- Survey management enhancements
- Advanced backup and versioning
- Workflow automation tools
- Third-party integrations (Qualtrics, etc.)

## Documentation

For detailed guides and examples, see the package vignettes:

- [Getting Started with REDCap Data Extraction](vignettes/redcap-data-extraction.Rmd)
- [Importing Data into REDCap](vignettes/redcap-data-import.Rmd)  
- [Participant Completion Reporting](vignettes/redcap-reporting.Rmd)
- [Data Quality and Validation](vignettes/data-quality-validation.Rmd)

## Data Quality & Validation

`sardine` includes comprehensive data quality assessment tools:

```r
# Quick quality assessment
quality_report <- generate_data_quality_report(project)
print(quality_report)

# Focus on fields with high missing rates
missing_analysis <- analyze_missing_data(
  project, 
  threshold = 0.15,  # Flag fields >15% missing
  by_form = TRUE,    # Group by REDCap forms
  by_field = TRUE    # Individual field analysis
)

# Find data entry errors and validation issues
validation <- validate_data_types(project, strict = TRUE)

# Export professional reports for sharing
generate_data_quality_report(
  project,
  output_file = "quality_report.md"
)

# Test with sample data
sample_project <- create_sample_redcap_project()
demo_report <- generate_data_quality_report(sample_project)
```

### Quality Assessment Features

- **Missing Data Analysis**: Field-level, form-level, and record-level missing data patterns
- **Data Type Validation**: Validate against REDCap data dictionary (numeric, choice, range validation)
- **Executive Reporting**: Professional summaries with key metrics and recommendations
- **Flexible Thresholds**: Configurable alerting for different study types and phases
- **Export Capabilities**: Generate markdown reports for team sharing and documentation

## Advanced Usage

### Working with REDCap Metadata

```r
# Explore field metadata
metadata <- project$metadata

# Get field types
metadata %>%
  count(field_type)

# Find all required fields
required_fields <- metadata %>%
  filter(required_field == "y")

# Get validation types
metadata %>%
  filter(!is.na(text_validation_type_or_show_slider_number)) %>%
  select(field_name, text_validation_type_or_show_slider_number)

# Find all choice fields with their options
choice_fields <- metadata %>%
  filter(field_type %in% c("radio", "dropdown", "checkbox")) %>%
  select(field_name, field_label, select_choices_or_calculations)
```

### Data Filtering and Selection Patterns

```r
library(dplyr)
library(tidyr)

# Select specific demographics
demo_data <- project$data %>%
  select(record_id, age, gender, race, ethnicity)

# Filter completed baseline surveys
complete_baseline <- project$data %>%
  filter(baseline_survey_complete == 2)

# Get records with missing critical fields
incomplete_records <- project$data %>%
  filter(is.na(consent_date) | is.na(enrollment_date))

# Pivot longer for analysis
long_data <- project$data %>%
  select(record_id, starts_with("pain_score_")) %>%
  pivot_longer(
    cols = starts_with("pain_score_"),
    names_to = "timepoint",
    values_to = "pain_score"
  )

# Join with metadata for labels
labeled_data <- project$data %>%
  select(record_id, age) %>%
  left_join(
    metadata %>% select(field_name, field_label),
    by = c("age" = "field_name")
  )
```

### Quality Control Workflows

```r
# Create a quality control function
qc_project <- function(project, missing_threshold = 0.15) {
  # Generate quality report
  report <- generate_data_quality_report(
    project,
    missing_threshold = missing_threshold,
    include_validation = TRUE
  )
  
  # Check for critical issues
  critical_issues <- report$summary$high_missing_fields > 10 ||
                    report$summary$overall_missing_rate > 0.25
  
  if (critical_issues) {
    warning("Critical data quality issues detected!")
  }
  
  # Return detailed analysis
  list(
    report = report,
    missing = analyze_missing_data(project, threshold = missing_threshold),
    validation = validate_data_types(project)
  )
}

# Run QC
qc_results <- qc_project(project)
```

### Testing Connection

```r
# Test REDCap API connection
# Note: This uses the legacy redcap_connection object
conn <- redcap_connection(
  url = "https://redcap.example.edu/api/",
  token = "YOUR_TOKEN"
)
is_valid <- test_connection(conn)

# For redcap_project objects, connection is tested automatically on creation
# If creation succeeds, connection is valid
```

### Combining Multiple Data Sources

```r
# Create multiple project connections
project_a <- redcap_project(env_prefix = "PROJECTA")
project_b <- redcap_project(env_prefix = "PROJECTB")

# Extract and combine data
combined_data <- bind_rows(
  project_a$data %>% mutate(source = "Project A"),
  project_b$data %>% mutate(source = "Project B")
)

# Compare data quality across projects
qa_compare <- bind_rows(
  analyze_missing_data(project_a)$summary %>% mutate(project = "A"),
  analyze_missing_data(project_b)$summary %>% mutate(project = "B")
)
```

## Comprehensive Example

Here's a complete workflow demonstrating common tasks:

```r
library(sardine)
library(dplyr)
library(tidyr)

# 1. Setup and connection
project <- redcap_project()
project$info()

# 2. Explore the project structure
cat("Total records:", nrow(project$data), "\n")
cat("Total fields:", ncol(project$data), "\n")

# View forms
forms <- unique(project$metadata$form_name)
print(forms)

# 3. Data quality assessment
quality_report <- generate_data_quality_report(project, missing_threshold = 0.15)
print(quality_report)

# 4. Extract and analyze specific data
analysis_dataset <- project$data %>%
  # Filter to complete baseline surveys
  filter(baseline_demographics_complete == 2) %>%
  # Select relevant fields
  select(record_id, age, gender, treatment_group, 
         starts_with("outcome_")) %>%
  # Add calculated fields
  mutate(
    age_group = cut(age, breaks = c(0, 30, 50, 70, 100),
                   labels = c("18-30", "31-50", "51-70", "71+")),
    outcome_mean = rowMeans(select(., starts_with("outcome_")), na.rm = TRUE)
  )

# 5. Summary statistics by group
summary_stats <- analysis_dataset %>%
  group_by(treatment_group) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_outcome = mean(outcome_mean, na.rm = TRUE),
    missing_outcomes = sum(is.na(outcome_mean))
  )

print(summary_stats)

# 6. Check for data issues
validation <- validate_data_types(project)
if (length(validation$invalid_fields) > 0) {
  warning("Found validation issues in ", 
          length(validation$invalid_fields), " fields")
  print(validation$invalid_fields)
}

# 7. Missing data analysis
missing_analysis <- analyze_missing_data(project, threshold = 0.20)
high_missing <- missing_analysis$by_field %>%
  filter(above_threshold) %>%
  arrange(desc(missing_rate))

if (nrow(high_missing) > 0) {
  cat("\nFields with high missing rates:\n")
  print(high_missing)
}

# 8. Save results
write.csv(analysis_dataset, "analysis_dataset.csv", row.names = FALSE)
generate_data_quality_report(project, output_file = "quality_report.md")
```

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
