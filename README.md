# sardine <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/1068834156.svg)](https://doi.org/10.5281/zenodo.17255208)
[![R-CMD-check](https://github.com/jackmanners/sardine/workflows/R-CMD-check/badge.svg)](https://github.com/jackmanners/sardine/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/sardine)](https://CRAN.R-project.org/package=sardine)
<!-- badges: end -->

## Overview

**S**tructured **A**rchitecture for **R**esearch **D**ata **I**ntegration and **E**valuation

`sardine` provides a modern, object-oriented interface for research data APIs, starting with comprehensive REDCap support. The package emphasizes performance, data integrity, and ease of use through project objects that cache data and provide clean method interfaces.

## Key Features

- **Object-Oriented Design**: Project objects that test connections automatically and cache data for performance
- **Data Quality & Validation**: Comprehensive missing data analysis and data type validation against REDCap dictionaries
- **Fail-Fast Error Handling**: Connection issues caught immediately during project creation
- **Data Caching**: Full datasets cached on project creation, with manual refresh capabilities
- **Quality Reporting**: Professional data quality reports with executive summaries and detailed breakdowns
- **Import Warnings**: Alerts when imports make cached data outdated
- **Comprehensive REDCap Coverage**: All major REDCap API endpoints supported
- **Modular Architecture**: Base classes ready for additional API sources (Qualtrics, etc.)
- **Security First**: Environment variable management with `.env` file support
- **Error Handling**: Comprehensive error reporting and debugging tools
- **Extensible Architecture**: Designed to support additional APIs in future versions

## Installation

You can install the development version of sardine from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jackmanners/sardine")
```

## Quick Start

### 1. Environment Setup

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
project <- redcap_project_from_env()

# View project information  
project$info()
```

### 3. Access Data

```r
# Access all cached data
all_data <- project$data

# Export specific fields
demographics <- export_records(
  project,
  fields = c("record_id", "age", "gender")
)

# Export specific records
subset <- export_records(
  project,
  records = c("001", "002", "003")
)

# Export from specific forms
baseline <- export_records(project, forms = "baseline_survey")
```

### 4. Import Data

```r
# Prepare data for import
new_data <- tibble::tibble(
  record_id = c("004", "005"),
  age = c(25, 30),
  gender = c("Male", "Female")
)

# Import records
result <- import_records(project, new_data)

# Refresh cached data after imports
project$refresh()
```

### 5. Data Quality Assessment

```r
# Comprehensive data quality report
quality_report <- generate_data_quality_report(
  project,
  missing_threshold = 0.15  # Flag fields with >15% missing data
)
print(quality_report)

# Detailed missing data analysis
missing_analysis <- analyze_missing_data(project, threshold = 0.20)
print(missing_analysis)

# Data type validation against REDCap dictionary
validation <- validate_data_types(project)
print(validation)
```

### 6. Generate Reports

```r
# Participant completion report
completion <- get_participant_completion(project)
print_completion_report(completion)

# Quick completion summary
quick_report <- quick_completion_report(project)
```

### 7. Project Metadata

```r
# Access project metadata (cached automatically)
instruments <- project$instruments
dictionary <- project$dictionary
events <- project$events
users <- project$users

# Or export fresh metadata
fresh_instruments <- export_instruments(project)
```

## Features

- **Object-oriented interface**: Work with `redcap_project()` objects that cache data and metadata
- **Data quality assessment**: Comprehensive missing data analysis and data type validation
- **Professional reporting**: Executive summaries, detailed breakdowns, and exportable quality reports
- **Environment management**: Secure credential handling with `.env` files  
- **Clean API**: Intuitive methods for data operations
- **Comprehensive reporting**: Built-in participant completion analysis
- **Fail-fast validation**: Automatic connection testing and error handling
- **Data caching**: Automatic caching with smart refresh capabilities

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

```r
# Work with longitudinal projects
arms <- project$arms
events <- project$events

# Generate survey links
survey_url <- export_survey_link(project, "001", "baseline_survey")

# Export project logging  
logs <- export_logging(project, begin_time = "2023-01-01 00:00")

# Create project backups
project_xml <- export_project_xml(project)
writeLines(project_xml, "backup.xml")

# Integrate quality checks into workflow
weekly_quality_check <- function(project) {
  report <- generate_data_quality_report(project, missing_threshold = 0.10)
  if (report$summary$high_missing_fields > 5) {
    warning("Data quality issues detected - review needed")
  }
  return(report)
}
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
