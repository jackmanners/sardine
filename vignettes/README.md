# Sardine Package Vignettes

This directory contains comprehensive guides for using the sardine package with REDCap and other research data platforms.

## Getting Started

- **[Getting Started](getting-started.html)** - Overview of the sardine package and quick start guide

## REDCap Vignettes

Detailed guides for common REDCap tasks using the sardine package:

### Core Operations

- **[01-data-extraction.Rmd](redcap/01-data-extraction.html)** - Comprehensive guide to extracting data from REDCap projects
- **[02-data-import.Rmd](redcap/02-data-import.html)** - Importing data into REDCap with validation and best practices
- **[03-report-generation.Rmd](redcap/03-report-generation.html)** - Generating completion reports and data quality summaries

## Key Features Covered

### Object-Oriented Design

- Creating project objects that cache data and test connections automatically
- Using clean method names without `redcap_` prefixes
- Fail-fast error handling and validation

### Data Management

- Efficient data caching to reduce API calls
- Targeted data extraction for specific analyses
- Import warnings and cache management

### Reporting & Analytics

- Participant completion tracking
- Data quality and missing data reports
- Automated and scheduled reporting
- Longitudinal project support

## Quick Reference

### Basic Usage Pattern

```r
# Setup (one-time)
create_env_template()  # Edit .env with credentials
load_env()

# Create project
project <- redcap_project()

# Access data
all_data <- project$data
demographics <- export_records(project, fields = c("age", "gender"))

# Generate reports
completion <- get_participant_completion(project)
print_completion_report(completion)
```

### Migration from Old Approach

```r
# Old way (deprecated)
conn <- redcap_connection(url, token)
data <- redcap_export_records(conn)

# New way (current)
project <- redcap_project(url, token)  # Tests connection & caches data
data <- project$data                   # Fast cached access
```

## Building the Vignettes

To build these vignettes locally:

```r
# Build all vignettes
devtools::build_vignettes()

# Build specific vignette
rmarkdown::render("vignettes/redcap/01-data-extraction.Rmd")
```

## Getting Help

- Read the vignettes for detailed examples and best practices
- Check function documentation: `?redcap_project`
- View package overview: `help(package = "sardine")`
- Report issues on GitHub (when available)

These vignettes provide everything needed to effectively use the sardine package for research data management and analysis.
