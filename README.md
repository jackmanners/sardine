# sardine <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/jackmanners/sardine/workflows/R-CMD-check/badge.svg)](https://github.com/jackmanners/sardine/actions)
[![Codecov test coverage](https://codecov.io/gh/jackmanners/sardine/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jackmanners/sardine?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/sardine)](https://CRAN.R-project.org/package=sardine)
<!-- badges: end -->

## Overview

**S**tructured **A**rchitecture for **R**esearch **D**ata **I**ntegration and **E**valuation

`sardine` is an R package that provides a structured and secure interface for research data integration and evaluation. The package is designed to facilitate seamless data exchange between R and various research data platforms, with initial focus on REDCap API integration.

## Features

- **Secure REDCap API Integration**: Connect to REDCap projects with robust authentication and error handling
- **Comprehensive REDCap Coverage**: Support for all major REDCap API endpoints including:
  - Records (export, import, delete, rename)
  - Metadata and project information
  - Arms and events (for longitudinal projects)
  - Instruments and forms
  - User management and roles
  - Data Access Groups (DAGs)
  - File operations
  - Survey functionality
  - Reports and logging
  - Project backup and XML export
- **Data Validation**: Built-in validation for data integrity and completeness
- **Flexible Export Options**: Support for various data formats (JSON, CSV, XML) and filtering options
- **Error Handling**: Comprehensive error reporting and debugging tools
- **Extensible Architecture**: Designed to support additional APIs in future versions

## Installation

You can install the development version of sardine from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jackmanners/sardine")
```

## Quick Start

### Setting up REDCap Connection

```r
library(sardine)

# Set up your REDCap connection
redcap_config <- redcap_connection(
  url = "https://redcap.your-institution.edu/api/",
  token = "YOUR_API_TOKEN"
)

# Test the connection
test_connection(redcap_config)
```

### Exporting Data

```r
# Export all records
data <- redcap_export_records(redcap_config)

# Export specific fields
data <- redcap_export_records(
  redcap_config,
  fields = c("record_id", "age", "gender")
)

# Export with filters
data <- redcap_export_records(
  redcap_config,
  records = c("001", "002", "003"),
  events = "baseline_arm_1"
)
```

### Project Metadata

```r
# Get project information
project_info <- redcap_project_info(redcap_config)

# Get data dictionary
metadata <- redcap_metadata(redcap_config)

# Get field names
field_names <- redcap_field_names(redcap_config)

# Get instruments (forms)
instruments <- redcap_export_instruments(redcap_config)
```

### Advanced Operations

```r
# Import new records
new_data <- data.frame(
  record_id = "new_001",
  first_name = "John",
  last_name = "Doe",
  age = 30
)
redcap_import_records(redcap_config, new_data)

# Work with longitudinal projects
arms <- redcap_export_arms(redcap_config)
events <- redcap_export_events(redcap_config)

# Manage users and permissions
users <- redcap_export_users(redcap_config)
user_roles <- redcap_export_user_roles(redcap_config)

# Work with Data Access Groups
dags <- redcap_export_dags(redcap_config)
user_dag_mappings <- redcap_export_user_dag_mappings(redcap_config)

# Generate survey links
survey_url <- redcap_export_survey_link(redcap_config, "001", "baseline_survey")

# Export project logging
logs <- redcap_export_logging(redcap_config, begin_time = "2023-01-01 00:00")

# Create project backups
project_xml <- redcap_export_project_xml(redcap_config)
writeLines(project_xml, "backup.xml")
```

## Configuration

### Environment Variables

For security, store your API credentials as environment variables:

```r
# In your .Renviron file:
REDCAP_URL=https://redcap.your-institution.edu/api/
REDCAP_TOKEN=your_api_token_here

# Then use in R:
redcap_config <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("REDCAP_TOKEN")
)
```

### Configuration Files

You can also use configuration files:

```r
# config.yml
redcap:
  url: "https://redcap.your-institution.edu/api/"
  token: "your_token_here"

# In R:
redcap_config <- redcap_connection_from_config("config.yml")
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