# sardine <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/1068834156.svg)](https://doi.org/10.5281/zenodo.17255208)
[![R-CMD-check](https://github.com/jackmanners/sardine/workflows/R-CMD-check/badge.svg)](https://github.com/jackmanners/sardine/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/sardine)](https://CRAN.R-project.org/package=sardine)
<!-- badges: end -->

## Overview

**S**tructured **A**rchitecture for **R**esearch **D**ata **I**ntegration and **E**valuation

Unlike the acronym suggests, `sardine` is designed to help access research data without excessive ceremony. It currently has a focus on REDCap and device data, but the architecture is designed to be extensible to other research data platforms in the future.

## Key Features

- **REDCap Integration**
  - Project Management: Easily connect to REDCap projects using API tokens, with automatic data caching for efficient access.
  - Quality Assessment: Generate comprehensive data quality reports, including missing data analysis and data type validation against REDCap metadata.
  - Table One Generation: Create detailed Table One summaries with options for filtering, stratification, and statistical testing.
  - Retention Tracking: Tools for tracking participant retention and attrition over time, with customizable plotting functions.
  - Safe Data Importing: Import data back into REDCap with built-in safety checks to prevent accidental overwrites, including change analysis and confirmation prompts.
  - Cross-Project Data Piping: Transfer data between multiple REDCap projects with ease, maintaining data integrity through safety features.

- **Device Data Manipulation**
  - Withings data handling:
    - Summarise and format **Withings** data
    - Generate **Sleep Regularity Index (SRI)** from sleep/wake data.
    - Reformat for use with other packages (e.g., `GGIR`, `sleepreg`).

## Installation

You can install the development version of sardine from [GitHub](https://github.com/) using `devtools`.
Make sure you have the `devtools` package installed first.

```r
# install.packages("devtools")
devtools::install_github("jackmanners/sardine")
```

## Quick Start

The full documentation and reference can be found at <https://jackmanners.github.io/sardine/>.
Below are some basic examples to get you started.

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

# Access project data
all_data <- project$data
```

### 2. Viewing Project Structure and Attrition Report

By default, reports and data tables are returned as `data.frame` objects.
The `DT` package is recommended for exploring and searching data more easily using interactive tables.

```r
library(DT)

# View project structure summary (shiny GUI)
project$explore_project_shiny()

# View project metadata
metadata <- project$metadata
datatable(project$metadata, filter = "top")

# Generate and view data quality report
dq_report <- project$generate_data_quality_report()
datatable(dq_report, filter = "top")
# Plot event completion summary
library(ggplot2)
ggplot(get_event_completion_summary(project), aes(x = event_name, fill = completed)) +
  geom_bar(position = "dodge")
```

### 3. Withings Sleep Data

```r
# Load example Withings data
summary_data <- read.csv("withings_summaries.csv")
epoch_data  <- read.csv("withings_epochs.csv")

# Create Withings sleep object
wsleeps <- withings_sleep(summary_data, epoch_data)

# Generate sleep report
report <- wsleeps$report(output = "html", file = "withings_sleep_report")

# Calculate Sleep Regularity Index (SRI) and save raster plot
wsleep_sri <- wsleeps$sri(plot_path = "sri_plot.png")

# Print SRI results and plot
print(wsleep_sri$sri)
print(wsleep_sri$plot)
```

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- REDCap (Research Electronic Data Capture) is developed and supported by Vanderbilt University
- Thanks to the R community for the excellent HTTP and data manipulation packages
- Thanks to Dan Windred for assistance with [Sleep Regularity Index (SRI)](https://doi.org/10.1093/sleep/zsab254) calculations
