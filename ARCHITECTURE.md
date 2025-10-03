# sardine Package Architecture

## Overview
The sardine package has been refactored with a modular, source-agnostic architecture that supports multiple research data platforms.

## Directory Structure

```
R/
├── connection-base.R           # Legacy connection functions
├── connection.R               # Legacy connection functions  
├── metadata.R                 # General metadata utilities
├── sardine-package.R          # Main package documentation
├── utils.R                    # General utility functions
├── sources/                   # Platform-specific integrations
│   ├── base_connection.R      # Base connection class for all sources
│   ├── sources.R              # Sources module documentation
│   └── redcap/                # REDCap-specific functionality
│       ├── connection.R       # REDCap connection management
│       ├── environment.R      # Environment variable handling
│       ├── export.R           # REDCap data export functions
│       ├── redcap-*.R         # Core REDCap API functions
│       └── reports/           # REDCap-specific reporting
│           ├── completion_report_generator.R
│           ├── quick_completion_report.R
│           ├── simple_completion_functions.R
│           ├── test_and_report.R
│           └── reports.R      # Reports module documentation
└── features/                  # Advanced cross-source functionality
    └── features.R             # Features module documentation
```

## Core Architecture Components

### 1. Base Connection System
- **`source_connection()`**: Base class for all API connections
- **Generic methods**: `test_connection()`, `get_connection_info()`, `is_valid_connection()`
- **Extensible design**: Easy to add new data sources

### 2. Source-Specific Modules
- **REDCap Module**: Complete REDCap API wrapper with reporting capabilities
- **Future sources**: Qualtrics, SurveyMonkey, Database connections

### 3. Environment Management
- **`.env` file support**: Secure credential storage
- **`load_env()`**: Environment variable loading with validation
- **`create_env_template()`**: Generate configuration templates

### 4. Reporting Framework  
- **Source-specific reports**: Tailored to each platform's capabilities
- **Completion analysis**: Participant completion status reporting
- **Extensible design**: Easy to add new report types

## Key Features

### Connection Management
```r
# Environment-based connection (recommended)
load_env()
conn <- redcap_connection_from_env()

# Direct connection
conn <- redcap_connection(
  url = "https://redcap.example.edu/api/",
  token = "your_token_here"
)

# Test connection
test_connection(conn)
```

### Environment Configuration
```r
# Create .env template
create_env_template()

# Load environment variables
load_env()

# Validate REDCap configuration
conn <- redcap_connection_from_env()
```

### Reporting Capabilities
```r
# Quick completion report
report <- quick_completion_report(conn)

# Comprehensive completion analysis
analysis <- generate_completion_report(conn)
```

## Benefits of New Architecture

1. **Modularity**: Clean separation of concerns between different data sources
2. **Extensibility**: Easy to add new API integrations
3. **Security**: Proper environment variable management
4. **Consistency**: Common interfaces across all data sources
5. **Organization**: Logical grouping of related functionality

## Migration Notes

- All existing REDCap functions remain available
- New environment management functions added
- Improved connection handling with better error messages
- Reports organized in dedicated subfolder for better maintenance

## Future Enhancements

1. **Additional Sources**: Qualtrics, SurveyMonkey, Database connections
2. **Advanced Features**: Multi-source data integration, advanced analytics
3. **Enhanced Reporting**: Cross-source completion analysis, automated reporting
4. **Data Harmonization**: Standardize data across different platforms