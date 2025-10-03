# Migration Guide: New Object-Oriented Approach

The sardine package has been refactored to use an object-oriented approach that provides better caching, cleaner semantics, and fail-fast connection testing.

## Quick Migration

### Old Approach (Deprecated)
```r
# Old way - functional approach
library(sardine)
load_dot_env()

conn <- redcap_connection(
  url = Sys.getenv("REDCAP_URL"),
  token = Sys.getenv("REDCAP_TOKEN")
)

# Test connection separately
test_connection(conn)

# Export data with prefixed functions
data <- redcap_export_records(conn)
instruments <- redcap_export_instruments(conn)
```

### New Approach (Current)
```r
# New way - object-oriented approach
library(sardine)
load_dot_env()

# Creates project, tests connection, and caches data automatically
project <- redcap_project_from_env()

# Access cached full dataset
data <- project$data

# Export specific data without redcap_ prefix
demographics <- export_records(project, fields = c("record_id", "age", "gender"))
instruments <- export_instruments(project)

# View project info
project$info()
```

## Key Benefits

### 1. **Fail-Fast Connection Testing**
- Connection is tested immediately when creating the project
- No separate `test_connection()` calls needed
- Clear error messages if connection fails

### 2. **Data Caching**
- Full dataset is cached on project creation
- Access via `project$data` without repeated API calls  
- Refresh manually with `project$refresh()` when needed

### 3. **Cleaner Function Names**
- No more `redcap_` prefixes on every function
- `export_records()` instead of `redcap_export_records()`
- More semantic and easier to use

### 4. **Import Warnings**
- Import functions warn when cached data may be outdated
- Suggests refreshing cache after imports
- Prevents silent data inconsistencies

## Complete Example

```r
library(sardine)

# Setup (one-time)
create_env_template()  # Creates .env template
# Edit .env file with your credentials
load_env()

# Create project (tests connection, caches data)
project <- redcap_project_from_env()

# View project information
project$info()
# REDCap Project
# ==============
# 
# Title: My Research Study
# URL: https://redcap.example.edu
# Created: 2025-10-03 10:30:00
# Cached Data: 150 records, 25 fields
# 
# Access data with: project$data
# Refresh data with: project$refresh()

# Access all data (cached)
all_participants <- project$data

# Get specific fields
demographics <- export_records(
  project, 
  fields = c("record_id", "age", "gender", "group")
)

# Get specific records
pilot_data <- export_records(
  project,
  records = c("001", "002", "003")
)

# Import new data (with warnings)
new_records <- tibble::tibble(
  record_id = "004",
  age = 28,
  gender = "F"
)

import_records(project, new_records)
# Warning: Importing data will make cached project data outdated
# Info: Consider running project$refresh() after import

# Refresh cached data after import
project$refresh()

# Generate completion reports (updated for new approach)
completion_report <- get_participant_completion(project)
print_completion_report(completion_report)
```

## Function Mapping

| Old Function | New Function | Notes |
|-------------|-------------|--------|
| `redcap_connection()` | `redcap_project()` | Now caches data and tests connection |
| `redcap_connection_from_env()` | `redcap_project_from_env()` | Same but with caching |
| `test_connection()` | *automatic* | Built into project creation |
| `redcap_export_records()` | `export_records(project, ...)` | Takes project object |
| `redcap_import_records()` | `import_records(project, ...)` | Warns about cache |
| `redcap_export_instruments()` | `export_instruments(project)` | Cleaner name |
| `redcap_export_events()` | `export_events(project)` | Cleaner name |
| `redcap_export_users()` | `export_users(project)` | Cleaner name |

## Reporting Functions Updated

All completion reporting functions now take project objects:

```r
# Old
completion_result <- get_participant_completion(connection)

# New  
completion_result <- get_participant_completion(project)
```

The object-oriented approach provides a much cleaner and more reliable interface for working with REDCap data!