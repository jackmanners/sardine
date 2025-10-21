# Test script for table_one with character-stored numeric data

# Load the package
library(sardine)

# Create test data similar to the issue
test_data <- data.frame(
  record_id = 1:37,
  age = c(rep(NA, 28), "45", "52", "38", "61", "49", "57", "44", "50", "42"),
  rand_sex = c(rep(NA, 26), "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2"),
  rand_bmi = c(rep(NA, 26), "25.3", "28.1", "22.5", "31.2", "26.8", "24.9", "29.3", "27.6", "23.8", "30.1", "25.7"),
  stringsAsFactors = FALSE
)

# Create mock project object
mock_project <- list(
  data = test_data,
  id_field = "record_id",
  metadata = data.frame(
    field_name = c("record_id", "age", "rand_sex", "rand_bmi"),
    field_type = c("text", "text", "radio", "text"),
    text_validation_type_or_show_slider_number = c(NA, "number", NA, "number"),
    stringsAsFactors = FALSE
  ),
  project_info = list(
    project_title = "Test Project"
  )
)
class(mock_project) <- "redcap_project"

# Test 1: Auto-detection
cat("\n=== Test 1: Auto-detection ===\n")
table_one <- generate_table_one(
  mock_project,
  vars = c("age", "rand_sex", "rand_bmi")
)
print(table_one)

# Test 2: Force continuous (original issue)
cat("\n=== Test 2: Force continuous ===\n")
table_one <- generate_table_one(
  mock_project,
  vars = c("age", "rand_sex", "rand_bmi"),
  cont_vars = c("rand_bmi", "age"),
  cat_vars = c("rand_sex")
)
print(table_one)

# Test 3: With stratification
cat("\n=== Test 3: With stratification ===\n")
table_one <- generate_table_one(
  mock_project,
  vars = c("age", "rand_bmi"),
  strata = "rand_sex",
  cont_vars = c("age", "rand_bmi")
)
print(table_one)

cat("\n=== Tests complete! ===\n")
cat("The age and rand_bmi fields should now display as:\n")
cat("- Mean (SD) format\n")
cat("- With 'Missing' counts shown\n")
cat("- No 'NA (NA)' values\n")
