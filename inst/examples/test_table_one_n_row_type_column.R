# Test script for new table_one features: N row and Type column

library(sardine)

# Create test data
test_data <- data.frame(
  record_id = 1:50,
  age = c(rep(NA, 10), round(rnorm(40, 45, 10))),
  gender = c(rep(NA, 5), sample(c("Male", "Female"), 45, replace = TRUE)),
  bmi = c(rep(NA, 8), round(rnorm(42, 26, 4), 1)),
  treatment = c(sample(c("Control", "Treatment"), 50, replace = TRUE)),
  cholesterol = c(rep(NA, 12), round(rexp(38, 1/200) + 150, 0)),
  stringsAsFactors = FALSE
)

# Create mock project
mock_project <- list(
  data = test_data,
  id_field = "record_id",
  metadata = data.frame(
    field_name = c("record_id", "age", "gender", "bmi", "treatment", "cholesterol"),
    field_type = c("text", "text", "radio", "text", "radio", "text"),
    text_validation_type_or_show_slider_number = c(NA, "number", NA, "number", NA, "number"),
    stringsAsFactors = FALSE
  ),
  project_info = list(
    project_title = "Test Clinical Study"
  )
)
class(mock_project) <- "redcap_project"

cat("\n=== Test 1: Basic Table (Overall) ===\n")
cat("Should show:\n")
cat("- N row at the top\n")
cat("- Type column showing 'Mean (SD)' or 'n (%)'\n\n")

table1 <- generate_table_one(
  mock_project,
  vars = c("age", "gender", "bmi"),
  cont_vars = c("age", "bmi"),
  cat_vars = c("gender")
)
print(table1)

cat("\n=== Test 2: Stratified Table ===\n")
cat("Should show:\n")
cat("- N row showing count for each group\n")
cat("- Type column for all variables\n\n")

table2 <- generate_table_one(
  mock_project,
  vars = c("age", "gender", "bmi"),
  strata = "treatment",
  cont_vars = c("age", "bmi"),
  cat_vars = c("gender")
)
print(table2)

cat("\n=== Test 3: With Non-Normal Distribution ===\n")
cat("Should show:\n")
cat("- Type = 'Median [IQR]' for cholesterol\n")
cat("- Type = 'Mean (SD)' for age\n\n")

table3 <- generate_table_one(
  mock_project,
  vars = c("age", "cholesterol"),
  cont_vars = c("age", "cholesterol"),
  non_normal = "cholesterol"
)
print(table3)

cat("\n=== Test 4: Stratified with Statistical Tests ===\n")
cat("Should show:\n")
cat("- N row for each group\n")
cat("- Type column\n")
cat("- P-value column\n\n")

table4 <- generate_table_one(
  mock_project,
  vars = c("age", "gender", "bmi"),
  strata = "treatment",
  cont_vars = c("age", "bmi"),
  cat_vars = c("gender"),
  test = TRUE
)
print(table4)

cat("\n=== All tests complete! ===\n")
cat("\nExpected improvements:\n")
cat("1. ✓ N row appears at the top of every table\n")
cat("2. ✓ Type column shows the statistic type for each variable\n")
cat("3. ✓ For continuous: 'Mean (SD)' or 'Median [IQR]'\n")
cat("4. ✓ For categorical: 'n (%)'\n")
cat("5. ✓ Missing rows show 'n (%)'\n")
cat("6. ✓ Works with both overall and stratified tables\n")
