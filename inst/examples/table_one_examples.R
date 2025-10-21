# Table One Generation Examples for sardine Package
# ==================================================

# Load the package (dev mode)
devtools::load_all()

# Create sample project
project <- redcap_project()

# Example 1: Basic Table One (all variables, no stratification)
# --------------------------------------------------------------
table_one_basic <- generate_table_one(project)
print(table_one_basic)

# Example 2: Table One with selected variables
# ---------------------------------------------
table_one_selected <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi", "study_group")
)
print(table_one_selected)

# Example 3: Filtered Table One
# ------------------------------
# Exclude withdrawn participants and include only those who consented
table_one_filtered <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi"),
  filter = c("withdrawn != 1")
)
print(table_one_filtered)

# Example 4: Multiple filters
# ----------------------------
# Complex filtering with multiple conditions
table_one_multi_filter <- generate_table_one(
  project,
  vars = c("age", "bmi", "baseline_score"),
  filter = c(
    "age >= 18",           # Adults only
    "consent_complete == 2",  # Complete consent
    "!is.na(baseline_score)"  # Has baseline data
  )
)
print(table_one_multi_filter)

# Example 5: Stratified Table One
# --------------------------------
# Stratify by treatment group to compare characteristics
table_one_stratified <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi", "baseline_score"),
  strata = "study_group"
)
print(table_one_stratified)

# Example 6: With Statistical Tests
# ----------------------------------
# Include p-values comparing groups
table_one_with_tests <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi", "baseline_score"),
  strata = "study_group",
  test = TRUE
)
print(table_one_with_tests)

# Example 7: Force Variable Types
# --------------------------------
# Override automatic type detection
table_one_forced_types <- generate_table_one(
  project,
  vars = c("age", "gender", "study_site", "bmi", "smoking_pack_years"),
  cat_vars = c("gender", "study_site"),  # Force as categorical
  cont_vars = c("age", "bmi", "smoking_pack_years"),  # Force as continuous
  strata = "treatment_group"
)
print(table_one_forced_types)

# Example 8: Custom Variable Types and Non-normal Distributions
# --------------------------------------------------------------
# Specify which variables to treat as non-normal (use median/IQR)
table_one_custom <- generate_table_one(
  project,
  vars = c("age", "bmi", "cholesterol", "gender", "smoking_status"),
  cont_vars = c("age", "bmi", "cholesterol"),  # Ensure these are continuous
  cat_vars = c("gender", "smoking_status"),    # Ensure these are categorical
  non_normal = c("bmi", "cholesterol"),        # These will show median [IQR]
  strata = "treatment_group",
  test = TRUE,
  digits = 2  # 2 decimal places for continuous vars
)
print(table_one_custom)

# Example 9: Filtered AND Stratified with Tests
# ----------------------------------------------
# Combine filtering, stratification, and statistical tests
table_one_complete <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi", "baseline_score"),
  filter = c("withdrawn != 1", "age >= 18"),
  strata = "treatment_group",
  cont_vars = c("age", "bmi", "baseline_score"),
  cat_vars = c("gender"),
  non_normal = "bmi",
  test = TRUE,
  test_type = "auto"
)
print(table_one_complete)

# Example 10: Export to Different Formats
# ---------------------------------------
# As knitr kable (for R Markdown)
table_kable <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi"),
  strata = "study_group",
  output_format = "kable"
)
print(table_kable)

# Example 11: Exclude Missing Data from Display
# ---------------------------------------------
table_no_missing <- generate_table_one(
  project,
  vars = c("age", "gender", "bmi"),
  cat_vars = "gender",
  cont_vars = c("age", "bmi"),
  include_missing = FALSE
)
print(table_no_missing)

# Example 12: Save to File
# -----------------------
table_for_export <- generate_table_one(
  project,
  filter = c("withdrawn != 1"),
  strata = "study_group",
  test = TRUE
)

# Save as CSV
write.csv(table_for_export, "table_one_results.csv", row.names = FALSE)

# Example 13: Integration with Data Quality Report
# -------------------------------------------------
# Generate both quality report and Table One for comprehensive overview
quality_report <- generate_data_quality_report(project, missing_threshold = 0.15)
table_one <- generate_table_one(
  project,
  filter = "consent_complete == 1",
  strata = "study_group"
)

cat("\n=== Data Quality Summary ===\n")
print(quality_report)

cat("\n=== Descriptive Statistics ===\n")
print(table_one)
