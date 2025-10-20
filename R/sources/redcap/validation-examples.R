#' Example and Testing Functions for Data Validation
#'
#' @description
#' Functions to create example data and test the validation functionality
#'
#' @name validation-examples
NULL

#' Create Sample REDCap Project for Testing
#'
#' @description
#' Creates a mock REDCap project object with sample data and metadata
#' for testing the data validation functions.
#'
#' @return A mock redcap_project object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample project
#' sample_project <- create_sample_redcap_project()
#' 
#' # Test missing data analysis
#' missing_analysis <- analyze_missing_data(sample_project)
#' print(missing_analysis)
#' 
#' # Test data type validation
#' validation <- validate_data_types(sample_project)
#' print(validation)
#' 
#' # Generate complete quality report
#' quality_report <- generate_data_quality_report(sample_project)
#' print(quality_report)
#' }
create_sample_redcap_project <- function() {
  
  # Create sample data with various data types and missing patterns
  sample_data <- tibble::tibble(
    record_id = sprintf("%03d", 1:50),
    # Demographics form
    age = c(25, 34, NA, 45, 67, 23, NA, 56, 41, 38,
            29, NA, 52, 31, 44, 22, 39, NA, 48, 33,
            27, 55, NA, 42, 36, 28, 51, NA, 37, 46,
            24, NA, 43, 35, 49, 26, 54, NA, 40, 32,
            30, 47, NA, 53, 41, 25, 38, NA, 45, 29),
    gender = c("Male", "Female", "Male", NA, "Female", "Male", "Female", NA, "Male", "Female",
               "Male", "Female", NA, "Male", "Female", "Male", NA, "Female", "Male", "Female",
               "Male", NA, "Female", "Male", "Female", NA, "Male", "Female", "Male", NA,
               "Female", "Male", NA, "Female", "Male", "Female", NA, "Male", "Female", "Male",
               NA, "Female", "Male", "Female", NA, "Male", "Female", "Male", NA, "Female"),
    education = c(1, 2, 3, NA, 2, 1, NA, 3, 2, 1,
                  2, NA, 3, 1, 2, NA, 3, 2, 1, NA,
                  2, 3, NA, 1, 2, 3, NA, 1, 2, 3,
                  NA, 1, 2, 3, NA, 1, 2, 3, NA, 1,
                  2, NA, 3, 1, 2, NA, 3, 1, 2, NA),
    # Medical history form  
    diabetes = c(1, 0, NA, 1, 0, 1, NA, 0, 1, 0,
                 NA, 1, 0, 1, NA, 0, 1, 0, NA, 1,
                 0, 1, NA, 0, 1, 0, NA, 1, 0, 1,
                 NA, 0, 1, 0, NA, 1, 0, 1, NA, 0,
                 1, 0, NA, 1, 0, 1, NA, 0, 1, 0),
    bp_systolic = c(120, 135, NA, 145, 128, NA, 142, 138, 125, NA,
                    132, 140, NA, 135, 128, 145, NA, 138, 132, 125,
                    NA, 142, 135, 128, NA, 145, 138, 132, NA, 125,
                    140, NA, 135, 128, 145, NA, 138, 132, 125, 142,
                    NA, 135, 128, 145, NA, 138, 132, 125, 140, NA),
    # Survey form (lots of missing data)
    satisfaction = c(5, NA, 4, NA, 3, NA, 5, NA, 4, NA,
                     3, NA, 5, NA, 4, NA, 3, NA, 5, NA,
                     4, NA, 3, NA, 5, NA, 4, NA, 3, NA,
                     5, NA, 4, NA, 3, NA, 5, NA, 4, NA,
                     3, NA, 5, NA, 4, NA, 3, NA, 5, NA),
    comments = c(rep(NA, 30), 
                 "Good service", NA, "Very satisfied", NA, "Could be better",
                 NA, "Excellent", NA, "Average", NA, "Poor", 
                 NA, "Good", NA, "Fair", NA, "Excellent", 
                 NA, "Average", NA),
    # Data with validation issues
    invalid_age = c("25", "thirty-four", "45", "67", "twenty-three", "56", "41", "38", 
                    "29", "52", "31", "44", "22", "39", "48", "33", "27", "55", 
                    "42", "36", "28", "51", "37", "46", "24", "43", "35", "49", 
                    "26", "54", "40", "32", "30", "47", "53", "41", "25", "38", 
                    "45", "29", "33", "27", "55", "42", "36", "28", "51", "37", 
                    "46", "24"),
    invalid_gender = c("M", "F", "Male", "Female", "X", "1", "2", "Other", "M", "F",
                       "Male", "Female", "M", "F", "Male", "Female", "X", "M", "F", "Male",
                       "Female", "M", "F", "Male", "Female", "X", "M", "F", "Male", "Female",
                       "M", "F", "Male", "Female", "X", "M", "F", "Male", "Female", "M",
                       "F", "Male", "Female", "X", "M", "F", "Male", "Female", "M", "F")
  )
  
  # Create sample data dictionary
  sample_dictionary <- tibble::tibble(
    field_name = c("record_id", "age", "gender", "education", "diabetes", "bp_systolic", 
                   "satisfaction", "comments", "invalid_age", "invalid_gender"),
    form_name = c("demographics", "demographics", "demographics", "demographics", 
                  "medical_history", "medical_history", "survey", "survey",
                  "validation_test", "validation_test"),
    section_header = c("", "Demographics", "", "", "Medical History", "", 
                       "Satisfaction Survey", "", "Validation Test", ""),
    field_type = c("text", "text", "radio", "radio", "yesno", "text", 
                   "radio", "notes", "text", "radio"),
    field_label = c("Record ID", "Age (years)", "Gender", "Education Level", 
                    "Diabetes History", "Systolic Blood Pressure", "Overall Satisfaction",
                    "Additional Comments", "Age (with invalid data)", "Gender (with invalid data)"),
    select_choices_or_calculations = c("", "", "Male, Male | Female, Female", 
                                       "1, High School | 2, College | 3, Graduate",
                                       "", "", "1, Very Poor | 2, Poor | 3, Fair | 4, Good | 5, Excellent",
                                       "", "", "Male, Male | Female, Female"),
    field_note = c("", "Enter age in years", "", "", "", "mmHg", "", "", "", ""),
    text_validation_type_or_show_slider_number = c("", "integer", "", "", "", "integer", "", "", "integer", ""),
    text_validation_min = c("", "18", "", "", "", "80", "", "", "18", ""),
    text_validation_max = c("", "100", "", "", "", "200", "", "", "100", ""),
    identifier = c("", "", "", "", "", "", "", "", "", ""),
    branching_logic = c("", "", "", "", "", "", "", "", "", ""),
    required_field = c("y", "", "", "", "", "", "", "", "", ""),
    custom_alignment = c("", "", "", "", "", "", "", "", "", ""),
    question_number = c("", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    matrix_group_name = c("", "", "", "", "", "", "", "", "", ""),
    matrix_ranking = c("", "", "", "", "", "", "", "", "", ""),
    field_annotation = c("", "", "", "", "", "", "", "", "", "")
  )
  
  # Create mock project object
  project <- list(
    short_name = "sample_validation_project",
    data = sample_data,
    dictionary = sample_dictionary,
    instruments = tibble::tibble(
      instrument_name = c("demographics", "medical_history", "survey", "validation_test"),
      instrument_label = c("Demographics", "Medical History", "Satisfaction Survey", "Validation Test")
    )
  )
  
  class(project) <- c("redcap_project", "list")
  return(project)
}