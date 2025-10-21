#' Explore REDCap Project in a Shiny App
#'
#' Launches a Shiny app to interactively explore project structure, metadata, and data.
#'
#' @param project A redcap_project object
#' @export
explore_project_shiny <- function(project) {
  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("DT", quietly = TRUE)) {
    stop("Both shiny and DT packages are required. Please install.packages(c('shiny', 'DT'))")
  }
  arms <- project$arms %||% data.frame()
  events <- project$events %||% data.frame()
  instruments <- project$instruments %||% data.frame()
  fields <- project$fields %||% data.frame()
  metadata <- project$metadata %||% data.frame()
  data <- project$data %||% data.frame()

  dt_opts <- list(filter = "top", options = list(pageLength = 20))

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("REDCap Project Explorer"),
      shiny::tabsetPanel(
        shiny::tabPanel("Structure",
          shiny::tabsetPanel(
            shiny::tabPanel("Arms", DT::dataTableOutput("arms_table")),
            shiny::tabPanel("Events", DT::dataTableOutput("events_table")),
            shiny::tabPanel("Instruments", DT::dataTableOutput("instruments_table")),
            shiny::tabPanel("Fields", DT::dataTableOutput("fields_table"))
          )
        ),
        shiny::tabPanel("Metadata", DT::dataTableOutput("metadata_table")),
        shiny::tabPanel("Data", DT::dataTableOutput("data_table"))
      )
    ),
    server = function(input, output, session) {
      output$arms_table <- DT::renderDataTable(arms, filter = "top", options = list(pageLength = 20))
      output$events_table <- DT::renderDataTable(events, filter = "top", options = list(pageLength = 20))
      output$instruments_table <- DT::renderDataTable(instruments, filter = "top", options = list(pageLength = 20))
      output$fields_table <- DT::renderDataTable(fields, filter = "top", options = list(pageLength = 20))
      output$metadata_table <- DT::renderDataTable(metadata, filter = "top", options = list(pageLength = 20))
      output$data_table <- DT::renderDataTable(data, filter = "top", options = list(pageLength = 20))
    }
  )
}

# Add project$explore() method in project creation
