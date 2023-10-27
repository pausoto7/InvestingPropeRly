#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'






# UI definition
mod_landing_page_ui <- fluidPage(
  shinydashboard::dashboardSidebar(),
  fluidRow(
    column(4,
           "Map",
           selectInput(inputId = "NicknameID",
                       label = "Select property nickname name",
                       choices = c("Woodward", "Murphy", "Home")
           )
    ),
    column(8,
           "IPM Total Herd Population",
           uiOutput("daterange"),
           selectInput("year", "Select a Year:",
                       choices = c("all", 2020:2030),
                       selected = "all"
           ),
           checkboxGroupInput("costType", "Select Cost Type(s):",
                              choices = c("all", unique(scotia_mortgage_doc$TypeAndInfo)),  # Use unique values from the "CostType" column
                              selected = "all"  # Set the default selection
           )
    )
  ),
  fluidRow(
    column(12,
           "NetIncome",
           plotly::plotlyOutput("linegraph")
    )
  ),
  fluidRow(
    column(4,
           "Summary",
           tableOutput("costdata")
    ),
    column(8,
           "Monthly costs",
           plotOutput("bargraph", click = "plot_click")
    )
  )
)


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "InvestingPropeRly"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
