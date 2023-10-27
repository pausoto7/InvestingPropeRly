#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'



mod_landing_page_ui <- fluidPage(

  titlePanel("Malaria facility visualisation app"),

  sidebarLayout(

    sidebarPanel(
      # selector for district
      selectInput(inputId = "NicknameID",
                  label = "Select property nickname name",
                  choices = c("Woodward", "Murphy", "Home")
      ),


      # selector for
      selectInput("year", "Select a Year:",
                  choices = c("all", 2020:2030),
                  selected = "all"
      ),

      checkboxGroupInput("costType", "Select Cost Type(s):",
                         choices = c("all", unique(scotia_mortgage_doc$TypeAndInfo)),  # Use unique values from the "CostType" column
                         selected = "all"  # Set the default selection
      ),

    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Net Cost",
          plotly::plotlyOutput("linegraph"),
          DT::dataTableOutput("scotia_mortgage_doc")

        ),

        tabPanel(
          "Monthly Costs",
          shiny::plotOutput("bargraph", click = "plot_click")
        )
      )
      # br(),
      # hr(),
      # p("Welcome to the malaria facility visualisation app! To use this app. The data dictionary is as follows:"),

    )
  )
)






