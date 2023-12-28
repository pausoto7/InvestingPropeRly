#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'



mod_landing_page_ui <- fluidPage(

  titlePanel("Investment Property Visualization App."),

  sidebarLayout(

    sidebarPanel(
      # selector for district
      selectInput(inputId = "NicknameID",
                  label = "Select property nickname name",
                  choices = c("Woodward", "Murphy", "Home")
      ),


      # selector for
      selectInput("year", "Select a Year:",
                  choices = c("all", unique(property_md_lookup$Year)),
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
          plotly::plotlyOutput("net_income_linegraph"),
          DT::DTOutput('scotia_mortgage_doc'),
          shiny::verbatimTextOutput("cost_message_output")
          #shiny::verbatimTextOutput("cost_output")


        ),

        tabPanel(
          "Monthly Costs",
          plotly::plotlyOutput("cost_linegraph")
        )
      )
      # br(),
      # hr(),
      # p("Welcome to the malaria facility visualisation app! To use this app. The data dictionary is as follows:"),

    )
  )
)






