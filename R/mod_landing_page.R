#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

# mod_landing_page_ui <- function(id){
#
# }

data('scotia_mortgage_doc')

mod_landing_page_ui <- fluidPage(
  shinydashboard::dashboardSidebar(),
  fluidRow(
    column(4,
           "Map",
           selectInput(inputId = "NicknameID",
                       label = "Select property nickname name",
                       choices = c("Woodward", "Murphy", "Home"))
    ),
    column(8,
           "IPM Total Herd Population",
            uiOutput("daterange")
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
           tableOutput("costdata")),
    column(8,
           "Monthly costs",
           plotOutput("bargraph", click = "plot_click"))
  )
)



#' landing_page Server Functions
#'
#' @noRd

# mod_landing_page_server <- function(id){
#   moduleServer( id, function(input, output, session){})
#   }


mod_landing_page_server <- function(input, output, session){

  output$result <- renderText({
    paste("You chose", input$variable)
  })

  # map
  # output$myMap  <- leaflet::renderLeaflet({
  #   caribouMap(input$variable)
  # })

  output$daterange <- renderUI({
    dateRangeInput("daterange", "Select the date range:",
                   start = as.Date(min(scotia_mortgage_doc$Date), format = "%M %dd %yyyy"),
                   end = as.Date(max(scotia_mortgage_doc$Date), format = "%M %dd %yyyy"),
                   min = as.Date(min(scotia_mortgage_doc$Date), format = "%M %dd %yyyy"),
                   max = as.Date(max(scotia_mortgage_doc$Date), format = "%M %dd %yyyy")
    )
  })

# make bar plot
  output$linegraph <- plotly::renderPlotly({
    net_income_plot(scotia_mortgage_doc, input$NicknameID, input$daterange[1], input$daterange[2], sum_property_md_lookup)

})

  output$bargraph <- renderPlot({
    cost_bar_plot(scotia_mortgage_doc, input$NicknameID)

  })

  output$costdata <- renderTable(
    nearPoints(monthly_cost_summary(scotia_mortgage_doc, input$NicknameID), input$plot_click, xvar = "month", yvar = "costs_total")

  )



}


shinyApp(mod_landing_page_ui, mod_landing_page_server)



## To be copied in the UI
#mod_landing_page_ui("landing_page_1")

## To be copied in the server
#mod_landing_page_server("landing_page_1")
