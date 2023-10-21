library(shiny)

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
           selectInput("costType", "Select Cost Type:",
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

# Server definition
mod_landing_page_server <- function(input, output, session) {
  output$result <- renderText({
    paste("You chose", input$variable)
  })

  output$daterange <- renderUI({
    dateRangeInput("daterange", "Select the date range:",
                   start = as.Date(min(scotia_mortgage_doc$Date), format = "%M %dd %yyyy"),
                   end = as.Date(max(scotia_mortgage_doc$Date), format = "%M %dd %yyyy"),
                   min = as.Date(min(scotia_mortgage_doc$Date), format = "%M %dd %yyyy"),
                   max = as.Date(max(scotia_mortgage_doc$Date), format = "%M %dd %yyyy")
    )
  })

  output$linegraph <- plotly::renderPlotly({
    net_income_plot(scotia_mortgage_doc, input$daterange[1], input$daterange[2], sum_property_md_lookup_clean)
  })

  output$bargraph <- renderPlot({
    cost_bar_plot(scotia_mortgage_doc, input$year, input$costType)  # Using input$costType
  })

  output$costdata <- renderTable(
    nearPoints(monthly_cost_summary(scotia_mortgage_doc), input$plot_click, xvar = "month", yvar = "costs_total")
  )
}

# Shiny app
shinyApp(mod_landing_page_ui, mod_landing_page_server)
