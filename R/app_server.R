#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
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

  output$scotia_mortgage_doc <- DT::renderDT({
                 DT::datatable(scotia_mortgage_doc,
                               filter = "top",
                               options = list(
                                 dom = 'lrtip',
                                 pageLength = 5
                               ))})



  output$linegraph <- plotly::renderPlotly({
    net_income_plot(filtered_property_raw(scotia_mortgage_doc, input$year, input$costType), input$daterange[1], input$daterange[2], sum_property_md_lookup_clean)
  })

  output$cost_message_output <- renderText({
    get_cost_msg_txt(input$costType)
  })

  output$bargraph <- renderPlot({
    cost_bar_plot(filtered_property_raw(scotia_mortgage_doc, input$year, input$costType))  # Using input$costType
  })

  output$costdata <- renderTable(
    nearPoints(monthly_cost_summary(scotia_mortgage_doc), input$plot_click, xvar = "month", yvar = "costs_total")
  )
}
