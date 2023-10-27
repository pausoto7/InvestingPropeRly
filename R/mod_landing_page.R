library(shiny)


data('scotia_mortgage_doc')
data('property_md_lookup')


# Shiny app
shinyApp(mod_landing_page_ui, mod_landing_page_server)
