library(shiny)
library(devtools)

data('scotia_mortgage_doc')
data('property_md_lookup')

#load_all()

# Shiny app3
shinyApp(mod_landing_page_ui, mod_landing_page_server)
