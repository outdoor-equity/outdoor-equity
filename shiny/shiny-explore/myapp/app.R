# librarian::shelf(shiny,
#                  tidyverse,
#                  palmerpenguins,
#                  DT,
#                  shinyWidgets,
#                  bslib,
#                  thematic,
#                  reactlog)

librarian::shelf(shiny)

# create UI ----
ui <- fluidPage() # fluidPage creates the UI we are going to see 


# server instructions ----
server <- function(input, output){
  
}

# combine UI and server into an app ----
shinyApp(ui = ui, server = server)