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
# fluidPage creates the UI we are going to see
# tags$h1 creates a level one header
# p and strong are more tags, where p is paragraph and strong is bold text
# separate elements using commas
# most input functions take a label
ui <- fluidPage(
  
  # app title ----
  tags$h1("Halina's App"),
  
  # app subtitle ----
  p(strong("I am so excited!")),
  
  # body mass slider input ----
  sliderInput(inputId = "body_mass",
              label = "Select a range of body masses (g):",
              min = 2700, max = 6300, value = c(3000, 4000)), # range of slider
  
  # body mass output ----
  plotOutput(outputId = "bodyMass_scatterPlot")
  
) 



# break ------------------



# server instructions ----
# curly braces allows us to write as many lines we want for our output
server <- function(input, output){
  # render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({
    
    # code to generate scatterplot here
    
    
  })
    
}

# combine UI and server into an app ----
shinyApp(ui = ui, server = server)
