# librarian::shelf(shiny,
#                  tidyverse,
#                  palmerpenguins,
#                  DT,
#                  shinyWidgets,
#                  bslib,
#                  thematic,
#                  reactlog)

librarian::shelf(shiny,
                 tidyverse,
                 palmerpenguins,
                 DT)

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
  plotOutput(outputId = "bodyMass_scatterPlot"),
  
  # DT data table output (does not need input) ----
  DT::dataTableOutput(outputId = "penguins_dt")
  
) 



# break ------------------



# server instructions ----
# curly braces allows us to write as many lines we want for our output
server <- function(input, output){
  
  # filter body masses ----
  body_mass_df <- reactive({
    
    penguins %>% filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2])
    
  })
  
  # render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({
    
    # code to generate scatterplot here
    # reactive df always needs an open/close paraentheses 
    ggplot(na.omit(body_mass_df()), aes(x = flipper_length_mm, y = bill_length_mm,
                                  color = species, shape = species)) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Adelie" = "#FEA346", 
                                    "Chinstrap" = "#B251F1", 
                                    "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19, 
                                   "Chinstrap" = 17, 
                                   "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", 
           y = "Bill length (mm)",
           color = "Penguin species",
           shape = "Penguin species")
    
  })
  
  # render data table ----
  output$penguins_dt <- renderDataTable({
    DT::datatable(data = penguins,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left",
                    htmltools::em("Table 1: Palmer Penguins characteristics")),
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 20)),
                  class = "cell-border stripe",
                  colnames = colnames)
  })
    
}

# combine UI and server into an app ----
shinyApp(ui = ui, server = server)
