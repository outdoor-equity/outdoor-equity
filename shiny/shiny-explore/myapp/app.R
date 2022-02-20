# librarian::shelf(shiny,
#                  tidyverse,
#                  palmerpenguins,
#                  DT,
#                  shinyWidgets,
#                  bslib,
#                  thematic,
#                  reactlog)

# load packages ----
library(shiny)
library(tidyverse)
library(palmerpenguins)
library(DT)
library(rsconnect)
library(shinyWidgets)
library(bslib)

# user interface ----
# fluidPage creates the UI we are going to see
# tags$h1 creates a level one header
# p and strong are more tags, where p is paragraph and strong is bold text
# separate elements using commas
# most input functions take a label
ui <- fluidPage(
  
  # set theme ----
  theme = bs_theme(bootswatch = "minty"),
  
  # layout
  navbarPage(
  "Title here",
  # app title ----
  tags$h1("Halina's App"),
  
  # app subtitle ----
  p(strong("I am so excited!")),
  
  tabPanel("Background",
           "penguin photos go here"),
  
  tabPanel("Penguin Plots",
           tabsetPanel(
             tabPanel("Scatterplot",
                      # body mass slider input ----
                      sliderInput(inputId = "body_mass",
                                  label = "Select a range of body masses (g):",
                                  min = 2700, max = 6300, value = c(3000, 4000)), # range of slider
                      
                      # body mass output ----
                      plotOutput(outputId = "bodyMass_scatterPlot")),
             tabPanel("Histogram",
                      # island input ----
                      shinyWidgets::pickerInput(inputId = "island",
                                                label = "Select an island:",
                                                choices = c("Torgersen", "Dream", "Biscoe"),
                                                selected = c("Torgersen", "Dream", "Biscoe"),
                                                multiple = TRUE,
                                                options = pickerOptions(actionsBox = TRUE)),
                      # flipper length plot output ----
                      plotOutput(outputId = "flipperLength_hist")
                      )
             ),
           
           "penguin plots go here"),
  
  tabPanel("Weather Plots",
           "weather plots go here"),
  
  tabPanel("Explore the Data",
           # DT data table output (does not need input) ----
           DT::dataTableOutput(outputId = "penguins_dt"),
           "put DT tables here"))
  
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
    # reactive df always needs an open/close parentheses 
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
    colnames = c("Species", "Island", "Bill Length (mm)", "Bill Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Sex", "Year")
    DT::datatable(data = penguins,
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left",
                    htmltools::em("Table 1: Palmer Penguins characteristics")),
                  options = list(pageLength = 5, lengthMenu = c(5, 10, 20)),
                  class = "cell-border stripe",
                  colnames = colnames)
  })
  
  # filter island data ----
  # validate and need work together really well
  # if false then the error message will show up
  island_df <- reactive({
    
    validate(
      need(length(input$island) > 0, "Please choose at least one island to visualize.")
    )
    penguins %>% 
      filter(island == input$island)
  })

  # render flipper length histogram ---- 
  output$flipperLength_hist <- renderPlot({
    ggplot(na.omit(island_df()), aes(x = flipper_length_mm, fill = species)) +
      geom_histogram(alpha = 0.6) +
      scale_fill_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      labs(x = "Flipper length (mm)", y = "Frequency", 
           fill = "Penguin species") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.background = element_rect(color = "white"))
})
}
# combine UI and server into an app ----
shinyApp(ui = ui, server = server)