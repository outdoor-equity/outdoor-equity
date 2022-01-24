# R Shiny App tutorial with Allison

# QUESTIONS
# how should I name apps specifically? other than app.R? 

# google R Shiny widgets gallery to see the different widget options

# attach packages
library(shiny)
library(tidyverse)

# get ridb 06 data
ridb_06 <- read.csv(here("data", "ridb-06-clean.csv"))

# create 'ui'
ui <- fluidPage(
  titlePanel("Test App Title of RIDB Data 2006"),
  sidebarLayout(
    sidebarPanel("Here are my widgets", 
                 radioButtons(inputId = "state",
                              label = "Choose state:",
                              choices = c("OR", "UT", "WA", "TX", "MO", "California is best"="CA", "AR", "UNKN",
                                          "NY", "OK", "OH", "KS", "ON", "IL", "LA", "WI", "AZ",
                                          "PR", "ID", "TN","ME", "IA", "NV", "MI", "NM", "PA", "NA"="--",
                                          "GA", "MD", "NE", "NJ", "NC", "AL", "KY", "BC", "WV",
                                          "PQ", "MN", "CT"))),
    mainPanel("Here is my graph", 
              plotOutput(outputId = "state_plot"))
  )
)

# create 'server'
server <- function(input, output) {
  # created reactive data frame that depends on selection from widget state
  state_select <- reactive({
    ridb_06 %>% 
      filter(customer_state == input$state)
  })
  
  output$state_plot <- renderPlot({
    ggplot(data = state_select(), aes(x = "number_of_people", y = "total_paid")) +
      geom_point()
  })
  
  
}

# let R know that you want to combine the ui & server into an app
shinyApp(ui = ui, server = server)




  