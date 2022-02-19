# user interface ----
ui <- fluidPage(
  
  # set theme
  
  # app title ----
  tags$h1("Outdoor Equity App"),
  
  # app subtitle ----
  p(strong("Visualize and explore a joined dataset of RIDB and ACS data")),
  
  # layout
  navbarPage(
    # title of nav bar
    "Outdoor Equity",
    # nav bar tabs
    tabPanel("Background and User Guide",
             "background information and user guide documentation here"),
    tabPanel("Spatial Analysis",
             "spatial analysis maps and inputs here"),
    tabPanel("Agency Comparisons",
             "graphs and inputs to compare agenices here"),
    tabPanel("Reservable Site Comparisons",
             "graphs and inputs to compare reservable sites here"),
    tabPanel("Temporal Analysis",
             "graphs that show interesting temporal trends")
  )
  
)