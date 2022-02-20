library(rsconnect)
library(bslib)

# user interface ----
ui <- fluidPage(
  
  # set theme ----
  theme = bs_theme(bootswatch = "minty"),
  
  # app title ----
  tags$h1("Outdoor Equity App"),
  
  # app subtitle ----
  p(strong("Visualize and explore a joined dataset of RIDB and ACS data")),
  
  # layout
  navbarPage(
    # title of nav bar
    "Outdoor Equity",
    # nav bar tabs
    navbarMenu("About",
               tabPanel(
                 title = "Background",
                 # Note(HD): need . in front of file path for relative path
                 includeMarkdown("./text/about-background.md"),
                 ),
               tabPanel(
                 title = "User Guide",
                 includeMarkdown("./text/userGuide-background.md")
                 ),
               tabPanel(
                 title = "Metdata",
                 includeMarkdown("./text/metadata-background.md")
                 )),
    tabPanel("Agencies",
             "graphs and inputs to compare agenices here"),
    tabPanel("Reservable Sites",
             "graphs and inputs to compare reservable sites here"),
    tabPanel("Spatial Analysis",
             "spatial analysis maps and inputs here"),
    tabPanel("Temporal Analysis",
             "graphs that show interesting temporal trends")
  )
  
)
