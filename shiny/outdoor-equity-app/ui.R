library(rsconnect)
library(bslib)
library(shinyWidgets)

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
                 includeMarkdown("./text/background-about.md"),
                 ),
               tabPanel(
                 title = "User Guide",
                 includeMarkdown("./text/userGuide-about.md")
                 ),
               tabPanel(
                 title = "Metdata",
                 # Note(HD): need to change this to a rmd file to include DT table
                 includeMarkdown("./text/metadata-about.md")
                 )),
    tabPanel("Agencies",
             "graphs and inputs to compare agenices here"),
    tabPanel("Reservable Sites",
             "graphs and inputs to compare reservable sites here"),
    tabPanel("Spatial Analysis",
             "spatial analysis maps and inputs here",
             # subset agency picker input ----
             shinyWidgets::pickerInput(inputId = "agency",
                                       label = "Select an agency:",
                                       choices = c("USFS", "NPS", "USACE", "BOR"),
                                       selected = c("USFS", "NPS", "USACE", "BOR"),
                                       multiple = TRUE,
                                       options = pickerOptions(actionsBox = TRUE))
             ),
    tabPanel("Temporal Analysis",
             "graphs that show interesting temporal trends")
  )
  
)
