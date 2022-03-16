# user interface ----
ui <- fluidPage(
  
  # set theme ----
  theme = bs_theme(bootswatch = "minty"),
  
  # app title ----
  tags$h1("UCSB MEDS Capstone Outdoor Equity App"),
  
  # app subtitle ----
  p(strong("Visualize and explore a joined dataset of RIDB and ACS Census data")),
  
  # layout navbarPage ----
  navbarPage(
    # title of nav bar ----
    # need title to display other nav bar tabs
    ## this title is also the name of the tab on a browser ##
    "Get Started",
    # nav bar tabs ----
    # About tab ----
    navbarMenu("About",
               tabPanel(title = "Background",
                 # Note(HD): need . in front of file path for relative path
                 includeMarkdown("./text/background-about.md")
                 ), # end of Background tabPanel
               tabPanel(title = "User Guide",
                 includeMarkdown("./text/userGuide-about.md")
                 ), # end of User Guide tabPanel
               tabPanel(title = "Metadata",
                 # Note(HD): need to change this to a rmd file to include DT table
                 includeMarkdown("./text/metadata-about.md")
                 )), # end of About tab ----
    # Analysis tab ----
    navbarMenu("Analysis",
               tabPanel(title = "Agency Analysis",
                        "graphs and inputs to compare agencies here"
                        ), # end of Agency Analysis tabPanel
               tabPanel(title = "Reservable Site Analysis",
                        "graphs and inputs to compare reservable sites here"
                        ), # end of Reservable Site analysis tabPanel
               ), # end of Analysis tab ----
    # Data Download tab ----
    tabPanel(title = "Data Download",
             "DT Table and data download inputs here") # end of Data Download tabPanel
    
    
    
    #### TABS TO THINK ABOUT READDING LATER #### ----           
               # tabPanel(title = "Spatial Analysis",
               #   "spatial analysis maps and inputs here",
               #   # subset agency picker input ----
               #   shinyWidgets::pickerInput(inputId = "agency",
               #                             label = "Select an agency:",
               #                             choices = c("USFS", "NPS", "USACE", "BOR"),
               #                             # selected = c("USFS", "NPS", "USACE", "BOR"), 
               #                             # use this to automatically to set default choices
               #                             multiple = TRUE,
               #                             options = pickerOptions(actionsBox = TRUE)),
               #   shinyWidgets::pickerInput(inputId = "state",
               #                             label = "Select a state:",
               #                             choices = c("California", "Alaska", "Utah", "Maine"),
               #                             selected = "California",
               #                             multiple = TRUE,
               #                             options = pickerOptions(actionsBox = TRUE)),
               #   shinyWidgets::pickerInput(inputId = "year",
               #                             label = "Select a year:",
               #                             choices = 2018,
               #                             multiple = TRUE,
               #                             options = pickerOptions(actionsBox = TRUE)),
               #   shinyWidgets::pickerInput(inputId = "variable",
               #               label = "Select a variable",
               #               choices = c("Median income", "Race", "Transportation"),
               #               selected = " ", # need to figure out why there is a default select
               #               multiple = FALSE,
               #               options = pickerOptions(actionsBox = TRUE))
               # ),
               # tabPanel(title = "Temporal Analysis",
               #   "graphs that show interesting temporal trends"
               # ))
  ) # end of navbarPage ----
  ) # end of fluid page ---- 
