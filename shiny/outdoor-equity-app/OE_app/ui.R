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
    "Visualize RIDB Data",
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
                        # inputId = agency ----
                        selectizeInput(inputId = "agency",
                                       label = "1. Select an agency:",
                                       choices = c("BOR", "NPS", "USACE", "USFS"),
                                       multiple = TRUE,
                                       options = list(
                                         placeholder = "Type to search for an agency",
                                         onInitialize = I('function() { this.setValue(""); }')
                                         )),
                        # inputId = analysis ----
                        selectizeInput(inputId = "analysis",
                                       label = "2. What kind of analysis do you want to see?",
                                       # conditional panel options / "id's" ---- 
                                       choices = c(Comparison = "compare", Distribution = "hist"),
                                       multiple = FALSE,
                                       options = list(
                                         placeholder = "Type to select an analysis type",
                                         # need to figure out what onInitialize does exactly?
                                         onInitialize = I('function() { this.setValue(""); }')
                                       )),
                        # only show panel if analysis type is comparison ----
                        conditionalPanel(condition = "input.analysis == 'compare'",
                                         # inputId = compare ----
                                         selectizeInput(input = "comparison",
                                                        label = "3. Pick two variables to compare",
                                                        # mean vs median?
                                                        # need to add dist traveled 
                                                        choices = c("mean_daily_cost_per_visitor",
                                                                    "mean_booking_window",
                                                                    # count is number of visits to a park
                                                                    "count"),
                                                        multiple = TRUE,
                                                        options = list(
                                                          placeholder = "Type to select two variables",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                          )))
                        ), # end of Agency Analysis tabPanel
               tabPanel(title = "Reservable Site Analysis",
                        "graphs and inputs to compare reservable sites here"
                        ), # end of Reservable Site analysis tabPanel
               ), # end of Analysis tab ----
    # Data Download tab ----
    tabPanel(title = "Data Download",
             "DT Table and data download inputs here") # end of Data Download tabPanel
    
    
    
    #### TABS TO THINK ABOUT READDING LATER #### ----  
    
    
    # selectizeInput(inputId = "distribution",
    #                label = "3. Pick a variable to see its distribution",
    #                choices = c()),
    
    
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
