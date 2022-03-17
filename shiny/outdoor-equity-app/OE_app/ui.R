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
                                         # Note(HD) when created set a value for the input to an empty string
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
                                         onInitialize = I('function() { this.setValue(""); }')
                                       )),
                        # conditional analysis type is comparison first variable ----
                        conditionalPanel(condition = "input.analysis == 'compare'",
                                         # inputId = comparison ----
                                         # have to inputs that dynamically change for second input
                                         selectizeInput(inputId = "comparison",
                                                        label = "3. Pick first variable to compare",
                                                        # median right now bc more robust to outliers
                                                        choices = c(booking_scat_var = "book_scat",
                                                                    agency_comp_acs_col_vars = "acs_scat"),
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                          ))), # end of conditional comparison first variable
                        # conditional comparison is booking window second variable ----
                        conditionalPanel(condition = "input.comparison == 'book_scat'",
                                         # inputId = scat_ridb_vars ----
                                         selectizeInput(inputId = "scat_ridb_vars",
                                                        label = "4. Pick second variable to compare",
                                                        choices = agency_comp_scat_vars,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))), # end of conditional compare booking window
                        # conditional comparison is agency_comp_acs_col_vars second variable ----
                        conditionalPanel(condition = "input.comparison == 'acs_scat'", 
                                         # inputId = comp_col_vars ----
                                         selectizeInput(inputId = "comp_col_vars",
                                                        label = "4. Pick second variable to compare",
                                                        choices = agency_comp_col_vars, # showing up in distribution
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))), # end of conditional compare acs vars
                        # conditional analysis is distribution
                        conditionalPanel(condition = "input.analysis == 'hist'",
                                         # inputId = agency_hist_vars ----
                                         selectizeInput(inputId = "agency_hist_vars",
                                                     label = "3. Pick a variable to see its distribution",
                                                     choices = agency_hist_vars,
                                                     multiple = FALSE,
                                                     options = list(
                                                       placeholder = "Type to select a variable",
                                                       onInitialize = I('function() { this.setValue(""); }')
                                                     ))) # end of conditional distribution
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
