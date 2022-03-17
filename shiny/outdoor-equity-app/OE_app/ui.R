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
                                         placeholder = "Select an analysis type",
                                         onInitialize = I('function() { this.setValue(""); }')
                                       )),
                        
                        
                        # conditional analysis type is comparison first variable ----
                        conditionalPanel(condition = "input.analysis == 'compare'",
                                         # inputId = comparison ----
                                         # have to inputs that dynamically change for second input
                                         selectizeInput(inputId = "comparison",
                                                        label = "3. Pick first variable to compare",
                                                        # median right now bc more robust to outliers
                                                        choices = c(booking_scat_var,
                                                                    agency_comp_acs_col_vars),
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                          ))), # end of conditional comparison first variable
                        # conditional analysis is distribution
                        conditionalPanel(condition = "input.analysis != '' && input.analysis == 'hist'",
                                         # inputId = agency_hist_vars ----
                                         selectizeInput(inputId = "agency_hist_vars",
                                                        label = "3. Pick a variable to see its distribution",
                                                        choices = agency_hist_vars,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))), # end of conditional distribution
                        
                        
                        # conditional comparison is booking window second variable ----
                        conditionalPanel(condition = "input.analysis != 'hist' && input.analysis != '' && input.comparison != '' && input.comparison == 'median_booking_window'",
                                         # inputId = scat_ridb_vars ----
                                         # 2 options 
                                         selectizeInput(inputId = "scat_ridb_vars",
                                                        label = "4. Pick second variable to compare",
                                                        choices = agency_comp_scat_vars,
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))), # end of conditional compare booking window
                        # conditional comparison is agency_comp_acs_col_vars second variable ----
                        conditionalPanel(condition = "input.analysis != '' && input.analysis != 'hist' && input.comparison != '' && input.comparison != 'median_booking_window'", 
                                         # inputId = comp_col_vars ----
                                         # 5 options
                                         selectizeInput(inputId = "comp_col_vars",
                                                        label = "4. Pick second variable to compare",
                                                        choices = agency_comp_col_vars, # showing up in distribution
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))) # end of conditional compare acs vars
                        
                        
                        ), # end of Agency Analysis tabPanel
               tabPanel(title = "Reservable Site Analysis",
                        # inputId = admin_unit ----
                        selectizeInput(inputId = "admin_unit",
                                       label = "1. Select an administrative unit:",
                                       choices = c("BOR", "NPS", "USACE", "USFS"),
                                       multiple = TRUE,
                                       options = list(
                                         placeholder = "Type to search for an admin unit",
                                         onInitialize = I('function() { this.setValue(""); }')
                                       )),
                        # inputId = site_info_comp
                        selectizeInput(inputId = "site_info_comp",
                                       label = "2. What kind of information do you want to see?",
                                       choices = c('Site Information' = "site_info", 'Variable Analysis' = "var_analysis"),
                                       multiple = FALSE,
                                       options = list(
                                         placeholder = "Select an information type",
                                         onInitialize = I('function() { this.setValue(""); }')
                                         )), # end of pick which info you want to see ----
                        # conditional is site_info ----
                        conditionalPanel(condition = "input.site_info_comp == 'site_info'",
                                         # inputId = site_info ----
                                         selectizeInput(inputId = "site_info",
                                                        label = "3. Pick a reservable site",
                                                        choices = agency_comp_scat_vars, # need to change
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a reservable site",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        ))),
                        # conditional is var_analysis ----
                        conditionalPanel(condition = "input.site_info_comp == 'var_analysis'",
                                         # inputId = site_info ----
                                         selectizeInput(inputId = "var_analysis",
                                                        label = "3. Pick a variable to visualize",
                                                        choices = agency_comp_scat_vars, # need to change
                                                        multiple = FALSE,
                                                        options = list(
                                                          placeholder = "Type to select a variable",
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        )))
                        ), # end of Reservable Site analysis tabPanel
               ), # end of Analysis tab ----
    # Data Download tab ----
    tabPanel(title = "Data Download",
             "DT Table and data download inputs here") # end of Data Download tabPanel

  
  ) # end of navbarPage ----
  ) # end of fluid page ---- 
