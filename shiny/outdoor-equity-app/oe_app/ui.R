# user interface ----
ui <- fluidPage(
  
  # set theme ----
  theme = bs_theme(bootswatch = "minty"),
  
  # app title ----
  tags$h1("UCSB MEDS Capstone Outdoor Equity App"),
  
  # app subtitle ----
  p(strong("Visualize and explore a joined dataset of RIDB and ACS Census data")),
  
  # navbarPage structure ----
  navbarPage(
    # title of nav bar and title of tab in web browser 
    "Visulize RIDB Data",
    
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
               
               # Agency Analysis dist ----
               tabPanel(title = "Agency/ Site Single Variable Analysis",
                        fluid = TRUE, # (HD) not sure what this argument does
                        
                        # sidebar layout ----
                        sidebarLayout(
                          # agency analysis side bar panel ----
                          sidebarPanel(
                            
                            titlePanel("Visualize a variable distribution"),
                            # agency input ----
                            selectizeInput(inputId = "agency_single",
                                           label = "1. Select an agency:",
                                           choices = c("BOR", "NPS", "USACE", "USFS"),
                                           multiple = TRUE,
                                           options = list(
                                             placeholder = "Type to search for an agency",
                                             # Note(HD) when created set a value for the input to an empty string
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )), # end of step 1 select an agency ----

                            # admin input ----
                            selectizeInput(inputId = "admin_unit_single",
                                           label = "2. Select an Administrative Unit",
                                           choices = admin_units,
                                           multiple = TRUE,
                                           options = list(
                                             placeholder = "Type to search for an admin unit",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )), # end of step 2 select an admin unit ----

                            # variable input ----
                            selectizeInput(inputId = "vars_single",
                                           label = "3. Pick a variable to see its distribution",
                                           choices = c("Distance traveled" = "distance_traveled_mi",
                                                       "Race" = "race"),
                                           multiple = FALSE,
                                           options = list(
                                             placeholder = "Type to select a variable",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )) # end of step 3 select variable ----
                            
                          ), # end of sidebar panel ----
                          
                          # main panel aka visual? ----
                          mainPanel(
                            
                            plotOutput(outputId = "agency_analysis")
                            
                          ) # end of mainPanel ----
                        ) # end of sidebar layout ----
                        
                        ), # end of agency / site single var analysis ----
               
               tabPanel(title = "Agency/ Site Comparison Analysis",
                        fluid = TRUE,
                        
                        # sidebar layout ----
                        sidebarLayout(
                          # agency analysis side bar panel ----
                          sidebarPanel(
                            
                            titlePanel("Comparison Analysis"),
                            # agency input ----
                            selectizeInput(inputId = "agency_compare",
                                           label = "1. Select an agency:",
                                           choices = c("BOR", "NPS", "USACE", "USFS"),
                                           multiple = TRUE,
                                           options = list(
                                             placeholder = "Type to search for an agency",
                                             # Note(HD) when created set a value for the input to an empty string
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )), # end of step 1 select an agency ----
                            
                            # admin input ----
                            selectizeInput(inputId = "admin_unit_compare",
                                           label = "2. Select an Administrative Unit",
                                           choices = admin_units,
                                           multiple = TRUE,
                                           options = list(
                                             placeholder = "Type to search for an admin unit",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )), # end of step 2 select an admin unit ----
                            
                            # compare first input ----
                            selectizeInput(inputId = "compare_first",
                                           label = "3. Pick first variable",
                                           choices = c("Distance traveled" = "distance_traveled_mi",
                                                       "Race" = "race"),
                                           multiple = FALSE,
                                           options = list(
                                             placeholder = "Type to select a variable",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )), # end of step 3 compare first variable ----
                            
                            # compare second input ----
                            selectizeInput(inputId = "compare_second",
                                           label = "4. Pick a second variable to compare",
                                           choices = c("Distance traveled" = "distance_traveled_mi",
                                                       "Race" = "race"),
                                           multiple = FALSE,
                                           options = list(
                                             placeholder = "Type to select a variable",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )) # end of step 3 compare first variable ----
                            
                          ), # end of sidebar panel ----
                          
                          # main panel aka visual? ----
                          mainPanel(
                            
                            plotOutput(outputId = "agency_analysis")
                        
                        ) # end of main panel
                        ) # end of sidebar layout ----
                        ), # end of agency / site comparison analysis ----

               # Reservable Site Analysis ----
               tabPanel(title = "Reservable Site Analysis") # end of Reservable Site Analysis ----
               
               ), # end of Analysis tab ----
    
    # Data Download ----
    tabPanel("Data Download",
             "Tables of data go here") # end of Data Download ----
    
  ) # end of navbarPage ----
  
  
) # end of fluid page ----
