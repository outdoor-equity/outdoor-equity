# # UI ----
ui <- fluidPage(
  # use shiny dashboard elements in shiny
  useShinydashboard(),
  
  # set theme ----
  theme = bs_theme(bootswatch = "minty"),
  
  # app title ----
  tags$h1("UCSB MEDS Capstone Outdoor Equity App"),
  
  # app subtitle ----
  p(strong("Visualize and explore a joined dataset of RIDB and ACS Census data")),
  
  # navbarPage structure ----
  navbarPage(
    # TO nav bar and TO tab in web browser 
    "Visualize RIDB Data",
    
    ## About tab ----
    navbarMenu("About", icon = icon("info-circle"),
               
               tabPanel(title = "Background",
                        # Note(HD): need . in front of file path for relative path
                        includeMarkdown("./text/background-about.md")
               ), # EO Background tabPanel
               
               
               tabPanel(title = "User Guide",
                        includeMarkdown("./text/userGuide-about.md")
               ), # EO User Guide tabPanel
               
               
               tabPanel(title = "Metadata",
                        # Note(HD): need to change this to a rmd file to include DT table
                        includeMarkdown("./text/metadata-about.md")
                        
               )), ## EO About tab ----
    
    ## Analysis tab ---- 
    navbarMenu("Analysis", icon = icon("chart-bar"),
               
               ### SO distribution / single variable analysis ----
               tabPanel(title = "Agency/ Site Single Variable Analysis",
                        fluid = TRUE, # (HD) not sure what this argument does
                        
                        # distribution sidebar layout
                        sidebarLayout(
                          # distribution analysis side bar panel
                          sidebarPanel(
                            width = 3,
                            titlePanel("Visualize a variable distribution"),
                            # agency input
                            select_agency(),
                            # admin input
                            select_admin_unit(),
                            # variable input
                            selectizeInput(inputId = "vars_single",
                                           label = "3. Pick a variable to see its distribution",
                                           choices = dist_vars,
                                           multiple = FALSE,
                                           options = list(
                                             placeholder = "Type to search for a variable",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )) # end of step 3 select variable
                            
                          ), # EO distribution sidebar panel
                          
                          # distribution main panel aka visual?
                          mainPanel(
                            
                            plotOutput(outputId = "vars_single_plot")
                            
                          ) # EO distribution main panel
                        ) # EO distribution sidebar layout
                        
                        ), #### EO distribution / single variable analysis ----
               
               ### SO comparison / multiple variable analysis ----
               tabPanel(title = "Agency/ Site Comparison Analysis",
                        fluid = TRUE,
                        
                        # comparison sidebar layout
                        titlePanel("Comparison Analysis"),
                        sidebarLayout(
                          # comparison analysis side bar panel
                          sidebarPanel(
                            
                            # agency input
                            select_agency(),
                            # admin input
                            select_admin_unit(),
                            # comparison first var
                            selectizeInput(inputId = "compare_first",
                                           label = "3. Select the first variable",
                                           choices = c("Distance traveled" = "distance_traveled_mi",
                                                       "Race" = "race"),
                                           multiple = FALSE,
                                           options = list(
                                             placeholder = "Type to search for a variable",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )), # end of step 3 compare first var
                            
                            # comparison second var 
                            selectizeInput(inputId = "compare_second",
                                           label = "4. Select a second variable to compare",
                                           choices = c("Distance traveled" = "distance_traveled_mi",
                                                       "Race" = "race"),
                                           multiple = FALSE,
                                           options = list(
                                             placeholder = "Type to search for a variable",
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )) # end of step 4 compare second var 
                            
                          ), # EO comparison sidebar panel
                          
                          # comparison main panel aka visual?
                          mainPanel(
                            
                            plotOutput(outputId = "compare_plot")
                        
                        ) # EO comparison main panel
                        ) # EO comparison sidebar layout
                        ), #### EO comparison / multiple variable analysis ----

               ### SO site analysis / maps ----
               tabPanel(title = "Reservable Site Analysis",
                        fluid = TRUE,
                        # site sidebar layout
                        sidebarLayout(
                          # site side bar panel
                          sidebarPanel(
                            
                            titlePanel("Visualize a Reservable Site"),
                            # site agency input
                            select_agency(),
                            # site admin input
                            select_admin_unit(),
                            # select a site input 
                            select_site(),
                            
                          ), # EO site sidebar panel
                          
                          # site main panel aka visual?
                          mainPanel(
                            
                            plotOutput(outputId = "site_plot")
                            
                          ) # EO site main panel
                        ) # EO site sidebar layout
                        ) #### EO site analysis / maps ----

               ), ## EO Analysis tab ----
    
    ## Data Download ----
    tabPanel("Data Download", icon = icon("download-alt", lib = "glyphicon"),
             
             titlePanel("Create a subsetted dataset to download"),
             # Note (HD) each fluidRow has a default width of 12
             # Note (HD) should figure out if wellPanel is doable
             fluidRow(
               # Note (HD) box width defaults 6
               box(
                 width = 12,
                 title = "Test",
                 status = "primary",
                 solidHeader = TRUE,
                 column(width = 4,
                        select_agency()),
                 column(width = 4,
                        select_admin_unit()),
                 column(width = 4,
                        select_site())
               ) # EO box
             ), # EO fluidRow
             
             ### OP data download ----
             #DT::dataTableOutput(outputId = "data_download")
               
             

             ) ## EO Data Download ----
    
  ) # EO navbarPage
  
) # EO fluid page
