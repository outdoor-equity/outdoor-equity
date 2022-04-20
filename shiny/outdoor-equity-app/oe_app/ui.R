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
               
               ### SO data summary ----
               tabPanel(title = "Data Summary",
                        fluid = TRUE, # (HD) not sure what this argument does
                        
                        titlePanel("Visualize a data summary"),
                        # SO data summary FR layout
                        
                        # data summary sidebar layout
                        sidebarLayout(
                          # data summary analysis side bar panel
                          sidebarPanel(
                            width = 12,
                            # agency input
                            select_agency(locationId = "summary"),
                            # admin input
                            select_admin_unit(locationId = "summary"),
                            # site input
                            select_site(locationId = "summary"),
                            # data summary vars
                            select_data_summary_vars()
                            
                          ), # EO data summary sidebar panel
                          
                          # data summary main panel aka visual
                          mainPanel(
                            
                            plotlyOutput(outputId = "data_summary_plot") %>% 
                              withSpinner(color="#0dc5c1")
                            
                          ) # EO data summary main panel
                        ) # EO data summary sidebar layout
                        
                        ), #### EO data summary ----
               
               ### SO data relationships ----
               tabPanel(title = "Data Relationships",
                        fluid = TRUE,
                        
                        titlePanel("Visualize a relationship"),
                        # data relationships sidebar layout
                        sidebarLayout(
                          # data relationships analysis side bar panel
                          # IMPORTANT NOTE(HD) MAKE SIDEBAR PANEL HORIZONTAL ----
                          sidebarPanel(
                            
                            # agency input
                            select_agency(locationId = "relationships"),
                            # admin input
                            select_admin_unit(locationId = "relationships"),
                            # site input
                            select_site(locationId = "relationships"),
                            # data relationship input
                            select_data_relationship()

                          ), # EO data relationships sidebar panel
                          
                          # data relationships main panel aka visual
                          mainPanel(
                            
                            plotOutput(outputId = "data_relationships_plot") %>% 
                              withSpinner(color="#0dc5c1")
                        
                        ) # EO data relationships main panel
                        ) # EO data relationships sidebar layout
                        ), #### EO data relationships ----

               ### SO visitorsheds ----
               tabPanel(title = "Visitorsheds",
                        fluid = TRUE,
                        
                        titlePanel("Visualize a visitorshed"),
                        # visitorsheds sidebar layout
                        sidebarLayout(
                          # visitorsheds sidebar panel
                          sidebarPanel(
                          
                            # agency input
                            select_agency(locationId = "visitorsheds",
                                          isMultiple = FALSE),
                            # admin input
                            select_admin_unit(locationId = "visitorsheds",
                                              isMultiple = FALSE),
                            # site input 
                            select_site(locationId = "visitorsheds",
                                        isMultiple = FALSE),
                            
                          ), # EO visitorsheds sidebar panel
                          
                          # visitorsheds main panel aka visual
                          mainPanel(
                  
                            fluidRow(
                              
                              box(
                                tmapOutput(outputId = "usVisitorshed_plot") %>% 
                                  withSpinner(color="#0dc5c1"),
                                width = 6,
                                title = "US Visitorshed"
                              ), # EO box US map
                              
                              box(
                                tmapOutput(outputId = "caVisitorshed_plot") %>% 
                                  withSpinner(color="#0dc5c1"),
                                with = 6,
                                title = "California Visitorshed"
                              ) # EO box site map
                            ), # EO fluidRow maps
                            
                            fluidRow(
                              
                              box(
                                #DTOutput(outputId = "") %>% withSpinner(color="#0dc5c1"),
                                width = 12,
                                title = "Reservable Site Summary"
                              ) # EO box summary table
                            ) # EO fluidRow summary table

                            
                          ) # EO visitorsheds main panel
                        ) # EO visitorsheds sidebar layout
                        ) #### EO visitorsheds ----

               ), ## EO Analysis tab ----
    
    ## Data Download ----
    tabPanel("Data Download", icon = icon("download-alt", lib = "glyphicon"),
             
             titlePanel("Create a subsetted dataset to download"),
             # SO data download FR layout
             fluidRow(
               # SO box inputs
               box(width = 12, # Note(HD): not seeing even inputs???
                 splitLayout(
                   # select agency
                   select_agency(locationId = "data_download"),
                   # select admin unit
                   select_admin_unit(locationId = "data_download"),
                   # select reservable site
                   select_site(locationId = "data_download"),
                   tags$head(tags$style(HTML(
                     ".shiny-split-layout > div {
                     overflow: visible;
                     }"
                   ))) # EO tags$head making drop down visible 
                 ) # EO split layout
               ), # EO box layout
               # SO box data table
               box(width = 12,
                   title = "Preview of subsetted table to download" #,
                   #plotOutput(outputId = ) %>% withSpinner(color="#0dc5c1")
                   ) # EO box data table
             ) # EO data download FR layout
             ) ## EO Data Download ----
    
  ) # EO navbarPage
  
) # EO fluid page
