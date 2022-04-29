# # UI ----
ui <- fluidPage(
  # use shiny dashboard elements in shiny
  useShinydashboard(),
  useShinyjs(),
  
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
                        fluidRow(
                          # SO pick a var input box
                          box(width = 12,
                              title = "1. Select a variable and how many sites to compare",
                              splitLayout(
                              # choose a var
                              select_data_summary_vars(),
                              #### SO select number of visuals ----
                              selectizeInput(inputId = "num_viz",
                                             label = "Select number of visuals",
                                             choices = c(1, 2, 3, 4),
                                             multiple = FALSE,
                                             options = list(
                                               placeholder = "Select number of visuals",
                                               # Note(HD) when created set a value for the input to an empty string
                                               onInitialize = I('function() { this.setValue("1"); }')
                                             )) # EO num viz input
                              ) # EO split layout var input & num visual
                          ), # EO pick a var & num visual input box
                          
                          # SO data summary plot 1 output box
                          box(id = "num_viz_1",
                              width = 6,
                              splitLayout(
                                # agency input
                                select_agency(locationId = "summary_1"),
                                # admin input
                                select_admin_unit(locationId = "summary_1"),
                                # site input
                                select_site(locationId = "summary_1")
                              ),
                              plotlyOutput(outputId = "data_summary_plot_1") %>%
                                withSpinner(color = "#0dc5c1")
                          ), # EO data summary plot 1 output box
                          
                          # SO data summary plot 2 output box
                          box(id = "num_viz_2",
                              width = 6,
                              splitLayout(
                                # agency input
                                select_agency(locationId = "summary_2"),
                                # admin input
                                select_admin_unit(locationId = "summary_2"),
                                # site input
                                select_site(locationId = "summary_2")
                              ),
                              plotlyOutput(outputId = "data_summary_plot_2") %>%
                                withSpinner(color = "#0dc5c1")
                              ) # EO data summary plot 2 output box
                        ) # EO data summary FR layout
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
                            
                            # data relationship input
                            select_data_relationship(),
                            # agency input
                            select_agency(locationId = "relationships"),
                            # admin input
                            select_admin_unit(locationId = "relationships"),
                            # site input
                            select_site(locationId = "relationships"),

                          ), # EO data relationships sidebar panel
                          
                          # data relationships main panel aka visual
                          mainPanel(
                            
                            plotlyOutput(outputId = "data_relationships_plot") %>% 
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
                 splitLayout(cellWidths = c("33.3%", "33.3%", "33.3%"),
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
