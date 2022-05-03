# # UI ----
ui <- fluidPage(
  # use shiny dashboard elements in shiny
  useShinydashboard(),
  useShinydashboardPlus(),
  useShinyjs(),
  # call css styles sheet
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css")
  ), # EO tag$head
  
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
                              title = "Select a variable and how many sites to compare",
                              splitLayout(
                              # choose a var
                              select_data_summary_vars(),
                              #### SO select number of visuals ----
                              selectizeInput(inputId = "num_viz",
                                             label = "Select number of visuals",
                                             choices = c(1, 2),
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
                              width = 12,
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
                        ), #### EO data summary
               
               ### SO data relationships ----
               tabPanel(title = "Data Relationships",
                        fluid = TRUE,
                        
                        titlePanel("Visualize a relationship"),
                        # SO data relationships FR layout
                        fluidRow(
                          # SO pick relationship input box
                          box(width = 4,
                              title = "1. Pick a relationship to visualize",
                              select_data_relationship()
                          ), # EO pick relationship input box
                          
                          # SO subset relationship data box
                          box(width = 8,
                              title = "2. Subset the data",
                              splitLayout(
                                # agency input
                                select_agency(locationId = "relationships"),
                                # admin input
                                select_admin_unit(locationId = "relationships"),
                                # site input
                                select_site(locationId = "relationships")
                              ) # EO split layout relationship subset
                              ), # EO relationship subset box
                          
                          # relationships plot output box
                          box(width = 12,
                              plotlyOutput(outputId = "data_relationships_plot") %>%
                                withSpinner(color = "#0dc5c1")
                              ) # EO relationships plot output box
                        ) # EO FR data relationships
                        )#, #### EO data relationships ----

               ### SO visitorsheds ----
               # tabPanel(title = "Visitorsheds",
               #          fluid = TRUE,
               #          
               #          titlePanel("Visualize a visitorshed"),
               #          # SO FR visitorsheds   
               #          fluidRow(
               #            # subset visitorshed data box
               #            box(width = 12,
               #                title = "Subset the data",
               #                splitLayout(
               #                  # agency input
               #                  select_agency(locationId = "visitorsheds"),
               #                  # admin input
               #                  select_admin_unit(locationId = "visitorsheds"),
               #                  # site input
               #                  select_site(locationId = "visitorsheds")
               #                ) # EO visitorshed split layout
               #                ), # EO subset visitorshed data box
               #            
               #            # SO US visitorshed map box
               #            box(width = 6,
               #                title = "US Visitorshed",
               #                tmapOutput(outputId = "usVisitorshed_plot") %>% 
               #                  withSpinner(color="#0dc5c1")
               #            ), # EO US visitorshed map box
               #            
               #            # SO CA visitorshed map box
               #            box(width = 6,
               #                title = "California Visitorshed",
               #                tmapOutput(outputId = "caVisitorshed_plot") %>% 
               #                  withSpinner(color="#0dc5c1"),
               #            ), # EO US visitorshed map box
               #            
               #            # SO visitorshed info box
               #            box(width = 12,
               #                title = "Reservable Site Summary"
               #            ) # EO visitorshed info box
               #          ) # EO FR visitorsheds
               #          ) #### EO visitorsheds ----

               ), ## EO Analysis tab ----
    
    ## Data Download ----
    tabPanel("Data Download", icon = icon("download-alt", lib = "glyphicon"),
             
             titlePanel("Create a subsetted dataset to download"),
             # SO data download FR layout
             fluidRow(
               # SO box inputs
               box(width = 12,
                   splitLayout(cellWidths = c("33.3%", "33.3%", "33.3%"),
                               # select agency
                               select_agency(locationId = "data_download",
                                             isMultiple = TRUE),
                               # select admin unit
                               select_admin_unit(locationId = "data_download",
                                                 isMultiple = TRUE),
                               # select reservable site
                               select_site(locationId = "data_download",
                                           isMultiple = TRUE) 
                 ) # EO split layout
               ), # EO box layout
               # SO box data table
               box(width = 12,
                   title = "Preview of subsetted table to download",
                   DT::DTOutput(outputId = "data_download_table")
                   ) # EO box data table
             ) # EO data download FR layout
             ) ## EO Data Download ----
    
  ) # EO navbarPage
  
) # EO fluid page
