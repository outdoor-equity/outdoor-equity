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
    tabPanel("About", icon = icon("info-circle")
               # 
               # tabPanel(title = "Background",
               #          # Note(HD): need . in front of file path for relative path
               #          includeMarkdown("./text/background-about.md")
               # ), # EO Background tabPanel
               # 
               # 
               # tabPanel(title = "User Guide",
               #          includeMarkdown("./text/userGuide-about.md")
               # ), # EO User Guide tabPanel
               # 
               # 
               # tabPanel(title = "Metadata",
               #          # Note(HD): need to change this to a rmd file to include DT table
               #          includeMarkdown("./text/metadata-about.md")
               #          
               # )
               ), ## EO About tab ----
    
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
                                             label = "Select 1 to see single site, or 2 if you want to compare two different sites",
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
                          
                          # dynamic relationships plot output box
                          box(id = "relationships_outputs",
                              width = 8,
                              plotlyOutput(outputId = "data_relationships_plot") %>%
                                withSpinner(color = "#0dc5c1"),
                              uiOutput(outputId = "relationships_tab_layout")
                              ), # EO relationships plot output box
                          
                          # context relationships plot 
                          box(id = "high_relationships_output",
                              width = 4,
                              plotlyOutput(outputId = "high_relationships_plot") %>% 
                                withSpinner(color = "#0dc5c1")
                              
                              ) # EO context relationship plot box
                        ) # EO FR data relationships
                        ), #### EO data relationships

               ### SO visitorsheds ----
               tabPanel(title = "Visitorsheds",
                        fluid = TRUE,
                        
                        titlePanel("Visualize a visitorshed"),
                        # SO FR visitorsheds
                        fluidRow(
                          # SO subset visitorshed data box
                          box(width = 12,
                              title = "Subset the data",
                              splitLayout(
                                # agency input
                                select_agency(locationId = "visitorsheds"),
                                # admin input
                                select_admin_unit(locationId = "visitorsheds"),
                                # site input
                                select_site(locationId = "visitorsheds")
                              ) # EO splitLayout visitorshed
                          ), # EO subset visitorshed data box
                          
                          # SO US visitorshed map box
                          box(width = 6,
                              title = "US Visitorshed",
                              tmapOutput(outputId = "usVisitorshed_plot") %>% 
                                withSpinner(color="#0dc5c1")
                          ), # EO US visitorshed map box
                          
                          # SO CA visitorshed map box
                          box(width = 6,
                              title = "CA Visitorshed",
                              tmapOutput(outputId = "caVisitorshed_plot") %>% 
                                withSpinner(color="#0dc5c1")
                          ), # EO CA visitorshed map box
                          
                          # SO visitorshed info box
                          box(width = 12,
                              title = "Summary Stats of Site"
                          ) # EO visitorshed info box
                        ) # EO FR visitorsheds
                        ) # EO tabpanel visitorsheds

               ), ## EO Analysis tab ----
    
    tabPanel("Metadata"), # Note(HD): ADD ICON
    
    ## Data Download ----
    tabPanel("Data Download", icon = icon("download-alt", lib = "glyphicon"),
             
             #titlePanel("Create a subsetted dataset to download"),
             # SO data download FR layout
             fluidRow(
               # SO box inputs
               box(width = 12,
                   title = NULL,
                   splitLayout(cellWidths = c("33.3%", "33.3%", "33.3%"),
                               # select agency
                               select_agency(locationId = "data_download",
                                             isMultiple = TRUE),
                               # select admin unit
                               select_admin_unit(locationId = "data_download",
                                                 isMultiple = TRUE),
                               # select reservable site
                               select_site(locationId = "data_download",
                                           isMultiple = TRUE),
                 ) # EO split layout
               ), # EO box layout
               # SO columns subset box
               box(id = "cols_select",
                   width = 12,
                   selectizeInput(inputId = "cols_data_download",
                                  label = "Select columns",
                                  choices = cols_data_joined_2018,
                                  selected = cols_data_joined_2018,
                                  multiple = TRUE,
                                  size = 8,
                                  options = list(plugins = list("remove_button"))),
                   tags$head(tags$style('#cols_select .box-header{ display: none}'))
                   ), # EO columns subset box
               # SO box data table
               box(id = "download_box",
                   width = 12,
                   downloadButton(outputId = "data_download",
                                  label = "Download selection as CSV"),
                   DT::DTOutput(outputId = "data_download_table")
                   ), # EO box data table
               tags$head(tags$style('#download_box .box-header{ display: none}')) # remove title from box
             ) # EO data download FR layout
             ) ## EO Data Download ----
    
  ) # EO navbarPage
  
) # EO fluid page
