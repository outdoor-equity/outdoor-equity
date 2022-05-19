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
  theme = bs_theme(bootswatch = "sandstone",
                   bg = "#ffff", # blank
                   fg = "#1F303E", # ridb dark blue from bottom of page
                   primary = "#005ea2", # ridb dark blue font
                   secondary = "#659dc7", # ridb light blue font
                   success = "#397B1E", # ridb light green
                   info = "#97D4EA", # ridb light blue from bar
                   warning = "#C3512C",# ridb yellow
                   danger = "#FACE00", # ridb orange red
                   base_font = font_google("Open Sans"),
                   heading_font = font_google("Source Sans Pro")), # Note(HD): don't know if this really made a difference, tried to make the headers bold
  
  # app title ----
  tags$head(HTML("<title>Visualize RIDB Data</title>")), # Note(HD): this needs to be before titlePanel so that the title comes up in the tab online
  titlePanel(title = span(img(src = "logos/bren_meds_hex.png", height = 50),
                          img(src = "logos/RecLogo.png", height = 70))),
  
  # app subtitle ----
  #p(strong("Visualize and explore a joined dataset of recreation and US Census data")),
  
  # navbarPage structure ----
  navbarPage(
    # TO nav bar and TO tab in web browser 
    title = "Visualize Recreation Data",
    # SO footer
    footer = span(#h6("test"),
                  img(src = "logos/bren_meds_hex.png", height = 30),
                  img(src = "logos/uomontana-horizontal-maroon.png", height = 25),
                  img(src = "logos/RecLogo.png", height = 40),
                  img(src = "logos/NCEAS-square-logo.png", height = 30)
                  ), # EO footer
    
    ## About tab ----
    tabPanel("About",
             tags$img(src = "images/white_mountains_national_forest_large.jpg", 
                      height = 175, 
                      width = 1190),
             fluidRow(
               # SO About tabs
               box(width = 12,
                   status = "primary",
                   title = "About this App",
                   #includeMarkdown("text/about_intro.md")
                   tabBox(width = 12,
                          tabPanel(title = "What is the Outdoor Equity App?",
                                   includeMarkdown("text/about_app.md")),
                          tabPanel(title = "What is outdoor recreation and why does it matter?",
                                   includeMarkdown("text/about_background.md"))
                          ) # EO tabBox
                   ), # EO About tabs intro
               # SO About examples
               box(width = 12,
                   status = "primary",
                   title = "Example Questions to Explore",
                   #includeMarkdown("text/about_examples.md")
                   tabBox(width = 12,
                          tabPanel(title = "How far in advance to reserve site?",
                                   includeMarkdown("text/about_example1.md"),
                                   plotlyOutput("about_example_1_plot") %>%
                                     withSpinner(color = spinner_color)
                          ), # EO tabPanel example 1
                          tabPanel(title = "How far are people of different races traveling?",
                                   includeMarkdown("text/about_example2.md"),
                                   plotlyOutput(outputId = "about_example_2_plot") %>%
                                     withSpinner(color = spinner_color)
                          ), # EO tabPanel example 2
                          tabPanel(title = "How far are people with different incomes traveling?",
                                   includeMarkdown("text/about_example3.md"),
                                   plotlyOutput(outputId = "about_example_3_plot") %>%
                                     withSpinner(color = spinner_color)
                          ), # EO tabPanel example 3
                          tabPanel(title = "Visitorshed Map",
                                   tmapOutput(outputId = "about_example_4_plot") %>%
                                     withSpinner(color = spinner_color)
                          ) # EO tabPanel example 4
                   ) # EO tabBox
                   ) # EO About examples intro
             ) # EO About FR layout
    ), ## EO About tab ----
    
    ## Analysis tab ---- 
    navbarMenu("Analysis", icon = icon("chart-bar"),
               
               ### SO data summary ----
               tabPanel(title = "Data Summary",
                        fluid = TRUE, # (HD) not sure what this argument does
                        tags$img(src = "images/king_range_nat_conserv.jpg", 
                                 height = 175,
                                 width = 1190),
                        # SO data summary FR layout
                        fluidRow(
                          # SO explanatory data summary text
                          box(width = 8,
                              status = "primary",
                              title = "Data Summary Plots",
                              includeMarkdown("text/data_summary_explanatory.md")
                          ), # EO explanatory data summary text
                          
                          # SO pick a var input box
                          box(width = 4,
                              title = "Select a variable and how many sites to compare",
                              status = "primary",
                              # choose a var
                              select_data_summary_vars(),
                              #### SO select number of visuals
                              radioButtons(inputId = "num_viz",
                                           label = "2. Select 1 to see a single site, or 2 to compare two different sites",
                                           choices = c(1, 2),
                                           selected = 2,
                                           inline = TRUE # makes choices horizontal
                                           ) # EO radioButton
                          ), # EO pick a var & num visual input box
                          
                          # SO explanatory text for language and median income plots
                          box(id = "text_lang_medInc",
                              width = 12,
                              textOutput(outputId = "data_summary_captions")
                              ), # EO explanatory text for language and median income plots
                          tags$head(tags$style('#text_lang_medInc .box-header{ display: none}')), # remove title from box
                          
                          # SO data summary plot 1 output box
                          box(id = "num_viz_1",
                              width = 12,
                              status = "success",
                              splitLayout(
                                # agency input
                                select_agency(locationId = "summary_1"),
                                # admin input
                                select_admin_unit(locationId = "summary_1"),
                                # site input
                                select_site(locationId = "summary_1")
                              ),
                              plotlyOutput(outputId = "data_summary_plot_1") %>%
                                withSpinner(color = spinner_color)
                          ), # EO data summary plot 1 output box
                          tags$head(tags$style('#num_viz_1 .box-header{ display: none}')), # remove title from box
                          
                          # SO data summary plot 2 output box
                          box(id = "num_viz_2",
                              width = 6,
                              status = "success",
                              splitLayout(
                                # agency input
                                select_agency(locationId = "summary_2",
                                              defaultValue = "US Forest Service"),
                                # admin input
                                select_admin_unit(locationId = "summary_2",
                                                  defaultValue = "Sierra National Forest"),
                                # site input
                                select_site(locationId = "summary_2",
                                            defaultValue = "Dinkey Creek")
                              ), # EO split layout data summary plot 2 output box
                              plotlyOutput(outputId = "data_summary_plot_2") %>%
                                withSpinner(color = spinner_color)
                              ), # EO data summary plot 2 output box
                          tags$head(tags$style('#num_viz_2 .box-header{ display: none}')), # remove title from box
                        ) # EO data summary FR layout
                        ), #### EO data summary
               
               ### SO data relationships ----
               tabPanel(title = "Data Relationships",
                        fluid = TRUE,
                        tags$img(src = "images/milky_way.jpg", # need to use a better photo
                                 height = 175,
                                 width = 1190),
                        # SO data relationships FR layout
                        fluidRow(
                          # SO explanation data relationship text
                          box(width = 12,
                              status = "primary",
                              title = "Data Relationship Plots",
                              includeMarkdown("text/data_relationship_explanatory.md")
                              ), # EO explanatory data relationship text
                          
                          # SO pick relationship input box
                          box(width = 4,
                              status = "success",
                              title = "1. Pick a relationship to visualize",
                              select_data_relationship()
                          ), # EO pick relationship input box
                          
                          # SO subset relationship data box
                          box(width = 8,
                              status = "success",
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
                              status = "success",
                              plotlyOutput(outputId = "data_relationships_plot") %>%
                                withSpinner(color = spinner_color),
                              uiOutput(outputId = "relationships_tab_layout")
                              ), # EO relationships plot output box
                          tags$head(tags$style('#relationships_outputs .box-header{ display: none}')), # remove title from box
                          
                          # context relationships plot 
                          box(id = "high_relationships_output",
                              width = 4,
                              status = "success",
                              plotlyOutput(outputId = "high_relationships_plot") %>% 
                                withSpinner(color = spinner_color)
                              ), # EO context relationship plot box
                          tags$head(tags$style('#high_relationships_output .box-header{ display: none}')) # remove title from box
                        ) # EO FR data relationships
                        ), #### EO data relationships

                ### SO visitorsheds ----
               tabPanel(title = "Visitorshed Maps",
                        fluid = TRUE,
                        tags$img(src = "images/autum_trees.jpg", # need to use a better photo
                                 height = 175,
                                 width = 1190),
                        # SO FR visitorsheds
                        fluidRow(
                          # SO explanatory visitorshed text
                          box(
                            width = 8,
                            status = "primary",
                            title = "Visitorshed Maps",
                            includeMarkdown("text/visitorshed_explanatory.md")
                          ), # EO explanatory visitorshed text
                          
                          # SO subset visitorshed data box
                          box(width = 4,
                              status = "primary",
                              title = "Subset the data",
                              # agency input
                              select_agency(locationId = "visitorsheds"),
                              # admin input
                              select_admin_unit(locationId = "visitorsheds"),
                              # site input
                              select_site(locationId = "visitorsheds")
                          ), # EO subset visitorshed data box
                          
                          # SO US visitorshed map box
                          box(width = 6,
                              status = "success",
                              title = "US Visitorshed Map",
                              tmapOutput(outputId = "usVisitorshed_plot") %>% 
                                withSpinner(color = spinner_color)
                          ), # EO US visitorshed map box
                          
                          # SO CA visitorshed map box
                          box(width = 6,
                              status = "success",
                              title = "CA Visitorshed Map",
                              tmapOutput(outputId = "caVisitorshed_plot") %>% 
                                withSpinner(color = spinner_color)
                          ), # EO CA visitorshed map box
                          
                          # SO visitorshed info box
                          box(width = 12,
                              title = "Summary Stats of Site"
                          ) # EO visitorshed info box
                        ) # EO FR visitorsheds
                        ) # EO tabpanel visitorsheds

               ), ## EO Analysis tab ----
    
    tabPanel(title = "Metadata",
             fluid = TRUE,
             tags$img(src = "images/desolation_canyon.jpg", 
                      height = 175,
                      width = 1190),
             fluidRow(
               # SO explanatory metadata text
               box(width = 8,
                   status = "primary",
                   title = "Metadata",
                   includeMarkdown("text/metadata_explanatory.md")
                   ) # EO explanatory text
               ) # EO FR metadata
             ), # Note(HD): ADD ICON
    
    ## Data Download ----
    tabPanel(title = "Data Download", 
             icon = icon("download-alt", lib = "glyphicon"),
             fluid = TRUE,
             tags$img(src = "images/snowy_mountains.jpg", 
                      height = 175,
                      width = 1190),
             #titlePanel("Create a subsetted dataset to download"),
             # SO data download FR layout
             fluidRow(
               # SO box inputs
               box(id = "download_box",
                   width = 12,
                   status = "primary",
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
               tags$head(tags$style('#download_box .box-header{ display: none}')), # remove title from box
               # SO columns subset box
               box(id = "cols_select",
                   width = 12,
                   status = "success",
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
