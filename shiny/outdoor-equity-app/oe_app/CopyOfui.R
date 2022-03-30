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
                        fluid = TRUE,
                        
                        # sidebar layout ----
                        sidebarLayout(
                          # agency analysis side bar panel ----
                          sidebarPanel(
                            
                            titlePanel("1. Select an Agency")
                            
                            
                            
                          ) # end of sidebar panel ----
                        ) # end of sidebar layout ----
                        
                        ), # end of Agency Analysis ----
               
               tabPanel(title = "Agency/ Site Comparison Analysis"),
               
               # Reservable Site Analysis ----
               tabPanel(title = "Reservable Site Analysis"), # end of Reservable Site Analysis ----
               
               ) # end of Analysis tab ----
    
  )
  
  
)