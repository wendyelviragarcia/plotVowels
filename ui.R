ui <- fluidPage(
  titlePanel("Plot vowel formants"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload your file (Excel with columns f1, f2, [f3], vowel, speaker)"),
      selectInput(
        "normalization_method", 
        "Normalization method:", 
        choices = c("none", "bark", "lobanov", "wattfabricius"),
        selected = "none"
      )
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Graphs", # Output: Data file ----
                        h3("Plot (F1 vs F2)"),
                        p("Vowel plot using the normalization method selected."),
                        plotOutput("bark_plot", height = "500px"),
                        hr(),
                                         
                        h3("3D Vowel chart (F1, F2, F3)"),
                        p("Only if you have a f3 column"),
                        plotOutput("plot3d", height = "500px")
                        
                  
                        
                        
                      ),
                 
      
                  tabPanel("Info", 
                           p("Plots vowels in 2D (using phonR package) and 3D using scatterplot"),
                           tags$hr(),
                           
                           h4("Input"),
                           tags$b("An Excel file with formant values and the following columns. Column names must match and be in lowercase"),
                           tags$ol(
                             tags$li("vowel"),
                             tags$li("f1"),
                             tags$li("f2"),
                             tags$li("f3")),
                           tags$b("Output"),
                           tags$ol(
                             tags$li("Figures that can be saved with right click or dragging to Desktop"),
                             tags$ol(
                               tags$li("2D Figure"),
                               tags$li("3D Figure")))
                  ),
    
                  # tab citar ----
                  tabPanel("Citing information",
                           tags$b("Elvira-García, Wendy. 2025. A RShiny interface for plotting vowel formants. [RShiny App]")
                  )
    
      )
    
  )
)
)
