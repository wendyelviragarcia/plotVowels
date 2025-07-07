

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Plot vowel formants"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,
      
      # Input: Select a file ----
      fileInput("file1", "Upload your Excel database (vocal, f1, f2, f3)",
                multiple = FALSE),
    
      # Horizontal line ----
      tags$hr(),
      
      
      radioButtons("normalization_method", "Choose a normalization method:",
                   choices = list("Not normalized" = "waw",
                                  "Normalized Bark" = "bark",
                                  "Normalized Lobanov" = "lobanov",
                                  "Normalized Watt & Fabricius" = "watfabricius"),
                   selected = "lobanov"),
      tags$hr(),
      
      
      # Input: Select number of rows to display ----
      checkboxGroupInput("display", "Figures",
                   choices = c("2D figures" = "twoD", "3D figures" = "threeD"),
                   selected = c("twoD","threeD")),
    ),
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
       
      tabsetPanel(type = "tabs",
                  tabPanel("Graphs", # Output: Data file ----
                           h4("Graphs"),
                           
                           textOutput("selected_method"),
                
                           tableOutput("contents"),
                           plotOutput("plot1"),
                           plotOutput("plot2"),
                           plotOutput("bark_plot")
                           
                  ),
                  # tab con la rubrica ----
                  
                  tabPanel("Info", 
                           p("Plots vowels in 2D (using phonR package) and 3D using scatterplot"),
                           tags$hr(),
                    
                           h4("Input"),
                           tags$b("A csv with formant values and the following columns. Column names must match and be in lowercase"),
                           tags$ol(
                             tags$li("vocal"),
                             tags$li("f1"),
                           tags$li("f2"),
                            tags$li("f3")),
                           tags$b("Output"),
                           tags$ol(
                             tags$li("Figures that can be saved with right click or dragging to Dsektop."),
                             tags$ol(
                               tags$li("2D Figure"),
                               tags$li("3D Figure")))
                  ),
                  # tab citar ----
                  tabPanel("Citing information",
                           tags$b("Elvira-García, Wendy. 2021. A RShiny interface for plotting vowel formants. [RShiny App]")
                  )
                
      )
      )
  )
)
