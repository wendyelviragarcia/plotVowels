# Define server logic to read selected file ----
####
# shyny app for plotting vowels
####
#setwd("/Users/weg/OneDrive - UNED/git-me/vowelFigures")
library(shiny)
library("phonR") 
library("scatterplot3d")
library("readxl")



server <- function(input, output) {
  #options(shiny.maxRequestSize=30*1024^2) 
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$display)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        #df <- read_textgrid(input$file1$datapath[1])
        nFiles<- length(input$file1$datapath)
        files<- input$file1$datapath
        #vowelData <- read.table("/Users/weg/OneDrive - UNED/git-me/vowelFigures/vocales.csv", sep=";", header = TRUE, stringsAsFactors =TRUE)
        #vowelData <- read_excel("/Users/weg/OneDrive - UNED/git-me/vowelFigures/vocales.xlsx")
        
        fileName<- input$file1$name[1]
        #vowelData <- read.table(files[1], sep=";", header = TRUE, stringsAsFactors =TRUE)
        vowelData <- read_excel(files[1])
        vowelData$vocal <- as.factor(vowelData$vocal)
        
        colors <- rainbow(nlevels(vowelData$vocal))[as.numeric(vowelData$vocal)]
        
        #plot formants, using painted big dots and colors given by the vector we created
        formantPlot <- scatterplot3d(vowelData$f1,vowelData$f3,vowelData$f2, pch=NA, color=colors, type="p",
                                     main="",xlab="F1",ylab="F3", zlab="F2")
        ## create labels
        #find label position
        attach(vowelData)
        formantPlot.coords <- formantPlot$xyz.convert(f1, f3, f2) # convert 3D coords to 2D projection
        
        #write labels$

        
       
       
        
        
        if (length(input$display) ==1){
          if (input$display == "twoD"){
            output$plot1 <- renderPlot({
              with(vowelData, plotVowels(f1, f2, vocal, plot.tokens = TRUE, pch.tokens = vocal, cex.tokens = 1.2, alpha.tokens = 0.4, plot.means = TRUE, pch.means = vocal,cex.means = 4, var.col.by = vocal, pretty = TRUE))
            }) 
            
          } else if (input$display == "threeD"){
              output$plot2 <- renderPlot({
                scatterplot3d(vowelData$f1,vowelData$f3,vowelData$f2, pch=NA, color=colors, type="p",
                              main="",xlab="F1",ylab="F3", zlab="F2")
                text(formantPlot.coords$x,formantPlot.coords$y, cex=1, pos=1, labels = vowelData$vocal, col= colors)
                           }) 
          } 
          } else if (length(input$display) ==2){
            output$plot1 <- renderPlot({
              with(vowelData, plotVowels(f1, f2, vocal, plot.tokens = TRUE, pch.tokens = vocal, cex.tokens = 1.2, alpha.tokens = 0.4, plot.means = TRUE, pch.means = vocal,cex.means = 4, var.col.by = vocal, pretty = TRUE))
            }) 
            
            output$plot2 <- renderPlot({
              scatterplot3d(vowelData$f1,vowelData$f3,vowelData$f2, pch=NA, color=colors, type="p",
                            main="",xlab="F1",ylab="F3", zlab="F2")
              text(formantPlot.coords$x,formantPlot.coords$y, cex=1, pos=1, labels = vowelData$vocal, col= colors)              }) 
          } 
        
     
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #if(input$disp == "head") {
    #  return(head(df))
    #}
    #else {
      return()
    #}
    
  })
  
}