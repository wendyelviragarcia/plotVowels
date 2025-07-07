server <- function(input, output) {
  # options(shiny.maxRequestSize=30*1024^2)
  
  output$contents <- renderTable({
    req(input$file1)
    req(input$display)
    
    output$selected_method <- renderText({
      paste("You selected:", input$normalization_method)
    })
    
    tryCatch({
      nFiles <- length(input$file1$datapath)
      files <- input$file1$datapath
      fileName <- input$file1$name[1]
      
      vowelData <- read_excel(files[1])
      vowelData$vocal <- as.factor(vowelData$vocal)
      vowelData$vowel <- as.factor(vowelData[["vocal"]])
      #esto da error vowelData$vowel <- vowelData[, 1]
      
      if (input$normalization_method == "bark") {
        hz_to_bark <- function(f) {
          bark <- 26.81 * f / (1960 + f) - 0.53
          ifelse(bark < 2, bark + 0.15 * (2 - bark), bark)
        }
        
        vowelData <- vowelData %>%
          mutate(
            f1_bark = hz_to_bark(f1),
            f2_bark = hz_to_bark(f2),
            f3_bark = hz_to_bark(f3)
          )
        
        rm(hz_to_bark)
        
        output$bark_plot <- renderPlot({
          centroids <- vowelData %>%
            group_by(vowel) %>%
            summarise(
              f1_mean = mean(f1_bark, na.rm = TRUE),
              f2_mean = mean(f2_bark, na.rm = TRUE),
              .groups = "drop"
            )
          
          ggplot(vowelData, aes(x = f2_bark, y = f1_bark, color = vowel)) +
            geom_point(size = 2, alpha = 0.6) +
            stat_ellipse(aes(group = vowel), level = 0.68, type = "norm", linewidth = 1) +
            geom_text(
              data = vowelData %>%
                group_by(vowel) %>%
                summarise(f1_mean = mean(f1_bark), f2_mean = mean(f2_bark)),
              aes(x = f2_mean, y = f1_mean, label = vowel, color = vowel),
              fontface = "bold", size = 6, inherit.aes = FALSE
            ) +
            scale_x_reverse() +
            scale_y_reverse() +
            scale_color_brewer(palette = "Dark2") +
            theme_minimal() +
            labs(title = "Vowel Plot", x = "F2 (Bark)", y = "F1 (Bark)", color = "Vowel")
          
        })
        
        # end of if
      }
      
      colors <- rainbow(nlevels(vowelData$vocal))[as.numeric(vowelData$vocal)]
     # attach(vowelData)
      
      f2_rev <- max(vowelData$f2) - vowelData$f2
      f1_rev <- max(vowelData$f1) - vowelData$f1
      
      formantPlot <- scatterplot3d(
        x = f2_rev, y = vowelData$f3, z = f1_rev,
        pch = NA, color = colors, type = "p",
        main = "", xlab = "F2", ylab = "F3", zlab = "F1",
        x.ticklabs = seq(pretty(max(f2_rev))[1], pretty(min(f2_rev))[2], -200),
        z.ticklabs = seq(pretty(max(f1_rev))[1], pretty(min(f1_rev))[2], -200)
      )
      
      formantPlot.coords <- formantPlot$xyz.convert(f2_rev, f3, f1_rev)
      
      text(formantPlot.coords$x, formantPlot.coords$y, cex = 1, pos = 1,
           labels = vowelData$vocal, col = colors, srt = 45)
      
      if (length(input$display) == 1) {
        if (input$display == "twoD") {
          output$plot1 <- renderPlot({
            with(vowelData, plotVowels(
              f1, f2, vocal,
              plot.tokens = TRUE, pch.tokens = vocal, cex.tokens = 1.2, alpha.tokens = 0.4,
              plot.means = TRUE, pch.means = vocal, cex.means = 4,
              var.col.by = vocal, pretty = TRUE
            ))
          })
        } else if (input$display == "threeD") {
          output$plot2 <- renderPlot({
            scatterplot3d(
              x = f2_rev, y = vowelData$f3, z = f1_rev,
              pch = NA, color = colors, type = "p",
              main = "", xlab = "F2", ylab = "F3", zlab = "F1",
              x.ticklabs = seq(pretty(max(f2_rev))[1], pretty(min(f2_rev))[1], -200),
              z.ticklabs = seq(pretty(max(f1_rev))[1], pretty(min(f1_rev))[1], -100)
            )
            text(formantPlot.coords$x, formantPlot.coords$y, cex = 1, pos = 1,
                 labels = vowelData$vocal, col = colors)
          })
        }
      } else if (length(input$display) == 2) {
        output$plot1 <- renderPlot({
          with(vowelData, plotVowels(
            f1, f2, vocal,
            plot.tokens = TRUE, pch.tokens = vocal, cex.tokens = 1.2, alpha.tokens = 0.4,
            plot.means = TRUE, pch.means = vocal, cex.means = 4,
            var.col.by = vocal, pretty = TRUE
          ))
        })
        
        output$plot2 <- renderPlot({
          scatterplot3d(
            f2_rev, vowelData$f3, f1_rev,
            pch = NA, color = colors, type = "p",
            main = "", xlab = "F2", ylab = "F3", zlab = "F1",
            x.ticklabs = seq(pretty(max(f2_rev))[1], pretty(min(f2_rev))[1], -200),
            z.ticklabs = seq(pretty(max(f1_rev))[1], pretty(min(f1_rev))[1], -200)
          )
          text(formantPlot.coords$x, formantPlot.coords$y, cex = 1, pos = 1,
               labels = vowelData$vocal, col = colors)
        })
      }
      
    }, error = function(e) {
      stop(safeError(e))
    })
    
    return(NULL)
  })
}
