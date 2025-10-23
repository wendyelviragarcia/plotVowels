library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(phonTools)
library(phonR)
library(RColorBrewer)
library(scatterplot3d)

server <- function(input, output) {
  
  # ---- GRÁFICO 2D (BARK / NORMALIZACIONES) ----
  output$bark_plot <- renderPlot({
    req(input$file1)
    req(input$normalization_method)
    
    # Leer archivo
    file <- input$file1$datapath[1]
    vowelData <- read_excel(file)
    
    # Convertir noms de columnes a minúscules
    names(vowelData) <- tolower(names(vowelData))
    
    # Detectar automàticament la columna de la vocal
    possible_vowel_names <- c("vowel", "vowels", "vocal", "vocals")
    vowel_col <- intersect(possible_vowel_names, names(vowelData))
    
    validate(need(length(vowel_col) == 1,
                  "No s'ha trobat cap columna de vocals (hauria de ser 'vowel', 'vowels', 'vocal' o 'vocals')."))
    
    # Renombrar la columna detectada a "vowel"
    names(vowelData)[names(vowelData) == vowel_col] <- "vowel"
    
    
    validate(need(all(c("f1", "f2", "vowel") %in% names(vowelData)), 
                  "Faltan columnas necesarias: f1, f2 o vowel"))
    
    vowelData$vowel <- as.factor(vowelData$vowel)
    
    if (input$normalization_method == "lobanov") {
      validate(need("speaker" %in% names(vowelData), 
                    "You need a col with the 'speaker' to use this normalization."))
      vowelData$speaker <- as.factor(vowelData$speaker)
    }
    
    # Conversión a Bark
    hz_to_bark <- function(f) {
      bark <- 26.81 * f / (1960 + f) - 0.53
      ifelse(bark < 2, bark + 0.15 * (2 - bark), bark)
    }
    
    # Definición de métodos
    norm_methods <- list(
      none = function(df) df,
      lobanov = function(df) df %>%
        group_by(speaker) %>%
        mutate(f1 = scale(f1), f2 = scale(f2)) %>%
        ungroup(),
      wattfabricius = function(df) {
        norm <- normVowels(df$f1, df$f2, df$speaker, method = "wattfabricius")
        df$f1 <- norm$f1
        df$f2 <- norm$f2
        df
      },
      bark = function(df) df %>%
        mutate(f1 = hz_to_bark(f1), f2 = hz_to_bark(f2))
    )
    
    norm_method <- input$normalization_method
    validate(need(norm_method %in% names(norm_methods), "Error method"))
    
    df_norm <- norm_methods[[norm_method]](vowelData)
    
    # Calcular centroides
    centroids <- df_norm %>%
      group_by(vowel) %>%
      summarise(f1_mean = mean(f1, na.rm = TRUE),
                f2_mean = mean(f2, na.rm = TRUE))
    
    # Graficar 2D
    ggplot(df_norm, aes(x = f2, y = f1, color = vowel)) +
      geom_point(size = 2, alpha = 0.4) +
      stat_ellipse(aes(group = vowel), level = 0.68, type = "norm", linewidth = 1) +
      geom_text(data = centroids,
                aes(x = f2_mean, y = f1_mean, label = vowel),
                fontface = "bold", size = 11, inherit.aes = FALSE, color = "white") +
      geom_text(data = centroids,
                aes(x = f2_mean, y = f1_mean, label = vowel, color = vowel),
                size = 10, inherit.aes = FALSE) +
      scale_x_reverse() +
      scale_y_reverse() +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal() +
      labs(title = paste("Method:", norm_method),
           x = "F2", y = "F1", color = "Vowel")
  })
  
  
  # ---- GRÁFICO 3D (FORMANTES) ----
  output$plot3d <- renderPlot({
    req(input$file1)
    
    file <- input$file1$datapath[1]
    vowelData <- read_excel(file)
    
    # Convertir noms de columnes a minúscules
    names(vowelData) <- tolower(names(vowelData))
    
    # Detectar automàticament la columna de la vocal
    possible_vowel_names <- c("vowel", "vowels", "vocal", "vocals", "label")
    vowel_col <- intersect(possible_vowel_names, names(vowelData))
    
    validate(need(length(vowel_col) == 1,
                  "The label column has not been found (should be 'label', 'vowel', 'vowels', 'vocal' or 'vocals')."))
    
    # Renombrar la columna detectada a "vowel"
    names(vowelData)[names(vowelData) == vowel_col] <- "vowel"
    
    
    validate(need(all(c("f1", "f2", "f3", "vowel") %in% names(vowelData)),
                  "Need cols: f1, f2, f3 and vowel"))
    
    vowelData$vowel <- as.factor(vowelData$vowel)
    colors <- brewer.pal(length(levels(vowelData$vowel)), "Dark2")[vowelData$vowel]
    
    # Invertir ejes F1 y F2 (convención acústica)
    f2_rev <- max(vowelData$f2, na.rm = TRUE) - vowelData$f2
    f1_rev <- max(vowelData$f1, na.rm = TRUE) - vowelData$f1
    f3_rev <- vowelData$f3
    
    # Crear gráfico base sin puntos
    s3d <- scatterplot3d(
      x = f2_rev, y = f3_rev, z = f1_rev,
      color = colors, pch = NA,  # no dibujar puntos
      main = "3D (F1, F2, F3)",
      xlab = "F2 (reversed)", ylab = "F3", zlab = "F1 (reversed)",
      tick.marks = FALSE,       
      box = TRUE,               
      angle = 55                
      )
    
    # Añadir etiquetas de las vocales directamente en 3D
    text(
      s3d$xyz.convert(f2_rev, f3_rev, f1_rev),
      labels = vowelData$vowel,
      cex = 2, font = 2, col = colors
    )
  })
  
  
  

  
  
}
