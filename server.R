# ----- Servidor de la Aplicación ----
server <- function(input, output, session) {
  
  # ----- Variables Reactivas -----
  # Código para el servidor
  physeq <- reactive({
    if (input$data_source == "Archivo") {
      if (!is.null(input$file1)) {
        return(readRDS(input$file1$datapath))
      }
    } else {
      if (input$dataset == "Global Patterns") {
        data("GlobalPatterns")
        return(GlobalPatterns)
      } else if (input$dataset == "Enterotype") {
        data("enterotype")
        return(enterotype)
      } else if (input$dataset == "Soil") {
        data("soilrep")
        return(soilrep)
      }
    }
  })
  
  # Verificar que el objeto phyloseq tiene árbol filogenético
  has_tree <- tryCatch({
    phy_tree(physeq())
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # Descargar el objeto phyloseq
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("phyloseq-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(physeq(), file)
    }
  )
 
  observeEvent(input$update, {
    # Mostrar la estructura del objeto phyloseq
    output$physeq_summary <- renderPrint({
      print(physeq())
    })
    
    # Generar el árbol filogenético
    if (has_tree) { 
      
      output$phylo_tree <- renderPlot({
        
        plot_tree(physeq(), color = "SampleType")
        
      })
      
    } else { 
      
      shinyalert(title = "Warning", text = "El objeto phyloseq cargado carece de árbol filogenético", type = "warning")
      
    }
  })
  
  # Filtrado del objeto phyloseq
  output$variableui <- renderUI({
    selectInput("variable", "Selecciona una variable de metadata:", choices = colnames(sample_data(physeq())))
  })
  
  output$valueui <- renderUI({
    req(input$variable)
    selectizeInput("value", "Seleccione los valores que desea  filtrar:", choices = unique(sample_data(physeq())[,input$variable]), multiple = TRUE)
  })
  
  # Bug a arreglar
  physeq_filtered <- reactive({
    filtered_data <- physeq()
    if (!is.null(input$filter)) {
      return(ps_filter(filtered_data, input$variable %in% input$value))
    }
    return(filtered_data)
  })
  
  observeEvent(input$filter, {
    
    # physeq() <- prune_taxa(taxa_sums(physeq()) > input$min_count)
    
    # Mostrar la estructura del objeto phyloseq
    output$filtered_physeq_summary <- renderPrint({
      print(physeq_filtered())
    })
    
    # Generar el árbol filogenético
    if (has_tree) { 
      
      output$phylo_tree <- renderPlot({
        
        plot_tree(physeq_filtered(), color = "SampleType")
        
      })
      
    } else { 
      
      shinyalert(title = "Warning", text = "El objeto phyloseq cargado carece de árbol filogenético", type = "warning")
      
    }

  })
  
  ### DIVIDIR EN DOS BOX Y AÑADIR QUE EL USUARUI ELIHJA VARIABLE PARA COLOREAR
  observeEvent(input$update_diversity, {
    
    # Realizar el análisis de diversidad alfa
    diversity_data <- plot_richness(physeq(), measures = input$diversity, color = "SampleType")
    
    # Generar el gráfico de diversidad
    output$diversityPlot <- renderPlot({
      diversity_data
    })
    
  })
  
  
  observeEvent(input$update_heatmaps, {
    
    # Generar los mapas de calor de abundancia de OTUs y especies
    output$heatmap_otus <- renderPlot({
      plot_heatmap(physeq(), taxa.label = "OTU")
    })
    
    output$heatmap_species <- renderPlot({
      plot_heatmap(physeq(), taxa.label = "Species")
    })
    
  })
  
  # Realizar PCoA
  # pcoa <- ape::pcoa(distance_matrix)
  
  # Generar el gráfico de PCoA
  #output$pcoaPlot <- renderPlot({
  #  ggplot(pcoa$vectors, aes_string(x = "Axis.1", y = "Axis.2")) +
  #    geom_point() +
  #    theme_minimal()
  #})
  
  # Realizar la prueba PERMANOVA
  # permanova_results <- adonis(distance_matrix ~ sample_data(physeq())$Group)
  
  # Mostrar los resultados de la prueba PERMANOVA
  #output$permanovaResults <- renderTable({
  #  permanova_results$aov.tab
  #})
  
  
  
  
}