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
  
  # Descargar el objeto phyloseq
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("phyloseq-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(physeq(), file)
    }
  )
  
  # Filtrado del objeto phyloseq
  observeEvent(input$filter, {
    physeq_temp <- prune_taxa(taxa_sums(physeq()) > input$min_count, physeq())
    physeq(physeq_temp)
  })
  
 
  observeEvent(input$update, {
    # Mostrar la estructura del objeto phyloseq
    output$physeq_summary <- renderPrint({
      print(physeq())
    })
    
    # Generar el árbol filogenético
    output$phylo_tree <- renderPlot({
      plot_tree(physeq(), color = "SampleType")
    })
    
    # Realizar el análisis de diversidad alfa
    diversity_data <- plot_richness(physeq(), measures = input$diversity, color = "SampleType")
    
    # Generar el gráfico de diversidad
    output$diversityPlot <- renderPlot({
      diversity_data
    })
    
    # Realizar el análisis de diversidad beta
    # distance_matrix <- phyloseq::distance(physeq(), method = input$distance)
    
    # Generar los mapas de calor de abundancia de OTUs y especies
    output$heatmap_otus <- renderPlot({
      plot_heatmap(physeq(), taxa.label = "OTU")
    })
    output$heatmap_species <- renderPlot({
      plot_heatmap(physeq(), taxa.label = "Species")
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
  })
  
  
  
}