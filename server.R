# ----- Servidor de la Aplicación ----
server <- function(input, output, session) {
  
  # ----- Variables Reactivas -----
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
  
  # ----- Descargar el objeto Phyloseq -----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("phyloseq-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(physeq(), file)
    }
  )
 
  # ----- Observar el objeto Phyloseq seleccionado -----
  observeEvent(input$update, {
    # Mostrar la estructura del objeto phyloseq
    output$physeq_summary <- renderPrint({
      print(physeq())
    })
    
    # Generar el árbol filogenético si existe dentro del objeto
    if (has_tree) { 
      
      output$phylo_tree <- renderPlot({
        
        plot_tree(physeq(), color = "SampleType")
        
      })
      
    } else { 
      
      shinyalert(title = "Warning", text = "El objeto phyloseq cargado carece de árbol filogenético", type = "warning")
      
    }
  })
  
  # Servidor de Tab para el Filtrado del Objeto Phyloseq
  
  # Inicializa physeq_filtered como NULL
  physeq_filtered <- reactiveVal(NULL)
  
  # Actualiza physeq_filtered cuando se carga physeq
  observe({
    if (!is.null(physeq())) {
      physeq_filtered(physeq())
    }
  })
  
  output$variableui <- renderUI({
    selectInput("variable", "Selecciona una variable de metadata", choices = colnames(sample_data(physeq_filtered())))
  })
  
  output$valueui <- renderUI({
    req(input$variable)
    selectizeInput("value", "Seleccione los valores que desea  filtrar", choices = unique(sample_data(physeq_filtered())[,input$variable]), multiple = TRUE)
  })
  
  # Actualiza physeq_filtered cada vez que el usuario aplique un filtro
  observeEvent(input$filter, {
    
    req(input$variable)
    req(input$value)
    
    # Verifica si la variable seleccionada existe en los datos de muestra
    if (input$variable %in% colnames(sample_data(physeq_filtered()))) {
      # Aplica el filtro a physeq_filtered en lugar de physeq
      filtered <- tryCatch({
        ps_filter(physeq_filtered(), eval(parse(text = input$variable)) %in% input$value)
      }, error = function(e) {
        NULL
      })
      
      # Verifica si el filtro fue exitoso
      if (!is.null(filtered)) {
        # Actualiza physeq_filtered con el resultado del filtro
        physeq_filtered(filtered)
        
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
      } else {
        # Muestra una alerta shiny si el filtro no fue exitoso
        shinyalert(title = "Error", text = "El filtro no pudo ser aplicado. Por favor, verifica las variables y los valores seleccionados.", type = "error")
      }
    } else {
      # Muestra una alerta shiny si la variable seleccionada no existe en los datos de muestra
      shinyalert(title = "Error", text = paste("La variable seleccionada", input$variable, "no existe en los datos de muestra"), type = "error")
    }
  })

  
  
  ### Dividir la interfaz en dos cajas y dejar que el usuario coloree por la variable de metadata que quiera
  observeEvent(input$update_diversity, {
    
    # Realizar el análisis de diversidad alfa
    diversity_data <- plot_richness(physeq(), measures = input$diversity) #, color = "SampleType")
    
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