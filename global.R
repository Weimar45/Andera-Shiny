# ----- Descripción del Proyecto ----
#
# Aplicación en Shiny para el estudio de microbiomas
# Fecha: 14/07/2023
# Última vez editado por: Alejandro Navas González
#

# ----- Zona General ----

# Carga de los paquetes de shiny
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(shinymeta)
library(htmlwidgets)

# Carga del paquete utilizado para editar el tema de la interfaz
library(fresh)

# Carga de los paquetes a utilizar en la aplicación. 
library(tidyverse)
library(data.table)
library(DT)

# Paquetes para el estudio 
library(phyloseq)
library(vegan)
library(skimr)
library(ggcorrplot)
library(ggstatsplot)
library(PMCMRplus)
library(psych)


# Carga de paquetes para editar las paletas de los gráficos
library(palettetown)
library(ggsci)
library(viridis)
library(RColorBrewer)



# Establecer un tema común para los gráficos.
theme_set(theme_light() +
            theme(text = element_text(size = 16, family = "serif")))

# Creación de un tema propio para la Dashboard (paquete fresh).
mytheme <- create_theme(
  
  adminlte_color(
    
    light_blue = "#5D263E"
    
  ),
  
  adminlte_sidebar(
    
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#5D263E",
    dark_submenu_color = "#5D263E"
    
  ),
  
  adminlte_global(
    
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
    
  )
)