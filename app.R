# GasApp: SHINY APP -------------------------------------------------------

# Trabajo final de la asignatura Lenguajes de Programacion Estadistica

# Realizado por:
# Rocio COLMENA
# Andres FERNANDEZ
# Lucas LE MORE
# Lucia NUNEZ
# Laura TERUEL

# Run before executing the app to load all libraries
pacman :: p_load (tidyverse,readxl,sparklyr,janitor,leaflet,stringr,httr,xml2,jsonlite,leaflet.extras,mapsapi,geosphere, ggmap, shinythemes)

# GCP Geocoding API key (turn addresses into coordinates)
key <- read.delim("key.txt", encoding='UTF-8')
register_google(key$key)
# When the API returns several possible results, the program takes the first one


# (Only used for testing)
# If testing is set to 1 the program will log (glimpse) at several points
testing <- 1 

# Default coordinates (in case user doesn't input an address)
lat <- 40.49414
lon <- -3.390917
coordenates <- c(lon,lat)
distance <- 10000

# For the open 24h filter
open_24h_filter <- FALSE
open_24h <- TRUE

# Localidad filter
localidad_filter <- 'NONE'

# Expensive brand filter
marcas <- c('TODAS','REPSOL','CEPSA','SHELL','BP','CAMPSA','GALP','PETRONOR',"AVIA","Q8")


#  Data download ----------------------------------------------------------
df <- read_csv("https://storage.googleapis.com/lpe-gasolineras/ultimo.csv")


# SERVER FUNCTION ---------------------------------------------------------

# To take into consideration: ---------------------------------------------

# The code inside the server function is reactive
# (when the user clicks a button, the code automatically
# runs again)

# Any variable declared inside the function gets deleted when
# it ends the execution

# To store variables and use them through all the app,
# we use .GlobalEnv$var <- "variable"

server <- function(input, output, session) {
  
  # "Consumidor" panel ------------------------------------------------------
  
  output$distPlot <- renderLeaflet({
    
    open_24h_filter <- input$abierto24h != "Todos los horarios"
    open_24h <- input$abierto24h == "Abiertas 24 horas"
    nada <-input$direccion_in_v2
    
    # Filters
    .GlobalEnv$df_to_map <- df %>% 
      filter( (open_24h_filter == FALSE) |(open_24h == abierto24h)) %>%  # 24h filter
      # filter( (input$marca_v2 == FALSE) |(franquicia == input$marca_v2)) %>%  # Franquicias filter
      filter( (localidad_filter == 'NONE') |(localidad == localidad_filter)) %>%  # Localidad filter
      filter( (input$ofreceGasolina95 == FALSE) |(!is.na(precio_gasolina_95_e5))) %>%  # Gasolina filter
      filter( (input$ofreceDiesel == FALSE) |(!is.na(precio_gasoleo_a))) %>%  # Gasoleo filter
      filter( (input$ofreceGas == FALSE) |(!is.na(precio_gas_natural_comprimido))) %>%  # Gas natural filter
      filter( (input$ofreceGLP == FALSE) |(!is.na(precio_gases_licuados_del_petroleo))) %>% # GLP filter
      filter(  distGeo(matrix(c(longitud_wgs84, latitud), ncol = 2),.GlobalEnv$coordenates) <= input$distancia)
    
    .GlobalEnv$df_to_map <- .GlobalEnv$df_to_map %>% 
      mutate(etiqueta = paste("<b>",rotulo,"</b><br>",direccion,"<br> Horario:",horario,"<br> <small>precio 95:", as.character(precio_gasolina_95_e5)," <br> precio diesel:",as.character(precio_gasoleo_a)," <br> precio Gas natural:",as.character(precio_gas_natural_comprimido)," <br> precio GCP:",as.character(precio_gases_licuados_del_petroleo)," </small>"    ) ) 
    
    basemap = .GlobalEnv$df_to_map %>% leaflet() %>% addProviderTiles("CartoDB") %>% 
      addAwesomeMarkers(lng= ~longitud_wgs84, lat = ~latitud,popup = ~etiqueta )
    
  })
  
  output$table_usu <- renderDataTable({
    # All inputs are named here to force a refresh when they're modified
    input$direccion_in_v2
    input$ofreceGasolina95_v2 
    input$ofreceDiesel_v2 
    input$ofreceGas_v2
    input$ofreceGLP_v2 
    input$abierto24h
    
    rename(select(.GlobalEnv$df_to_map,rotulo,direccion,horario,precio_gasolina_95_e5,precio_gasoleo_a,precio_gas_natural_comprimido,precio_gases_licuados_del_petroleo), 'Marca' = rotulo, 'Direccion' = direccion, 'Horario' = horario, 'Gasolina95' = precio_gasolina_95_e5, 'GasoleoA' =  precio_gasoleo_a,'GasNatural' = precio_gas_natural_comprimido, 'GLP' = precio_gases_licuados_del_petroleo)
  })
  
  observeEvent(input$irConsumidor_v2, {
    
    dir <- input$direccion_in_v2
    dir <- geocode(dir, output = "latlona", source = "google")
    
    .GlobalEnv$coordenates <- c(dir$lon, dir$lat)
    
    updateTabsetPanel(session, "idTabset",
                      selected = "panelConsumidores")
  })
  
  observeEvent(input$irConsumidor, {
    updateTabsetPanel(session, "idTabset",
                      selected = "panelConsumidores")
    
    # To convert addresses into coordinates
    dir <- input$direccion_in
    dir <- geocode(dir, output = "latlona", source = "google")
    
    .GlobalEnv$coordenates <- c(dir$lon, dir$lat) 
    
  })
  
  
  # "Empresario" panel ------------------------------------------------------
  
  output$distPlot_v2 <- renderLeaflet({
    
    open_24h_filter <- input$abierto24h_v2 != "Todos los horarios"
    open_24h <- input$abierto24h_v2 == "Abiertas 24 horas"
    
    # Filters
    .GlobalEnv$df_to_map <- df %>% 
      filter( (open_24h_filter == FALSE) |(open_24h == abierto24h)) %>%  # 24h filter
      # filter( (input$marca_v2 == FALSE) |(franquicia == input$marca_v2)) %>%  # Franquicias filter
      filter( (localidad_filter == 'NONE') |(localidad == localidad_filter)) %>%  # Localidad filter
      filter( (input$ofreceGasolina95_v2 == FALSE) |(!is.na(precio_gasolina_95_e5))) %>%  # Gasolina filter
      filter( (input$ofreceDiesel_v2 == FALSE) |(!is.na(precio_gasoleo_a))) %>%  # Gasoleo filter
      filter( (input$ofreceGas_v2 == FALSE) |(!is.na(precio_gas_natural_comprimido))) %>%  # Gas natural filter
      filter( (input$ofreceGLP_v2 == FALSE) |(!is.na(precio_gases_licuados_del_petroleo))) %>% # GLP filter
      filter(  distGeo(matrix(c(longitud_wgs84, latitud), ncol = 2),.GlobalEnv$coordenates) <= input$distancia_v2)
    
    .GlobalEnv$df_to_map <- .GlobalEnv$df_to_map %>% 
      mutate(etiqueta = paste("<b>",rotulo,"</b><br>",direccion,"<br> Horario:",horario,"<br> <small>precio 95:", as.character(precio_gasolina_95_e5)," <br> precio diesel:",as.character(precio_gasoleo_a)," <br> precio Gas natural:",as.character(precio_gas_natural_comprimido)," <br> precio GLP:",as.character(precio_gases_licuados_del_petroleo)," </small>"    ) ) 
    
    palcontinua <- colorNumeric(palette = "Spectral", domain = .GlobalEnv$df_to_map$precio_gasolina_95_e5)
    
    basemap = .GlobalEnv$df_to_map %>% leaflet() %>% addProviderTiles("CartoDB") %>% 
      addCircleMarkers(radius = 5,lng= ~longitud_wgs84, lat = ~latitud,color = ~palcontinua(precio_gasolina_95_e5),popup = ~etiqueta ) %>% 
      addLegend(title = "Leyenda escala continua", pal = palcontinua, values = .GlobalEnv$df_to_map$precio_gasolina_95_e5,
                position = "bottomright")
    
  })
  
  output$table_v2 <- renderDataTable({
    # All inputs are named here to force a refresh when they're modified
    input$direccion_in_v2
    input$ofreceGasolina95_v2 
    input$ofreceDiesel_v2 
    input$ofreceGas_v2
    input$ofreceGLP_v2 
    input$abierto24h
    
    rename(select(.GlobalEnv$df_to_map,rotulo,direccion,horario,precio_gasolina_95_e5,precio_gasoleo_a,precio_gas_natural_comprimido,precio_gases_licuados_del_petroleo), 'Marca' = rotulo, 'Direccion' = direccion, 'Horario' = horario, 'Gasolina95' = precio_gasolina_95_e5, 'GasoleoA' =  precio_gasoleo_a,'GasNatural' = precio_gas_natural_comprimido, 'GLP' = precio_gases_licuados_del_petroleo)
    })
  
  observeEvent(input$irEmpresario, {
    updateTabsetPanel(session, "idTabset",
                      selected = "panelEmpresarios")
    
    # To convert directions into coordinates
    dir <- input$direccion_in
    dir <- geocode(dir, output = "latlona", source = "google")
    
    .GlobalEnv$coordenates <- c(dir$lon, dir$lat) 
  })
}


# UI FUNCTION: User interface ---------------------------------------------

# To take into consideration: ---------------------------------------------

# Inside each layout we can insert HTML. E.g.: p("Introduzca una direccion")
# or shiny elements, which are sent to server and get transformed to HTML.
# In that case, there has to be an element related in server.R.

# E.g.: plotOutput("plot")
#   and in server.R:   output$plot <- renderPlot({ hist(datos) })

# It is not possible in Shiny to have two panels using the same output

# We load shinythemes library to use one of the themes provided by Shiny

ui <- navbarPage("GasApp", id = "idTabset",
                 
                 # "Inicio" panel user interface
                 tabPanel("Inicio", value = "panelInicio",
                          # First parameter in fluidPage function refers to the theme used
                          fluidPage(theme = shinytheme("superhero"), 
                                    verticalLayout(
                                      h2("Hey there, you are using GasApp!"),
                                      p("Introduzca una direccion:"),
                                      textInput("direccion_in", NULL),
                                      fluidRow(actionButton('irConsumidor', 'Herramientas para consumidores'),
                                               actionButton('irEmpresario', 'Herramientas para empresarios'))
                                    )
                          )
                 ),
                 
                 # "Consumidores" panel user interface
                 tabPanel("Consumidores", value = "panelConsumidores",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                h5("Introduzca una nueva direccion:"),
                                textInput("direccion_in_v2", NULL),
                                actionButton('irConsumidor_v2', 'Buscar direccion', class = "btn-success"),
                                h3(""),
                                
                                h5("Establezca los filtros para su mapa:"),
                                
                                selectInput("abierto24h", 
                                            label = "Seleccione un horario:",
                                            choices = c("Abiertas 24 horas", 
                                                        "No abiertas 24h",
                                                        "Todos los horarios"),
                                            selected = "Todos los horarios"),
                                
                                h3(""), 
                                sliderInput("distancia", 
                                            label = "Rango de gasolineras (en metros):",
                                            min = 0, max = 10000, value = 10000),
                               
                                h3(""), 
                                helpText("Tipo de combustible:"),
                                
                                # Checkboxes
                                checkboxInput("ofreceGasolina95", label = "Gasolina E95", value = FALSE, width = NULL),
                                checkboxInput("ofreceDiesel", label = "Gasoleo A", value = FALSE, width = NULL),
                                checkboxInput("ofreceGLP", label = "GLP", value = FALSE, width = NULL),
                                checkboxInput("ofreceGas", label = "Gas natural", value = FALSE, width = NULL)
                              ),
                              
                              mainPanel(
                                leafletOutput(outputId = "distPlot", width = "700px", 
                                              height = "300px"),
                                h3(""),
                                h5("Listado de gasolineras:"),
                                dataTableOutput("table_usu")
                              )
                            )
                          )
                 ),
                 
                 # "Empresario" panel user interface
                 tabPanel("Empresario", value = "panelEmpresarios",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                h5("Establezca los filtros para su mapa:"),
                                
                                selectInput("marca_v2", 
                                            label = "Seleccione una marca:",
                                            choices = marcas,
                                            selected = "TODAS"),
                                selectInput("abierto24h_v2", 
                                            label = "Seleccione un horario:",
                                            choices = c("Abiertas 24 horas", 
                                                        "No abiertas 24h",
                                                        "Todos los horarios"),
                                            selected = "Todos los horarios"),
                                
                                sliderInput("distancia_v2", 
                                            label = "Rango de gasolineras (en metros):",
                                            min = 0, max = 10000, value = 10000),
                                
                                helpText("Tipo de combustible:"),
                                checkboxInput("ofreceGasolina95_v2", label = "Gasolina E95", value = FALSE, width = NULL),
                                checkboxInput("ofreceDiesel_v2", label = "Gasoleo A", value = FALSE, width = NULL),
                                checkboxInput("ofreceGLP_v2", label = "GLP", value = FALSE, width = NULL),
                                checkboxInput("ofreceGas_v2", label = "Gas natural", value = FALSE, width = NULL)
                              ),
                              
                              
                              mainPanel(
                                leafletOutput(outputId = "distPlot_v2", width = "700px", 
                                              height = "300px"),
                                h3(""),
                                h5("Listado de gasolineras:"),
                                dataTableOutput("table_v2")
                              )
                            )
                          )
                 ),
)

# To run the app:
shinyApp(ui, server)
