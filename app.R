library(shiny)
library(leaflet)
library(reticulate)

PROYECTO_GCP <- "capable-boulder-511318-c7"

# Ruta del asset con los límites departamentales (revísala en la pestaña
# Assets del Code Editor; si no coincide, la capa de límites no se mostrará)
RUTA_LIMITES <- "projects/capable-boulder-511318-c7/assets/DEP_PERU"

# Conexión a Earth Engine:
# - En la nube: usa GEE_SA_EMAIL + GEE_KEY_JSON (contenido de la clave)
# - En tu PC:   usa GEE_SA_EMAIL + GEE_KEY_FILE (ruta del archivo de clave)
# - Sin variables: usa tu sesión personal de Earth Engine
inicializar_gee <- function() {
  ee_py  <- reticulate::import("ee")
  correo <- Sys.getenv("GEE_SA_EMAIL")
  json   <- Sys.getenv("GEE_KEY_JSON")
  clave  <- Sys.getenv("GEE_KEY_FILE")
  
  cred <- NULL
  if (nzchar(correo) && nzchar(json)) {
    cred <- ee_py$ServiceAccountCredentials(correo, key_data = json)
  } else if (nzchar(correo) && nzchar(clave)) {
    cred <- ee_py$ServiceAccountCredentials(correo, clave)
  }
  
  if (!is.null(cred)) {
    message("Earth Engine: usando cuenta de servicio")
    ee_py$Initialize(credentials = cred, project = PROYECTO_GCP)
  } else {
    message("Earth Engine: usando sesión personal")
    ee_py$Initialize(project = PROYECTO_GCP)
  }
  ee_py
}

ee <- inicializar_gee()

ui <- fluidPage(
  title = "Visor SENAMHI",
  tags$head(tags$style(HTML("
    html, body {margin:0; padding:0; height:100%; overflow:hidden;}
    .container-fluid {padding:0;}
    .cabecera {height:70px; display:flex; align-items:center; gap:20px;
               padding:0 20px; background:#fff;
               border-bottom:1px solid #ddd;}
    .cabecera h3 {margin:0; color:#0b5394; font-size:20px;}
    .contenedor {display:flex; height:calc(100vh - 70px);}
    .panel-lateral {width:290px; min-width:290px; background:#1f2a35;
                    color:#cfd8e0; overflow-y:auto;}
    .panel-lateral .etiqueta-temas {padding:18px; font-size:12px;
                    letter-spacing:1px; color:#8fa3b5;}
    .panel-lateral details {border-top:1px solid #2c3a47;}
    .panel-lateral summary {padding:14px 18px; cursor:pointer;
                    font-weight:600; font-size:13px; letter-spacing:.5px;
                    list-style:none;}
    .panel-lateral summary:hover {background:#2c3a47;}
    .panel-lateral details[open] > summary {background:#2a7fd4; color:#fff;}
    .panel-lateral .contenido {padding:12px 18px 18px 18px;}
    .panel-lateral label {color:#cfd8e0; font-weight:normal;}
    .panel-lateral .form-control {color:#222;}
    .panel-lateral .irs--shiny .irs-bar {background:#2a7fd4;
                    border-color:#2a7fd4;}
    .mapa {flex:1; height:100%;}
  "))),
  
  div(class = "cabecera",
      tags$img(src = "logos.png", height = 50),
      h3("Visor de monitoreo agrometeorológico")),
  
  div(class = "contenedor",
      div(class = "panel-lateral",
          div(class = "etiqueta-temas", "TEMAS"),
          
          tags$details(open = NA,
                       tags$summary("MONITOREO"),
                       div(class = "contenido",
                           radioButtons("capa", "Capa",
                                        choices = c("Precipitación acumulada (CHIRPS)" = "pp",
                                                    "NDVI (MODIS)" = "ndvi")),
                           dateRangeInput("fechas", "Rango de fechas",
                                          start = Sys.Date() - 30,
                                          end   = Sys.Date() - 5,
                                          language = "es", separator = " a ")
                       )
          ),
          
          tags$details(
            tags$summary("CAPAS Y VISUALIZACIÓN"),
            div(class = "contenido",
                checkboxInput("limites", "Límites departamentales", TRUE),
                sliderInput("opacidad", "Opacidad de la capa",
                            min = 0.1, max = 1, value = 0.8, step = 0.1)
            )
          )
      ),
      div(class = "mapa", leafletOutput("mapa", height = "100%"))
  )
)

server <- function(input, output, session) {
  
  # Mapa base (se dibuja una sola vez)
  output$mapa <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap, group = "Calles") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") |>
      setView(lng = -76.0, lat = -9.5, zoom = 6) |>
      addLayersControl(baseGroups = c("Calles", "Satélite"),
                       position = "topright")
  })
  
  # Capa de Earth Engine (se actualiza al cambiar capa, fechas u opacidad)
  observe({
    req(input$capa, input$fechas)
    ini <- as.Date(input$fechas[1])
    fin <- as.Date(input$fechas[2])
    
    if (input$capa == "pp") {
      img <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
        filterDate(as.character(ini), as.character(fin + 1))$
        sum()
      vis <- list(min = 0, max = 100,
                  palette = c("#FFFFFF", "#00FFFF", "#0000FF",
                              "#800080", "#FF0000"))
      etiquetas <- c("0", "25", "50", "75", "100+")
      titulo <- "Precipitación (mm)"
    } else {
      # MODIS NDVI es compuesto de 16 días: se amplía el inicio para
      # asegurar que haya al menos una imagen en el rango
      img <- ee$ImageCollection("MODIS/061/MOD13Q1")$
        filterDate(as.character(ini - 16), as.character(fin + 1))$
        select("NDVI")$
        mean()
      vis <- list(min = 0, max = 9000,
                  palette = c("#d73027", "#fdae61", "#ffffbf",
                              "#a6d96a", "#1a9850"))
      etiquetas <- c("0", "0.23", "0.45", "0.68", "0.9")
      titulo <- "NDVI"
    }
    
    url <- img$getMapId(vis)$tile_fetcher$url_format
    
    leafletProxy("mapa") |>
      clearGroup("gee") |>
      removeControl("leyenda") |>
      addTiles(urlTemplate = url, group = "gee",
               options = tileOptions(opacity = input$opacidad, zIndex = 10)) |>
      addLegend(position = "bottomright", colors = vis$palette,
                labels = etiquetas, title = titulo,
                opacity = 1, layerId = "leyenda")
  })
  
  # Límites departamentales
  observe({
    proxy <- leafletProxy("mapa") |> clearGroup("limites")
    if (isTRUE(input$limites)) {
      url <- tryCatch({
        ee$FeatureCollection(RUTA_LIMITES)$
          style(color = "#333333", fillColor = "#00000000", width = 1.5)$
          getMapId()$tile_fetcher$url_format
      }, error = function(e) NULL)
      
      if (is.null(url)) {
        showNotification("No se pudo cargar la capa de límites. Revisa la ruta del asset.",
                         type = "warning")
      } else {
        proxy |> addTiles(urlTemplate = url, group = "limites",
                          options = tileOptions(zIndex = 20))
      }
    }
  })
}

shinyApp(ui, server)