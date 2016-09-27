library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
# library(sp)

# PM25_sat <- readOGR(dsn = "Copia.geojson",layer = "OGRGeoJSON")
# read GeoJSON file
# PM25_sat <- readOGR(dsn = "Copia.ENGLAND_geojson_PM25_1km_Sat_2009_2011",
#                       layer = "OGRGeoJSON")

# save GeoJSON file into RDS
# saveRDS(PM25_sat, "England_GWR_Local_Authorities.rds")
 PM25_sat <- readRDS("England_GWR_Local_Authorities.rds")

qpal_SAT <- colorQuantile("Reds", PM25_sat$pm25_mean, n = 7)
qpal_UK_AIR <- colorQuantile("Reds", PM25_sat$pm25_mean_UK_AIR, n = 7)
qpal_GWR <- colorQuantile("Reds", PM25_sat$pm25_mean_AVG_GWR, n = 7)
qpal_pcm <- colorQuantile("Reds", PM25_sat$pm25_mean_pcm, n = 7)
qpal_cmaq <- colorQuantile("Reds", PM25_sat$pm25_mean_cmaq, n = 7)

### color palettes for legends for Joined data by Local Authorities ##############

pal_SAT <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean)

pal_UK_AIR <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_UK_AIR)

pal_GWR <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_AVG_GWR)

pal_pcm <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_pcm)

pal_cmaq <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_cmaq)

### Popouts for Local Authorities values 

popup_PM25_sat <- paste0("<strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                         PM25_sat$pm25_mean)

popup_PM25_UK_AIR <- paste0("<strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                            PM25_sat$pm25_mean_UK_AIR)

popup_GWR <- paste0("<strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                    PM25_sat$pm25_mean_AVG_GWR)

popup_pcm <- paste0("<strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                    PM25_sat$pm25_mean_pcm)

popup_cmaq <- paste0("<strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                     PM25_sat$pm25_mean_cmaq)

#### Load rasters (only tif files format)

URB_Cover_tif <- raster::raster("URB_cover.tif")
PM25_SAT_tif <- raster::raster("PM25_EN_SAT_2014.tif")
PM25_UK_AIR_tif <- raster::raster("PM25_UK_AIR.tif")
PM25_GWR_tif <- raster::raster("GWR_Donkelaar.tif")
PM25_pcm_tif <- raster::raster("pm252014g.tif")
PM25_cmaq_tif <- raster::raster("PM25_cmaq_1km.tif")

### colors for raster URB land cover (England region)
pal_URB <- colorNumeric(c("#FFFFCC", "#41B6C4","#0C2C84"), getValues(URB_Cover_tif),
                        na.color = "transparent")

### colors for raster PM25_sat
pal_PM25_SAT <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                             getValues(PM25_SAT_tif),na.color = "transparent")

### colors for raster PM25 UK-AIR
pal_PM25_UK_AIR <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                                getValues(PM25_UK_AIR_tif),na.color = "transparent")

### colors for raster GWR_1km new data 2009_2016 Donkelaar (2016)
pal_PM25_GWR <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                                     getValues(PM25_GWR_tif),na.color = "transparent")

### colors for raster pcm_PM25
pal_PM25_pcm <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                                  getValues(PM25_pcm_tif),na.color = "transparent")

### colors for raster cmaq_PM25 10km
pal_PM25_cmaq <- colorNumeric(c("#0000FF", "#FFFF00","#FF0000"),
                                        getValues(PM25_cmaq_tif),na.color = "transparent")


###############################################################

shinyServer(function(input, output) {
  
  finalMap <- reactive({
    # Local authorites joined data
    PM25_OGR <- input$variable_OGR
    # Raster data
    PM25_raster <- input$variable_raster
    
    withProgress(message = "processing.....",  detail = 'this may take a while...', value = 0.25, { background= "yellow"  
    # Number of times we'll go through the loop
    for(i in 1:2) {
    
    TYPE <- input$type
    
    # Create base map
    map <- leaflet() %>% 
      addTiles() %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
      addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      setView(-2, 52.5, 6)
    
    
    if (TYPE == "OGR" & PM25_OGR == "pm25_mean") {
      
      map <- map %>% 
        addPolygons(data = PM25_sat,
                    stroke = FALSE, smoothFactor = 0.2, 
                    fillOpacity = 0.5, 
                    color = ~ qpal_SAT(pm25_mean),
                    popup = popup_PM25_sat, group = PM25_OGR) %>%
        addLegend("bottomright", pal = pal_SAT, values = PM25_sat$pm25_mean,
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) Sat.(MODIS) : </strong>",
                  labFormat = labelFormat(prefix = ""), opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_OGR,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "OGR" & PM25_OGR == "pm25_mean_UK_AIR") {
      
      map <- map %>% 
        addPolygons(data = PM25_sat,
                    stroke = FALSE, smoothFactor = 0.2, 
                    fillOpacity = 0.5, 
                    color = ~ qpal_UK_AIR(pm25_mean_UK_AIR),
                    popup = popup_PM25_UK_AIR, group = PM25_OGR) %>%
        addLegend("bottomright", pal = pal_UK_AIR , values = PM25_sat$pm25_mean_UK_AIR,
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK-AIR : </strong>",
                  labFormat = labelFormat(prefix = ""), opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_OGR,
          options = layersControlOptions(collapsed = TRUE))
      
    } 
    
    
    if (TYPE == "OGR" & PM25_OGR == "pm25_mean_AVG_GWR") {
      
      map <- map %>% 
        addPolygons(data = PM25_sat,
                    stroke = FALSE, smoothFactor = 0.2, 
                    fillOpacity = 0.5, 
                    color = ~ qpal_GWR(pm25_mean_AVG_GWR),
                    popup = popup_GWR, group = PM25_OGR) %>%
        addLegend("bottomright", pal = pal_GWR , values = PM25_sat$pm25_mean_AVG_GWR,
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) GWR : </strong>",
                  labFormat = labelFormat(prefix = ""), opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_OGR,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "OGR" & PM25_OGR == "pm25_mean_pcm") {
      
      map <- map %>% 
        addPolygons(data = PM25_sat,
                    stroke = FALSE, smoothFactor = 0.2, 
                    fillOpacity = 0.5, 
                    color = ~ qpal_pcm(pm25_mean_pcm),
                    popup = popup_pcm, group = PM25_OGR) %>%
        addLegend("bottomright", pal = pal_pcm , values = PM25_sat$pm25_mean_pcm,
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) PMC : </strong>",
                  labFormat = labelFormat(prefix = ""), opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_OGR,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    if (TYPE == "OGR" & PM25_OGR == "pm25_mean_cmaq") {
      
      map <- map %>% 
        addPolygons(data = PM25_sat,
                    stroke = FALSE, smoothFactor = 0.2, 
                    fillOpacity = 0.5, 
                    color = ~ qpal_cmaq(pm25_mean_cmaq),
                    popup = popup_cmaq, group = PM25_OGR) %>%
        addLegend("bottomright", pal = pal_cmaq , values = PM25_sat$pm25_mean_cmaq,
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) CMAQ : </strong>",
                  labFormat = labelFormat(prefix = ""), opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_OGR,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "raster" & PM25_raster == "URB_Cover_tif") {
      
      map <- map %>% 
        addRasterImage(URB_Cover_tif, 
                       colors = pal_URB, 
                       opacity = 0.6, group = PM25_raster) %>%
        addLegend("bottomright",pal = pal_URB, values = getValues(URB_Cover_tif),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) URB Cover (%): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_raster,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "raster" & PM25_raster == "PM25_SAT_tif") {
      
      map <- map %>% 
        addRasterImage(PM25_SAT_tif, 
                       colors = pal_PM25_SAT, 
                       opacity = 0.6, group = PM25_raster) %>%
        addLegend("bottomright",pal = pal_PM25_SAT, values = getValues(PM25_SAT_tif),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) MODIS (10km): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_raster,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "raster" & PM25_raster == "PM25_UK_AIR_tif") {
      
      map <- map %>% 
        addRasterImage(PM25_UK_AIR_tif, 
                       colors = pal_PM25_UK_AIR, 
                       opacity = 0.6, group = PM25_raster) %>%
        addLegend("bottomright",pal = pal_PM25_UK_AIR, values = getValues(PM25_UK_AIR_tif),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK-AIR: </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_raster,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "raster" & PM25_raster == "PM25_GWR_tif") {
      
      map <- map %>% 
        addRasterImage(PM25_GWR_tif, 
                       colors = pal_PM25_GWR, 
                       opacity = 0.6, group = PM25_raster) %>%
        addLegend("bottomright",pal = pal_PM25_GWR, values = getValues(PM25_GWR_tif),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) GWR 1km (MODIS): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_raster,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "raster" & PM25_raster == "PM25_pcm_tif") {
      
      map <- map %>% 
        addRasterImage(PM25_pcm_tif, 
                       colors = pal_PM25_pcm, 
                       opacity = 0.6, group = PM25_raster) %>%
        addLegend("bottomright",pal = pal_PM25_pcm, values = getValues(PM25_pcm_tif),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) PCM (1km): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_raster,
          options = layersControlOptions(collapsed = TRUE))
      
    }
    
    
    if (TYPE == "raster" & PM25_raster == "PM25_cmaq_tif") {
      
      map <- map %>% 
        addRasterImage(PM25_cmaq_tif, 
                       colors = pal_PM25_cmaq, 
                       opacity = 0.6, group = PM25_raster) %>%
        addLegend("bottomright",pal = pal_PM25_cmaq, values = getValues(PM25_cmaq_tif),
                  title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) CMAQ (1km): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = PM25_raster,
          options = layersControlOptions(collapsed = TRUE))
    }
    

    # Increment the progress bar, and update the detail text.
    setProgress(message = 'message = "processing.....',
                detail = 'this may take a while...',
                value=i)
    print(i)
    Sys.sleep(0.1)
    # Pause for 0.5 seconds to simulate a long computation.
    }
    
    })
    
    # Return
    map
    
  })
  
  # Return to client
  output$myMap = renderLeaflet(finalMap())
  
})

