library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
# library(sp)
library(shinydashboard)

############################################################


jscode <- "shinyjs.refresh = function() { history.go(0); }"
    
    ui <- dashboardPage(skin = "blue",
                        dashboardHeader (title = "Satellite and UK AIR data (England and Wales)"),
                      
                        dashboardSidebar(
                          width = 290,
                          paste("Time:",Sys.time()),
                          sidebarMenu(

                 br(),
               selectInput("type", "Aggregates/Gridded ",                  
                           c("Aggregated data" = "OGR", "Gridded data" = "raster")),
               conditionalPanel(
                 condition = "input.type == 'OGR'",
                 br(),  # add some space
                 br(),
                   selectInput("variable_OGR", "Choose layer", c("PM2.5 satellite (MODIS 2009-11)" = "pm25_mean",
                                                               "PM2.5 UK-AIR (2009-2011)" = "pm25_mean_UK_AIR",
                                                               "PM2.5 Satellite (MODIS-GWR 2009-11)" = "pm25_mean_AVG_GWR",
                                                               "PM2.5 modeled data (pcm 2009-11)" = "pm25_mean_pcm",
                                                               "PM2.5 CTM cmaq model (2009-11)" = "pm25_mean_cmaq"))),
               
               conditionalPanel(
                 condition = "input.type == 'raster'",
                 selectInput("variable_raster", "Choose layer", c("% urban coverage (1km)" = "URB_Cover_tif",
                                                                  "PM2.5 satellite (MODIS 1km 2009-11)" = "PM25_SAT_tif",
                                                                  "PM2.5 UK-AIR (2009-11)" = "PM25_UK_AIR_tif",
                                                                  "PM2.5 Satellite 1km (MODIS-GWR 2014)" = "PM25_GWR_tif",
                                                                  "PM2.5 modeled data (pcm 2014)" = "PM25_pcm_tif",
                                                                  "PM2.5 CTM cmaq model (2009-11)" = "PM25_cmaq_tif"))),
    
      
      menuItem("Map", tabName = "MAP", icon = icon("th"))
    
    )),
    
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "MAP",
              fluidRow(
                tabBox(
                  height = 750, width = 950, selected = tags$b("Interactive map"),
                  tabPanel(
                    tags$b("Interactive map"),leafletOutput('myMap', height = 650, width = 750)
                  )
                )
              ))
      
    

  ))
)

