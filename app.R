#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(mapview)
library(leaflet)
library(leafpop)
library(sf)
sf_use_s2(FALSE)
library(data.table)
library(viridis)
library(ggplot2)
#library(mapiso)
#library(terra)
library(shiny)
library(dataRetrieval)
library(DT)
library(shinycssloaders)
library(shinyalert)
source("./locationDataExtractor.R")
source("./portfolioTableMaker.R")
source("./portfolioTablePlotter.R")
source("./locationPlotter.R")
source("./gageRssFeed_recentData.R")

customerInputTable = data.table::fread("./Data/CustomerOnboardingTemplate.csv")
options(spinner.type = 6)
ui <- fluidPage(
  titlePanel(title=div(img(src="./CAi2.png", height=60, width=200), "    River Transportation (beta)")),
  
  sidebarLayout(position='left',
                sidebarPanel('User Interface',
                             width = 2,
                             selectInput(inputId = "plotUsgsGages",
                                         label = "Show USGS Gage Locations:",
                                         c("Hide" = 0, "Show" = 1)),
                             radioButtons(inputId = "varType",
                                          label = "Data Type for Map:",
                                          choiceValues = c(1,2,3),
                                          choiceNames = c("Percentile (annual)", "Percentile (for this time of year)", "Raw Value")),
                             radioButtons(inputId = "shippingLoc",
                                          label = "Location to Plot:",
                                          choiceValues = 1:nrow(customerInputTable),
                                          choiceNames = customerInputTable$Location_Name),
                             actionButton("rssButton", "Pull Live Data")
                             ),
                mainPanel(width = 10,
                          fluidRow(
#                            withSpinner(mapviewOutput("myMap", height = "500px"), type=7)
                            withSpinner(leafletOutput("myMap", height = "500px"), type=7)
                          ),
                          hr(),
                          fluidRow(
                            column(6,
                                   plotOutput("locationPlotter"),
                                   downloadButton("downloadPlot", "Download Plot"),
                                   textOutput("liveDataText")
                            ),
                            column(6,
                                   DT::dataTableOutput("portfolioTable")
                            )
                          ),
                          hr(),
                          hr()
                )
  )
)


server <- function(input, output, session) {
  historicAndCurrentLevels = readRDS("./Data/gageDoyAvgs_ls.rds")
  mapData = subset(sf::st_read("./Data/waterwaysWithCurrentVals.gpkg"), WTWY_TYPE == 6 & 
                     STATE %in% c("MN","IL","OH","LA","TN","OK","MS","KY","AL","IA","AR","NE","MO","KS","IN"))
  customerInputTable = data.table::fread("./Data/CustomerOnboardingTemplate.csv")
  customerInputTable_sf = sf::st_as_sf(subset(customerInputTable, !is.na(Lat) & !is.na(Lon)),
                                       coords = c("Lon", "Lat"), crs = 4326)
  availableGages_Q = data.table::fread('./Data/usDailyStreamStageGages_Q_long.csv', colClasses = c('site_no' = 'character'))
  availableGages_Q_sf = sf::st_as_sf(availableGages_Q[ , c("agency_cd", "site_no", "station_nm","begin_date","end_date","dec_long_va", "dec_lat_va")],
                                     coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
  #  availableGages_H = data.table::fread('./Data/usDailyStreamStageGages_H_long.csv', colClasses = c('site_no' = 'character'))
  #  availableGages_H_sf = sf::st_as_sf(availableGages_H[ , c("agency_cd", "site_no", "station_nm","begin_date","end_date","dec_long_va", "dec_lat_va")],
  #                                     coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
  
  # pulling USGS data for each location
  customerGageData = locationDataExtractor_f(
    customerInputTable = customerInputTable, 
    customerInputTable_sf = customerInputTable_sf, 
    availableGages_Q = availableGages_Q, 
    mapData = mapData)
  
  # pulling info for tables and location vals
  portfolioTable = portfolioTableMaker_f(customerGageData = customerGageData, customerInputTable = customerInputTable)
  customerInputTable_sf = merge(customerInputTable_sf, portfolioTable, by.x="Location_Name",  by.y="Location")
  
  mapDataColNames = c("Annual_Avg_Pct", "Season_Avg_Pct", "Raw_Value")
  customerInputTableColNames = c("Current_Pct_Annual", "Current_Pct_Season", "Raw_Value")
  
  output$myMap = renderLeaflet({
    thisPal <- turbo(n=10, begin=0, end=1, direction = -1)
    thatPal <- colorNumeric(palette = thisPal, domain = mapData$Annual_Avg_Pct)
    
    thisCol = as.numeric(input$varType)   
    
    useMapview = FALSE    
    if(useMapview) {
      mapviewsCombined = 
       #      mapview(nodes_sf, lwd=4, lty=3, color='purple', alpha= 0.1, legend=FALSE)+
        mapview(availableGages_Q_sf, color='grey20', col.regions ='grey80', cex=2.75, legend=FALSE,
                alpha = as.numeric(input$plotUsgsGages), alpha.regions=as.numeric(input$plotUsgsGages)
                ) +
        mapview(mapData,
                zcol=mapDataColNames[thisCol], color=thisPal, at = seq(0,100,10), lwd = 2, legend.opacity=0.8,layer.name="percentile",
                popup = popupTable(mapData, zcol = c("Annual_Avg_Pct", "Season_Avg_Pct", "Raw_Value"))
                ) +
        mapview(customerInputTable_sf, 
                zcol = customerInputTableColNames[thisCol], col.regions=thisPal, at = seq(0,100,10), color = 'grey10',legend=FALSE, cex = 8
                )  
      #              popup = popupGraph(list(p2), width=400, height=200)) 
      #      mapview(mapCenter_sf, color='purple1', col.regions="yellow2", legend=FALSE) +
      mapviewsCombined@map %>% setView(lat=38, lng=-80, zoom = 5)
    } else {
      # Create a base map
      my_map <- leaflet() %>%
        addTiles() %>%
        setView(lng = -80, lat = 38, zoom = 5)  # Set the initial view

      theseDataForCols = as.data.frame(mapData[, which(names(mapData) == mapDataColNames[thisCol])])[,1]      
      theseDataForPointCols = as.data.frame(customerInputTable_sf[, which(names(customerInputTable_sf) == customerInputTableColNames[thisCol])])[,1]      
      # Add your layers
      my_map %>%
        #addPolygons(data = nodes_sf, color = "purple", weight = 4, opacity = 0.1) %>%
#        addPolygons(data = availableGages_Q_sf, color = "grey20", fillColor = "grey80",
 #                   fillOpacity = as.numeric(input$plotUsgsGages), opacity = as.numeric(input$plotUsgsGages)) %>%
        addPolylines(data = mapData,
                    color = thatPal(theseDataForCols),#thisPal, #fillColor = thisPal,
                    weight = 2,
#                    layerId = mapDataColNames[thisCol],#fillOpacity = 0.8, 
                    popup = popupTable(mapData, zcol = c("Annual_Avg_Pct", "Season_Avg_Pct", "Raw_Value"))) %>%
        addCircleMarkers(data = customerInputTable_sf,
                   fillColor = thatPal(theseDataForPointCols),
                   fillOpacity = 0.8, color = 'grey20', weight = 0 ) %>%#,
#                   popup = customerInputTableColNames) %>%
        addLayersControl(baseGroups = c("providers$Esri.WorldImagery", "StreetMap", "Stamen"), overlayGroups = c("Markers"),
                        position = "bottomleft") %>%
        addLegend(data = customerInputTable_sf,
                  position = "topright",
                  pal = thatPal,
                  values = ~theseDataForPointCols,
                  title = "Percentile",
                  opacity = 1)
    }
    
    
  })
  
  # plots for locations
  output$locationPlotter = renderPlot({
    locationPlotter_f(customerGageData = customerGageData, shippingLoc = as.numeric(input$shippingLoc), customerInputTable = customerInputTable)
  })
  
  
  # generating table
  output$portfolioTable <- DT::renderDataTable({
    portfolioTablePlotter_f(portfolioTable = portfolioTable)
  })
  
  output$downloadPlot = downloadHandler(
    filename = function(){
      paste0(customerInputTable$Location_Name[as.numeric(input$shippingLoc)], ".png")
    },
    content = function(file){
      png(file)
      locationPlotter_f(customerGageData = customerGageData, shippingLoc = as.numeric(input$shippingLoc), customerInputTable = customerInputTable)
      dev.off()
    }
  ) 
  #Download dataframe
  #  output$downloadData <- downloadHandler(
  #    filename = "waterwaysWithCurrentVals.gpkg",
  #    content = function(filename) {
  #      sf::st_write(Data, filename, row.names = FALSE)
  #    }
  #  )
  
  observeEvent(input$rssButton, {
    thisLoc = as.numeric(input$shippingLoc)
    if(customerInputTable$Use_USGS[thisLoc]) {
      whichIsCurrentConditions = data.table::last(which(!is.na(customerGageData[[thisLoc]]$thisYear)))
      currentConditions = customerGageData[[thisLoc]]$thisYear[whichIsCurrentConditions]
      units = " CFS"
    } else {
      currentConditions = gageRssFeed_recentData_f(customerInputTable = customerInputTable, userDataLocation = thisLoc)
      units = " ft"
    }
    shinyalert(title = paste0(customerInputTable$Location_Name[thisLoc]),
               text = paste0("Current conditions are ", currentConditions, units),
               type="info",
               closeOnClickOutside = TRUE
    )
  })
  

}

shinyApp(ui, server)
