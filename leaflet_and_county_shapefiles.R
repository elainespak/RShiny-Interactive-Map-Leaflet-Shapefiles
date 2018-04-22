###
### Author: Elaine Pak
###


###
### Data Preparation
###
# Set up libraries
library(dplyr)
library(purrr)
library(shiny)
library(leaflet)
library(yaml)
library(RColorBrewer)
library(sp)
library(rgdal)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(rgeos)
library(ggplot2)
library(plyr)

# Bring in the shapefiles from US Census
shapes <- readOGR(dsn = "Data/cb_2015_us_county_500k", layer = "cb_2015_us_county_500k", verbose = F)
shapes <- shapes %>%
  filter(!(STATEFP == "02" | STATEFP == "60" | STATEFP == "66" | STATEFP == "15" | STATEFP == "72" | STATEFP == "78")) # get rid of non-mainland states

# Bring in the cancer data
counties <- read.csv("Data/rdata.csv", stringsAsFactors = F)
imputed <- read.csv("Data/imputed_data.csv", stringsAsFactors = F) %>%
  select(FIPS, Cancer_Incidence)


###
### Data Cleaning
###
# Make data more readable
counties$Cancer[which(is.na(counties$Cancer) == TRUE)] = 0 # change NA to 0 to ease future computation
counties$FIPS[1:285] <- paste0("0", counties$FIPS[1:285]) # change 4-digit FIPs to 5-digit
counties$name <- gsub(",", ", ", counties$name) %>% # for each county name, add space after comma, and
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl = T) # capitalize every word

imputed$Cancer_Incidence[which(is.na(imputed$Cancer_Incidence) == TRUE)] = 0
imputed$FIPS <- as.character(imputed$FIPS)
imputed$FIPS[2431:2743] <- paste0("0", imputed$FIPS[2431:2743])

counties <- counties %>% left_join(imputed, by = "FIPS")
counties$Cancer[which(counties$Cancer == 0)] <- counties$Cancer_Incidence[which(counties$Cancer == 0)]

colnames(counties) <- c("number", "layerId", "GEOID", "Cancer") # change column names

# Merge the two data sets
shapes <- inner_join(shapes, counties, by = "GEOID") # retain only rows in both data sets (otherwise the map gets messed up)


###
### R Shiny portion
###
ui <- fluidPage(
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  title = "Total Exposure on Cancer Visualization",
  
  leafletOutput("CAmap", width = "100%", height = "400px"), # main map
  
  fluidRow(
    column(2,
           h4("Description and Guideline"), # dropdown menu to choose type of cancer
           h5("1. Click on any county."),
           h5("2. Control the values from the sliders.")
    ),
    column(4,
           h4(paste0("Selected County", ": ")),
           textOutput("countyName"), # selected county name
           leafletOutput("countyMap", width = "100%", height = "250px") # selected county map
    ),
    column(3,
           h4("Change Variables"), # 3 sliders
           sliderInput("smoking", 
                       label = "Smoking Level:",
                       min = 0, max = 10, value = 0, step=1),
           sliderInput("diabetes", 
                       label = "Diabetes:",
                       min = 0, max = 10, value = 0, step=1),
           sliderInput("pm", 
                       label = "PM 2.5 Level:",
                       min = 0, max = 10, value = 0, step=1)
    ),
    column(3,
           h4("Change Variables"), # 3 sliders
           sliderInput("obesity", 
                       label = "Obesity:",
                       min = 0, max = 10, value = 0, step=1),
           sliderInput("phys", 
                       label = "Physical Inactivity:",
                       min = 0, max = 10, value = 0, step=1)
    )
  )
)


server <- function(input, output, session) {
  
  levelCancer <- c("Missing",1,2,3,4,5) # legend color palette
  pal <- colorFactor(
    palette = c('#cccccc', # missing color is grey
                '#ffffb2', '#fecc5c', '#fd8d3c', '#f03b20', '#bd0026'), # 1 - 5 in red color scale
    levels = levelCancer)
  
  labels <- sprintf( # design element for mouse-over pop-up label
    "<strong>%s</strong><br/>",
    shapes$NAME
  ) %>% lapply(htmltools::HTML)
  
  allMap <- leaflet(data = shapes) %>% # the main map frame
    addTiles() %>% setView(-100, 38, 4) %>%
    addPolygons(fillColor = ~pal(Cancer),weight = 1, opacity = 1,
                color = "white", dashArray = "3", fillOpacity = 0.7,
                highlight = highlightOptions(weight = 5, color = "#666",
                                             dashArray = "", fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px", direction = "auto"),
                layerId = shapes$layerId) %>%
    addLegend(pal = pal, values = ~levelCancer, opacity = 0.7,
              title = "Cancer Likelihood", position = "bottomright")
  
  output$CAmap <- renderLeaflet({allMap}) # the main map
  
  observeEvent(input$CAmap_shape_click, { # user's choice of county returns the county map and name
    
    click <- input$CAmap_shape_click
    
    eachCounty <- leaflet(data = shapes[which(shapes$layerId == click$id),]) %>%
      addPolygons(fillColor = ~pal(Cancer), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7)
    
    output$countyName <- renderText({click$id}) # county name in the format of "State, County"
    output$countyMap <- renderLeaflet({eachCounty}) # county map
    
    updated <- read.csv("Data/rdata_updated.csv", stringsAsFactors = F) %>%
      mutate(FIPS = as.character(FIPS))
    updated$name <- gsub(",", ", ", updated$name) %>% # TERRIBLY INEFFICIENT - FIX WHEN POSSIBLE
      gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl = T)
    
    updateSliderInput(session, "smoking",
                      value = updated$Adult_Smoking[which(updated$name == click$id)])
    
    updateSliderInput(session, "diabetes",
                      value = updated$Diabetes_Value[which(updated$name == click$id)])
    
    updateSliderInput(session, "pm",
                      value = updated$a_pm25_mean[which(updated$name == click$id)])
    
    updateSliderInput(session, "obesity",
                      value = updated$Adult_Obesity[which(updated$name == click$id)])
    
    updateSliderInput(session, "phys",
                      value = updated$Physical_Inactivity[which(updated$name == click$id)])
  })
}

# Run
shinyApp(ui, server)
