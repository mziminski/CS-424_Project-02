
# libraries to include
install.packages("xlsx")   
install.packages('leaflet.extras')
library("xlsx")  
# install.packages("dplyr")
library(purrr)
library(dplyr)
library(shiny)
library(stringr)
library(shinydashboard)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(DT)
# Prevent numbers from going to Scientific Notation
options(scipen = 999)
options(java.parameters = "-Xmx2000m")

# Interested in rows: TYPE OF PRODUCER,ENERGY SOURCE equal to Total Electric Power Industry,Total
# [You] should convert the STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
setwd("/Users/mziminski/Developer/School Projects/cs424/CS-424_Project-02/Data")
file = "egrid2018_data_v2.xlsx"

dfdata <- xlsx::read.xlsx(file, sheetName="PLNT18", as.data.frame=TRUE)


dfdata$STATE <- as.factor(dfdata$STATE)

dfdata_NonDecom <- dfdata %>% filter(!is.na(NET_GEN))
dfdata_Decom <- dfdata %>% filter(is.na(NET_GEN))

cat1 <- c("ALL", "BIOMASS", "COAL", "GAS", "GEOTHERMAL", "HYDRO", "NUCLEAR", "OIL", "OTHER", "SOLAR", "WIND")
cat2 <- data.frame("Renewables"=c("BIOMASS", "GEOTHERMAL", "HYDRO", "SOLAR", "WIND"), 
                   "NonRenewables"=c("COAL", "GAS", "NUCLEAR", "OIL", "OTHER"))

myColors <- c("#70FFF8", "#628395", "#E7D7C1", "#8E443D", "#035E7B", "#36827F", 
              "#E0E1E1", "#DA4167", "#43BCCD", "#AA2288")

pal <- colorFactor(palette=myColors, domain=cat1[-c(1)])

get_map <- function(data, input, input2) {
  
  map <- leaflet() %>% 
    fitBounds(0, 0, 0, 0) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    setView(-89.398529, 40.633125, zoom = 7) %>%
    addLegend("bottomright", colors=myColors, labels=cat1[-c(1)], title="Main Fuel Source") %>%
    addResetMapButton()
  
  if (!is_empty(input)) {
    if (input == "ALL") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(cat1[-c(1)], "GEN", sep="_"))
      # Add Biomass markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal(cat1[-c(1)]), stroke = F, fillOpacity = 0.5)
      
    } else if (input == "BIOMASS") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$BIOMASS_GEN > 0)
      print(nrow(data))
      # Add Biomass markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("BIOMASS"), stroke = F, fillOpacity = 0.5)
    } else if (input == "COAL") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$COAL_GEN > 0)
      # Add Coal markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("COAL"), stroke = F, fillOpacity = 0.5)
    } else if (input == "GAS") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$GAS_GEN > 0)
      # Add Gas markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("GAS"), stroke = F, fillOpacity = 0.5)
    } else if (input == "GEOTHERMAL") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$GEOTHERMAL_GEN > 0)
      # Add Geothermal markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("GEOTHERMAL"), stroke = F, fillOpacity = 0.5)
    } else if (input == "HYDRO") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$HYDRO_GEN > 0)
      # Add Hydro markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("HYDRO"), stroke = F, fillOpacity = 0.5)
    } else if (input == "NUCLEAR") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$NUCLEAR_GEN > 0)
      # Add Nuclear markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("NUCLEAR"), stroke = F, fillOpacity = 0.5)
    } else if (input == "OIL") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$OIL_GEN > 0)
      # Add Oil markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("OIL"), stroke = F, fillOpacity = 0.5)
    } else if (input == "OTHER") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$OTHER_GEN > 0)
      # Add Other markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("OTHER"), stroke = F, fillOpacity = 0.5)
    } else if (input == "SOLAR") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$SOLAR_GEN > 0)
      # Add Solar markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("SOLAR"), stroke = F, fillOpacity = 0.5)
    } else if (input == "WIND") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(input, "GEN", sep="_")) %>% 
        filter(data$WIND_GEN > 0)
      # Add Wind markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal("WIND"), stroke = F, fillOpacity = 0.5)
    }
  } else if (!is_empty(input2)) {
    if (length(input2) != 2 && input2 == "Renewables") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(cat2$Renewables, "GEN", sep="_"))
      # Add Biomass markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal(cat2$Renewables), stroke = F, fillOpacity = 0.5)
      
    } else if (length(input2) != 2 && input2 == "NonRenewables") {
      data <- select(data, PLANT_NAME, LAT, LON, paste(cat2$NonRenewables, "GEN", sep="_"))
      # Add Caol markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal(cat2$NonRenewables), stroke = F, fillOpacity = 0.5)
    } else {
      data <- select(data, PLANT_NAME, LAT, LON, paste(cat1[-c(1)], "GEN", sep="_"))
      # Add Biomass markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=data$PLANT_NAME, weight = 3, radius=4,
                                      color=pal(cat1[-c(1)]), stroke = F, fillOpacity = 0.5)
    }
  }
  
  return(map)
}

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%; font-color:white;}"),
  leafletOutput("part1_map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                h4("Energy Sources:"),
                hr(),
                checkboxGroupInput("Select_Source1", NULL,
                                   choiceNames = cat1,
                                   choiceValues = cat1,
                                   selected="ALL",
                ), style="width:130px; color:white;",
                hr(),
                checkboxGroupInput("Select_Source2", NULL,
                                   choiceNames = colnames(cat2),
                                   choiceValues = colnames(cat2),
                ), style="width:130px; color:white;"
  )
)

# Here lies the code that handles the interactivity of the shiny app
server = function(input, output) {
  
  output$part1_map <- renderLeaflet({ 
    data <- dfdata_NonDecom %>% filter(STATE == "IL")
    get_map(data, input$Select_Source1, input$Select_Source2)
  })
}



shinyApp(ui, server)










data <- dfdata_NonDecom %>% filter(STATE == "IL")



data <- select(data, PLANT_NAME, LAT, LON, paste(cat2$Renewables, "GEN", sep="_")) 

length(c("1", "2"))


