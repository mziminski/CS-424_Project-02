
# libraries to include
# install.packages('leaflet.extras')
# install.packages("shinythemes")
# install.packages("dplyr")
library(purrr)
library(dplyr)
library(shiny)
library(stringr)
library(shinydashboard)
library(shinythemes)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(DT)
# Prevent numbers from going to Scientific Notation
options(scipen = 999)
options(java.parameters = "-Xmx2000m")

# Interested in rows: TYPE OF PRODUCER,ENERGY SOURCE equal to Total Electric Power Industry,Total
# [You] should convert the STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
# setwd("/Users/mziminski/Developer/School Projects/cs424/CS-424_Project-02/Data")
file18 = "egrid2018_data_v2.csv"
file10 = "egrid2010_data.csv"
file00 = "egrid2000_plant.csv"
# read data in as XSLX file then save as dataframe
dfdata_18 <- read.csv(file = file18, sep = ",")
dfdata_10 <- read.csv(file = file10, sep = ",")
dfdata_00 <- read.csv(file = file00, sep = ",")

# setwd("/Users/mziminski/Developer/School Projects/cs424/CS-424_Project-02/")

# set the STATE column to a factor
dfdata_18$STATE <- as.factor(dfdata_18$STATE)
dfdata_10$STATE <- as.factor(dfdata_10$STATE)
dfdata_00$STATE <- as.factor(dfdata_00$STATE)
# Seperate the Non-Decomissioned (currently producing) plants from the "decommissioned" plants
dfdata_18_NonDecom <- dfdata_18 %>% filter(!is.na(NET_GEN))
dfdata_10_NonDecom <- dfdata_10 %>% filter(!is.na(NET_GEN))
dfdata_00_NonDecom <- dfdata_00 %>% filter(!is.na(NET_GEN))
dfdata__18Decom <- dfdata_18 %>% filter(is.na(NET_GEN))
dfdata__10Decom <- dfdata_10 %>% filter(is.na(NET_GEN))
dfdata__00Decom <- dfdata_00 %>% filter(is.na(NET_GEN))

# make state lat/long df to zoom/move to new leaflet state location 
states_latlong <- data.frame(state.abb, state.center)
states_latlong <- rbind(states_latlong, c("US", -95.712891, 37.090240))
# make state name nameset to use for state selection 
states <- setNames(state.abb, state.name)
states <- sort(states)
states["Continential US"] <- "US"
years <- c("2000", "2010", "2018")

# Different categories of fuels: 
# cat1 is all + all individual fuels
# cat2 is renewables/nonrenewables respectivly
cat1 <- c("ALL", "BIOMASS", "COAL", "GAS", "GEOTHERMAL", "HYDRO", "NUCLEAR", "OIL", "OTHER", "SOLAR", "WIND")
cat2 <- data.frame("Renewables"=c("BIOMASS", "GEOTHERMAL", "HYDRO", "SOLAR", "WIND"), 
                   "NonRenewables"=c("COAL", "GAS", "NUCLEAR", "OIL", "OTHER"))

# a list of colors
myColors <- c("#70FFF8", "#628395", "#E7D7C1", "#8E443D", "#035E7B", "#36827F", 
              "#E0E1E1", "#DA4167", "#43BCCD", "#AA2288")
# a function that will return a color based on fuel passed to it, useful for keeping colors of fuels in sync everywhere
pal <- colorFactor(palette=myColors, domain=cat1[-c(1)])
# load different maps by CartoDB, but failed to solve reloading RESET when new fuel category selected
# cartoDB <- grep("^CartoDB", providers, value = TRUE)
# cartoDB[c(2,3,5,6)]

# *** EXTRA and NOT NEEDED ***
# Might be used in future Project, So keeping it here
# extract_fuels_used <- function(dfdata) {
#   # Extract Fuels used for each Powerplant
#   list <- list()
#   for (i in 1:nrow(dfdata)) {
#     a <- sapply(dfdata[i, c(5:14)], function(x) which(x > 0))
#     b <- subset(a, a == 1)
#     e <- ""
#     if (!is_empty(b)) {
#       c <- names(b)
#       # d <- str_replace(c, "_GEN", "")
#       e <- list(d)
#     }
#     
#     list[i] <- e
#   }
#   return(list)
# }

# A function that adds a POP UP description for all of the plants 
plant_desc <- function(dfdata) {
  list <- list()
  for (i in 1:nrow(dfdata)) {
    
    name <- paste("<b>", dfdata[i, "PLANT_NAME"],"</b>")
    # check if the fuel source has a value > 0, if yes format the number, then save the returned string
    coal <- ifelse(dfdata[i, "COAL_GEN"] > 0, paste("<br>Coal:", format(dfdata[i, "COAL_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    oil <- ifelse(dfdata[i, "OIL_GEN"] > 0, paste("<br>Oil:", format(dfdata[i, "OIL_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    gas <- ifelse(dfdata[i, "GAS_GEN"] > 0, paste("<br>Gas:", format(dfdata[i, "GAS_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    nuclear <- ifelse(dfdata[i, "NUCLEAR_GEN"] > 0, paste("<br>Nuclear:", format(dfdata[i, "NUCLEAR_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    hydro <- ifelse(dfdata[i, "HYDRO_GEN"] > 0, paste("<br>Hydro:", format(dfdata[i, "HYDRO_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    biomass <- ifelse(dfdata[i, "BIOMASS_GEN"] > 0, paste("<br>Biomass:", format(dfdata[i, "BIOMASS_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    wind <- ifelse(dfdata[i, "WIND_GEN"] > 0, paste("<br>Wind:", format(dfdata[i, "WIND_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    solar <- ifelse(dfdata[i, "SOLAR_GEN"] > 0, paste("<br>Solar:", format(dfdata[i, "SOLAR_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    geo <- ifelse(dfdata[i, "GEOTHERMAL_GEN"] > 0, paste("<br>Geothermal:", format(dfdata[i, "GEOTHERMAL_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    other <- ifelse(dfdata[i, "OTHER_GEN"] > 0, paste("<br>Other:", format(dfdata[i, "OTHER_GEN"], nsmall=2, big.mark=","), "(kwh)"), "")
    # get the (non)renewable fuel percentages of the total net generation
    pnonrenewables <- paste("<br>Percent Non-Renewables:", dfdata[i, "PERCENT_NONRENEWABLE_GEN"])
    prenewables <- paste("<br>Percent Renewables:", dfdata[i, "PERCENT_RENEWABLE_GEN"])
    
    list[i] <- paste(name, coal, oil, gas, nuclear, hydro, biomass, wind, solar, geo, other, pnonrenewables, prenewables)
  }
  return(list)
}

# Get plant pop-up descriptions
dfdata_18_NonDecom$POP_UP <- plant_desc(dfdata_18_NonDecom)
dfdata_10_NonDecom$POP_UP <- plant_desc(dfdata_10_NonDecom)
dfdata_00_NonDecom$POP_UP <- plant_desc(dfdata_00_NonDecom)

# A function the is used to categorize a number into one of the listed groups,
# Returns a radius number to be used by the leadlef addmarker radius argument
get_radius <- function(x) {
  rad <- 0
  if (x < 1000) {
    rad <- 2
  } else if (x < 10000) {
    rad <- 3
  } else if (x < 100000) {
    rad <- 4
  } else if (x < 1000000) {
    rad <- 5
  } else if (x < 10000000) {
    rad <- 6
  } else if (x < 100000000) {
    rad <- 7
  } else {
    rad <- 8
  }
  return(rad)
}

# A function that creates a leaflet map based on the parameters passed in
# data is the data used,
# input is the 1st set of checkboxes
# input2 is the 2nd set of checkboxes (nonrenewables/renewables)
# state is the state location selected
get_map <- function(data, input, input2, state) {
  state <- states_latlong %>% filter(state.abb == state)
  zoom <- 7
  # set different zoom for when state == continential US
  if ("US" %in% state) {
    zoom <- 4.3
  }
  
  # modify the data to better work with leaflet map
  data <- melt(data, id=c("STATE", "PLANT_NAME", "LAT", "LON", "POP_UP"))
  data <- data %>% filter(value > 0)
  data$radius <- lapply(data$value, get_radius)
  
  # Set up the initial leaflet map, then add on to it
  map <- leaflet(options = leafletOptions(preferCanvas = T)) %>% addTiles() %>%
    fitBounds(0, 0, 0, 0) %>%
    setView(state$x, state$y, zoom = zoom) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter", options = providerTileOptions(
      updateWhenZooming = F,      
      updateWhenIdle = T           
    )) %>% 
    addLegend("bottomright", colors=myColors, labels=cat1[-c(1)], title="Main Fuel Source") %>%
    addResetMapButton()
  
  if (!is_empty(input)) {
    if (input == "ALL") {
      # Add Biomass markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal(cat1[-c(1)]), stroke = F, fillOpacity = 0.5)
      
    } else if (input == "BIOMASS") {
      data <- data %>% filter(variable == "BIOMASS_GEN")
      # Add Biomass markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("BIOMASS"), stroke = F, fillOpacity = 0.5)
    } else if (input == "COAL") {
      data <- data %>% filter(variable == "COAL_GEN")
      # Add Coal markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("COAL"), stroke = F, fillOpacity = 0.5)
    } else if (input == "GAS") {
      data <- data %>% filter(variable == "GAS_GEN")
      # Add Gas markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("GAS"), stroke = F, fillOpacity = 0.5)
    } else if (input == "GEOTHERMAL") {
      data <- data %>% filter(variable == "GEOTHERMAL_GEN")
      # Add Geothermal markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("GEOTHERMAL"), stroke = F, fillOpacity = 0.5)
    } else if (input == "HYDRO") {
      data <- data %>% filter(variable == "HYDRO_GEN")
      # Add Hydro markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("HYDRO"), stroke = F, fillOpacity = 0.5)
    } else if (input == "NUCLEAR") {
      data <- data %>% filter(variable == "NUCLEAR_GEN")
      # Add Nuclear markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("NUCLEAR"), stroke = F, fillOpacity = 0.5)
    } else if (input == "OIL") {
      data <- data %>% filter(variable == "OIL_GEN")
      # Add Oil markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("OIL"), stroke = F, fillOpacity = 0.5)
    } else if (input == "OTHER") {
      data <- data %>% filter(variable == "OTHER_GEN")
      # Add Other markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("OTHER"), stroke = F, fillOpacity = 0.5)
    } else if (input == "SOLAR") {
      data <- data %>% filter(variable == "SOLAR_GEN")
      # Add Solar markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("SOLAR"), stroke = F, fillOpacity = 0.5)
    } else if (input == "WIND") {
      data <- data %>% filter(variable == "WIND_GEN")
      # Add Wind markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal("WIND"), stroke = F, fillOpacity = 0.5)
    }
  } else if (!is_empty(input2)) {
    if (length(input2) != 2 && input2 == "Renewables") {
      data <- data %>% filter(variable == paste(cat2$Renewables, "GEN", sep="_"))
      # Add Renewables markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal(cat2$Renewables), stroke = F, fillOpacity = 0.5)
      
    } else if (length(input2) != 2 && input2 == "NonRenewables") {
      data <- data %>% filter(variable == paste(cat2$NonRenewables, "GEN", sep="_"))
      # Add NonRenewables markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal(cat2$NonRenewables), stroke = F, fillOpacity = 0.5)
    } else {
      # Add All markers
      map <- map %>% addCircleMarkers(data=data, ~LON, ~LAT, popup=~POP_UP, weight = 3, radius=~radius,
                                      color=pal(cat1[-c(1)]), stroke = F, fillOpacity = 0.5)
    }
  }
  
  return(map)
}

# Set up the layout of the UI for part 1
p1_body <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;"),
  h3("Illinois - 2018", style="padding-left:20px;"),
  leafletOutput("part1_map", width = "100%", height = "93.5%"),
  absolutePanel(class = "controls panel panel-default",
                top=75, right=10,
                h4("Energy Sources:"),
                hr(),
                checkboxGroupInput("part1_Select1", NULL,
                                   choiceNames = cat1,
                                   choiceValues = cat1,
                                   selected="ALL",
                ),
                hr(),
                checkboxGroupInput("part1_Select2", NULL,
                                   choiceNames = colnames(cat2),
                                   choiceValues = colnames(cat2),
                ),
  )
)
# Set up the layout of the UI for part 2
p2_body <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;}"),
  column(6, class="containerL",
         fluidRow(class="f-row",
                  column(3, offset=3,
                         selectInput("part2_statesL", label = NULL,
                                     choices = states[c(-1)],
                                     selected = "IL")),
                  column(3, 
                         selectInput("part2_yearsL", label = NULL,
                                     choices = years,
                                     selected = "2000"))
         ),
         leafletOutput("part2_map1", width = "100%", height = "100%"),
         absolutePanel(id="control1", class = "controls panel panel-default",
                       top=85, right=25,
                       h4("Energy Sources:"),
                       hr(),
                       checkboxGroupInput("part2_Select1L", NULL,
                                          choiceNames = cat1,
                                          choiceValues = cat1,
                                          selected="ALL",
                       ),
                       hr(),
                       checkboxGroupInput("part2_Select2L", NULL,
                                          choiceNames = colnames(cat2),
                                          choiceValues = colnames(cat2)
                       ),
         ),
  ),
  
  column(6,  class="containerR",
         fluidRow(class="f-row",
                  column(3, offset=3,
                         selectInput("part2_statesR", label = NULL,
                                     choices = states[c(-1)],
                                     selected = "IL")),
                  column(3, 
                         selectInput("part2_yearsR", label = NULL,
                                     choices = years,
                                     selected = "2018"))
         ),
         leafletOutput("part2_map2", width = "100%", height = "100%"),
         style="height:92%;",
         absolutePanel(class = "controls panel panel-default",
                       top=85, right=25,
                       h4("Energy Sources:"),
                       hr(),
                       checkboxGroupInput("part2_Select1R", NULL,
                                          choiceNames = cat1,
                                          choiceValues = cat1,
                                          selected="ALL",
                       ),
                       hr(),
                       checkboxGroupInput("part2_Select2R", NULL,
                                          choiceNames = colnames(cat2),
                                          choiceValues = colnames(cat2)
                       ),
         )
  ),
  absolutePanel(top=15, left="43%",
                actionButton("sync_btn", "Compare in Sync"))
  
)

# Set up the layout of the UI for part 3
p3_body <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;"),
  fluidRow(class="f-row",
           column(3, offset=3,
                  selectInput("part3_states", label = NULL,
                              choices = states,
                              selected = "US")),
           column(3, 
                  selectInput("part3_years", label = NULL,
                              choices = years,
                              selected = "2018"))
  ),
  leafletOutput("part3_map", width = "100%", height = "93.5%"),
  absolutePanel(class = "controls panel panel-default",
                top=75, right=10,
                h4("Energy Sources:"),
                hr(),
                checkboxGroupInput("part3_Select1", NULL,
                                   choiceNames = cat1,
                                   choiceValues = cat1,
                ),
                hr(),
                checkboxGroupInput("part3_Select2", NULL,
                                   choiceNames = colnames(cat2),
                                   choiceValues = colnames(cat2),
                )
  )
)

# Pull all the above views (body 1-3) into the main nav bar UI
ui <- navbarPage("CS 424 - Project 2", id="nav", theme=shinytheme("cyborg"),
                 tabPanel("About",
                          h1("About this Project"),
                          hr(),
                          p("The original data is from https://www.epa.gov/egrid/download-data  (files used include years 2000, 2010, 2018) and \"heavaly\" modified by myself."),
                          p("The creator of this app is me (Matt Ziminski), I am currently taking CS 424 with with Dr. Andy Johnson."),
                          p("This is 1 of 3 projects done for CS 424, and it was worked on from March 3, 2021 to March 14, 2021."),
                          p("Other Attributes: https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example (for shiny app UI design help and CSS style sheet)")
                          
                 ),
                 tabPanel("Part 1",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              p1_body
                          )
                 ),
                 
                 tabPanel("Part 2",
                          div(class="outer",
                              tags$head(
                                includeScript("jquerry.js"),
                                includeCSS("styles.css")),
                              p2_body
                          )
                 ),
                 
                 tabPanel("Part 3",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              p3_body
                          )
                 )
)

# Here lies the code that handles the interactivity of the shiny app
server = function(input, output) {
  v <- reactiveValues(data=F)
  # Used to keep track of button on/off state for part 2 sync action button
  observeEvent(input$sync_btn, {
    v$data <- !v$data
  })
  # Render/setup the leaflet map for part 1
  output$part1_map <- renderLeaflet({ 
    data <- dfdata_18_NonDecom %>% filter(STATE == "IL")
    get_map(data, input$part1_Select1, input$part1_Select2, "IL")
  })
  # Render/setup the leaflet map for part 2
  output$part2_map1 <- renderLeaflet({ 
    state <- input$part2_statesL
    year <- input$part2_yearsL
    data <- 0
    switch(year,
           "2018" = data <- dfdata_18_NonDecom %>% filter(STATE == state),
           "2010" = data <- dfdata_10_NonDecom %>% filter(STATE == state),
           "2000" = data <- dfdata_10_NonDecom %>% filter(STATE == state))
    
    if (!v$data) {
      get_map(data, input$part2_Select1L, input$part2_Select2L, state)
    } else {
      get_map(data, input$part2_Select1R, input$part2_Select2R, state)
    }
  })
  # Render/setup the leaflet map for part 2
  output$part2_map2 <- renderLeaflet({ 
    state <- input$part2_statesR
    year <- input$part2_yearsR
    data <- 0
    
    switch(year,
           "2018" = data <- dfdata_18_NonDecom %>% filter(STATE == state),
           "2010" = data <- dfdata_10_NonDecom %>% filter(STATE == state),
           "2000" = data <- dfdata_00_NonDecom %>% filter(STATE == state))
    
    get_map(data, input$part2_Select1R, input$part2_Select2R, state)
  })
  
  # Render/setup the leaflet map for part 3
  output$part3_map <- renderLeaflet({ 
    state <- input$part3_states
    year <- input$part3_years
    data <- 0
    if ("US" %in% state) {
      switch(year,
             "2018" = data <- dfdata_18_NonDecom,
             "2010" = data <- dfdata_10_NonDecom,
             "2000" = data <- dfdata_00_NonDecom)
    } else {
      switch(year,
             "2018" = data <- dfdata_18_NonDecom %>% filter(STATE == state),
             "2010" = data <- dfdata_10_NonDecom %>% filter(STATE == state),
             "2000" = data <- dfdata_00_NonDecom %>% filter(STATE == state))
    }
    
    get_map(data, input$part3_Select1, input$part3_Select2, state)
  })
}


# Actually run the Shiny App
shinyApp(ui, server)


