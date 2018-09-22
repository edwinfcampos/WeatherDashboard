## Weather & Leaflet app.R ##
# By ecampos.phd@gmail.com
library(shiny)
library(leaflet)
library(shinydashboard)
library(jsonlite)  
library(shinyjs)
library(curl)
library(V8)
# To install dependencies, type in the Console window below
# > install.packages("shinydashboard")
# > install.packages("jsonlite", dependencies = TRUE, repos = "http://cran.rstudio.com/")
# > install.packages("shinyjs") and install.packages("V8")
# -----------------------------------------------------------------------------
# Utility functions 
# -----------------------------------------------------------------------------
jscode <- "shinyjs.refresh = function() { history.go(0); }"  # Needed for refresh button

lon_deg = -87.633966 # Longitude of Merchandise Mart, Chicago IL
lat_deg = 41.888675  # Latitude of Merchandise Mart, Chicago IL

fetch_location_data <- function(lon=-87.633966,lat=41.888675) {
  # Current weather from https://openweathermap.org/current
  query_string = paste("http://api.openweathermap.org/data/2.5/weather?lat=",lat,"&lon=",lon,"&units=metric&APPID=841a277f37fa28ab62457e7b6e03a954",sep="")
  weather <- fromJSON(query_string)
  #forecasts <- fromJSON("https://api.openweathermap.org/data/2.5/forecast?lat=41.888675&lon=-87.633966&units=metric&appid=841a277f37fa28ab62457e7b6e03a954")
  return(weather)
}
  
# Current weather from https://openweathermap.org/current
#weather <- fromJSON("http://api.openweathermap.org/data/2.5/weather?lat=41.888675&lon=-87.633966&units=metric&units=metric&APPID=841a277f37fa28ab62457e7b6e03a954")
#forecasts <- fromJSON("https://api.openweathermap.org/data/2.5/forecast?lat=41.888675&lon=-87.633966&units=metric&appid=841a277f37fa28ab62457e7b6e03a954")
weather_data <- fetch_location_data()
temperature_Celsius <- weather_data$main$temp
weather_description <- weather_data$weather$description
time_calc <- weather_data$dt  #Time of data calculation, unix, UTC
time_UTC <- as.POSIXct(time_calc, origin = "1970-01-01", tz = 'GMT')
time_local <- as.POSIXct(time_calc, origin = "1970-01-01")

# -----------------------------------------------------------------------------
# Dashboard User Interface
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Weather Dashboard"),
  
  dashboardSidebar(
    #sidebarMenu(
      #menuItem("Current location", tabName = "current", icon = icon("crosshairs")),
      #menuItem("Location history", tabName = "history", icon = icon("database"))
    #),
    numericInput('lat', 'Location Latitude (negative south)', lat_deg,
                   min = 0, max = 90),
    
    numericInput('lon', 'Location Longitude (negative west)', lon_deg,
                   min = 0, max = 180),
    
    # Refresh button
    useShinyjs(),
    extendShinyjs(text = jscode),
    div(style = "padding-left: 30px; padding-top: 20px;",
        actionButton(class = "btn-sm", "refresh",
                     tagList(icon("refresh"), "Reset Weather Data")
        )
    ),
    
    div(style = "padding-left: 15px; padding-top: 40px;",
        p(class = "small", "Made with ",
          a("R", href = "http://www.r-project.org/"),
          ", ",
          a("Shiny", href = "http://shiny.rstudio.com/"),
          ", ",
          a("shinydashboard", href = "http://rstudio.github.io/shinydashboard/"),
          ", ",
          "& leaflet",
          a("(1)", href = "http://leafletjs.com/"),
          " ",
          a("(2)", href = "http://rstudio.github.io/leaflet/")
        ),
        
        p(class = "small", "Weather Data from ",
          a("OpenWeatherMap", href="https://openweathermap.org")
        ),
        
        p(class = "small",
          a("Source code", href = "https://github.com/edwinfcampos/WeatherDashboard")
        )
    )
  ),
  
  dashboardBody(
    fluidRow(
      infoBoxOutput("temp_box"),
      infoBoxOutput("condition_box"),
      infoBoxOutput("time_box")
    ),
    fluidRow(
      box(width = 12, title = "Area of Interest", leafletOutput("map", height = 500)
      )
    )
  )
#  dashboardBody(leafletOutput("map"))
  
)

#  sliderInput("n", "Number of points:",
#              min = 10, max = 500, value = 100),
#  plotOutput("distPlot")
#)

# -----------------------------------------------------------------------------
# Dashboard server code
# -----------------------------------------------------------------------------
server <- function(input, output) {
  # Create a reactive object to simultaneously update the renders below
  #weather_data <- reactive({
  #  fetch_location_data(lon=input$lon,lat=input$lat)
  #})
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(input$lon, input$lat, zoom = 12) %>%
      addPopups(input$lon, input$lat, 'Here is the <b>Location</b> selected')
  })
  
  output$temp_box <- renderInfoBox({
    weather_data <- fetch_location_data(lon=input$lon,lat=input$lat)
    temperature_Celsius <- weather_data$main$temp
    
    infoBox(
      "Temperature ",
      color = "green",
      #icon = icon("bicycle"),
      value = temperature_Celsius,
      " Celsius"
    )
  })
  
  output$condition_box <- renderInfoBox({
    weather_data <- fetch_location_data(lon=input$lon,lat=input$lat)
    weather_description <- weather_data$weather$description
    
    infoBox(
      "Condition ",
      color = "blue",
      icon = icon("arrows"),
      value = weather_description
    )
  })
  
  output$time_box <- renderInfoBox({
    weather_data <- fetch_location_data(lon=input$lon,lat=input$lat)
    time_calc <- weather_data$dt  #Time of data calculation, unix, UTC
    time_UTC <- as.POSIXct(time_calc, origin = "1970-01-01", tz = 'GMT')
    time_local <- as.POSIXct(time_calc, origin = "1970-01-01")
    
    infoBox(
      "Local Time ",
      color = "red",
      icon = icon("compass"),
      value = time_local
    )
  })
  
  # Map click
  #input$map_click is an event that is sent when the map background or basemap is clicked. The value is a list with lat and lng.
  observeEvent(input$mymap_click,{
    print(input$mymap_click)
  })
  
  # Reset button
  observeEvent(input$refresh, {
    js$refresh();
    }
  )
  
}


# -----------------------------------------------------------------------------

shinyApp(ui, server)


