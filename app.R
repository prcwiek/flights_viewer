# Flights viewer over Poland using data from the OpenSky Network
#
# Information about the source of data:
# Matthias Schäfer, Martin Strohmeier, Vincent Lenders, Ivan Martinovic and Matthias Wilhelm.
# "Bringing Up OpenSky: A Large-scale ADS-B Sensor Network for Research".
# In Proceedings of the 13th IEEE/ACM International Symposium on Information Processing
# in Sensor Networks (IPSN), pages 83-94, April 2014.
# The OpenSky Network, https://opensky-network.org
#
# Aircraft database from https://opensky-network.org/aircraft-database

library(crosstalk)
library(dplyr)
library(DT)
library(httr2)
library(jsonlite)
library(leaflet)
library(sf)
library(shiny)
library(shinyjs)
library(stringr)

link <- request("https://opensky-network.org/api/states/all")

# load aircraft list
load("data/aircrafts.rda")

# reference points of Polish borders
n_point <- 54.835778
s_point <- 49.0025
e_point <- 24.15
w_point <- 14.11

# center of Poland
centerPL <- list(lon = w_point + (e_point - w_point) / 2, lat = s_point + (n_point - s_point) / 2)

# columns names for the respons
flights_colnames <- c("icao24", "Call sign", "Origin country", "Time position",
                      "Last contact", "Longitude", "Latitude", "Barometer altitude",
                      "on_ground", "Velocity (km/h)", "true_track", "vertical_rate",
                      "sensors", "Altitude (m)", "squawk")

# define function for leaflet custom legend
addLegendCustom <- function(map, position = "topright", colors, labels, sizes, opacity = 0.4, title = "") {
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px;", "border-radius: 50%;float: left;")
  labelAdditions <- paste0("<div style='display: inline-block;text-align: left;height: ", sizes, "px;margin-top: 2px;line-height: ",
                           sizes, "px;float: left;'>", labels, "</div>")
  return(addLegend(map, position = position, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
}

ui <- fluidPage(

  theme = bslib::bs_theme(version = 3, bootswatch = "superhero"),
  
  shinyjs::useShinyjs(),

  br(),
  
  fluidRow(
    column(3),
    column(6, htmlOutput("about")),
    column(3)
  ),

  hr(),
  
  fluidRow(
    column(12, align = "center", leafletOutput("flightsmap", height=600, width = 600)),
  ),
  
  hr(),
  
  fluidRow(
    column(1),
    column(10, 
           dataTableOutput("flightsoverPL")),
    column(1)
  )
)

server <- function(input, output, session) {

  flights_pl_reactive <- reactive({

    # get new data every 12SP seconds
    invalidateLater(120000, session)
    
    # request all flights
    flights_resp <- req_perform(link)
    
    if (resp_content_type(flights_resp) == "application/json") {
      flights_simpl <- flights_resp %>% resp_body_json(simplifyVector = TRUE)

      flights <- tryCatch({
        flights <- as.data.frame(flights_simpl$states)
        flights <- flights[,1:15]
        colnames(flights) <- flights_colnames
        
        flights$"Time position" <- as.character(as.POSIXct(as.numeric(flights$"Time position"), origin='1970-01-01', tz="UTC"))
        flights$Longitude <- as.numeric(flights$Longitude)
        flights$Latitude <- as.numeric(flights$Latitude)
        flights$"Velocity (km/h)" <- round(as.numeric(flights$"Velocity (km/h)") * 3.6, 0)
        flights$"Altitude (m)" <- round(as.numeric(flights$"Altitude (m)"), 0)
        flights$on_ground <- as.logical(flights$on_ground)
        
        out <- flights %>%
          filter(Longitude > w_point & Longitude < e_point & Latitude < n_point & Latitude > s_point & !on_ground) %>%
          left_join(aircrafts, by = "icao24") %>% 
          mutate(Model = if_else(is.na(Model), "", Model)) %>% 
          select("Time position", "Call sign", Model, "Origin country", Longitude, Latitude, "Velocity (km/h)", "Altitude (m)")
        return(out)
        
      },
        error = function(e) {
          return(NULL)
        }
      )
    } else {
      return(NULL)
    }
    
  })
  
  flights_pl_shared <- SharedData$new(flights_pl_reactive)
  
  output$flightsoverPL <- renderDT({
    if (!is.null(flights_pl_shared) & !is.null(flights_pl_shared$origData())) {
      datatable(flights_pl_shared, style = "bootstrap4",
                options = list(paging = TRUE, pageLength = 25))
    }
  }, server = FALSE)
  
  output$flightsmap <- renderLeaflet({

    if (!is.null(flights_pl_shared) & !is.null(flights_pl_shared$origData())) {
      fcolor <- flights_pl_shared$origData() %>%
        mutate(fcolor = if_else(str_detect(flights_pl_shared$origData()$"Call sign", pattern = "(^LOT|^SP)"), "Blue", "Red")) %>%
        mutate(legend = if_else(fcolor == "Blue", "Poland", "Other country")) %>% 
        select(fcolor)
      
      dfpopup <- flights_pl_shared$origData() %>%
        select("Call sign", Model, "Velocity (km/h)", "Altitude (m)")
      fpopup <- paste0("<b>", dfpopup$"Call sign", "</b><br>",
                       dfpopup$Model, "<br>",
                       "<b>Velocity:</b> ", dfpopup$"Velocity", " km/h<br>",
                       "<b>Altitude:</b> ", dfpopup$"Altitude (m)", " m<br>")
      
      
      leaflet(flights_pl_shared) %>% 
        fitBounds(lng1 = e_point, lat1 = n_point, lng2 = w_point, lat2 = s_point) %>% 
        addCircleMarkers(radius = 4, color = fcolor$fcolor, popup = ~fpopup) %>% 
        addLegendCustom(position = "topright", colors = c("Blue", "Red"), labels = c("Poland", "Other"), sizes = c(15, 15),
                        title = "Origin country:") %>%
        addProviderTiles(providers$OpenStreetMap)
    }
  })
  
  output$about <- renderText({
    paste0('<center><h3>Fligths over Poland using data from the OpenSky Network</h3></center><br>
      <p><u>Information about the source of data:</u><br>
      <p>Matthias Schäfer, Martin Strohmeier, Vincent Lenders, Ivan Martinovic and Matthias Wilhelm.
      "Bringing Up OpenSky: A Large-scale ADS-B Sensor Network for Research".
      In Proceedings of the 13th IEEE/ACM International Symposium on Information Processing
      in Sensor Networks (IPSN), pages 83-94, April 2014.<br>
      The OpenSky Network, <a href="https://opensky-network.org">
      https://opensky-network.org</a><br>
      Aircraft data base from <a href="https://opensky-network.org/aircraft-database">
      https://opensky-network.org/aircraft-database</a>
      ')
  })
  
  # End application when a window or a tab is closed
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
