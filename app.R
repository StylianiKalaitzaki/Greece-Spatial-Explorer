library(shiny)
library(sf)
library(terra)
library(dplyr)

# Load the raster file
raster_file = rast("data/Basemap.tif")

# Read shapefiles
nomoi <- st_read("data/GRC_ADM2/GRC_adm2.shp")
poleis <- st_read("data/poleis/poleis.shp")


# Get the bounding box of Greece
bbox_greece <- st_bbox(nomoi)

# Crop raster to the bounding box of Greece
raster_greece <- crop(raster_file, bbox_greece)

# Mask raster using the geometry of Greece
raster_greece <- mask(raster_greece, nomoi)

# Transform the coordinate reference system (CRS) of poleis to match raster's
poleis <- st_transform(poleis, crs(raster_greece))

# Extract elevation values from the raster map at the locations of cities
elev_values <- extract(raster_greece, poleis)

# Add elevation values in poleis_latlon
poleis$elevation <- elev_values

# Find the maximum and minimum elevation values for the cities
max_city_elev <- max(poleis$elevation)
min_city_elev <- min(poleis$elevation)



# Extract elevation values for each region
elev_values <- extract(raster_greece, nomoi)

# Factorize the ID column to perform a group by on it
elev_values$ID <- factor(elev_values$ID)


# Rename the elevation column
elev_values <- elev_values %>%
  rename_at(vars(2), ~ "elevation")

# Group by the "ID" column and Summarize the grouped data (calculate mean and sd)
elev_values <- elev_values %>% group_by(ID) %>% summarize(mean_value = mean(elevation),sd_value = sd(elevation))


# Add mean and sd to the original nomoi dataset
nomoi$mean_elevation <- elev_values[[2]]
nomoi$sd_elev <- elev_values[[3]]

# Find the maximum and minimum mean elevation values for the regions
max_region_elev <- ceiling(max(nomoi$mean_elevation))
min_region_elev <- floor(min(nomoi$mean_elevation))

# Find the maximum and std elevation values for the regions
max_region_std <- ceiling(max(nomoi$sd_elev))
min_region_std <- floor(min(nomoi$sd_elev))

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Homework 5"),
  
  sidebarLayout
  (
    sidebarPanel
    ( 
      # Slider input for specifying the range of city elevations
      sliderInput
      (
        "city_elevation", 
        label = "City elevation range:",
        min = min_city_elev, max = max_city_elev, value = c(min_city_elev, max_city_elev)
      ),
      
      # Slider input for specifying the range of region mean elevations
      sliderInput
      (
        "region_elevation", 
        label = "Region mean elevation range:",
        min = min_region_elev, max = max_region_elev, value = c(min_region_elev, max_region_elev)
      ),
      
      # Slider input for specifying the range of region standard deviations of elevation
      sliderInput
      (
        "region_sd_elev", 
        label = "Region STD of elevation range:",
        min = min_region_std, max = max_region_std, value = c(min_region_std, max_region_std)
      )
    ),
    
    # Right main panel for displaying the plot
    mainPanel
    (
      plotOutput("map", height = "800px") 
    )
  )
)


# Define a Shiny server
server <- function(input, output) 
{
  # Define a reactive expression to filter regions based on user input for elevation
  regions <- reactive({
    # Ensure that input$region_elevation is available
    req(input$region_elevation)
    # Subset regions based on specified elevation criteria
    subset(nomoi, mean_elevation >= input$region_elevation[1] & mean_elevation <= input$region_elevation[2] &
             sd_elev >= input$region_sd_elev[1] & sd_elev <= input$region_sd_elev[2])
  })
  
  # Define a reactive expression to mask the raster_greece using the filtered regions
  raster <- reactive({mask(raster_greece, regions())})
  
  # Define a reactive expression to filter cities based on user input for elevation
  cities <-   reactive({
    # Ensure that input$city_elevation is available
    req(input$city_elevation)
    # Subset cities based on specified elevation criteria
    subset(poleis, elevation[2] >= input$city_elevation[1] & elevation[2] <= input$city_elevation[2])
    })
    
  # Define the rendering of the plot for the Shiny app
  output$map <- renderPlot({
    # Ensure that regions(), raster(), and cities() are available
    req(regions(), raster(), cities())

    # Plot the raster map
    plot(raster(), col = terrain.colors(6), reset = FALSE)
    # Overlay the boundaries of filtered regions on the plot
    plot(regions()["geometry"],add = TRUE,axes = TRUE)

    # Add red dots for the locations of filtered cities on the plot
    plot(cities()[0], add = TRUE, col = "red", pch = 16, cex = 1.5)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)