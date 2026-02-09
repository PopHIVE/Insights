# PopHIVE Style Guide


# Created to ensure brand identity and visual consistency across PopHIVE products.

library(tidyverse)
library(ggplot2)
library(showtext)
library(sf)
library(scales)
library(ggstar)
library(maps)

# 1. Colors 

#Main:  
# Navy "#00356b"
# White: "#ffffff" 
# Light blue: "#bedeff"  

#Secondary:  
# Lime green: "#d6ef4a"  
# Orange: "#ffc232"  
# Pink: "#fecdd0"
# Purple: "#eadafa"

# 2. Fonts

# Load Google Fonts
font_add_google("Source Serif 4", # name in fonts.google.com
                "source_serif")   # name in session
font_add_google("Rubik",          # name in fonts.google.com
                "rubik")          # name in session
showtext_auto()



## 3. Plot Aesthetics

### Theme for Choropleth Maps

theme_pophive_map <- function() {
  theme_void() +
    theme(
      # Title styling
      plot.title = element_text(
        family = "source_serif",
        color = "#00356b",
        size = 18,
        face = "plain",
        margin = margin(b = 5),
        hjust=0
      ),
      plot.title.position = "plot",
      
      # Subtitle styling
      plot.subtitle = element_text(
        family = "rubik",
        color = "gray30",
        size=10,
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # Caption styling
      plot.caption = element_text(
        family = "rubik",
        color = "gray",
        size = 10,
        hjust = 0,
        margin = margin(t = 10)
      ),
      
      # Margins
      plot.margin = margin(20, 10, 10, 10, "pt"),
      
      # Legend positioning and styling
      legend.position = "right",
      legend.justification = c(1, 0),
      legend.direction = "horizontal",
      
      # Legend title
      legend.title = element_text(
        family = "rubik",
        color = "gray30",
        size = 10
      ),
      
      # Legend box styling
      legend.background = element_rect(
        fill = "white",
        color = "gray",
        linewidth = 0.5
      ),
      # legend.box.background = element_rect(
      #   fill = "white",
      #   color = "gray",
      #   linewidth = 0.5
      # ),
      
      # Legend key styling for horizontal bar
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.3, "cm"),
      
      # Legend margins
      # legend.box.margin = margin(5, 5, 5, 5),
      legend.margin = margin(5, 5, 5, 5),
      
      # Transparent background
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}

# Helper function to add map-specific elements
scale_fill_pophive_map <- function(legend_title = "legend title", ...) {
  list(
    scale_fill_viridis_c(
      option = "magma",
      direction=-1,
      name = legend_title,
      na.value = "transparent",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(4, "cm"),
        barheight = unit(0.4, "cm"),
        frame.colour = "gray",
        ticks.colour = "gray"
      ),
      ...
    )
  )
}




### Theme for Line Graphs

theme_pophive_line <- function() {
  theme_minimal() +
    theme(
      # Title styling
      plot.title = element_text(
        family = "source_serif",
        color = "#00356b",
        size = 18,
        face = "plain",
        margin = margin(b = 5),
        hjust=0
      ),
      plot.title.position = "plot",
      
      # Subtitle styling
      plot.subtitle = element_text(
        family = "rubik",
        color = "gray30",
        hjust = 0,
        size=10,
        margin = margin(b = 10)
      ),
      
      # Caption styling
      plot.caption = element_text(
        family = "rubik",
        color = "gray",
        size = 10,
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.caption.position = "plot",
      
      # White background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(20, 10, 10, 10, "pt"),
      
      # Panel grid styling - dotted horizontal lines only
      panel.grid.major.y = element_line(color = "gray", linetype = "dashed", linewidth = 0.2),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      # Y axis styling
      axis.text.y = element_text(
        family = "rubik",
        color = "#00356b",
        size = 10,
        hjust = 0,
        margin = margin(r = 5)
      ),
      axis.title.y = element_text(
        family = "rubik",
        color = "gray30",
        size = 10,
        angle = 90,
        margin = margin(r = 10)
      ),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      # X axis styling
      axis.text.x = element_text(
        family = "rubik",
        color = "#00356b",
        size = 10,
        margin = margin(t = 5)
      ),
      axis.title.x = element_text(
        family = "rubik",
        color = "gray30",
        size = 10,
        margin = margin(t = 10)
      ),
      axis.line.x = element_line(color = "gray", linewidth = 0.5),
      axis.ticks.x = element_line(color = "gray"),
      
      # Legend styling - top position, horizontal
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(
        family = "rubik",
        color = "#00356b",
        size = 10,
        margin = margin(r = 15)
      ),
      legend.key = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.margin = margin(b = 5)
    )
}

# Helper function for line colors and types
scale_color_pophive_line <- function(...) {
  scale_color_manual(
    values = c("#000004", "#29125a", "#6b1d80", "#ad347c", "#e95661", "#fe9e6d"),
    ...
  )
}

scale_linetype_pophive_line <- function(...) {
  scale_linetype_manual(
    values = c("solid", "dashed", "solid", "dashed", "solid", "dashed"),
    ...
  )
}



### Theme for Dot Plots

theme_pophive_dot <- function() {
  theme_minimal() +
    theme(
      # Title styling
      plot.title = element_text(
        family = "source_serif",
        color = "#00356b",
        size = 18,
        face = "plain",
        margin = margin(b = 5),
        hjust=0
      ),
      plot.title.position = "plot",
      
      # Subtitle styling
      plot.subtitle = element_text(
        family = "rubik",
        color = "gray30",
        size=10,
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # Caption styling
      plot.caption = element_text(
        family = "rubik",
        color = "gray",
        size = 10,
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.caption.position = "plot",
      
      # White background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(20, 10, 10, 10, "pt"),
      
      # Panel grid styling - dotted horizontal lines only
      panel.grid.major.y = element_line(color = "gray", linetype = "dotted", linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      # Y axis styling (categories)
      axis.text.y = element_text(
        family = "rubik",
        color = "#00356b",
        size = 12,
        hjust = 1,
        margin = margin(r = 10)
      ),
      axis.title.y = element_text(
        family = "rubik",
        color = "gray30",
        size = 10,
        angle = 90,
        margin = margin(r = 10)
      ),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      # X axis styling (values)
      axis.text.x = element_text(
        family = "rubik",
        color = "#00356b",
        size = 12,
        margin = margin(t = 5)
      ),
      axis.title.x = element_text(
        family = "rubik",
        color = "gray30",
        size = 10,
        margin = margin(t = 10)
      ),
      axis.line.x = element_blank(),
      axis.ticks.x = element_line(color = "gray"),
      
      # No legend for basic dot plot
      legend.position = "none"
    )
}



### Theme for bar charts

theme_pophive_col <- function() {
  theme_minimal() +
    theme(
      # Title styling
      plot.title = element_text(
        family = "source_serif",
        color = "#00356b",
        size = 18,
        face = "plain",
        margin = margin(b = 5),
        hjust=0
      ),
      plot.title.position = "plot",
      
      # Subtitle styling
      plot.subtitle = element_text(
        family = "rubik",
        color = "gray30",
        size=10,
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # Caption styling
      plot.caption = element_text(
        family = "rubik",
        color = "gray",
        size = 10,
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.caption.position = "plot",
      
      # White background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(20, 10, 10, 10, "pt"),
      
      # Panel grid styling - dotted horizontal lines only
      panel.grid.major.y = element_line(color = "gray", linetype = "dotted", linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      # Y axis styling
      axis.text.y = element_text(
        family = "rubik",
        color = "#00356b",
        size = 12,
        hjust = 0,
        margin = margin(r = 5)
      ),
      axis.title.y = element_text(
        family = "rubik",
        color = "gray30",
        size = 10,
        angle = 90,
        margin = margin(r = 10)
      ),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      # X axis styling
      axis.text.x = element_text(
        family = "rubik",
        color = "#00356b",
        size = 12,
        margin = margin(t = 5)
      ),
      axis.title.x = element_text(
        family = "rubik",
        color = "gray30",
        size = 10,
        margin = margin(t = 10)
      ),
      axis.line.x = element_line(color = "gray", linewidth = 0.5),
      axis.ticks.x = element_line(color = "gray"),
      
      # Legend styling - top position, horizontal
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(
        family = "rubik",
        color = "#00356b",
        size = 12,
        margin = margin(r = 15)
      ),
      legend.key = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.margin = margin(b = 5)
    )
}

# Helper function for column chart colors
scale_fill_pophive_col <- function(...) {
  scale_fill_manual(
    values = c("#29125a", "#6b1d80", "#ad347c", "#e95661", "#fe9e6d"),
    ...
  )
}


### Theme for bubble maps

theme_pophive_bubble_map <- function() {
  theme_void() +
    theme(
      # Title styling
      plot.title = element_text(
        family = "source_serif",
        color = "#00356b",
        size = 18,
        face = "plain",
        margin = margin(b = 5)
      ),
      plot.title.position = "plot",
      
      # Subtitle styling
      plot.subtitle = element_text(
        family = "rubik",
        color = "gray30",
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # Caption styling
      plot.caption = element_text(
        family = "rubik",
        color = "gray",
        size = 10,
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.caption.position = "plot",
      
      # Margins
      plot.margin = margin(20, 10, 10, 10, "pt"),
      
      # Legend positioning and styling
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      
      # Legend title
      legend.title = element_text(
        family = "rubik",
        color = "gray30",
        size = 10
      ),
      
      # Legend text
      legend.text = element_text(
        family = "rubik",
        color = "gray30",
        size = 8
      ),
      
      # Legend box styling
      legend.background = element_rect(
        fill = "white",
        color = "gray",
        linewidth = 0.5
      ),
      
      # Legend key styling
      legend.key = element_rect(color = "gray", linewidth = 0.3),
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.3, "cm"),
      
      # Legend margins
      legend.box.margin = margin(5, 5, 5, 5),
      legend.margin = margin(5, 5, 5, 5),
      legend.box.spacing = unit(0.3, "cm"),
      
      # Transparent background
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}

# Helper function for continuous color scale on bubble maps
scale_color_pophive_bubble_continuous <- function(legend_title = "legend title", ...) {
  scale_color_viridis_c(
    option = "magma",
    name = legend_title,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(3, "cm"),
      barheight = unit(0.4, "cm"),
      frame.colour = "gray",
      ticks.colour = "gray",
      order = 1
    ),
    ...
  )
}

# Helper function for discrete color scale on bubble maps
scale_color_pophive_bubble_discrete <- function(legend_title = "legend title", ...) {
  scale_color_manual(
    values = c("#29125a", "#6b1d80", "#ad347c", "#e95661", "#fe9e6d"),
    name = legend_title,
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      order = 1
    ),
    ...
  )
}

# Helper function for bubble size scale
scale_size_pophive_bubble <- function(legend_title = "Size", range = c(1, 10), ...) {
  scale_size_continuous(
    name = legend_title,
    range = range,
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      order = 2,
      override.aes = list(alpha = 0.6)
    ),
    ...
  )
}

# Helper function to prepare US map data with state boundaries
get_us_map_data <- function(level = "state") {
  library(sf)
  library(maps)
  
  if (level == "state") {
    # Get state boundaries
    us_states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
    return(us_states)
  } else if (level == "county") {
    # Get county boundaries
    us_counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
    return(us_counties)
  }
}

# Helper function to convert state/city data to coordinates for bubble placement
# This function helps position bubbles on the map
prepare_bubble_data <- function(data, location_col = "location", value_col = "value") {
  library(dplyr)
  library(sf)
  
  # This is a simple example - you may need to geocode your locations
  # or join with a dataset that has coordinates
  
  # Example state centroids (you can expand this)
  state_centroids <- data.frame(
    location = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                 "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                 "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                 "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
    longitude = c(-86.9023, -152.4044, -111.4312, -92.3731, -119.4179,
                  -105.5478, -72.7554, -75.5277, -81.5158, -83.3431,
                  -157.4983, -114.7420, -89.6501, -86.2604, -93.0977,
                  -98.4842, -84.6701, -91.8749, -69.3819, -76.6413,
                  -71.5301, -84.5361, -94.6859, -89.6678, -92.6034,
                  -110.3626, -99.9018, -117.0554, -71.5724, -74.4057,
                  -106.2371, -74.9179, -79.8064, -100.7837, -82.9071,
                  -97.5164, -120.5542, -77.1945, -71.4774, -80.9066,
                  -100.2263, -86.3505, -99.9018, -111.8910, -72.5778,
                  -78.6569, -120.7401, -80.4549, -89.6385, -107.2903),
    latitude = c(32.3182, 64.2008, 34.0489, 34.7465, 36.7783,
                 39.1130, 41.6032, 38.9108, 27.6648, 32.1574,
                 20.7967, 44.0682, 40.6331, 40.2672, 41.8780,
                 38.5266, 37.8393, 30.9843, 45.2538, 39.0458,
                 42.4072, 44.3148, 46.7296, 32.3547, 38.5767,
                 46.8797, 41.4925, 38.8026, 43.1939, 40.0583,
                 34.5199, 43.2994, 35.7596, 47.5515, 40.4173,
                 35.4676, 43.8041, 41.2033, 41.5801, 33.8361,
                 44.3683, 35.5175, 31.9686, 39.3210, 44.5588,
                 37.4316, 47.7511, 38.5976, 43.7844, 43.0759)
  )
  
  # Join data with centroids
  result <- data %>%
    left_join(state_centroids, by = setNames("location", location_col))
  
  # Convert to sf object
  result_sf <- st_as_sf(result, coords = c("longitude", "latitude"), crs = 4326)
  
  return(result_sf)
}

# For county-level bubble maps

get_county_centroids <- function() {
  # Get county boundaries from maps package
  counties_map <- maps::map("county", plot = FALSE, fill = TRUE)
  
  # Convert to sf object
  counties_sf <- st_as_sf(counties_map)
  
  # Fix any invalid geometries
  counties_sf <- st_make_valid(counties_sf)
  
  # Calculate centroids
  counties_sf$centroid <- st_centroid(counties_sf$geom)
  
  # Extract coordinates
  coords <- st_coordinates(counties_sf$centroid)
  
  # Create a clean dataframe
  county_centroids <- data.frame(
    county_name = counties_sf$ID,
    longitude = coords[, "X"],
    latitude = coords[, "Y"]
  )
  
  # Split the county_name into state and county
  # Format is "state,county" (e.g., "california,los angeles")
  county_centroids <- county_centroids %>%
    mutate(
      state = tools::toTitleCase(gsub(",.*", "", county_name)),
      county = tools::toTitleCase(gsub(".*,", "", county_name)),
      county_state = paste0(county, ", ", state)
    )
  
  return(county_centroids)
}

# Helper Function: Prepare County Bubble Data

prepare_county_bubble_data <- function(data, 
                                       county_col = "county", 
                                       state_col = "state",
                                       fips_col = NULL) {
  
  # Get county centroids
  county_centroids <- get_county_centroids()
  
  if (!is.null(fips_col)) {
    # If FIPS codes are provided, we need to join differently
    # Load the county.fips dataset
    data("county.fips", package = "maps")
    
    # Prepare FIPS lookup with proper formatting
    county_fips_lookup <- county.fips %>%
      mutate(
        # Extract state and county from polyname
        state_lower = gsub(",.*", "", polyname),
        county_lower = gsub(".*:", "", gsub(".*,", "", polyname)),
        state = tools::toTitleCase(state_lower),
        county = tools::toTitleCase(county_lower),
        fips_char = sprintf("%05d", fips)
      ) %>%
      # Remove duplicates (some counties have multiple polygons)
      distinct(fips_char, .keep_all = TRUE)
    
    # Ensure the FIPS column in data is character and 5 digits
    data <- data %>%
      mutate(!!fips_col := sprintf("%05d", as.numeric(!!sym(fips_col))))
    
    # Join data with FIPS lookup
    data <- data %>%
      left_join(county_fips_lookup, by = setNames("fips_char", fips_col))
    
    # Now join with centroids
    result <- data %>%
      left_join(county_centroids, by = c("state", "county"))
    
  } else {
    # Join using county and state names
    result <- data %>%
      left_join(county_centroids, 
                by = setNames(c("county", "state"), c(county_col, state_col)))
  }
  
  # Remove rows with missing coordinates
  result <- result %>%
    filter(!is.na(longitude) & !is.na(latitude))
  
  # Convert to sf object with turn off s2 to avoid geometry errors
  sf_use_s2(FALSE)
  result_sf <- st_as_sf(result, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326)
  sf_use_s2(TRUE)
  
  return(result_sf)
}

# Helper Function: Get County Map Data

get_county_map_data <- function(states = NULL) {
  # Turn off s2 to avoid geometry errors
  sf_use_s2(FALSE)
  
  # Get all county boundaries
  counties_map <- maps::map("county", plot = FALSE, fill = TRUE)
  counties_sf <- st_as_sf(counties_map)
  
  # Fix invalid geometries
  counties_sf <- st_make_valid(counties_sf)
  
  if (!is.null(states)) {
    # Filter to specific states if provided
    # Convert state names to lowercase for matching
    states_lower <- tolower(states)
    
    counties_sf <- counties_sf %>%
      mutate(state_name = tolower(gsub(",.*", "", ID))) %>%
      filter(state_name %in% states_lower)
  }
  
  sf_use_s2(TRUE)
  return(counties_sf)
}

# Helper Function: Get State Boundaries for Overlay (FIXED)

get_state_boundaries <- function() {
  sf_use_s2(FALSE)
  states_map <- maps::map("state", plot = FALSE, fill = TRUE)
  states_sf <- st_as_sf(states_map)
  states_sf <- st_make_valid(states_sf)
  sf_use_s2(TRUE)
  return(states_sf)
}
