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
      legend.position = c(0.95, 0.05),
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