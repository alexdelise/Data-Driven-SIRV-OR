# Load necessary libraries
library(tigris)
library(spdep)
library(sf)
library(igraph)
library(dplyr)
library(ggplot2)
library(magick)  # Library for creating GIFs

# Load healthcare facility capacity data
capacity_data <- read.csv("C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\inputData\\Capacity.csv", header = FALSE)
colnames(capacity_data) <- c("Capacity")

# Load pairwise distance matrix for counties
distance_data <- as.matrix(read.csv("C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\inputData\\Distance.csv", header = FALSE))

# Load hospitalization data
hospitalization_data <- read.csv("C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\outputData\\hospitalizationData.csv")

# Load travel data
travel_data <- read.csv("C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\outputData\\travelData.csv")

# Load Florida counties shapefile for visualization
options(tigris_use_cache = TRUE)
fl_counties <- as_Spatial(counties("Florida", year = 2018, cb = TRUE))
layout_coordinates <- layout.norm(coordinates(fl_counties))

# Prepare an empty list to store images for the GIF
frames <- list()

# Generate plots for each decision period and add them to the frames list
unique_periods <- sort(unique(travel_data$timePeriod))
for (period in unique_periods) {
  
  # Filter data for the current decision period
  period_travel_data <- travel_data %>% filter(timePeriod == period)
  period_hospitalization_data <- hospitalization_data %>% filter(timePeriod == period)
  
  # Calculate remaining capacity after hospitalized patients for each region
  remaining_capacity <- capacity_data$Capacity - period_hospitalization_data$hospitalized
  
  # Create adjacency matrix for the travel data specific to the current period
  travel_matrix <- matrix(0, nrow = 67, ncol = 67)
  for (i in 1:nrow(period_travel_data)) {
    from <- period_travel_data$fromRegion[i]
    to <- period_travel_data$toFacility[i]
    travel_matrix[from, to] <- period_travel_data$quantity[i]
  }
  
  # Create graph from adjacency matrix for travel data
  g_period <- graph_from_adjacency_matrix(travel_matrix, weighted = TRUE)
  
  # Scale edge widths based on the quantity of people moving
  ewidth <- E(g_period)$weight / max(E(g_period)$weight) * 5  # Adjust scale as needed
  
  # Set vertex sizes based on remaining capacity
  #vertex_sizes <- remaining_capacity / 1000
  
  # Create a plot for the current period and save it to the frames list
  img <- image_graph(width = 700, height = 500, res = 96)  # Open a new plot in memory
  par(mar = c(5, 2, 2, 2))
  plot.igraph(
    g_period,
    #vertex.size = vertex_sizes,
    vertex.label = fl_counties$NAME,
    vertex.label.color = "black",
    vertex.label.cex = 0.5,
    edge.arrow.size = 0.3,
    edge.width = ewidth,
    layout = layout_coordinates,
    edge.color = "red",
    main = paste("Patient Travel for Decision Period", period, "for All Counties")
  )
  dev.off()  # Close the plot
  frames[[length(frames) + 1]] <- img  # Add the image to the frames list
}

# Create and save the GIF
gif <- image_animate(image_join(frames), fps = 1)  # Adjust 'fps' for speed
image_write(gif, path = "C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\allocationPics\\travel_animation.gif")

# Print completion message
cat("GIF creation complete! The file has been saved to allocationPics folder.\n")
