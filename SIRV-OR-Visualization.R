# Load necessary libraries
library(readxl)  # For reading Excel files
library(igraph)
library(dplyr)
library(magick)  # For creating GIFs
library(tigris)
library(sf)
library(sp)

#--- Load the data ---
results_data <- read_excel(
  "C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\outputData\\results558.xlsx", 
  sheet = "z"
)  # Update with your file path

# Rename columns
colnames(results_data)[1:2] <- c("fromRegion", "toRegion")

# Convert fromRegion and toRegion to numeric
results_data$fromRegion <- as.numeric(as.character(results_data$fromRegion))
results_data$toRegion   <- as.numeric(as.character(results_data$toRegion))

# Replace NA or empty cells in all day columns with 0
results_data <- results_data %>%
  mutate(across(-c(fromRegion, toRegion), ~ ifelse(is.na(.), 0, .)))

# Debug: look at data
print(head(results_data))

#--- Load and prepare Florida counties ---
options(tigris_use_cache = TRUE)
fl_counties_sf <- counties("Florida", year = 2018, cb = TRUE)  # sf object
fl_counties    <- as(fl_counties_sf, "Spatial")                # convert to sp

# We want a consistent layout of all 67 counties
# so we do not slice the layout by 'max_region'.
layout_coordinates <- layout.norm(coordinates(fl_counties))  # normalizes bounding box

# Number of Florida counties
n_counties <- 67  # Florida has 67 counties

# Make sure fl_counties is indeed length 67
stopifnot(length(fl_counties) == n_counties)

#--- Prepare for visualization ---
day_columns <- colnames(results_data)[3:ncol(results_data)]  # The day columns
frames      <- list()
output_path <- "C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\allocationPics\\"

for (day in day_columns) {
  # Extract the data for this day
  day_results <- results_data %>%
    select(fromRegion, toRegion, all_of(day)) %>%
    rename(z = all_of(day)) %>%
    filter(z > 0)  # Only keep rows with positive transfers
  
  # If no transfers, skip creating a plot
  if (nrow(day_results) == 0) next
  
  # Build a 67 x 67 adjacency matrix
  travel_matrix <- matrix(0, nrow = n_counties, ncol = n_counties)
  
  # Populate the matrix with the day’s transfers
  for (i in seq_len(nrow(day_results))) {
    from_idx <- day_results$fromRegion[i]
    to_idx   <- day_results$toRegion[i]
    # Make sure these are valid in [1..67]
    if (!is.na(from_idx) && !is.na(to_idx) &&
        from_idx >= 1 && from_idx <= n_counties &&
        to_idx   >= 1 && to_idx   <= n_counties) {
      travel_matrix[from_idx, to_idx] <- day_results$z[i]
    }
  }
  
  # Create the igraph object
  g_day <- graph_from_adjacency_matrix(
    travel_matrix,
    weighted = TRUE,
    mode = "directed"
  )
  
  # Scale edge widths based on travel quantity
  ewidth <- E(g_day)$weight / max(E(g_day)$weight, na.rm = TRUE) * 5
  
  # Create the plot for the current day
  img <- image_graph(width = 5000, height = 5000, res = 512)
  par(mar = c(5, 2, 2, 2))
  
  plot.igraph(
    g_day,
    vertex.size       = 0,  # no node circles
    vertex.label      = fl_counties$NAME,
    vertex.label.color = "black",
    vertex.label.cex  = 0.8,
    edge.arrow.size   = 0.3,
    edge.width        = ewidth,
    edge.color        = "blue",
    layout            = layout_coordinates,
    main              = paste("Patient Travel on Day", day)
  )
  
  dev.off()
  
  # Save the daily image
  day_image_path <- file.path(output_path, paste0("\travel_day_", day, ".png"))
  image_write(img, path = day_image_path)
  message("Saved image for Day: ", day, " at ", day_image_path)
  
  # Collect frames for GIF
  frames[[length(frames) + 1]] <- img
}

#--- Create and save the GIF ---
gif <- image_animate(image_join(frames), fps = 1)  # 1 frame per second
gif_path <- file.path(output_path, "\travel_animation_TEST.gif")
image_write(gif, path = gif_path)

cat("GIF and individual images created successfully!\n",
    "Files saved to:", output_path, "\n")

