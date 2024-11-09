# Load necessary libraries
library(tigris)
library(spdep)
library(sf)
library(igraph)
library(readxl)
library(dplyr)
library(ggplot2)

# Set the target county for visualization (e.g., county 1)
target_county <- 1  # Replace with the desired county ID

# Load healthcare facility capacity data
capacity_data <- read.csv("path/to/Capacity.csv", header = FALSE)
colnames(capacity_data) <- c("Capacity")

# Load pairwise distance matrix for counties
distance_data <- as.matrix(read.csv("path/to/Distance.csv", header = FALSE))

# Load weekly vaccine summary data
vaccine_summary <- read.csv("path/to/Florida_Weekly_Vaccine_Summary.csv")

# Load hospital bed data and add 'Date' and 'Weeks' column for consistency
df <- read.csv("C:/path/to/Hospital_Capacity_Merged_dataframes.csv")
df$Date <- as.Date(gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", df$Date), format = "%Y-%m-%d")
df <- df %>%
  mutate(Weeks = as.numeric(difftime(Date, as.Date("2022-10-12"), units = "weeks")))

# Calculate hospitalized patients based on bed usage
df <- df %>%
  mutate(Hospitalized_Patients = (PctBeds * (Staffed.All.Beds + Staffed.ICU.Beds)))

# Load shape file for Florida counties
options(tigris_use_cache = TRUE)
fl_counties <- as_Spatial(counties("Florida", year = 2018, cb = TRUE))
layout_coordinates <- layout.norm(coordinates(fl_counties))

# Load travel and vaccine allocation data
travel_data <- read.csv("path/to/travelData.csv")
vaccine_allocation_data <- read.csv("path/to/vaccineAllocationData.csv")

# Filter travel data for the target county and across all decision periods
target_travel_data <- travel_data %>% filter(fromRegion == target_county)
target_vaccine_data <- vaccine_allocation_data %>% filter(region == target_county)

# Calculate remaining capacity after hospitalized patients
remaining_capacity <- capacity_data$Capacity - df$Hospitalized_Patients

# Generate plots for each decision period
unique_periods <- unique(travel_data$timePeriod)
for (period in unique_periods) {
  
  # Filter data for the current decision period
  period_travel_data <- target_travel_data %>% filter(timePeriod == period)
  period_vaccine_data <- target_vaccine_data %>% filter(timePeriod == period)
  
  # Create adjacency matrix for the travel data specific to the current period
  travel_matrix <- matrix(0, nrow = 67, ncol = 67)
  for (i in 1:nrow(period_travel_data)) {
    from <- period_travel_data$fromRegion[i]
    to <- period_travel_data$toFacility[i]
    travel_matrix[from, to] <- period_travel_data$quantity[i]
  }
  
  # Create graph from adjacency matrix for travel data
  g_period <- graph_from_adjacency_matrix(travel_matrix, weighted = TRUE)
  ewidth <- E(g_period)$weight / 1500  # Scale edge width
  
  # Plot for travel data in the current period
  png(file = paste0("path/to/output/travel_period_", period, "_county_", target_county, ".png"),
      width = 7, height = 5, units = 'in', res = 600)
  par(mar = c(5, 2, 2, 2))
  plot.igraph(
    g_period,
    vertex.size = (remaining_capacity + abs(min(remaining_capacity))) / 100,
    vertex.label = fl_counties$NAME,
    vertex.label.color = "black",
    vertex.label.cex = 0.5,
    edge.arrow.size = 0.3,
    edge.width = ewidth,
    layout = layout_coordinates,
    edge.color = "red",
    main = paste("Patient Travel for Decision Period", period, "in County", target_county)
  )
  dev.off()
  
  # Plot for vaccine allocation data in the current period
  if (nrow(period_vaccine_data) > 0) {
    # Plotting bar chart for allocated vaccines for the current period
    ggplot(period_vaccine_data, aes(x = as.factor(timePeriod), y = allocatedVaccines)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(title = paste("Vaccine Allocation for County", target_county, "in Period", period),
           x = "Time Period", y = "Allocated Vaccines") +
      theme(plot.title = element_text(size = 14))
    
    ggsave(filename = paste0("path/to/output/vaccine_allocation_period_", period, "_county_", target_county, ".png"))
  }
}
