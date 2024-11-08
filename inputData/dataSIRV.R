# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the CSV file
covid_data <- read.csv("C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\data\\COVID-19_Vaccinations_in_the_United_States_Jurisdiction_20241107.csv")

# Convert Date column to date format
covid_data$Date <- as.Date(covid_data$Date, format = "%m/%d/%Y")

# Define Florida's total population
florida_population <- 22610000 

# Filter for Florida data and for dates closest to the range (10/19/2022 to 4/22/2023)
start_date <- as.Date("2022-10-19")
end_date <- as.Date("2023-04-22")
florida_data <- covid_data %>%
  filter(Location == "FL" & Date >= start_date & Date <= end_date)

# Aggregate vaccination data for Florida by week
florida_vaccine_summary <- florida_data %>%
  mutate(week = floor_date(Date, unit = "week", week_start = 1)) %>%  # Set week start to Monday
  group_by(week) %>%
  summarize(
    total_administered = sum(Administered_Bivalent, na.rm = TRUE),
    total_distributed = sum(Dist_Bivalent_PFR, Dist_Bivalent_MOD, na.rm = TRUE),
    population_vaccinated = sum(Bivalent_Booster_5Plus, na.rm = TRUE)
  ) %>%
  mutate(
    # Calculate cumulative proportion of population vaccinated
    proportion_vaccinated = population_vaccinated / florida_population,
    # Calculate weekly change in proportion vaccinated (as vaccination rate)
    vaccination_rate = c(NA, diff(proportion_vaccinated))
  ) %>%
  arrange(week)  # Sort by week in ascending order

# Replace NA in vaccination_rate with 0 for the first week
florida_vaccine_summary$vaccination_rate[1] <- 0

# Save the weekly data to a new CSV file
write.csv(florida_vaccine_summary, "C:\\Users\\alexr\\OneDrive\\FSU\\FSU Sophomore Year\\SophomoreResearch\\Data-Driven-SIRV-OR\\data\\Florida_Weekly_Vaccine_Summary.csv", row.names = FALSE)
