#
install.packages('forecast')
library(forecast)
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages('sf')
library(sf)
install.packages('maps')
library(maps)
# Title: Spatiotemporal Modeling and Predictive Analytics of European Wildfire 
#        Trends (2020-2025)
# Author: Md Shahrin Parvez
# Background: BSc in Statistics (Final Year)
# Description: Self-contained, publication-quality pipeline combining 
#              Geospatial Analytics and Advanced Time-Series Forecasting.
# 

# STEP 1: ENVIRONMENT & PACKAGES SETUP
required_packages <- c("tidyverse", "lubridate", "sf", "maps", "forecast")
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}))

if (!dir.exists("plots")) dir.create("plots")

# STEP 2: STOCHASTIC SIMULATION OF RADIOMETRIC DATA (NASA FIRMS SPECIFICATION)
set.seed(101)

n_observations <- 25000
timeline <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2025-12-31"), by = "day")

seasonal_months <- as.numeric(format(timeline, "%m"))
seasonal_weights <- case_when(
  seasonal_months %in% c(6, 7, 8) ~ 0.75,  
  seasonal_months %in% c(5, 9)    ~ 0.15,  
  TRUE                            ~ 0.10   
)
normalized_weights <- seasonal_weights / sum(seasonal_weights)
simulated_dates <- sample(timeline, size = n_observations, replace = TRUE, prob = normalized_weights)

raw_wildfire_data <- tibble(
  latitude   = runif(n_observations, min = 36.5, max = 43.0),   
  longitude  = runif(n_observations, min = -8.5, max = 24.5),  
  brightness = rnorm(n_observations, mean = 328.5, sd = 18.2), 
  acq_date   = simulated_dates,
  confidence = sample(60:100, size = n_observations, replace = TRUE)
)

# STEP 3: DATA CLEANING & FEATURE ENGINEERING
cleaned_fire_data <- raw_wildfire_data %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(acq_date)) %>%
  mutate(
    year      = year(acq_date),
    month     = month(acq_date, label = TRUE, abbr = TRUE),
    month_num = month(acq_date),
    week      = week(acq_date)
  ) %>%
  mutate(
    intensity_level = case_when(
      brightness >= 345 ~ "Critical Intensity",
      brightness >= 320 & brightness < 345 ~ "Moderate Intensity",
      TRUE ~ "Low Intensity"
    )
  )

# STEP 4: GEOSPATIAL DENSITY HEATMAP ANALYSIS
europe_base_map <- map_data("world") %>%
  filter(region %in% c("Spain", "Portugal", "Italy", "Greece", "France", 
                       "Bulgaria", "Albania", "Macedonia", "Turkey"))

wildfire_geospatial_plot <- ggplot() +
  geom_polygon(data = europe_base_map, aes(x = long, y = lat, group = group), 
               fill = "#f5f5f7", color = "#ffffff", linewidth = 0.3) +
  stat_density_2d(data = cleaned_fire_data, aes(x = longitude, y = latitude, fill = after_stat(level)), 
                  geom = "polygon", alpha = 0.35) +
  geom_point(data = cleaned_fire_data %>% filter(intensity_level == "Critical Intensity"), 
             aes(x = longitude, y = latitude), color = "#d7191c", size = 0.2, alpha = 0.5) +
  scale_fill_viridis_c(option = "inferno", name = "Density Kernel Index") +
  coord_sf(xlim = c(-10, 26), ylim = c(35, 45), expand = FALSE) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Geospatial Topography of European Wildfire Clusters (2020-2025)",
    subtitle = "Macro-scale Hotspot Detection Using Variable Kernel Density Estimates",
    x = "Longitude Coordinate", y = "Latitude Coordinate",
    caption = "Analytical Framework Designed by Md Shahrin Parvez | Source: Remote Sensing Simulation"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#111111"),
    plot.subtitle = element_text(size = 10, color = "#555555"),
    legend.position = "right",
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.25)
  )

ggsave("plots/european_wildfire_spatial_density.png", plot = wildfire_geospatial_plot, 
       width = 11, height = 7, dpi = 300)

# STEP 5: TIME-SERIES AGGREGATION & FORECAST MODELLING
monthly_aggregated_series <- cleaned_fire_data %>%
  mutate(year_month = make_date(year, month_num, 1)) %>%
  group_by(year_month) %>%
  summarise(fire_count = n(), .groups = 'drop') %>%
  complete(year_month = seq.Date(min(year_month), max(year_month), by="month"), 
           fill = list(fire_count = 0))

fire_evolution_ts <- ts(monthly_aggregated_series$fire_count, start = c(2020, 1), frequency = 12)
predictive_arima_model <- auto.arima(fire_evolution_ts, seasonal = TRUE, stepw = FALSE, approximation = FALSE)
future_horizon_forecast <- forecast(predictive_arima_model, h = 24)

png("plots/wildfire_trend_forecast.png", width = 2800, height = 1600, res = 300)
plot(future_horizon_forecast, 
     main = "Stochastic Forecasting Model: European Wildfire Propagation (24-Month Horizon)",
     xlab = "Temporal Aggregations (Monthly Intervals)", 
     ylab = "Aggregated Volumetric Fire Incidences", 
     col = "#2c3e50", lwd = 2.5, fcol = "#e74c3c", 
     shadecols = c("#95a5a6", "#d5dbdb"))
dev.off()

summary(predictive_arima_model)

#
print(wildfire_geospatial_plot)

#
plot(future_horizon_forecast, main = "Forecast Display")

#
wildfire_geospatial_plot <- ggplot() +
  borders("world", regions = c("Spain", "Portugal", "Italy", "Greece", "France"), 
          fill = "#eaeaea", color = "#999999", linewidth = 0.4) +
  stat_density_2d(data = cleaned_fire_data, aes(x = longitude, y = latitude, fill = after_stat(level)), 
                  geom = "polygon", alpha = 0.4) +
  scale_fill_viridis_c(option = "inferno", name = "Density Index") +
  coord_sf(xlim = c(-10, 26), ylim = c(34, 46), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Geospatial Topography of European Wildfire Clusters (2020-2025)",
    subtitle = "Macro-scale Hotspot Detection Using Variable Kernel Density Estimates",
    x = "Longitude Coordinate", y = "Latitude Coordinate",
    caption = "Analytical Framework Designed by Md Shahrin Parvez"
  )

# 
print(wildfire_geospatial_plot)


