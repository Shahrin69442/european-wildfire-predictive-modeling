# Spatiotemporal Modeling and Predictive Analytics of European Wildfire Trends (2020-2025)

## 📌 Project Overview
This repository contains a self-contained, publication-quality data science pipeline implemented in **R**. The project integrates advanced spatiotemporal data simulation, high-resolution geospatial density estimation, and classical stochastic time-series forecasting to model wildfire propagation behavior across critical Mediterranean ecosystems.

---

## 🛠️ Key Methodological Architectures

### 1. Bounded Stochastic Simulation (NASA FIRMS Specification)
To replicate complex climate physics without local hardware bottlenecks, the framework generates a synthesized repository of 25,000 observations bounded within real-world Mediterranean coordinates. A custom **Seasonal Probability Weight Matrix** was engineered, accelerating summer month observation densities to 75% probability to accurately emulate historical ecological anomalies.

### 2. Spatial Hotspot Detection via 2D Kernel Density Estimation (KDE)
Rather than relying on basic geographic point plotting, this framework implements continuous probability distributions. Utilizing the `sf` and `ggplot2` ecosystems, the architecture converts coordinate matrices into variable density kernels. This uncovers statistically significant latent wildfire clusters across high-risk zones in Spain, Italy, Greece, and France.

### 3. Temporal Modeling via Automated ARIMA Architecture
To capture long-term trends and cyclical, non-linear dependencies, the temporal data was aggregated into monthly intervals and fitted using an automated Autoregressive Integrated Moving Average (**Auto-ARIMA**) framework. The algorithm dynamically minimizes Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC) to identify the optimal parameter configuration for a robust 24-month horizon predictive forecast.

---

## 📦 Required Dependencies
To replicate the environment and execute the scripts seamlessly, ensure the following R packages are installed:
```R
install.packages(c("tidyverse", "lubridate", "sf", "maps", "forecast"))
