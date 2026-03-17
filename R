# ==============================================================================
# Time-Series Demand Forecasting & Trend Decomposition Model
# Author: Kaiyi (Janice) Liu
# Target Application: Supply Chain Demand Planning & Inventory Optimization
# ==============================================================================

# 1. ENVIRONMENT SETUP & LIBRARY LOADING
# ------------------------------------------------------------------------------
# Load essential packages for data manipulation and visualization
library(ggplot2)
library(dplyr)
library(zoo)      # For rolling averages
library(forecast) # For time-series analysis

# 2. DATA INGESTION & PREPROCESSING
# ------------------------------------------------------------------------------
# Load historical demand/macroeconomic data
# Note: Ensure 'data.csv' is in the root directory of the repository
raw_data <- read.csv("data.csv")

# Clean and structure the dataset
demand_data <- raw_data %>%
  # Assuming standard column names; adjust as necessary based on raw data
  # Select relevant columns and handle missing values
  na.omit() %>%
  mutate(
    # Create a sequential time index for modeling
    Time_Index = row_number(),
    Time_Squared = Time_Index^2,
    # Log-scale transformation to analyze annualized growth rates
    Log_Value = log(Value) 
  )

# 3. LONG-TERM TREND MODELING (QUADRATIC FIT)
# ------------------------------------------------------------------------------
# Fit a quadratic regression model to capture non-linear baseline growth
# Equation: y = B0 + B1*t + B2*t^2
quad_model <- lm(Log_Value ~ Time_Index + Time_Squared, data = demand_data)

# Append predicted baseline trend to the dataset
demand_data$Baseline_Trend <- predict(quad_model)

# 4. CYCLICAL FLUCTUATIONS (MOVING AVERAGE)
# ------------------------------------------------------------------------------
# Isolate macroeconomic cycles to prevent bullwhip effect in inventory
# Using an Order-5 Moving Average to smooth out short-term noise
demand_data$Cyclical_MA5 <- rollmean(demand_data$Log_Value, 
                                     k = 5, 
                                     fill = NA, 
                                     align = "center")

# 5. SEASONALITY EXTRACTION
# ------------------------------------------------------------------------------
# Convert raw values into a formal Time-Series (ts) object (Quarterly Frequency)
ts_demand <- ts(demand_data$Value, frequency = 4)

# Decompose the time series to extract the exact seasonal coefficients
decomposed_ts <- decompose(ts_demand, type = "multiplicative")

# Extract seasonal indices (Used for adjusting quarterly safety stock)
seasonality_factors <- decomposed_ts$figure
print("Quarterly Seasonality Indices for Inventory Adjustment:")
print(seasonality_factors)

# 6. DATA VISUALIZATION (EXECUTIVE REPORTING)
# ------------------------------------------------------------------------------
# Plot 1: Actual Demand vs. Quadratic Baseline Trend
ggplot(demand_data, aes(x = Time_Index)) +
  geom_line(aes(y = Log_Value, color = "Actual Log-Demand"), size = 1) +
  geom_line(aes(y = Baseline_Trend, color = "Quadratic Trend"), size = 1.2, linetype = "dashed") +
  labs(title = "Long-Term Demand Trend Analysis",
       x = "Time (Quarters)",
       y = "Log(Demand Volume)",
       color = "Legend") +
  theme_minimal()

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
