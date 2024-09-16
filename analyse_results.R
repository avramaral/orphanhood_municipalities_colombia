source("header.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mort      <- data$mort
geo_info  <- data$geo_info
colombia  <- data$colombia

fitted_model <- readRDS(file = "FITTED/fitted_model_mortality.RDS")
