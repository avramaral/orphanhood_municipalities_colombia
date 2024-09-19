source("header.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mort      <- data$mort
geo_info  <- data$geo_info
colombia  <- data$colombia

processed_d_filed <- "FITTED/mu_municipality_processed.RDS"
if (!file.exists(processed_d_filed)) {
  d <- readRDS(file = "FITTED/full_mu_municipality_corrected.RDS")
  processed_d <- summarise_draws(d, default_summary_measures())
  saveRDS(object = processed_d, file = processed_d_filed)
} else {
  processed_d <- readRDS(file = processed_d_filed)
}

