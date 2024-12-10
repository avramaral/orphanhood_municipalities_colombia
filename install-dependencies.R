# List of required packages
required_packages <- c("tidyverse",
                       "reshape2",
                       "cmdstanr",
                       "posterior",
                       "bayesplot",
                       "sf",
                       "MASS",
                       "data.table",
                       "ggplot2",
                       "haven",
                       "labelled",
                       "here",
                       "readxl",
                       "zoo",
                       "patchwork",
                       "cowplot",
                       "ggsci",
                       "rgeoboundaries",
                       "plot3D",
                       "viridis",
                       "spdep",
                       "rstatix",
                       "scales",
                       "units",
                       "ggpattern",
                       "magick")

# Install missing packages
install_if_missing <- function (package, ...) { if (!require(package, character.only = TRUE)) { install.packages(package, dependencies = TRUE) }}

# Loop through and install any missing packages
invisible(lapply(required_packages, install_if_missing))

# Check if `cmdstanr` is set up
if (!cmdstanr::cmdstan_version() > 0) { cmdstanr::install_cmdstan() }

cat("All dependencies have been installed successfully!\n")
