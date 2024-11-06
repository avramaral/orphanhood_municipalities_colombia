source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mpi_info <- data$mort %>% dplyr::select(mun, mpi) %>% distinct()
geo_info <- data$geo_info 
colombia <- data$colombia

mpi_info <-  mpi_info %>% left_join(y = colombia, by = "mun")

# Filter out islands
isl1_mpi <- mpi_info %>% filter((mun %in% c(88001)))
isl2_mpi <- mpi_info %>% filter((mun %in% c(88564)))
mpi_info <- mpi_info %>% filter(!(mun %in% c(88001, 88564)))


p_raw_tmp <- plot_maps(data = mpi_info, my_var = "mpi", tt = "", nm_var = "MPI", ll = c(0, 100))
ggsave(filename = paste("docs/images/MPI.jpeg" , sep = ""), plot = p_raw_tmp , width = 1500, height = 1500, units = c("px"), dpi = 300, bg = "white")
