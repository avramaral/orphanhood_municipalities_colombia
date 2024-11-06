source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

###########
### MPI ###
###########

data <- readRDS(file = "DATA/mortality_bias_data.RDS")

mpi_info <- data$mort %>% dplyr::select(mun, mpi) %>% distinct()
colombia <- data$colombia

geo_info <- readRDS(file = "DATA/geo_info.RDS")
geo_info <- geo_info %>% mutate(dep_name = ifelse(dep_name == "Archipelago de San Andrés, Providencia y Santa Catalina", "Arch. de SA, Prov. y St. Cat.", dep_name))

mpi_info <- mpi_info %>% left_join(y = colombia, by = "mun")

# Filter out islands
isl1_mpi <- mpi_info %>% filter((mun %in% c(88001)))
isl2_mpi <- mpi_info %>% filter((mun %in% c(88564)))
mpi_info <- mpi_info %>% filter(!(mun %in% c(88001, 88564)))


p_raw_tmp <- plot_maps(data = mpi_info, my_var = "mpi", tt = "", nm_var = "MPI", ll = c(0, 100))
ggsave(filename = paste("docs/images/MPI.jpeg" , sep = ""), plot = p_raw_tmp , width = 1500, height = 1500, units = c("px"), dpi = 300, bg = "white")

##################
### ORPHANHOOD ###
##################

plot_orphans_inc <- function (data, my_var, labels, nm_var = "", tt = "", ll = NULL, my_colors = NA, ...) {
  
  if (is.na(my_colors)) { my_colors <- plot3D::jet.col(n = length(labels)) } 
  my_colors <- setNames(my_colors, labels)
  
  pp <- ggplot(data = data, aes(geometry = geometry)) + 
    geom_sf(aes(fill = .data[[my_var]]), color = "black") + 
    scale_fill_manual(values = my_colors, name = nm_var) +
    labs(title = tt) + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 9, family = "LM Roman 10"), 
          plot.title = element_text(size = 12),
          legend.title = element_text(size = 9),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  pp
}

plot_orphans_prev <- function (data, nm_y = "", tt = "", ll = NULL, is_count = TRUE, ...) {
  
  pp <- ggplot(data, aes(x = dep_name, y = n_orp, fill = reg_name)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "Region", values = c("Amazonía" = "#00008FFF", "Caribe" = "#004CFFFF", "Central" = "#19FFE5FF", "Eje cafetero y Antioquia" = "#E5FF19FF", "Llanos" = "#FF4C00FF",  "Pacífica" = "#800000FF")) +
    labs(y = nm_y, x = "", title = tt) +
    { if (is_count)  scale_y_continuous(labels = comma,   expand = expansion(mult = c(0, 0.025)), limits = ll) } +
    { if (!is_count) scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.025)), limits = ll) } +
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 10, family = "LM Roman 10"), 
          plot.title = element_text(size = 12),
          legend.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  pp
}


breaks_count <- c(0, 5, 10, 50, 100, 1000, 15001)
labels_count <- c("0-4", "5-9", "10-49", "50-99", "100-999", "1,000-15,000")

breaks_rate <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 3.0)
labels_rate <- c("[0.0%, 0.2%)", "[0.2%, 0.4%)", "[0.4%, 0.6%)", "[0.6%, 0.8%)", "[0.8%, 1.0%)", "[1.0%, 3.0%)")

###################
### UNCORRECTED ###
###################

file_name <- "_EMP"

#############
# INCIDENCE #
#############

## COUNT
inc_orphans_count <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_abs", file_name, ".csv", sep = ""))
inc_orphans_count <- inc_orphans_count %>% filter(year == 2021) %>% dplyr::select(-c(mun_name, year)) %>% group_by(mun) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(mun = factor(mun))
inc_orphans_count <- inc_orphans_count %>% mutate(n_orp_category = cut(n_orp, breaks = breaks_count, labels = labels_count, include.lowest = TRUE)) %>% mutate(n_orp_category = factor(n_orp_category))
inc_orphans_count <- inc_orphans_count %>% left_join(y = colombia, by = "mun")

isl1_inc_count <- inc_orphans_count %>% filter((mun %in% c(88001)))
isl2_inc_count <- inc_orphans_count %>% filter((mun %in% c(88564)))
inc_orphans_count <- inc_orphans_count %>% filter(!(mun %in% c(88001, 88564)))

## RATE
inc_orphans_rate <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_per_1000", file_name, ".csv", sep = ""))
inc_orphans_rate <- inc_orphans_rate %>% filter(year == 2021) %>% dplyr::select(-c(mun_name, year)) %>% group_by(mun) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(mun = factor(mun))
inc_orphans_rate <- inc_orphans_rate %>% mutate(n_orp = n_orp / 1000 * 100)
inc_orphans_rate <- inc_orphans_rate %>% mutate(n_orp_category = cut(n_orp, breaks = breaks_rate, labels = labels_rate, include.lowest = TRUE)) %>% mutate(n_orp_category = factor(n_orp_category))
inc_orphans_rate <- inc_orphans_rate %>% left_join(y = colombia, by = "mun")

isl1_inc_rate <- inc_orphans_rate %>% filter((mun %in% c(88001)))
isl2_inc_rate <- inc_orphans_rate %>% filter((mun %in% c(88564)))
inc_orphans_rate <- inc_orphans_rate %>% filter(!(mun %in% c(88001, 88564)))

p_inc_21_count <- plot_orphans_inc(data = inc_orphans_count, my_var = "n_orp_category", labels = labels_count, nm_var = "Orphanhood incidence in 2021\nby Municipality\n(Number of children aged 0-17 years)", tt = "Uncorrected orphanhood count")
p_inc_21_rate  <- plot_orphans_inc(data = inc_orphans_rate,  my_var = "n_orp_category", labels = labels_rate,  nm_var = "Orphanhood incidence in 2021\nby Municipality\n(% of children aged 0-17 years)", tt = "Uncorrected orphanhood rate")

ggsave(filename = paste("docs/images/orphan_inc_count_uncorrected.jpeg", sep = ""), plot = p_inc_21_count, width = 1900, height = 1500, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("docs/images/orphan_inc_rate_uncorrected.jpeg" , sep = ""), plot = p_inc_21_rate , width = 1900, height = 1500, units = c("px"), dpi = 300, bg = "white")


##############
# PREVALENCE #
##############

## COUNT
pre_orphans_count <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_abs", file_name, ".csv", sep = ""))
pre_orphans_count <- pre_orphans_count %>% group_by(dep) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(dep = factor(dep))
pre_orphans_count <- pre_orphans_count %>% left_join(y = distinct(geo_info[, c("dep", "dep_name", "reg_name")]))
pre_orphans_count <- pre_orphans_count %>% mutate(dep_name = factor(dep_name), reg_name = factor(reg_name))
pre_orphans_count <- pre_orphans_count %>% mutate(dep_name = factor(dep_name, levels = dep_name[order(n_orp, decreasing = TRUE)]))

## RATE
pre_orphans_rate <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_per_100000", file_name, ".csv", sep = ""))
pre_orphans_rate <- pre_orphans_rate %>% group_by(dep) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(dep = factor(dep))
pre_orphans_rate <- pre_orphans_rate %>% mutate(n_orp = n_orp / 100000)
pre_orphans_rate <- pre_orphans_rate %>% left_join(y = distinct(geo_info[, c("dep", "dep_name", "reg_name")]))
pre_orphans_rate <- pre_orphans_rate %>% mutate(dep_name = factor(dep_name), reg_name = factor(reg_name))
pre_orphans_rate <- pre_orphans_rate %>% mutate(dep_name = factor(dep_name, levels = dep_name[order(n_orp, decreasing = TRUE)]))

p_pre_count <- plot_orphans_prev(data = pre_orphans_count, nm_y = "Orphanhood prevalence in 2021 by Department\n(Total)",                         tt = "Uncorrected", is_count = TRUE )
p_pre_rate  <- plot_orphans_prev(data = pre_orphans_rate,  nm_y = "Orphanhood prevalence in 2021 by Department\n(% of children aged 0-17 years)", tt = "Uncorrected", is_count = FALSE)

ggsave(filename = paste("docs/images/orphan_pre_count_uncorrected.jpeg", sep = ""), plot = p_pre_count, width = 2200, height = 1500, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("docs/images/orphan_pre_rate_uncorrected.jpeg" , sep = ""), plot = p_pre_rate , width = 2200, height = 1500, units = c("px"), dpi = 300, bg = "white")

################################################################################
################################################################################

#################
### CORRECTED ###
#################

file_name <- "_MEAN"

#############
# INCIDENCE #
#############

## COUNT
inc_orphans_count <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_abs", file_name, ".csv", sep = ""))
inc_orphans_count <- inc_orphans_count %>% filter(year == 2021) %>% dplyr::select(-c(mun_name, year)) %>% group_by(mun) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(mun = factor(mun))
inc_orphans_count <- inc_orphans_count %>% mutate(n_orp_category = cut(n_orp, breaks = breaks_count, labels = labels_count, include.lowest = TRUE)) %>% mutate(n_orp_category = factor(n_orp_category))
inc_orphans_count <- inc_orphans_count %>% left_join(y = colombia, by = "mun")

isl1_inc_count <- inc_orphans_count %>% filter((mun %in% c(88001)))
isl2_inc_count <- inc_orphans_count %>% filter((mun %in% c(88564)))
inc_orphans_count <- inc_orphans_count %>% filter(!(mun %in% c(88001, 88564)))

## RATE
inc_orphans_rate <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/municipality_inc_orphans_per_1000", file_name, ".csv", sep = ""))
inc_orphans_rate <- inc_orphans_rate %>% filter(year == 2021) %>% dplyr::select(-c(mun_name, year)) %>% group_by(mun) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(mun = factor(mun))
inc_orphans_rate <- inc_orphans_rate %>% mutate(n_orp = n_orp / 1000 * 100)
inc_orphans_rate <- inc_orphans_rate %>% mutate(n_orp_category = cut(n_orp, breaks = breaks_rate, labels = labels_rate, include.lowest = TRUE)) %>% mutate(n_orp_category = factor(n_orp_category))
inc_orphans_rate <- inc_orphans_rate %>% left_join(y = colombia, by = "mun")

isl1_inc_rate <- inc_orphans_rate %>% filter((mun %in% c(88001)))
isl2_inc_rate <- inc_orphans_rate %>% filter((mun %in% c(88564)))
inc_orphans_rate <- inc_orphans_rate %>% filter(!(mun %in% c(88001, 88564)))

p_inc_21_count <- plot_orphans_inc(data = inc_orphans_count, my_var = "n_orp_category", labels = labels_count, nm_var = "Orphanhood incidence in 2021\nby Municipality\n(Number of children aged 0-17 years)", tt = "Corrected orphanhood count")
p_inc_21_rate  <- plot_orphans_inc(data = inc_orphans_rate,  my_var = "n_orp_category", labels = labels_rate,  nm_var = "Orphanhood incidence in 2021\nby Municipality\n(% of children aged 0-17 years)", tt = "Corrected orphanhood rate")

ggsave(filename = paste("docs/images/orphan_inc_count_corrected.jpeg", sep = ""), plot = p_inc_21_count, width = 1900, height = 1500, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("docs/images/orphan_inc_rate_corrected.jpeg" , sep = ""), plot = p_inc_21_rate , width = 1900, height = 1500, units = c("px"), dpi = 300, bg = "white")

##############
# PREVALENCE #
##############

## COUNT
pre_orphans_count <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_abs", file_name, ".csv", sep = ""))
pre_orphans_count <- pre_orphans_count %>% group_by(dep) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(dep = factor(dep))
pre_orphans_count <- pre_orphans_count %>% left_join(y = distinct(geo_info[, c("dep", "dep_name", "reg_name")]))
pre_orphans_count <- pre_orphans_count %>% mutate(dep_name = factor(dep_name), reg_name = factor(reg_name))
pre_orphans_count <- pre_orphans_count %>% mutate(dep_name = factor(dep_name, levels = dep_name[order(n_orp, decreasing = TRUE)]))

## RATE
pre_orphans_rate <- read_csv(file = paste("ORPHANHOOD/POSTPROCESSING/TABLES/department_pre_orphans_per_100000", file_name, ".csv", sep = ""))
pre_orphans_rate <- pre_orphans_rate %>% group_by(dep) %>% summarise(n_orp = sum(n_orp)) %>% ungroup() %>% mutate(dep = factor(dep))
pre_orphans_rate <- pre_orphans_rate %>% mutate(n_orp = n_orp / 100000)
pre_orphans_rate <- pre_orphans_rate %>% left_join(y = distinct(geo_info[, c("dep", "dep_name", "reg_name")]))
pre_orphans_rate <- pre_orphans_rate %>% mutate(dep_name = factor(dep_name), reg_name = factor(reg_name))
pre_orphans_rate <- pre_orphans_rate %>% mutate(dep_name = factor(dep_name, levels = dep_name[order(n_orp, decreasing = TRUE)]))

p_pre_count <- plot_orphans_prev(data = pre_orphans_count, nm_y = "Orphanhood prevalence in 2021 by Department\n(Total)",                         tt = "Corrected", is_count = TRUE )
p_pre_rate  <- plot_orphans_prev(data = pre_orphans_rate,  nm_y = "Orphanhood prevalence in 2021 by Department\n(% of children aged 0-17 years)", tt = "Corrected", is_count = FALSE)

ggsave(filename = paste("docs/images/orphan_pre_count_corrected.jpeg", sep = ""), plot = p_pre_count, width = 2200, height = 1500, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("docs/images/orphan_pre_rate_corrected.jpeg" , sep = ""), plot = p_pre_rate , width = 2200, height = 1500, units = c("px"), dpi = 300, bg = "white")











