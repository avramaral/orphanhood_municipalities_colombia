source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/mortality_bias_data.RDS")
data$mort <- data$mort %>% filter(!(gender == "Female" & age %in% c("60-64", "65-69", "70-74", "75-79", "80+")))
data$mort <- data$mort %>% filter(!(gender ==   "Male" & age %in% c(                  "70-74", "75-79", "80+")))

mpi_info <- data$mort %>% dplyr::select(mun, mpi) %>% distinct()
geo_info <- data$geo_info 
colombia <- data$colombia

prop_65_66_fem <- readRDS(file = "DATA/prop_65_66_fem.RDS")
prop_75_76_mal <- readRDS(file = "DATA/prop_75_76_mal.RDS")

Y <- data$mort$year   %>% unique() %>% length()
G <- data$mort$gender %>% unique() %>% length()
L <- data$mort$mun    %>% unique() %>% length()

raw_deaths <- data$mort
raw_deaths_filtered <- adj_mort_data(mort = raw_deaths, prop_fem = prop_65_66_fem, prop_75_76_mal)

p <- "mortality_v2_1.stan"
d <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))
draws <- d$draws
fit_d <- d$data
A_fem <- d$data$A_fem
A_mal <- d$data$A_mal
sample_size <- nrow(draws[, 1]) # 2000

fit_deaths <- readRDS(paste("FITTED/DATA/count_", strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))
fit_deaths <- fit_deaths %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, deaths = Median) %>% mutate(mun = factor(mun))
fit_deaths <- fit_deaths %>% left_join(y = data$mort[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_deaths <- fit_deaths %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
fit_deaths_filtered <- adj_mort_data(mort = fit_deaths, prop_fem = prop_65_66_fem, prop_75_76_mal)
fit_deaths <- fit_deaths %>% dplyr::select(year, mun, gender, age, deaths, population, death_rate) %>% arrange(year, mun, gender, age)
fit_deaths <- fit_deaths %>% rename(fit_deaths = deaths)

if (FALSE) {
  n_raw_deaths <- sum(raw_deaths_filtered$deaths)
  n_fit_deaths <- sum(fit_deaths_filtered$deaths)
} else {
  n_raw_deaths <- sum(raw_deaths$deaths)
  n_fit_deaths <- sum(fit_deaths$fit_deaths)
}

print(paste("The fitted number of deaths is ", round(n_fit_deaths / n_raw_deaths, 2), " larger than the raw number of deaths.", sep = ""))

raw_deaths <- raw_deaths %>% left_join(y = fit_deaths[, c("mun", "gender", "year", "age", "fit_deaths")], by = c("mun", "gender", "year", "age"))
raw_deaths <- raw_deaths %>% dplyr::select(year, mun, gender, age, population, deaths, fit_deaths) %>% mutate(mult_fact = fit_deaths / deaths)

mult_fact <- raw_deaths$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

# COMPUTE STANDARDISED RATES AND PLOTS

data_raw <- raw_deaths %>% 
            dplyr::select(year, mun, gender, age, deaths, population) %>% 
            mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
            mutate(mortality_rate = compute_rate(count = deaths, pop = population))
data_fit <- fit_deaths %>% 
            dplyr::select(year, mun, gender, age, fit_deaths) %>% rename(deaths = fit_deaths) %>% 
            arrange(year, mun, gender, age) %>% 
            left_join(y = raw_deaths[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
            mutate(deaths = ifelse(population == 0, 0, deaths)) %>% 
            mutate(mortality_rate = compute_rate(count = deaths, pop = population))

# PLOT STANDARDIZED RATES
for (y in 1998:2021) {
  # y <- 2018
  yy <- y - 1998 + 1
  y_lim <- c(0, 0.02)
  
  std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == y) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (mortality)", y_lim = y_lim)
  
  alpha_0 <- c(draws[, paste("alpha_0[", yy, "]", sep = "")])
  alpha_1 <- c(draws[, paste("alpha_1[", yy, "]", sep = "")])
  mpi_mun <- seq(0, 1, by = 0.01)
  std_mun <- alpha_0 + outer(alpha_1, mpi_mun)
  mpi_obs <- fit_d$mpi_municip
  std_obs <- alpha_0 + outer(alpha_1, mpi_obs) 
  
  # Calculate the quantiles (2.5%, 50%, 97.5%) for each x value (column-wise)
  quantiles_lin <- apply(std_mun, 2, quantile, probs = c(0.025, 0.5, 0.975))
  quantiles_lin <- data.frame(x = mpi_mun, ll = quantiles_lin[1, ], mm = quantiles_lin[2, ], uu = quantiles_lin[3, ])
  
  quantiles_obs <- apply(std_obs, 2, quantile, probs = c(0.025, 0.5, 0.975))
  quantiles_obs <- data.frame(x = mpi_obs, ll = quantiles_obs[1, ], mm = quantiles_obs[2, ], uu = quantiles_obs[3, ])
  
  std_fit <- compute_std_rate(data_fit); std_fit <- std_fit %>% filter(year == y) %>% dplyr::select(-year)
  std_fit <- std_fit %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital), mpi = mpi / 100)
  
  p_fit_pts <- plot_fit_std_rate(data_lin = quantiles_lin, data_fit = std_fit, tt = "Fitted data (mortality)", y_lim = y_lim)
  
  (p_tot_pts <- p_raw_pts + p_fit_pts)
  ggsave(filename = paste("IMAGES/STD_RATES_COMPARISON/MORTALITY/std_mortality_comparison_", y ,".jpeg" , sep = ""), plot = p_tot_pts , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
}

# FITTED VS EMPIRICAL
  
raw_deaths_y <- data_raw
raw_deaths_y <- raw_deaths_y %>% group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% mutate(age = factor(age), death_rate = compute_rate(count = deaths, pop = population))
raw_deaths_y <- raw_deaths_y %>% filter(!(gender == "Female" & age %in% c("60-64", "65-69", "70-74", "75-79", "80+")))
raw_deaths_y <- raw_deaths_y %>% filter(!(gender == "Male" & age %in% c("70-74", "75-79", "80+")))
raw_deaths_y <- raw_deaths_y %>% mutate(year = factor(year), gender = factor(gender))

fit_deaths_y <- readRDS(paste("FITTED/DATA/count_", strsplit(p, "\\.")[[1]][1], ".RDS", sep = "")) %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, Q500 = Median) %>% dplyr::select(year, mun, gender, age, Q025, Q500, Q975) %>% mutate(mun = factor(mun)) %>% left_join(y = data$mort[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_deaths_y <- fit_deaths_y %>% group_by(year, gender, age) %>% summarise(Q025 = sum(Q025), Q500 = sum(Q500), Q975 = sum(Q975), population = sum(population)) %>% ungroup() %>% mutate(age = factor(age), death_rate_Q025 = compute_rate(count = Q025, pop = population), death_rate_Q500 = compute_rate(count = Q500, pop = population), death_rate_Q975 = compute_rate(count = Q975, pop = population))
fit_deaths_y <- fit_deaths_y %>% filter(!(gender == "Female" & age %in% c("60-64", "65-69", "70-74", "75-79", "80+")))
fit_deaths_y <- fit_deaths_y %>% filter(!(gender == "Male" & age %in% c("70-74", "75-79", "80+")))
fit_deaths_y <- fit_deaths_y %>% mutate(year = factor(year), gender = factor(gender))

fit_deaths_y_nat <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))$draws
fit_deaths_y_nat_fem <- array(data = 0, dim = c(Y, A_fem, sample_size))
fit_deaths_y_nat_mal <- array(data = 0, dim = c(Y, A_mal, sample_size)) 
for (y in 1:Y) {
  for (a in 1:A_fem) { fit_deaths_y_nat_fem[y, a, ] <- inv_logit(c(fit_deaths_y_nat[, paste("inv_logit_death_rate_nat_fem[", y, ",", a, "]", sep = "")])) }
  for (a in 1:A_mal) { fit_deaths_y_nat_mal[y, a, ] <- inv_logit(c(fit_deaths_y_nat[, paste("inv_logit_death_rate_nat_mal[", y, ",", a, "]", sep = "")])) }
}
fit_deaths_y_nat_fem_Q025 <- apply(X = fit_deaths_y_nat_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.025); rownames(fit_deaths_y_nat_fem_Q025) <- unique(raw_deaths_y$year); colnames(fit_deaths_y_nat_fem_Q025) <- unique(filter(raw_deaths_y, gender == "Female")$age); fit_deaths_y_nat_fem_Q025 <- melt(fit_deaths_y_nat_fem_Q025)
fit_deaths_y_nat_fem_Q500 <- apply(X = fit_deaths_y_nat_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.500); rownames(fit_deaths_y_nat_fem_Q500) <- unique(raw_deaths_y$year); colnames(fit_deaths_y_nat_fem_Q500) <- unique(filter(raw_deaths_y, gender == "Female")$age); fit_deaths_y_nat_fem_Q500 <- melt(fit_deaths_y_nat_fem_Q500)
fit_deaths_y_nat_fem_Q975 <- apply(X = fit_deaths_y_nat_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.975); rownames(fit_deaths_y_nat_fem_Q975) <- unique(raw_deaths_y$year); colnames(fit_deaths_y_nat_fem_Q975) <- unique(filter(raw_deaths_y, gender == "Female")$age); fit_deaths_y_nat_fem_Q975 <- melt(fit_deaths_y_nat_fem_Q975)
fit_deaths_y_nat_fem <- cbind(fit_deaths_y_nat_fem_Q025, fit_deaths_y_nat_fem_Q500[, 3], fit_deaths_y_nat_fem_Q975[, 3]); colnames(fit_deaths_y_nat_fem) <- c("year", "age", "death_rate_Q025", "death_rate_Q500", "death_rate_Q975"); fit_deaths_y_nat_fem <- fit_deaths_y_nat_fem %>% as_tibble()
fit_deaths_y_nat_fem <- fit_deaths_y_nat_fem %>% mutate(gender = "Female")
fit_deaths_y_nat_mal_Q025 <- apply(X = fit_deaths_y_nat_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.025); rownames(fit_deaths_y_nat_mal_Q025) <- unique(raw_deaths_y$year); colnames(fit_deaths_y_nat_mal_Q025) <- unique(filter(raw_deaths_y, gender ==   "Male")$age); fit_deaths_y_nat_mal_Q025 <- melt(fit_deaths_y_nat_mal_Q025)
fit_deaths_y_nat_mal_Q500 <- apply(X = fit_deaths_y_nat_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.500); rownames(fit_deaths_y_nat_mal_Q500) <- unique(raw_deaths_y$year); colnames(fit_deaths_y_nat_mal_Q500) <- unique(filter(raw_deaths_y, gender ==   "Male")$age); fit_deaths_y_nat_mal_Q500 <- melt(fit_deaths_y_nat_mal_Q500)
fit_deaths_y_nat_mal_Q975 <- apply(X = fit_deaths_y_nat_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.975); rownames(fit_deaths_y_nat_mal_Q975) <- unique(raw_deaths_y$year); colnames(fit_deaths_y_nat_mal_Q975) <- unique(filter(raw_deaths_y, gender ==   "Male")$age); fit_deaths_y_nat_mal_Q975 <- melt(fit_deaths_y_nat_mal_Q975)
fit_deaths_y_nat_mal <- cbind(fit_deaths_y_nat_mal_Q025, fit_deaths_y_nat_mal_Q500[, 3], fit_deaths_y_nat_mal_Q975[, 3]); colnames(fit_deaths_y_nat_mal) <- c("year", "age", "death_rate_Q025", "death_rate_Q500", "death_rate_Q975"); fit_deaths_y_nat_mal <- fit_deaths_y_nat_mal %>% as_tibble()
fit_deaths_y_nat_mal <- fit_deaths_y_nat_mal %>% mutate(gender =   "Male")
fit_deaths_y_nat <- rbind(fit_deaths_y_nat_fem, fit_deaths_y_nat_mal) %>% dplyr::select(year, gender, age, death_rate_Q025, death_rate_Q500, death_rate_Q975) %>% arrange(year, gender, age) %>% mutate(year = factor(year), gender = factor(gender))

my_colors <- plot3D::jet.col(n = length(unique(raw_deaths_y$year)))

dr_plot_1 <- ggplot(data = raw_deaths_y) +
  geom_line(mapping = aes(x = age, y = death_rate, color = year, group = year), linetype = "solid") +
  geom_point(mapping = aes(x = age, y = death_rate, color = year), size = 2) +
  geom_errorbar(data = fit_deaths_y_nat, aes(x = age, ymin = death_rate_Q025, ymax = death_rate_Q975, color = year)) +
  scale_color_manual(name = "Year", values = my_colors) +
  facet_grid(~ gender) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

dr_plot_1.5 <- ggplot(data = raw_deaths_y) +
  geom_line(mapping = aes(x = age, y = death_rate, color = year, group = year), linetype = "solid") +
  geom_point(data = fit_deaths_y_nat, aes(x = age, y = death_rate_Q500, color = year)) +
  scale_color_manual(name = "Year", values = my_colors) +
  facet_grid(~ gender) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

dr_plot_2 <- ggplot(data = raw_deaths_y) +
  geom_line(mapping = aes(x = age, y = death_rate, color = gender, group = gender), linetype = "solid") +
  geom_point(mapping = aes(x = age, y = death_rate, color = gender), size = 2) +
  geom_errorbar(data = fit_deaths_y_nat, aes(x = age, ymin = death_rate_Q025, ymax = death_rate_Q975, color = gender)) +
  scale_color_manual(name = "Gender", values = c("#00008FFF", "#800000FF")) +
  facet_wrap(~ year, ncol = 4) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

dr_plot_2.5_fem <- ggplot(data = filter(raw_deaths_y, gender == "Female")) +
  geom_line(mapping = aes(x = age, y = death_rate, color = gender, group = gender), linetype = "solid") +
  geom_point(data = filter(fit_deaths_y_nat, gender == "Female"), aes(x = age, y = death_rate_Q500, color = gender)) +
  scale_color_manual(name = "Gender", values = c("#00008FFF")) +
  facet_wrap(~ year, ncol = 4) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

dr_plot_2.5_mal <- ggplot(data = filter(raw_deaths_y, gender == "Male")) +
  geom_line(mapping = aes(x = age, y = death_rate, color = gender, group = gender), linetype = "solid") +
  geom_point(data = filter(fit_deaths_y_nat, gender == "Male"), aes(x = age, y = death_rate_Q500, color = gender)) +
  scale_color_manual(name = "Gender", values = c("#800000FF")) +
  facet_wrap(~ year, ncol = 4) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

ggsave(filename = paste("IMAGES/RATE_COMPARISON/MORTALITY/mort_coloured_year_error_bar.jpeg",   sep = ""), plot = dr_plot_1,       width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/MORTALITY/mort_coloured_year_median.jpeg",      sep = ""), plot = dr_plot_1.5,     width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/MORTALITY/mort_coloured_gender_error_bar.jpeg", sep = ""), plot = dr_plot_2,       width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/MORTALITY/mort_coloured_female_error_bar.jpeg", sep = ""), plot = dr_plot_2.5_fem, width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/MORTALITY/mort_coloured_male_error_bar.jpeg",   sep = ""), plot = dr_plot_2.5_mal, width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")

# for (y in 1998:2021) {
#   # y  <- 2021
#   yy <- y - 1998 + 1
#   y_lim <- c(0, 0.15)
#   
#   raw_deaths_y <- data_raw %>% filter(year == y)
#   raw_deaths_y <- raw_deaths_y %>% group_by(year, gender, age) %>% summarise(deaths = sum(deaths), population = sum(population)) %>% ungroup() %>% mutate(age = factor(age), death_rate = compute_rate(count = deaths, pop = population))
#   raw_deaths_y <- raw_deaths_y %>% filter(!(gender == "Female" & age %in% c("60-64", "65-69", "70-74", "75-79", "80+")))
#   raw_deaths_y <- raw_deaths_y %>% filter(!(gender == "Male" & age %in% c("70-74", "75-79", "80+")))
#   
#   fit_deaths_y <- readRDS(paste("FITTED/DATA/count_", strsplit(p, "\\.")[[1]][1], ".RDS", sep = "")) %>% as_tibble() %>% filter(Year == y) %>% rename(mun = Location, gender = Gender, year = Year, age = Age, Q500 = Median) %>% dplyr::select(year, mun, gender, age, Q025, Q500, Q975) %>% mutate(mun = factor(mun)) %>% left_join(y = data$mort[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
#   fit_deaths_y <- fit_deaths_y %>% group_by(year, gender, age) %>% summarise(Q025 = sum(Q025), Q500 = sum(Q500), Q975 = sum(Q975), population = sum(population)) %>% ungroup() %>% mutate(age = factor(age), death_rate_Q025 = compute_rate(count = Q025, pop = population), death_rate_Q500 = compute_rate(count = Q500, pop = population), death_rate_Q975 = compute_rate(count = Q975, pop = population))
#   fit_deaths_y <- fit_deaths_y %>% filter(!(gender == "Female" & age %in% c("60-64", "65-69", "70-74", "75-79", "80+")))
#   fit_deaths_y <- fit_deaths_y %>% filter(!(gender == "Male" & age %in% c("70-74", "75-79", "80+")))
#   
#   dr_plot <- ggplot(data = raw_deaths_y) +
#     geom_line(mapping = aes(x = age, y = death_rate, group = 1), color = "red", linetype = "dashed") +
#     geom_point(mapping = aes(x = age, y = death_rate), color = "red", size = 2) +
#     geom_errorbar(data = fit_deaths_y, aes(x = age, ymin = death_rate_Q025, ymax = death_rate_Q975), color = "blue") +
#     geom_point(data = fit_deaths_y, mapping = aes(x = age, y = death_rate_Q500), color = "blue", size = 2) +
#     facet_grid(~ gender) +
#     labs(title = paste("National death rate in ", y, sep = ""), x = "Age", y = "Death rate") +
#     theme_bw() +
#     theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 
#   
#   ct_plot <- ggplot(data = raw_deaths_y) +
#     geom_line(mapping = aes(x = age, y = deaths, group = 1), color = "red", linetype = "dashed") +
#     geom_point(mapping = aes(x = age, y = deaths), color = "red", size = 2) +
#     geom_errorbar(data = fit_deaths_y, aes(x = age, ymin = Q025, ymax = Q975), color = "blue") +
#     geom_point(data = fit_deaths_y, mapping = aes(x = age, y = Q500), color = "blue", size = 2) +
#     facet_grid(~ gender) +
#     labs(title = paste("National death count in ", y, sep = ""), x = "Age", y = "Death count") +
#     theme_bw() +
#     theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 
#   
#   ggsave(filename = paste("IMAGES/COUNT_COMPARISON/FERTILITY/fertility_comparison_", y ,".jpeg" , sep = ""), plot = ct_plot , width = 1500, height = 1200, units = c("px"), dpi = 300, bg = "white")
# }


# BOXPLOT

# Compute standardized rage
std_raw_age_gender <- compute_std_rate_age_gender(data_raw); std_raw_age_gender <- std_raw_age_gender %>% filter(year == 2018) %>% dplyr::select(-year)
std_fit_age_gender <- compute_std_rate_age_gender(data_fit); std_fit_age_gender <- std_fit_age_gender %>% filter(year == 2018) %>% dplyr::select(-year)
# Add capital and MPI information
std_raw_age_gender <- std_raw_age_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
std_fit_age_gender <- std_fit_age_gender %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
# Add MPI class
std_raw_age_gender <- std_raw_age_gender %>% mutate(mpi_class = ifelse(mpi <= 25, 1, ifelse(mpi <= 50, 2, ifelse(mpi <= 75, 3, 4)))) %>% mutate(mpi_class = factor(mpi_class, labels = c("0-25", "26-50", "51-75", "76-100")))
std_fit_age_gender <- std_fit_age_gender %>% mutate(mpi_class = ifelse(mpi <= 25, 1, ifelse(mpi <= 50, 2, ifelse(mpi <= 75, 3, 4)))) %>% mutate(mpi_class = factor(mpi_class, labels = c("0-25", "26-50", "51-75", "76-100")))
# Filter out unnecessary age classes
# TBD: Not for now

p_raw_box <- plot_std_rate_boxplot(data = std_raw_age_gender, tt = "Raw data (mortality)")
p_fit_box <- plot_std_rate_boxplot(data = std_fit_age_gender, tt = "Fit data (mortality)")
p_tot_box <- p_raw_box + p_fit_box
ggsave(filename = paste("IMAGES/std_mortality_boxplot_comparison.jpeg" , sep = ""), plot = p_tot_box , width = 3000, height = 2000, units = c("px"), dpi = 300, bg = "white")

# MAP

std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == 2018) %>% dplyr::select(-year)
std_fit <- compute_std_rate(data_fit); std_fit <- std_fit %>% filter(year == 2018) %>% dplyr::select(-year)

u_limit <- max(std_raw$std_rate, std_fit$std_rate)
l_limit <- min(std_raw$std_rate, std_fit$std_rate)

std_raw <- std_raw %>% left_join(y = colombia, by = "mun")
std_fit <- std_fit %>% left_join(y = colombia, by = "mun")

# Filter out islands
isl1_rw <- std_raw %>% filter((mun %in% c(88001))) 
isl2_rw <- std_raw %>% filter((mun %in% c(88564))) 
std_raw <- std_raw %>% filter(!(mun %in% c(88001, 88564)))

isl1_ft <- std_fit %>% filter((mun %in% c(88001))) 
isl2_ft <- std_fit %>% filter((mun %in% c(88564))) 
std_fit <- std_fit %>% filter(!(mun %in% c(88001, 88564)))

p_raw <- plot_maps(data = std_raw, my_var = "std_rate", tt = "Original data", nm_var = "Standardised\nmortality rate\n(in 2018)", ll = c(l_limit, u_limit))
p_fit <- plot_maps(data = std_fit, my_var = "std_rate", tt = "Fitted data",   nm_var = "Standardised\nmortality rate\n(in 2018)", ll = c(l_limit, u_limit))
p_tot <- p_raw + p_fit
ggsave(filename = paste("IMAGES/std_mortality_comparison.jpeg" , sep = ""), plot = p_tot , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")


