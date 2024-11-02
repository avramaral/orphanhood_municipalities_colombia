source("R/header.R")
source("R/aux.R")
source("R/header_plotting.R")

data <- readRDS(file = "DATA/fertility_bias_data.RDS")

mpi_info  <- data$fert %>% dplyr::select(mun, mpi) %>% distinct()
geo_info  <- data$geo_info 
colombia  <- data$colombia

Y <- data$fert$year   %>% unique() %>% length()
G <- data$fert$gender %>% unique() %>% length()
L <- data$fert$mun    %>% unique() %>% length()

raw_births <- data$fert

p <- "fertility_v1.stan"
d <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))
draws <- d$draws
fit_d <- d$data
A_fem <- d$data$A_fem
A_mal <- d$data$A_mal
sample_size <- nrow(draws[, 1]) # 2000

fit_births <- readRDS(paste("FITTED/DATA/count_", strsplit(p, "\\.")[[1]][1], ".RDS", sep = ""))
fit_births <- fit_births %>% as_tibble() %>% rename(mun = Location, gender = Gender, year = Year, age = Age, births = Mean) %>% mutate(mun = factor(mun))
fit_births <- fit_births %>% left_join(y = data$fert[, c("mun", "gender", "year", "age", "population")], by = c("mun", "gender", "year", "age"))
fit_births <- fit_births %>% mutate(fertility_rate = compute_rate(count = births, pop = population))
fit_births <- fit_births %>% dplyr::select(year, mun, gender, age, births, population, fertility_rate) %>% arrange(year, mun, gender, age)
fit_births <- fit_births %>% rename(fit_births = births)

n_raw_births <- sum(raw_births$births)
n_fit_births <- sum(fit_births$fit_births)

print(paste("The fitted number of births is ", round(n_fit_births / n_raw_births, 2), " larger than the raw number of births.", sep = ""))

raw_births <- raw_births %>% left_join(y = fit_births[, c("mun", "gender", "year", "age", "fit_births")], by = c("mun", "gender", "year", "age"))
raw_births <- raw_births %>% dplyr::select(year, mun, gender, age, population, births, fit_births) %>% mutate(mult_fact = fit_births / births)

mult_fact <- raw_births$mult_fact; mult_fact <- mult_fact[!is.infinite(mult_fact) & !is.na(mult_fact)]

# COMPUTE STANDARDISED RATES AND PLOT

data_raw <- raw_births %>% 
            dplyr::select(year, mun, gender, age, births, population) %>% 
            mutate(births = ifelse(population == 0, 0, births)) %>% 
            mutate(fertility_rate = compute_rate(count = births, pop = population))
data_fit <- fit_births %>% 
            dplyr::select(year, mun, gender, age, fit_births) %>% rename(births = fit_births) %>% 
            arrange(year, mun, gender, age) %>% 
            left_join(y = raw_births[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) %>%
            mutate(births = ifelse(population == 0, 0, births)) %>% 
            mutate(fertility_rate = compute_rate(count = births, pop = population))

# FITTED VS EMPIRICAL (NATIONAL)

raw_births_y <- data_raw
raw_births_y <- raw_births_y %>% group_by(year, gender, age) %>% summarise(births = sum(births), population = sum(population)) %>% ungroup() %>% mutate(age = factor(age), fertility_rate = compute_rate(count = births, pop = population))
raw_births_y <- raw_births_y %>% mutate(year = factor(year), gender = factor(gender))

fit_births_y_nat <- readRDS(file = paste("FITTED/", strsplit(p, "\\.")[[1]][1], "_dat.RDS", sep = ""))$draws
fit_births_y_nat_fem <- array(data = 0, dim = c(Y, A_fem, sample_size))
fit_births_y_nat_mal <- array(data = 0, dim = c(Y, A_mal, sample_size)) 
for (y in 1:Y) {
  for (a in 1:A_fem) { fit_births_y_nat_fem[y, a, ] <- c(fit_births_y_nat[, paste("fertility_rate_nat_fem[", y, ",", a, "]", sep = "")]) }
  for (a in 1:A_mal) { fit_births_y_nat_mal[y, a, ] <- c(fit_births_y_nat[, paste("fertility_rate_nat_mal[", y, ",", a, "]", sep = "")]) }
}
fit_births_y_nat_fem_Q025 <- apply(X = fit_births_y_nat_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.025); rownames(fit_births_y_nat_fem_Q025) <- unique(raw_births_y$year); colnames(fit_births_y_nat_fem_Q025) <- unique(filter(raw_births_y, gender == "Female")$age); fit_births_y_nat_fem_Q025 <- melt(fit_births_y_nat_fem_Q025)
fit_births_y_nat_fem_Q500 <- apply(X = fit_births_y_nat_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.500); rownames(fit_births_y_nat_fem_Q500) <- unique(raw_births_y$year); colnames(fit_births_y_nat_fem_Q500) <- unique(filter(raw_births_y, gender == "Female")$age); fit_births_y_nat_fem_Q500 <- melt(fit_births_y_nat_fem_Q500)
fit_births_y_nat_fem_Q975 <- apply(X = fit_births_y_nat_fem, MARGIN = c(1, 2), FUN = quantile, probs = 0.975); rownames(fit_births_y_nat_fem_Q975) <- unique(raw_births_y$year); colnames(fit_births_y_nat_fem_Q975) <- unique(filter(raw_births_y, gender == "Female")$age); fit_births_y_nat_fem_Q975 <- melt(fit_births_y_nat_fem_Q975)
fit_births_y_nat_fem <- cbind(fit_births_y_nat_fem_Q025, fit_births_y_nat_fem_Q500[, 3], fit_births_y_nat_fem_Q975[, 3]); colnames(fit_births_y_nat_fem) <- c("year", "age", "fertility_rate_Q025", "fertility_rate_Q500", "fertility_rate_Q975"); fit_births_y_nat_fem <- fit_births_y_nat_fem %>% as_tibble()
fit_births_y_nat_fem <- fit_births_y_nat_fem %>% mutate(gender = "Female")
fit_births_y_nat_mal_Q025 <- apply(X = fit_births_y_nat_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.025); rownames(fit_births_y_nat_mal_Q025) <- unique(raw_births_y$year); colnames(fit_births_y_nat_mal_Q025) <- unique(filter(raw_births_y, gender ==   "Male")$age); fit_births_y_nat_mal_Q025 <- melt(fit_births_y_nat_mal_Q025)
fit_births_y_nat_mal_Q500 <- apply(X = fit_births_y_nat_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.500); rownames(fit_births_y_nat_mal_Q500) <- unique(raw_births_y$year); colnames(fit_births_y_nat_mal_Q500) <- unique(filter(raw_births_y, gender ==   "Male")$age); fit_births_y_nat_mal_Q500 <- melt(fit_births_y_nat_mal_Q500)
fit_births_y_nat_mal_Q975 <- apply(X = fit_births_y_nat_mal, MARGIN = c(1, 2), FUN = quantile, probs = 0.975); rownames(fit_births_y_nat_mal_Q975) <- unique(raw_births_y$year); colnames(fit_births_y_nat_mal_Q975) <- unique(filter(raw_births_y, gender ==   "Male")$age); fit_births_y_nat_mal_Q975 <- melt(fit_births_y_nat_mal_Q975)
fit_births_y_nat_mal <- cbind(fit_births_y_nat_mal_Q025, fit_births_y_nat_mal_Q500[, 3], fit_births_y_nat_mal_Q975[, 3]); colnames(fit_births_y_nat_mal) <- c("year", "age", "fertility_rate_Q025", "fertility_rate_Q500", "fertility_rate_Q975"); fit_births_y_nat_mal <- fit_births_y_nat_mal %>% as_tibble()
fit_births_y_nat_mal <- fit_births_y_nat_mal %>% mutate(gender =   "Male")
fit_births_y_nat <- rbind(fit_births_y_nat_fem, fit_births_y_nat_mal) %>% dplyr::select(year, gender, age, fertility_rate_Q025, fertility_rate_Q500, fertility_rate_Q975) %>% arrange(year, gender, age) %>% mutate(year = factor(year), gender = factor(gender))
  
my_colors <- plot3D::jet.col(n = length(unique(raw_births_y$year)))
  
fr_plot_1 <- ggplot(data = raw_births_y) +
  geom_line(mapping = aes(x = age, y = fertility_rate, color = year, group = year), linetype = "solid") +
  geom_point(mapping = aes(x = age, y = fertility_rate, color = year), size = 2) +
  geom_errorbar(data = fit_births_y_nat, aes(x = age, ymin = fertility_rate_Q025, ymax = fertility_rate_Q975, color = year)) +
  scale_color_manual(name = "Year", values = my_colors) +
  facet_grid(~ gender) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

fr_plot_1.5 <- ggplot(data = raw_births_y) +
  geom_line(mapping = aes(x = age, y = fertility_rate, color = year, group = year), linetype = "solid") +
  geom_point(data = fit_births_y_nat, aes(x = age, y = fertility_rate_Q500, color = year)) +
  scale_color_manual(name = "Year", values = my_colors) +
  facet_grid(~ gender) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

fr_plot_2 <- ggplot(data = raw_births_y) +
  geom_line(mapping = aes(x = age, y = fertility_rate, color = gender, group = gender), linetype = "solid") +
  geom_point(mapping = aes(x = age, y = fertility_rate, color = gender), size = 2) +
  geom_errorbar(data = fit_births_y_nat, aes(x = age, ymin = fertility_rate_Q025, ymax = fertility_rate_Q975, color = gender)) +
  scale_color_manual(name = "Gender", values = c("#00008FFF", "#800000FF")) +
  facet_wrap(~ year, ncol = 4) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

fr_plot_2.5_fem <- ggplot(data = filter(raw_births_y, gender == "Female")) +
  geom_line(mapping = aes(x = age, y = fertility_rate, color = gender, group = gender), linetype = "solid") +
  geom_point(data = filter(fit_births_y_nat, gender == "Female"), aes(x = age, y = fertility_rate_Q500, color = gender)) +
  scale_color_manual(name = "Gender", values = c("#00008FFF")) +
  facet_wrap(~ year, ncol = 4) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

fr_plot_2.5_mal <- ggplot(data = filter(raw_births_y, gender == "Male")) +
  geom_line(mapping = aes(x = age, y = fertility_rate, color = gender, group = gender), linetype = "solid") +
  geom_point(data = filter(fit_births_y_nat, gender == "Male"), aes(x = age, y = fertility_rate_Q500, color = gender)) +
  scale_color_manual(name = "Gender", values = c("#800000FF")) +
  facet_wrap(~ year, ncol = 4) +
  labs(title = paste("National fertility rate", sep = ""), x = "Age", y = "Fertility rate") +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 

ggsave(filename = paste("IMAGES/RATE_COMPARISON/FERTILITY/fert_coloured_year_error_bar.jpeg",   sep = ""), plot = fr_plot_1,       width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/FERTILITY/fert_coloured_year_median.jpeg",      sep = ""), plot = fr_plot_1.5,     width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/FERTILITY/fert_coloured_gender_error_bar.jpeg", sep = ""), plot = fr_plot_2,       width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/FERTILITY/fert_coloured_female_error_bar.jpeg", sep = ""), plot = fr_plot_2.5_fem, width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")
ggsave(filename = paste("IMAGES/RATE_COMPARISON/FERTILITY/fert_coloured_male_error_bar.jpeg",   sep = ""), plot = fr_plot_2.5_mal, width = 1500, height = 1800, units = c("px"), dpi = 300, bg = "white")

# PLOT STANDARDIZED RATES

for (y in 1998:2021) {
  # y  <- 2018
  yy <- y - 1998 + 1
  y_lim <- c(0, 0.15)
  
  std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == y) %>% dplyr::select(-year)
  std_raw <- std_raw %>% left_join(y = geo_info[, c("mun", "capital")], by = "mun") %>% left_join(y = mpi_info, by = "mun") %>% mutate(capital = factor(capital))
  
  p_raw_pts <- plot_std_rate(data = std_raw, tt = "Raw data (fertility)", y_lim = y_lim)
  
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
  
  p_fit_pts <- plot_fit_std_rate(data_lin = quantiles_lin, data_fit = std_fit, tt = "Fitted data (fertility)", y_lim = y_lim)
  
  (p_tot_pts <- p_raw_pts + p_fit_pts)
  ggsave(filename = paste("IMAGES/STD_RATES_COMPARISON/FERTILITY/std_fertility_comparison_", y ,".jpeg" , sep = ""), plot = p_tot_pts , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
}

# # MAP
# 
# std_raw <- compute_std_rate(data_raw); std_raw <- std_raw %>% filter(year == 2018) %>% dplyr::select(-year)
# std_fit <- compute_std_rate(data_fit); std_fit <- std_fit %>% filter(year == 2018) %>% dplyr::select(-year)
# 
# u_limit <- max(std_raw$std_rate, std_fit$std_rate)
# l_limit <- min(std_raw$std_rate, std_fit$std_rate)
# 
# std_raw <- std_raw %>% left_join(y = colombia, by = "mun")
# std_fit <- std_fit %>% left_join(y = colombia, by = "mun")
# 
# # Filter out islands
# isl1_rw <- std_raw %>% filter((mun %in% c(88001))) 
# isl2_rw <- std_raw %>% filter((mun %in% c(88564))) 
# std_raw <- std_raw %>% filter(!(mun %in% c(88001, 88564)))
# 
# isl1_ft <- std_fit %>% filter((mun %in% c(88001))) 
# isl2_ft <- std_fit %>% filter((mun %in% c(88564))) 
# std_fit <- std_fit %>% filter(!(mun %in% c(88001, 88564)))
# 
# p_raw <- plot_maps(data = std_raw, my_var = "std_rate", tt = "Original data", nm_var = "Standardised\nfertility rate\n(in 2018)", ll = c(l_limit, u_limit))
# p_fit <- plot_maps(data = std_fit, my_var = "std_rate", tt = "Fitted data",   nm_var = "Standardised\nfertility rate\n(in 2018)", ll = c(l_limit, u_limit))
# p_tot <- p_raw + p_fit
# ggsave(filename = paste("IMAGES/std_fertility_comparison.jpeg" , sep = ""), plot = p_tot , width = 3000, height = 1500, units = c("px"), dpi = 300, bg = "white")
