range_0_1 <- function (x, ...) { (x - min(x)) / (max(x) - min(x)) }

inv_logit <- function (x, ...) { exp(x) / (1 + exp(x)) }

inv_logit_2 <- function (x, ...) { 1 / (1 + exp(-x)) }

compute_rate <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0 # 0/0 
  # r[is.infinite(r)] <- 0 # x/0, x > 0
  r
}

compute_std_rate <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mortality_rate", "fertility_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2018) %>% dplyr::select(mun, gender, age, year, population) %>% group_by(mun, gender, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("mun", "year", "age", "gender", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "gender"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(mun, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

compute_std_rate_gender <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mortality_rate", "fertility_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2018) %>% dplyr::select(mun, gender, age, year, population) %>% group_by(mun, gender, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("mun", "year", "age", "gender", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "gender"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(mun, gender, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

compute_std_rate_age_gender <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mortality_rate", "fertility_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2018) %>% dplyr::select(mun, gender, age, year, population) %>% group_by(mun, gender, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("mun", "year", "age", "gender", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "gender"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(mun, age, gender, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

compute_national_std_rate <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mortality_rate", "fertility_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2018) %>% dplyr::select(gender, age, year, population) %>% group_by(gender, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("year", "age", "gender", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "gender"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

compute_national_std_rate_age_gender <- function (data, ...) {
  cns <- colnames(data)
  ctg <- ifelse("deaths" %in% cns, "deaths", "births")                 
  rte <- ifelse("deaths" %in% cns, "mortality_rate", "fertility_rate") 
  
  pop_ref  <- data %>% filter(year %in% 2018) %>% dplyr::select(gender, age, year, population) %>% group_by(gender, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("year", "age", "gender", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "gender"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(age, gender, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
  
  std_rate
}

adj_mort_data <- function (mort, prop_fem, prop_mal, ...) {
  
  valid_ages_fem <- c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")
  valid_ages_mal <- c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
  
  mort <- mort %>% filter(((gender == "Female") & (age %in% valid_ages_fem)) | ((gender == "Male") & (age %in% valid_ages_mal)))
  mort <- mort %>% left_join(y = prop_fem, by = "mun") %>% left_join(y = prop_mal, by = "mun")
  mort <- mort %>% mutate(deaths = ifelse((gender == "Female") & (age == "65-69"), deaths * prop_65_66_fem, deaths)) %>% mutate(population = ifelse((gender == "Female") & (age == "65-69"), population * prop_65_66_fem, population))
  mort <- mort %>% mutate(deaths = ifelse((gender ==   "Male") & (age == "75-79"), deaths * prop_75_76_mal, deaths)) %>% mutate(population = ifelse((gender ==   "Male") & (age == "75-79"), population * prop_75_76_mal, population))
  mort <- mort %>% dplyr::select(-c(prop_65_66_fem, prop_75_76_mal))
  mort <- mort %>% mutate(death_rate = compute_rate(count = deaths, pop = population))
  mort <- mort %>% mutate(age = ifelse((gender == "Female") & (age == "65-69"), "65-66", age))
  mort <- mort %>% mutate(age = ifelse((gender ==   "Male") & (age == "75-79"), "75-76", age))
  
  mort
}

adj_fert_data <- function (fert, ...) {
  fert <- fert %>% filter(!(gender == "Female" & (age %in% c("50-54", "55-59")))) 
  
  fert
}


plot_maps <- function (data, my_var, nm_var = "", tt = "", ll = NULL, my_colors = NA, ...) {
  
  if (is.na(my_colors)) { my_colors <- c("#00008FFF", "#0000F2FF", "#0063FFFF", "#00D4FFFF", "#46FFB8FF", "#B8FF46FF", "#FFD400FF", "#FF6300FF", "#F00000FF", "#800000FF" )}
  
  pp <- ggplot(data = data, aes(geometry = geometry)) + 
    geom_sf(aes(fill = .data[[my_var]]), color = "black") + 
    scale_fill_gradientn(name = nm_var, colours = my_colors, limits = ll) +
    labs(title = tt) + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  pp
}

plot_std_rate <- function (data, tt = "", y_lim = NULL, ...) {
  y_ran <- range(data$std_rate)
  ggplot(data = data) +
    geom_point(mapping = aes(x = mpi, y = std_rate, color = capital, size = capital)) + 
    scale_color_manual(name = "", values = c("#FF000099", "#0000FF99"), labels = c("Non-capital", "Department capital")) +
    scale_size_manual(name = "", values = c(1, 3), labels = c("Non-capital", "Department capital")) +
      geom_smooth(data = data[data$capital == 1, ], mapping = aes(x = mpi, y = std_rate), method = "lm", formula = y ~ x, se = TRUE, color = "blue", linewidth = 0.5, linetype = "solid") + 
    # geom_smooth(data = data[data$capital == 0, ], mapping = aes(x = mpi, y = std_rate), method = "lm", formula = y ~ x, se = TRUE, color = "red",  linewidth = 0.5, linetype = "solid") +  
    scale_x_continuous(breaks = seq(0, 100, 25), labels = seq(0, 100, 25), limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
    labs(title = tt, x = "MPI", y = "Standardised rate") +
    # scale_x_log10() +
    # { if (!is.na(y_lim[1])) ylim(y_lim) } + 
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10")) 
}

plot_fit_std_rate <- function (data_lin, data_fit, tt = "", y_lim = NULL, ...) {
  ggplot(data_lin, aes(x = x)) +
    geom_ribbon(aes(ymin = ll, ymax = uu), fill = "gray", alpha = 0.5) +
    geom_line(aes(y = mm), color = "blue") +
    { if (FALSE) geom_point(data = quantiles_obs, aes(x = x, y = mm), size = 0.25) } +
    geom_point(data = data_fit, mapping = aes(x = mpi, y = std_rate, color = capital, size = capital)) + 
    scale_color_manual(name = "", values = c("#FF000099", "#0000FF99"), labels = c("Non-capital", "Department capital")) +
    scale_size_manual(name = "", values = c(1, 3), labels = c("Non-capital", "Department capital")) +
    scale_x_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 100, 25), limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
    labs(title = tt, x = "MPI", y = "Standardised rate") +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10")) 
}


plot_std_rate_boxplot <- function (data, tt = "", ...) {
  mean_data <- data %>% group_by(age, mpi_class, gender) %>% summarise(mean_std_rate = mean(std_rate, na.rm = TRUE), .groups = "drop")
  
  ggplot(data = data) + 
    facet_grid(mpi_class ~ gender) + 
    geom_boxplot(mapping = aes(x = age, y = std_rate, color = mpi_class)) + 
    geom_line(data = mean_data, mapping = aes(x = age, y = mean_std_rate, group = mpi_class), color = "black", linewidth = 0.5, linetype = "solid") +
    scale_color_manual(name = "MPI CLASS", values = c("#00008FFF", "#46FFB8FF", "#FFD400FF", "#F00000FF")) + 
    labs(title = tt, x = "Age", y = "Standardised rate") +
    scale_y_sqrt() + 
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 12, family = "LM Roman 10"), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) 
}

plot_national_std_rate <- function (data, tt = "", ...) {
  ggplot(data) +
    geom_point(mapping = aes(x = std_rate_raw, y = std_rate_fit, shape = age, colour = gender), size = 3) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "solid") +
    scale_color_manual(name = "Gender", values = c("#00008FFF", "#FFD400FF")) +
    scale_shape_manual(name = "Age",    values = c(0, 2, 4, 6, 8, 11, 15, 17, 19, 23, 1, 3, 5, 7, 9, 10)) +  
    coord_fixed() + 
    scale_x_continuous(limits = c(0, max(data$std_rate_raw, data$std_rate_fit))) + 
    scale_y_continuous(limits = c(0, max(data$std_rate_raw, data$std_rate_fit))) +
    labs(title = tt, x = "Raw standardised rates", y = "Fitted standardised rates") +
    theme_bw() +
    theme(legend.position = "right", text = element_text(size = 10, family = "LM Roman 10"))
}
