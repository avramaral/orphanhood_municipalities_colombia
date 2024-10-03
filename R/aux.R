range_0_1 <- function (x, ...) { (x - min(x)) / (max(x) - min(x)) }

inv_logit <- function (x, ...) { exp(x) / (1 + exp(x)) }

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
  
  pop_ref  <- data %>% filter(year %in% 2017:2019) %>% dplyr::select(mun, gender, age, year, population) %>% group_by(mun, gender, age) %>% summarise(population = mean(population)) %>% ungroup()
  p_nat    <- pop_ref %>% group_by(gender, age) %>% summarise(p_nat = sum(population)) %>% ungroup()
  p_nat_total <- sum(pop_ref$population)
  
  my_clmns <- c("mun", "year", "age", "gender", ctg, "population", rte)
  std_rate <- data %>% dplyr::select(all_of(my_clmns))
  std_rate <- std_rate %>% left_join(y = p_nat, by = c("age", "gender"))
  std_rate <- std_rate %>% mutate(p_nat_total = p_nat_total)
  std_rate <- std_rate %>% mutate(std_rate = ((p_nat / p_nat_total) * get(rte)))
  std_rate <- std_rate %>% group_by(mun, year) %>% summarise(std_rate = mean(std_rate)) %>% ungroup()
  
  std_rate
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
