convert_resolution <- function (geo_info, mortality_rates, fertility_rates, population, death_count, prop_15_17, type.input, per1K, ...) {
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  if (type.input != "Municipality") {
    
    mortality_rates <- mortality_rates %>% rename(mun = loc) %>% dplyr::select(-mpi)
    fertility_rates <- fertility_rates %>% rename(mun = loc) %>% dplyr::select(-mpi)
    
    mortality_rates <- left_join(x = mortality_rates, y = geo_info, by = c("mun")) %>% dplyr::select(year, all_of(tmp_loc), gender, age, deaths, population, death_rate,     mpi)
    fertility_rates <- left_join(x = fertility_rates, y = geo_info, by = c("mun")) %>% dplyr::select(year, all_of(tmp_loc), gender, age, births, population, fertility_rate, mpi)
    
    mortality_rates <- mortality_rates %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(deaths = sum(deaths), population = sum(population), death_rate     = sum(death_rate    )) %>% ungroup()
    fertility_rates <- fertility_rates %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(births = sum(births), population = sum(population), fertility_rate = sum(fertility_rate)) %>% ungroup()
    
    mortality_rates <- mortality_rates %>% mutate(death_rate     = (compute_rate(count = deaths, pop = population) * ifelse(per1K, 1000, 1)))
    fertility_rates <- fertility_rates %>% mutate(fertility_rate = (compute_rate(count = births, pop = population) * ifelse(per1K, 1000, 1)))
    
    mortality_rates <- mortality_rates %>% rename(loc = all_of(tmp_loc))
    fertility_rates <- fertility_rates %>% rename(loc = all_of(tmp_loc))
    
    population <- population %>% rename(mun = loc)
    population <- left_join(x = population, y = geo_info, by = c("mun")) %>% dplyr::select(year, all_of(tmp_loc), gender, age, population)
    population <- population %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(population = sum(population)) %>% ungroup()
    population <- population %>% rename(loc = all_of(tmp_loc))
    
    death_count <- death_count %>% rename(mun = loc)
    death_count <- left_join(x = death_count, y = geo_info, by = c("mun")) %>% dplyr::select(year, all_of(tmp_loc), gender, age, deaths)
    death_count <- death_count %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(deaths = sum(deaths)) %>% ungroup() %>% select(gender, all_of(tmp_loc), year, deaths, age)
    death_count <- death_count %>% rename(loc = all_of(tmp_loc))
    
    prop_15_17 <- readRDS(file = paste("DATA/prop_15_17_", tmp_loc, ".RDS", sep = ""))
  }
  
  list(mortality_rates = mortality_rates, fertility_rates = fertility_rates, population = population, death_count = death_count, prop_15_17 = prop_15_17)
}

convert_resolution_pop <- function (type.input, population, geo_info, ...) {
  
  prop_15_17 <- readRDS(file = "DATA/prop_15_17.RDS")
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  if (type.input != "Municipality") {
    
    population <- population %>% rename(mun = loc)
    population <- left_join(x = population, y = geo_info, by = c("mun")) %>% dplyr::select(year, all_of(tmp_loc), gender, age, population)
    population <- population %>% group_by(gender, .data[[tmp_loc]], year, age) %>% summarise(population = sum(population)) %>% ungroup()
    population <- population %>% rename(loc = all_of(tmp_loc))
    
    prop_15_17 <- readRDS(file = paste("DATA/prop_15_17_", tmp_loc, ".RDS", sep = ""))
  }
  
  list(population = population, prop_15_17 = prop_15_17)
}

########################################
# NUMBER OF CHILDREN
########################################

process_number_children_year <- function (yy, type.input, fertility_rates, per1K = TRUE, ...) {
  
  # MALE
  
  data_m <- fertility_rates %>% filter(gender == "Male")
  data_m$fertility_rate <- data_m$fertility_rate / ifelse(per1K, 1000, 1)
  
  locs <- unique(data_m$loc)
  
  print("Processing children of male individuals...")
  count <- 1
  if (type.input != "National") { pb <- txtProgressBar(min = 1, max = length(locs), initial = 1) }
  for (l in locs) {
    tmp <- data_m %>% filter(loc == l)
    group <- paste("col_", gsub(" ", "-", yy), "_", gsub(" ", "-", l), sep = "")
    
    process_children_father_55_plus_year(type.input = type.input, yy = yy, group = group, data_m = tmp)
    
    count <- count + 1
    if (type.input != "National") { setTxtProgressBar(pb, count) }
  }
  if (type.input != "National") { close(pb) }
  
  # FEMALE
  
  data_f <- fertility_rates %>% filter(gender == "Female")
  data_f$fertility_rate <- data_f$fertility_rate / ifelse(per1K, 1000, 1)
  
  locs <- unique(data_f$loc)
  
  print("Processing children of female individuals...")
  count <- 1
  if (type.input != "National") { pb <- txtProgressBar(min = 1, max = length(locs), initial = 1) }
  for (l in locs) {
    tmp <- data_f %>% filter(loc == l)
    group <- paste("col_", gsub(" ", "-", yy), "_", gsub(" ", "-", l), sep = "")
    
    process_children_all_year(type.input = type.input, yy = yy, group = group, data_f = tmp)
    
    count <- count + 1
    if (type.input != "National") { setTxtProgressBar(pb, count) }
  }
  if (type.input != "National") { close(pb) }
}

# Process the number of children for male individuals

process_children_father_55_plus_year <- function (type.input, yy, group, data_m, ...) {
  
  children <- matrix(rep(0, 100 * 18), nrow = 100) # 100 x 18: father age x children age
  colnames(children) <- paste(seq(0,  17), " years", sep = "")
  rownames(children) <- paste(seq(1, 100), " years", sep = "")
  # As men may have children until 59, i.e., 59 + 17 = 76
  
  children[76, 18]    <- c(data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year ==  (yy - 17))])
  children[75, 18:17] <- c(data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy - 17, yy - 16))])
  children[74, 18:16] <- c(data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy - 17, yy - 15))])
  children[73, 18:15] <- c(data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy - 17, yy - 14))])
  children[72, 18:14] <- c(data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy - 17, yy - 13))])
  
  children[71, 18:13] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year == yy - 17)],
                           data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year %in% seq(yy - 16, yy - 12))])   
  
  children[70, 18:12] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year %in% seq(yy - 17, yy - 16))] ,
                           data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year %in% seq(yy - 15, yy - 11))]) 
  
  children[69, 18:11] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-17,yy-15))] ,
                           data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-14, yy-10))]) 
  
  children[68, 18:10] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-17,yy-14))] ,
                           data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-13, yy-9))])
  
  children[67, 18:9] <- c(data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-17,yy-13))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-12, yy-8))]) 
  
  children[66, 18:8] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-17)]  ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-16,yy-12))] ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-11, yy-7))])
  
  children[65, 18:7] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year%in% seq(yy-17,yy-16))]  ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-15,yy-11))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-10, yy-6))])
  
  children[64, 18:6] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year%in% seq(yy-17,yy-15))] ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-14,yy-10))] ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-9, yy-5))])
  
  
  children[63, 18:5] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year%in% seq(yy-17,yy-14))] ,
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-13,yy-9))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-8, yy-4))])
  
  children[62, 18:4] <- c(data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-17, yy-13))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-12,yy-8))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-7, yy-3))])
  
  children[61, 18:3] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-17)], 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-16, yy-12))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-11,yy-7))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-6, yy-2))])
  
  children[60, 18:2] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-16))]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-15, yy-11))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-10,yy-6))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-5, yy-1))])
  
  children[59, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-15))]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-14, yy-10))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-9,yy-5))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-4, yy))])
  
  children[58, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-14))], 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-13, yy-9))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-8,yy-4))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-3, yy))])
  
  children[57, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-17, yy-13))] , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-12, yy-8))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-7,yy-3))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-2, yy))])
  
  
  children[56, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-16, yy-12))] , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-11, yy-7))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-6,yy-2))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year%in% seq(yy-1, yy))])
  
  children[55, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year %in% seq(yy-17, yy-16))] , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year %in% seq(yy-15, yy-11))] , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year %in% seq(yy-10, yy-6))],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-5,yy-1))]  ,
                          data_m$fertility_rate[which(data_m$age == "55-59" & data_m$year == yy)])
  
  children[54, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-4,yy))])
  
  children[53, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)], 
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-3,yy))])
  
  children[52, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-2,yy))])
  
  children[51, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year%in% seq(yy-1,yy))])
  
  children[50, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)],
                          data_m$fertility_rate[which(data_m$age == "50-54" & data_m$year == yy)])
  
  children[49, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[48, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[47, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[46, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[45, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "45-49" & data_m$year == yy)])
  
  children[44, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[43, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[42, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[41, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[40, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "40-44" & data_m$year == yy)])
  
  children[39, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[38, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[37, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[36, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[35, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "35-39" & data_m$year == yy)])
  
  children[34, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[33, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[32, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-17)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)]  , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-12)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)]  , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-7)]  ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)]  , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-2)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)]  , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[31, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[30, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "30-34" & data_m$year == yy)])
  
  children[29, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] ,
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[28, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[27, 18:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-17)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[26, 17:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-16)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[25, 16:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-15)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "25-29" & data_m$year == yy)])
  
  children[24, 15:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-14)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[23, 14:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-13)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[22, 13:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-12)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[21, 12:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-11)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[20, 11:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-10)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-5)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "20-24" & data_m$year == yy)])
  
  children[19, 10:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-9)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                          data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] ,
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-4)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                          data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[18, 9:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-8)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[17, 8:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-7)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[16, 7:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-6)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[15, 6:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-5)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] ,
                         data_m$fertility_rate[which(data_m$age == "15-19" & data_m$year == yy)])
  
  children[14, 5:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-4)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[13, 4:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-3)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[12, 3:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-2)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[11, 2:1] <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy-1)] , 
                         data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )
  
  children[10, 1]   <- c(data_m$fertility_rate[which(data_m$age == "10-14" & data_m$year == yy)] )  
  
  children <- as.data.frame(children)
  names(children) = paste(seq(0:17) - 1, " years", sep = "")
  
  write_csv(x = children, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_raw_m.csv", sep = ""))
  
  plot_c <- as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age <- rep(1:100, 18)
  plot_c$child_age  <- sort(rep(seq(18) - 1, 100))
  setnames(plot_c, 1, "prob")
  plot_c$gender = "male"
  
  write_csv(x = plot_c, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_m.csv", sep = ""))
  
  ddf <- as.data.frame(apply(children, 1, sum))
  names(ddf) <- "children"
  ddf$gender <- "male"
  ddf$age <- 1:100
  
  write_csv(x = ddf, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_children_m.csv", sep = ""))
}

# Process the number of children for female individuals

process_children_all_year <- function (type.input, yy, group, data_f, ...) {
  
  children <- matrix(rep(0, 100 * 18), nrow = 100)
  names(children) <- paste(seq(0:17), "years", sep = "")
  
  children[66, 18]    <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-17)]) 
  children[65, 18:17] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17,yy-16))]
  children[64, 18:16] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17, yy-15))]
  children[63, 18:15] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17, yy-14))])
  children[62, 18:14] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-17, yy-13))])
  children[61, 18:13] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-17)], data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-16, yy-12))])
  
  #####
  
  children[60, 18:12] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-16))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-15, yy-11))])
  #####
  
  children[59, 18:11] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-15))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-14, yy-10))])
  
  
  children[58, 18:10] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-14))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-13, yy-9))])
  
  
  children[57, 18:9] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-17, yy-13))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-12, yy-8))])
  
  
  children[56, 18:8] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-17)], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-16, yy-12))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-11, yy-7))])
  
  
  children[55, 18:7] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-16))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-15, yy-11))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-10, yy-6))])
  
  children[54, 18:6] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-15))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-14, yy-10))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-9, yy-5))])
  
  children[53, 18:5] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-14))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-13, yy-9))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-8, yy-4))])
  
  children[52, 18:4] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-17, yy-13))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-12, yy-8))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-7, yy-3))])
  
  children[51, 18:3] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)], 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-16, yy-12))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-11, yy-7))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-6, yy-2))])
  
  children[50, 18:2] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year %in% seq(yy-17,yy-16))],
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year %in% seq(yy-15, yy-11))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year %in% seq(yy-10, yy-6))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year %in% seq(yy-5, yy-1))])
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)], 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)]) 
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-12)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)],
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$year == yy)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[42,18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[41,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[40,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$year == yy)])
  
  children[39,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[38,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[37,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[36,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[35,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$year == yy)])
  
  children[34,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[33,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-13)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[32,18:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[31,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[30,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$year == yy)])
  
  children[29,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] ,
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[28,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[27,18:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-17)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[26,17:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-16)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[25,16:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-15)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$year == yy)])
  
  children[24,15:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-14)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[23,14:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-13)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[22,13:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-12)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[21,12:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-11)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[20,11:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-10)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-5)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$year == yy)])
  
  children[19,10:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-9)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                          data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-4)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[18,9:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-8)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[17,8:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-7)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[16,7:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-6)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[15,6:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-5)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$year == yy)])
  
  children[14,5:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-4)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[13,4:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-3)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[12,3:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-2)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[11,2:1] <- c( data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy-1)] , 
                         data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children[10,1] <- c(  data_f$fertility_rate[which(data_f$age == "10-14" & data_f$year == yy)] )
  
  children <- as.data.frame(children)
  names(children) <- paste(seq(0:17) - 1, " years", sep = "")
  write_csv(x = children, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_raw_f.csv", sep = ""))
  
  plot_c <- as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$mother_age <- rep(1:100, 18)
  plot_c$child_age <- sort(rep(seq(18) - 1, 100))
  setnames(plot_c, 1, "prob")
  plot_c$gender <- "female"
  write_csv(x = plot_c, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_f.csv", sep = ""))
  
  setnames(plot_c, "mother_age", "parents_age")
  plott <- read.csv(paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_m.csv", sep = ""))
  setnames(plott, "father_age", "parents_age")
  plot_all <- rbind(plot_c, plott)
  write_csv(x = plot_all, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_both.csv", sep = ""))
  
  ddf <- as.data.frame(apply(children, 1, sum))
  names(ddf) <- "children"
  ddf$gender <- "female"
  ddf$age <- 1:100
  write_csv(x = ddf, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_children_f.csv", sep = ""))
  
  ddf   <- read.csv(paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_children_f.csv", sep = ""))
  ddf_2 <- read.csv(paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_children_m.csv", sep = ""))
  # Truncate fertility for men for analysis (unnecessary, as I already set the limiting fertility age for men to 59)
  ddf_2$children[ddf_2$age > 77] <- 0
  ddf <- rbind(ddf, ddf_2)
  write_csv(x = ddf, file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_children.csv", sep = ""))
}

########################################
# ORPHANS
########################################

# Process the number of orphans - MAIN FUNCTION

process_nb_orphans_table_dep_national_year <- function (yy, type.input, death_count, ...) {
  
  d_deaths <- death_count %>% filter(year == yy)
  d_deaths <- na.omit(d_deaths)
  d_deaths <- as.data.table(d_deaths)
  
  locs <- unique(d_deaths$loc)
  i <- 0
  dor <- vector('list', length(unique(locs)))
  dor.age <- vector('list', length(unique(locs)))
  
  # TODO olli temporary for checking see 91430 Amazonas La Victoria area 1443 sqkm (dark blue)
  # 91460 Mirití - Paraná  16564sqkm (light blue)
  for (l in locs) {
    
    # Process the orphans by age of adults
    i <- i + 1
    tmp <- d_deaths[loc == l]
    # TODO 
    # olli note
    # 
    # for 2021, Mirití - Paraná there is only one death in Female 50-66 and all else is zero
    # inappropriate rounding could be the key issue at this stage
    # 
    # total male pop > 20 yrs is 475, so expect something like 2.85 deaths here
    
    # If due to suppression issue, the subset table contains no data, then we skip that
    if (nrow(tmp) > 0) {
      group <- paste0("col", "_", gsub(' ', '-', yy), "_", gsub(' ', '-', l))
      
      # print(paste("Processing orphans by age of parents in file: ", group, "...", sep = ""))
      out <- process_orphans_dep_national(d_merge = tmp, group = group)
      dor[[i]] <- out$d_age
      
      # print(paste("Processing orphans by age of children ...", sep = ""))
      out.age <- process_orphans_with_age(d_merge = tmp, group = group, l = l)
      dor.age[[i]] <- out.age$d_age
      
    }
  }
  
  tmp <- data.table::rbindlist(dor, use.names = TRUE, fill = TRUE)
  tmp.age <- data.table::rbindlist(dor.age, use.names = TRUE, fill = TRUE)
  
  write_csv(x = tmp,     file = paste("ORPHANHOOD/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_summary_",          yy, ".csv", sep = ""))
  write_csv(x = tmp.age, file = paste("ORPHANHOOD/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_with_age_summary_", yy, ".csv", sep = ""))
}

# Process the number of orphans without age

process_orphans_dep_national <- function (d_merge, group, ...) {
  
  d_children <- as.data.table(read_csv(file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_children.csv", sep = ""), col_types = cols()))
  # ANDRE #
  d_children_fem <- d_children %>% as_tibble() %>% filter(gender == "female")
  d_children_fem <- d_children_fem %>% mutate(age = age %/% 5) %>% mutate(age = paste0(age * 5, '-' , (age + 1) * 5 - 1))
  d_children_fem[50:66, "age"] <- "50-66"
  d_children_mal <- d_children %>% as_tibble() %>% filter(gender ==   "male")
  d_children_mal <- d_children_mal %>% mutate(age = age %/% 5) %>% mutate(age = paste0(age * 5, '-' , (age + 1) * 5 - 1))
  d_children_mal[60:76, "age"] <- "60-76"
  d_children <- bind_rows(d_children_fem, d_children_mal) %>% as.data.table()
  ##########
  d_children[, age := ifelse(age %in% c('0-4', '5-9'), '0-9', ifelse(age %in% c('85-89', '90-94', '95-99', '100-104'), '85+', age))]
  
  # Truncate male fertility to 59 (so no men over 76 have children under 18) - again, unnecessary, I already dealt with this before
  d_children$ageid <- rep(seq(1, 100, 1), 2)
  d_children[gender == 'male' & ageid > 76, children := 0]
  d_children[, ageid := NULL]
  
  # Truncate female fertility to 49 (so no women over 66 have children under 18) - again, unnecessary, I already dealt with this before
  d_children$ageid <- rep(seq(1, 100, 1), 2)
  d_children[gender == 'female' & ageid > 66, children := 0]
  d_children[, ageid := NULL]
  
  d_children <- d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>% select(-children) %>% ungroup() %>% distinct()
  if ('sex' %in% colnames(d_merge)) {
    d_merge$gender <- as.character(d_merge$sex)
  } else{
    d_merge$gender <- as.character(d_merge$gender)
    
  }
  d_children$gender <- ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 <- merge(d_merge, d_children, by = c('age', 'gender'), all.x = T)
  d_m1 <- as.data.table(d_m1)
  
  # TODO
  # please remove all rounding throughout
  # my clear steer here is that we NEVER round 
  # until plotting or producing tables
  # eg for the national tables takes real valued numbers and write to file using sprintf
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf
  
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39" ,"40-44", "45-49", "50-54", "50-66", "55-59", "60-76"))
  # d_m1[!is.na(d_m1$age), ]
  write_csv(x = d_m1, file = paste("ORPHANHOOD/RESULTS/", toupper(type.input), "/parents_deaths_orphans_", group, ".csv", sep = ""))
  
  d_summary <- d_m1 %>% select(age, gender, loc, deaths, orphans)
  d_summary$age <- as.character(d_summary$age)
  # Merge to age groups for each state
  # d_summary$age <- ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29', ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), '30-64', ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))
  
  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = TRUE)), nb_orphans = round(sum(orphans, na.rm = TRUE))), by = c('loc', 'age', 'gender')]
  
  list(d_summary = d_summary, d_age = d_m1)
}

# Process the number of orphans with age

process_orphans_with_age <- function(d_merge, group, ...) {
  
  # Additional analysis: age of orphans estimation
  # All contains the huge matrix which are really useful to explore the age of children
  d_children <- as.data.table(read_csv(file = paste("ORPHANHOOD/CHILDREN/", toupper(type.input), "/", group, "_child_all_list_both.csv", sep = ""), col_types = cols()))
  # ANDRE #
  d_children_fem <- d_children %>% as_tibble() %>% filter(gender == "female")
  d_children_fem <- d_children_fem %>% mutate(age = parents_age %/% 5) %>% mutate(age = paste0(age * 5, '-' , (age + 1) * 5 - 1))
  d_children_fem <- d_children_fem %>% mutate(age = ifelse(parents_age %in% 50:66, "50-66", age))
  d_children_mal <- d_children %>% as_tibble() %>% filter(gender ==   "male")
  d_children_mal <- d_children_mal %>% mutate(age = parents_age %/% 5) %>% mutate(age = paste0(age * 5, '-' , (age + 1) * 5 - 1))
  d_children_mal <- d_children_mal %>% mutate(age = ifelse(parents_age %in% 60:76, "60-76", age))
  d_children <- bind_rows(d_children_fem, d_children_mal) %>% as.data.table()
  ##########
  d_children[, age := ifelse(age %in% c('0-4', '5-9'), '0-9', ifelse(age %in% c('85-89', '90-94', '95-99', '100-104'), '85+', age))]
  
  # Truncate male fertility to 59 (so no men over 76 have children under 18)
  d_children[gender == 'male' & parents_age > 76, prob := 0]
  # Truncate female fertility to 49 (so no women over 66 have children under 18)
  d_children[gender == 'female' & parents_age > 66, prob := 0]
  
  d_children <- d_children[, list(nb_c = mean(prob)), by = c('age', 'gender', 'child_age')]
  if ('sex' %in% colnames(d_merge)) {
    d_merge$gender <- as.character(d_merge$sex)
  } else {
    d_merge$gender <- as.character(d_merge$gender)
    
  }
  d_children$gender <- ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 <- merge(d_merge, d_children, by = c('age', 'gender'), all.x = T, allow.cartesian = T)
  d_m1 <- as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39" ,"40-44", "45-49", "50-54", "50-66", "55-59", "60-76"))
  
  d_summary <- d_m1 %>% select(age, gender, child_age, loc, deaths, orphans)
  d_summary$age <- as.character(d_summary$age)
  # Merge to age groups for each state
  # d_summary$age <- ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29', ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), '30-64', ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))
  setnames(d_summary, 'child_age', 'age.children')
  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = T)), nb_orphans = round(sum(orphans, na.rm = T))), by = c('loc', 'age', 'gender', 'age.children')]
  
  list(d_summary = d_summary, d_age = d_m1)
}

########################################
# OUTPUT
########################################

# Create table with number of orphans

orphan_table <- function (type.input, population, prop_15_17, geo_info, per_n_children = 100000, file_name = "", ...) {
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  infiles <- (list.files(path = paste("ORPHANHOOD/RESULTS/", toupper(type.input), sep = ""), pattern = paste(type.input, "_parents_deaths_orphans_summary", sep = ""), full.names = TRUE, recursive = FALSE))
  orphans_all_years <- data.table()
  for (i in seq_len(length(infiles))) {
    infile <- infiles[i]
    orphans_all_years <- rbind(orphans_all_years, data.table(read.csv(infile)))
  }
  
  write_csv(x = orphans_all_years, file = paste("ORPHANHOOD/SUMMARY/orphans_by_age_gender_year_loc_", tmp_loc, file_name, ".csv", sep = ""))
  
  orphans_locs_years <- orphans_all_years[, list(orphans = sum(orphans)), by = c("loc", "year")]
  orphans_locs_years <- as_tibble(orphans_locs_years) %>% mutate({{tmp_loc}} := loc) %>% select(all_of(tmp_loc), year, orphans)
  orphans_locs_years[, tmp_loc] <- factor(unname(unlist(c(orphans_locs_years[, tmp_loc]))))
  cp_orphans_locs_years <- orphans_locs_years; colnames(cp_orphans_locs_years) <- c("loc", "year", "orphans") # For later use
  tmp_geo_info <- geo_info[, c(tmp_loc, paste(tmp_loc, "_name", sep = ""))] %>% distinct()
  if (type.input != "Municipality") {
    orphans_locs_years <- left_join(x = orphans_locs_years, y = tmp_geo_info, by = tmp_loc) %>%                                                            select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphans)
  } else {
    orphans_locs_years <- left_join(x = orphans_locs_years, y = tmp_geo_info, by = tmp_loc) %>% mutate(mun_name = paste(mun, " ", mun_name, sep = "")) %>% select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphans)
  }
  
  orphans_table <- orphans_locs_years %>% pivot_wider(names_from = year, values_from = orphans)
  setnames(orphans_table, paste(tmp_loc, "_name", sep = ""), type.input)
  write_csv(x = orphans_table, file = paste("ORPHANHOOD/SUMMARY/orphans_by_year_", tmp_loc, file_name, ".csv", sep = ""))
  
  ##############################
  ##############################
  
  tmp_population <- population %>% filter(age %in% c("0-9", "10-14", "15-19"))
  tmp_population <- left_join(x = tmp_population, y = prop_15_17, by = c("loc"))
  tmp_population <- tmp_population %>% mutate(population = ifelse(age == "15-19", population * prop_15_17, population)) %>% select(-prop_15_17)
  tmp_population <- tmp_population %>% group_by(loc, year) %>% summarize(children = sum(population)) %>% ungroup()
  
  orphans_and_rate <- left_join(x = cp_orphans_locs_years, y = tmp_population, by = c("loc", "year"))
  orphans_and_rate <- orphans_and_rate %>% mutate(orphan_rate = ceiling(orphans * per_n_children / children))
  write_csv(x = orphans_and_rate, file = paste("ORPHANHOOD/SUMMARY/orphans_and_rate_by_year_", tmp_loc, "_list_per_", as.integer(per_n_children), file_name, ".csv", sep = ""))
  
  colnames(orphans_and_rate)[1] <- tmp_loc
  if (type.input != "Municipality") {
    orphans_per_child <- left_join(x = orphans_and_rate, y = tmp_geo_info, by = tmp_loc) %>%                                                            select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphan_rate)
  } else {
    orphans_per_child <- left_join(x = orphans_and_rate, y = tmp_geo_info, by = tmp_loc) %>% mutate(mun_name = paste(mun, " ", mun_name, sep = "")) %>% select(all_of(paste(tmp_loc, "_name", sep = "")), year, orphan_rate)
  }
  orphans_rate_table <- orphans_per_child %>% pivot_wider(names_from = year, values_from = orphan_rate) 
  write_csv(x = orphans_rate_table, file = paste("ORPHANHOOD/SUMMARY/orphans_rate_by_year_", tmp_loc, "_per_", as.integer(per_n_children), file_name, ".csv", sep = ""))
}

########################################
# POSTPROCESSING, INCIDENCE & PREVALENCE
########################################

# INCIDENCE

generate_incidence_table <- function (type.input, per_n_children, geo_info, should_round = TRUE, file_name = "", ...) {
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  orphans_all_years <- read_csv(file = paste("ORPHANHOOD/SUMMARY/orphans_by_age_gender_year_loc_", tmp_loc, file_name, ".csv", sep = ""))
  orphans_all_years <- as.data.table(orphans_all_years)
  
  ##############################

  orphans_locs_years <- orphans_all_years[, list(orphans = sum(orphans)), by = c("loc", "year", "gender")]
  orphans_locs_years <- as_tibble(orphans_locs_years) %>% mutate({{tmp_loc}} := loc) %>% dplyr::select(all_of(tmp_loc), gender, year, orphans) %>% arrange({{tmp_loc}}, gender, year)
  orphans_locs_years[, tmp_loc] <- factor(unname(unlist(c(orphans_locs_years[, tmp_loc]))))

  cp_orphans_locs_years <- orphans_locs_years; colnames(cp_orphans_locs_years) <- c("loc", "gender", "year", "orphans") # For later use
  tmp_geo_info <- geo_info[, c(tmp_loc, paste(tmp_loc, "_name", sep = ""))] %>% unique()
  orphans_locs_years <- left_join(x = orphans_locs_years, y = tmp_geo_info, by = tmp_loc) %>% dplyr::select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), gender, year, orphans) 
  orphans_locs_years <- orphans_locs_years %>% filter((year >= 2015),  (year <= 2021))
  
  ##############################
  
  tmp_population <- population %>% filter(age %in% c("0-9", "10-14", "15-19"))
  tmp_population <- left_join(x = tmp_population, y = prop_15_17, by = c("loc"))
  tmp_population <- tmp_population %>% mutate(population = ifelse(age == "15-19", population * prop_15_17, population)) %>% dplyr::select(-prop_15_17)
  tmp_population <- tmp_population %>% group_by(loc, year) %>% summarize(children = sum(population)) %>% ungroup()
  
  orphans_and_rate <- left_join(x = cp_orphans_locs_years, y = tmp_population, by = c("loc", "year"))
  if (should_round) {
    orphans_and_rate <- orphans_and_rate %>% mutate(orphan_rate = ceiling(orphans * per_n_children / children))
  } else {
    orphans_and_rate <- orphans_and_rate %>% mutate(orphan_rate =        (orphans * per_n_children / children))
  }
  orphans_and_rate <- orphans_and_rate %>% filter((year >= 2015), (year <= 2021))
  
  colnames(orphans_and_rate)[1] <- tmp_loc
  orphans_per_child <- left_join(x = orphans_and_rate, y = tmp_geo_info, by = tmp_loc) %>% dplyr::select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), gender, year, orphan_rate)
  
  ##############################
  
  # Convert columns and types (not mandatory)
  
  colnames(orphans_locs_years)[ncol(orphans_locs_years)] <- "n_orp"
  colnames(orphans_per_child)[ncol(orphans_per_child)]   <- "n_orp"
  
  orphans_locs_years$n_orp <- as.numeric(orphans_locs_years$n_orp)
  orphans_per_child$n_orp  <- as.numeric(orphans_per_child$n_orp)
  
  orphans_locs_years$year <- as.integer(orphans_locs_years$year)
  orphans_per_child$year  <- as.integer(orphans_per_child$year)
  
  ##############################
  
  # Check for missing locations (i.e., in case there are municipalities with no orphans)
  
  if (type.input == "Municipality") {
    i <- 1
    for (yy in 2015:2021) {
      tmp_orphans_locs_years <- orphans_locs_years %>% filter(year == yy)
      tmp_orphans_per_child  <- orphans_per_child  %>% filter(year == yy)
      for (g in c("Female", "Male")) {
        tmp_tmp_orphans_locs_years <- tmp_orphans_locs_years %>% filter(gender == g)
        tmp_tmp_orphans_locs_years <- right_join(x = tmp_tmp_orphans_locs_years, y = tmp_geo_info, by = c(tmp_loc, paste(tmp_loc, "_name", sep = "")))
        tmp_tmp_orphans_locs_years <- tmp_tmp_orphans_locs_years %>% mutate(gender = g, year = yy, n_orp = ifelse(is.na(n_orp), 0, n_orp))
        
        tmp_tmp_orphans_per_child  <- tmp_orphans_per_child  %>% filter(gender == g)
        tmp_tmp_orphans_per_child  <- right_join(x = tmp_tmp_orphans_per_child , y = tmp_geo_info, by = c(tmp_loc, paste(tmp_loc, "_name", sep = "")))
        tmp_tmp_orphans_per_child  <- tmp_tmp_orphans_per_child  %>% mutate(gender = g, year = yy, n_orp = ifelse(is.na(n_orp), 0, n_orp))
        
        if (i == 1) {
          final_orphans_locs_years <- tmp_tmp_orphans_locs_years
          final_orphans_per_child  <- tmp_tmp_orphans_per_child
        } else {
          final_orphans_locs_years <- bind_rows(final_orphans_locs_years, tmp_tmp_orphans_locs_years)
          final_orphans_per_child  <- bind_rows(final_orphans_per_child , tmp_tmp_orphans_per_child ) 
        }
        i <- i + 1
      }
    }
    
    orphans_locs_years <- final_orphans_locs_years
    orphans_per_child  <- final_orphans_per_child 
  }
  
  list(orphans = orphans_locs_years, orphans_per_child = orphans_per_child, tmp_population = tmp_population, per_n_children = per_n_children)
}

# PREVALENCE

generate_prevalence_table <- function (type.input, per_n_children, geo_info, should_round = TRUE, ...) {
  
  if (should_round) { decimal_places <- 0 } else { decimal_places <- 2 }
  
  if (type.input == "Municipality") { tmp_loc <- "mun" } else if (type.input == "Department") { tmp_loc <- "dep" } else if (type.input == "Region") { tmp_loc <- "reg" } else if (type.input == "National") { tmp_loc <- "nat" } else { stop("Choose a valid `type.input`.") }
  
  prop_15_17 <- readRDS(file = "DATA/prop_15_17.RDS") %>% rename(loc = mun) %>% arrange(loc)
    
  pop_all_years <- readRDS(file = "DATA/population_all.RDS")
  pop_all_years <- pop_all_years %>% dplyr::select(year, mun, gender, age, population) %>% mutate(mun = as.numeric(as.character(mun)))
  pop_all_years <- pop_all_years %>% filter(mun %in% valid_muns) %>% mutate(mun = factor(mun))
  pop_all_years <- pop_all_years %>% filter(year <= 2021) %>% arrange(year, mun, gender, age)
  pop_all_years <- pop_all_years %>% rename(loc = mun) %>% left_join(y = prop_15_17, by = "loc")
  pop_all_years <- pop_all_years %>% mutate(population = if_else(age == "15-19", population * prop_15_17, population)) %>% dplyr::select(year, loc, gender, age, population)
  
  if (length(unique(pop_all_years$loc)) < 1000) { stop("Error. `pop_all_years` is assumed to be at the municipality level.") }
  if (type.input == "Department") {
    pop_all_years <- pop_all_years %>% rename(mun = loc)
    pop_all_years <- pop_all_years %>% left_join(y = geo_info[, c("mun", "dep")], by = "mun") %>% dplyr::select(year, dep, gender, age, population) %>% group_by(gender, dep, year, age) %>% summarise(population = sum(population)) %>% ungroup() %>% rename(loc = dep)
  } else if (type.input == "Region") {
    pop_all_years <- pop_all_years %>% rename(reg = loc)
    pop_all_years <- pop_all_years %>% left_join(y = geo_info[, c("mun", "reg")], by = "mun") %>% dplyr::select(year, reg, gender, age, population) %>% group_by(gender, reg, year, age) %>% summarise(population = sum(population)) %>% ungroup() %>% rename(loc = reg)
  } else if (type.input == "National") {
    pop_all_years <- pop_all_years %>% rename(nat = loc)
    pop_all_years <- pop_all_years %>% left_join(y = geo_info[, c("mun", "nat")], by = "mun") %>% dplyr::select(year, nat, gender, age, population) %>% group_by(gender, nat, year, age) %>% summarise(population = sum(population)) %>% ungroup() %>% rename(loc = nat)
  } else { error("Choose a valid `type.input`.") }
  
  
  nb_orphs_fem       <- nb_orphs_mal     <- list()
  nb_orphs_per_n_fem <- nb_orphs_per_n_mal <- list()
  nb_children_pp_fem <- nb_children_pp_mal <- list()
  total_pop <- 0
  j <- 0
  for (y in 2004:2021) {
    diff_y <- 2021 - y
    age_ch <- 17 - diff_y
    
    j <- j + 1
    
    orphans_by_age <- read_csv(paste("ORPHANHOOD/RESULTS/", toupper(type.input), "/", type.input, "_parents_deaths_orphans_with_age_summary_", y, ".csv", sep = ""), col_types = cols())
    # Due to rounding effects, the numbers in `department_parents_deaths_orphans_with_age_summary` are slightly different than the ones in `department_parents_deaths_orphans_summary`.
    orphans_by_age <- orphans_by_age %>% filter(child_age <= age_ch)
    locs <- orphans_by_age %>% select(loc) %>% unique() %>% c() %>% unlist() %>% unname()
    nb_orphs_fem[[j]]       <- nb_orphs_mal[[j]]       <- rep(0, length(locs))
    nb_orphs_per_n_fem[[j]] <- nb_orphs_per_n_mal[[j]] <- rep(0, length(locs))
    nb_children_pp_fem[[j]] <- nb_children_pp_mal[[j]] <- rep(0, length(locs))
    i <- 0
    for (l in locs) {
      i <- i + 1
      orphans_by_age_loc <- orphans_by_age %>% filter(loc == l)
      
      for (g in c("Female", "Male")) {
        orphans_by_age_loc_gender <- orphans_by_age_loc %>% filter(gender == g)
        
        total_nb_orphs <- orphans_by_age_loc_gender %>% select(orphans) %>% sum()
        # Already corrected. `15-19` actually refers to `15-17`.
        y <- 2021
        tmp_population <- pop_all_years %>% filter(year == y, loc == l, age %in% c("0-9", "10-14", "15-19")) %>% select(population) %>% sum()
        if (g == "Female") {
          nb_orphs_fem[[j]][i]       <- round(total_nb_orphs, 2)
          nb_orphs_per_n_fem[[j]][i] <- round(total_nb_orphs / tmp_population * per_n_children, 2)
          nb_children_pp_fem[[j]][i] <- round(tmp_population, 2)
        } else {
          nb_orphs_mal[[j]][i]       <- round(total_nb_orphs, 2)
          nb_orphs_per_n_mal[[j]][i] <- round(total_nb_orphs / tmp_population * per_n_children, 2)
          nb_children_pp_mal[[j]][i] <- round(tmp_population, 2)
        }
      }
    }
  }
  
  aux_nb_orphs <- function (data, locs, tmp_loc, g, ...) {
    data <- as.data.frame(do.call(cbind, data))
    colnames(data) <- 2004:2021
    data <- data.frame(loc = locs, n_orp = rowSums(data))
    colnames(data) <- c(tmp_loc, "n_orp")
    data[, tmp_loc] <- factor(data[, tmp_loc])
    tmp_geo_info <- geo_info %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = ""))) %>% distinct()
    data <- left_join(x = data, y = tmp_geo_info, by = tmp_loc)
    data <- data %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), "n_orp") %>% arrange({{tmp_loc}})
    data$gender <- g
    data %>% select(all_of(tmp_loc), all_of(paste(tmp_loc, "_name", sep = "")), gender, n_orp)
  }
  
  nb_orphs_fem <- aux_nb_orphs(data = nb_orphs_fem, locs = locs, tmp_loc = tmp_loc, g = "Female") 
  nb_orphs_mal <- aux_nb_orphs(data = nb_orphs_mal, locs = locs, tmp_loc = tmp_loc, g = "Male")
  
  nb_orphs <- bind_rows(nb_orphs_fem, nb_orphs_mal)
  
  ##########
  
  nb_orphs_per_n_fem <- aux_nb_orphs(data = nb_orphs_per_n_fem, locs = locs, tmp_loc = tmp_loc, g = "Female")
  nb_orphs_per_n_mal <- aux_nb_orphs(data = nb_orphs_per_n_mal, locs = locs, tmp_loc = tmp_loc, g = "Male")
  
  nb_orphs_per_n <- bind_rows(nb_orphs_per_n_fem, nb_orphs_per_n_mal)
  
  ##########
  
  tmp_nb_children_pp_fem <- nb_orphs_fem %>% rename(children = n_orp)
  tmp_nb_children_pp_fem$children <- nb_children_pp_fem[[1]]
  nb_children_pp_fem <- tmp_nb_children_pp_fem
  
  tmp_nb_children_pp_mal <- nb_orphs_mal %>% rename(children = n_orp)
  tmp_nb_children_pp_mal$children <- nb_children_pp_mal[[1]]
  nb_children_pp_mal <- tmp_nb_children_pp_mal
  
  nb_children_pp <- as_tibble(bind_rows(nb_children_pp_fem, nb_children_pp_mal))
  
  ##########
  
  list(orphans = as_tibble(nb_orphs), orphans_per_child = as_tibble(nb_orphs_per_n), nb_children = nb_children_pp, per_n_children = per_n_children)
}

