updated_deaths <- updated_deaths %>% as_tibble()
updated_deaths <- updated_deaths %>% rename(mun = Location, gender = Gender, age = Age, year = Year) %>% mutate(mun = factor(mun))
updated_deaths <- updated_deaths %>% left_join(y = pop, by = c("year", "mun", "gender", "age"))
updated_deaths <- updated_deaths %>% left_join(y = p_nat, by = c("gender", "age"))
updated_deaths <- updated_deaths %>% dplyr::select(-death_rate) %>% mutate(death_rate = compute_rate(count = Mean, pop = population))
updated_deaths <- updated_deaths %>% mutate(std_rate = (p_nat / p_nat_total) * death_rate)
updated_deaths <- updated_deaths %>% dplyr::select(mun, gender, year, age, std_rate)
updated_deaths <- updated_deaths %>% group_by(mun, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
updated_deaths %>% filter(year == 2018)


y <- 21 # 2018
l <-  1
tmp_alpha_0 <- c(draws[, paste("alpha_0[", y, "]", sep = "")])
tmp_alpha_1 <- c(draws[, paste("alpha_1[", y, "]", sep = "")])
tmp_mpi_mun <- mpi_municip[l]
tmp_linear_mean <- tmp_alpha_0 + tmp_alpha_1 * tmp_mpi_mun
tmp_linear_mean[1]



d_fem <- deaths_fem[, , 1]; rownames(d_fem) <- unique(pop$year); colnames(d_fem) <- unique(pop$age)
d_mal <- deaths_mal[, , 1]; rownames(d_mal) <- unique(pop$year); colnames(d_mal) <- unique(pop$age)

d_fem <- melt(d_fem) %>% as_tibble() %>% rename(year = Var1, age = Var2, death_rate = value) %>% mutate(mun = 5001, gender = "Female")
d_mal <- melt(d_mal) %>% as_tibble() %>% rename(year = Var1, age = Var2, death_rate = value) %>% mutate(mun = 5001, gender =   "Male")

d <- bind_rows(d_fem, d_mal) %>% dplyr::select(year, mun, gender, age, death_rate) %>% mutate(mun = factor(mun))
d <- d %>% left_join(y = pop[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) 
d <- d %>% mutate(deaths = population * death_rate)
d <- d %>% left_join(y = p_nat, by = c("gender", "age"))
d <- d %>% mutate(std_rate = (p_nat / p_nat_total) * death_rate)
d <- d %>% group_by(mun, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup()
d %>% filter(year == 2018)




dths_fem <- log_death_rate_fem[,,1]; rownames(dths_fem) <- unique(pop$year); colnames(dths_fem) <- unique(pop$age)
dths_mal <- log_death_rate_mal[,,1]; rownames(dths_mal) <- unique(pop$year); colnames(dths_mal) <- unique(pop$age)
dths_fem <- melt(dths_fem) %>% as_tibble() %>% mutate(gender = "Female")
dths_mal <- melt(dths_mal) %>% as_tibble() %>% mutate(gender =   "Male")
dths <- bind_rows(dths_fem, dths_mal)
dths <- dths %>% rename(year = Var1, age = Var2, death_rate = value) %>% mutate(death_rate = exp(death_rate)) %>% dplyr::select(year, age, gender, death_rate)
dths <- dths %>% mutate(mun = 5001) %>% mutate(mun = factor(mun))
dths <- dths %>% left_join(y = pop[, c("year", "mun", "gender", "age", "population")], by = c("year", "mun", "gender", "age")) 
dths <- dths %>% left_join(y = p_nat, by = c("gender", "age"))
dths <- dths %>% mutate(std_rate = (p_nat / p_nat_total) * death_rate)
dths %>% group_by(mun, year) %>% summarise(std_rate = sum(std_rate)) %>% ungroup() %>% filter(year == 2018)


y <- 21
c(draws[, paste("std_death_rate_nat[", y, "]", sep = "")])[1]

cum <- 0
for (a in 1:A) {
  cum <- cum + (inv_logit(c(draws[, paste("inv_logit_death_rate_nat[1,", y, ",", a, "]", sep = "")])[1]) * p_nat_mat[1, a] / p_nat_total)
  cum <- cum + (inv_logit(c(draws[, paste("inv_logit_death_rate_nat[2,", y, ",", a, "]", sep = "")])[1]) * p_nat_mat[2, a] / p_nat_total)
}
cum  














y <- 21
tmp_death_rate_nat_fem <- matrix(0, nrow = A, ncol = 4000)
tmp_death_rate_nat_mal <- matrix(0, nrow = A, ncol = 4000)
for (a in 1:A) {
  # Fitted national rates
  tmp_death_rate_nat_fem[a, ] <- c(draws[, paste("inv_logit_death_rate_nat[1,", y, ",", a, "]", sep = "")])
  tmp_death_rate_nat_mal[a, ] <- c(draws[, paste("inv_logit_death_rate_nat[2,", y, ",", a, "]", sep = "")])
}
tmp_nu_fem <- inv_logit(tmp_death_rate_nat_fem[, 1]) * (p_nat_mat[1, ] / p_nat_total)
tmp_nu_mal <- inv_logit(tmp_death_rate_nat_mal[, 1]) * (p_nat_mat[2, ] / p_nat_total)
tmp_nu <- sum(tmp_nu_fem) + sum(tmp_nu_mal) 

(tmp_mult <- tmp_linear_mean[1] / tmp_nu)

y <- 21
n_deaths <- 0
for (a in 1:A) {
  for (g in 1:G) {
    n_deaths <- n_deaths + (multiplier[1, 21, 1] * inv_logit(c(draws[, paste("inv_logit_death_rate_nat[1", ",", y, ",", a, "]", sep = "")])) * pop_fem[1, y, a])
    n_deaths <- n_deaths + (multiplier[1, 21, 1] * inv_logit(c(draws[, paste("inv_logit_death_rate_nat[2", ",", y, ",", a, "]", sep = "")])) * pop_mal[1, y, a])
  }
}
n_deaths[1]


  