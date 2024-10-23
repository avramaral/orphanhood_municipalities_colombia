
dd <- 1 # Posterior sample
yy <- 1
ll <- 1


# Reference (blue line)
tmp_alpha_0 <- c(draws[, paste("alpha_0[", yy, "]", sep = "")])[dd]
tmp_alpha_1 <- c(draws[, paste("alpha_0[", yy, "]", sep = "")])[dd]
tmp_mpi_mun <- mpi_municip[ll]
tmp_alpha_0 + tmp_alpha_1 * tmp_mpi_mun + draws[, "std_death_rate_capital_sigma"][dd]

# Fitted 
death_rate_fem <- exp(log_death_rate_fem[yy, , dd]) # (Y x A)
death_rate_mal <- exp(log_death_rate_mal[yy, , dd])

aa <- death_rate_fem * pop_2018_mat[ll, 1, ] / p_nat_total
bb <- death_rate_mal * pop_2018_mat[ll, 2, ] / p_nat_total
sum(aa) + sum(bb) 

# Raw or empirical
std_death_rate_mat[ll, yy]
