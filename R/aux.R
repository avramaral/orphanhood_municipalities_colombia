range_0_1 <- function (x, ...) { (x - min(x)) / (max(x) - min(x)) }

inv_logit <- function (x, ...) { exp(x) / (1 + exp(x)) }

compute_rate <- function (count, pop, ...) { 
  r <- (count / pop) 
  r[is.nan(r)] <- 0 # 0/0 
  # r[is.infinite(r)] <- 0 # x/0, x > 0
  r
}
