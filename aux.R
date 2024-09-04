
death_rate_pop_array <- function (array_file = "DATA/death-rate_pop_array.RDS", ...) {

  if (!file.exists(array_file)) {
    
    pb <- txtProgressBar(min = 1, max = L, initial = 1)
    death_rate <- array(data = 0, dim = c(L, Y, A, G))
    total_popu <- array(data = 0, dim = c(L, Y, A, G))
    for (l in 1:L) {
      lls <- unique(mort$mun)[l]
      mort_tmp <- mort %>% filter(mun == lls)
      for (y in 1:Y) {
        yys <- unique(mort$year)[y]
        mort_tmp_tmp <- mort_tmp %>% filter(year == yys)
        for (a in 1:A) {
          aas <- unique(mort$age)[a]
          mort_tmp_tmp_tmp <- mort_tmp_tmp %>% filter(age == aas)
          for (g in 1:G) {
            ggs <- unique(mort$gender)[g]
            mort_tmp_tmp_tmp_tmp <- mort_tmp_tmp_tmp %>% filter(gender == ggs)
            
            death_rate[l, y, a, g] <- unname(unlist(c(mort_tmp_tmp_tmp_tmp$death_rate)))
            total_popu[l, y, a, g] <- unname(unlist(c(mort_tmp_tmp_tmp_tmp$population)))
          }
        }
      }
      setTxtProgressBar(pb, l)
    }
    close(pb)
    
    death_rate_pop <- list(death_rate = death_rate, total_popu = total_popu)
    saveRDS(object = death_rate_pop, file = array_file)
    
  } else {
    death_rate_pop <- readRDS(array_file)
  } 
  
  death_rate_pop
}

