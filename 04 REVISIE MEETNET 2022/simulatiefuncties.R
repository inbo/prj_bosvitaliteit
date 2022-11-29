#Keuze van trend per jaar (relatief percentage)
  #bv 12% reductie op 12 jaar: 0.25 * (1-0.12) (of moet je hier 11 nemen?)
  #(1+x)^12 = 0.88
  #1+x = 0.88^(1/12)
  #x = 0.88^(1/12) - 1

#keuze van aantal plots
#keuze van aantal jaar
#keuze van aantal bomen per plot
#keuze range effectief bladverlies
#keuze van betrouwbaarheids van telproces
#keuze van correlatie van een boom naar volgend jaar toe
#keuze van correlatie tussen bomen binnen een plot
#keuze van correlatie tussen plots (misschien niet nodig, want volgt uit bomen)

library(tidyverse)

value_per_timestep <- function(value_t0, value_tn, tn, 
                               keep_t0 = TRUE, relative = TRUE) {

  if (!relative) {
    values <- seq(value_t0, value_tn, length = tn+1)
    attr(values, "trend" ) <- values[2] - values[1]
    return(values)
  }
  ratio <- value_tn / value_t0
  trend <- ratio ^ (1 / (tn))
  values <- value_t0  * trend ^ (0:(tn))
  attr(values, "trend") <- trend
  return(values)
}
rbind(value_per_timestep(0.25, 0.22, 11),
      value_per_timestep(0.25, 0.22, 11, relative = FALSE))

generate_autocor <- function(n, mu, rho, sd) {
  n_var <- length(mu)
  Sigma <- matrix(nrow = n_var, ncol = n_var, data = 0)
  for (i in 1:n_var) {
    for (j in i:n_var) {
      if(i == j) {
        Sigma[i,j] <- sd ^ 2
      } else {
        tdiff <- abs(i-j)
        Sigma[i,j] <- Sigma[j,i] <- rho ^ tdiff * sd * sd
      }
    }
  }
  MASS::mvrnorm(n, mu =  mu, Sigma = Sigma)
}
test <- generate_autocor(100000, rep(0,3), rho = 0.6, sd = 2)


#gemeten = (echte_waarde, sd_observer)
#echte_waarde = (real_expected_value, sd_natuur)
#modelschatting = (modelschatter, sd_model) --> sd_model en sd_natuur samenvooegen?
#variantie_model = var_ruis + jaar_tot_jaar_var_op_trend + var_tussen_plots + var_boom_in_plot + var_metingen_over_tijd_per_boom (autocorrelatie)


simulatie_bosvitaliteit <- function(n_plots=100, n_trees=20, 
                                    start_year = 1, end_year = 12, 
                                    mu_pop_t0 = 0.25,
                                    mu_relative_tn = 0.88, 
                                    sd_between_plots = 0.001, 
                                    sd_between_trees = 0.001,
                                    sd_within_trees = 0.001, 
                                    autocor = 0.001, 
                                    sd_error = 0.001) {
  
  mu_pop_tn <- mu_pop_t0 * mu_relative_tn
  mu_pop_t0_tn <- value_per_timestep(mu_pop_t0, mu_pop_tn, 
                                     end_year - start_year, 
                                     relative = TRUE)
  trend <- attr(mu_pop_t0_tn, "trend")
  
  data <- expand.grid(tree = 1:n_trees, plot = 1:n_plots, 
                      year = start_year:end_year, 
                      nnv_pop_t0 = mu_pop_t0)
  data <- data %>% 
    mutate(nnv_pop = nnv_pop_t0 * trend^(year - start_year))
  
  between_plots <- 
    data.frame(plot = 1:n_plots, 
               plot_effect = rnorm(n_plots, mean = 0, sd = sd_between_plots))
               
  between_trees <- 
    expand.grid(plot = 1:n_plots, tree = 1:n_trees) %>% 
    mutate(tree_effect = rnorm(n(), mean = 0, sd = sd_between_trees))

  between_years <- data %>% 
    group_by(plot, tree) %>% 
    summarise(.groups = "drop", 
              year = start_year:end_year, 
              autocor_effect = generate_autocor(1, 
                                          mu = rep(0, n()), 
                                          sd = sd_within_trees , 
                                          rho = autocor))
  data <- data %>% 
    inner_join(between_plots, by = "plot") %>% 
    inner_join(between_trees, by = c("tree", "plot")) %>% 
    inner_join(between_years, by = c( "tree", "plot", "year")) %>% 
    mutate(epsilon = rnorm(n(), 0, sd_error),
           nnv_expected = nnv_pop + plot_effect + 
             tree_effect + autocor_effect + epsilon, 
           nnv_measured = round(nnv_expected * 20) / 20,#in 5% cats
           nnv_measured = if_else(nnv_measured < 0, 
                                  0, 
                                  if_else(nnv_measured > 1, 
                                          1, 
                                          nnv_measured))) 
  
  data
}
test <- simulatie_bosvitaliteit()



execute_simulation <- function(configset, row) {
  n_sims <- configset$n_sims[row]
  n_plots <- configset$n_plots[row]
  n_trees <- configset$n_trees[row]
  start_year <- configset$start_year[row]
  end_year <- configset$end_year[row]
  mu_pop_t0 <- configset$mu_pop_t0[row]
  mu_relative_tn <- configset$mu_relative_tn[row]
  sd_between_plots <- configset$sd_between_plots[row]
  sd_between_trees <- configset$sd_between_trees[row]
  sd_within_trees <- configset$sd_within_trees[row]
  autocor <- configset$autocor[row]
  sd_error <- configset$sd_error[row]
  
  base_data <- expand.grid(plot = 1:n_plots, 
                           tree = 1:n_trees, 
                           year = start_year:end_year)
  rv <- data.frame(sim = 1:n_sims, est = NA, pval = NA)
  for (i in 1:n_sims) {
    cat("Executing iteration", i, "of", n_sims, "\n")
    sim <- simulatie_bosvitaliteit(n_plots, n_trees, start_year, end_year, 
                                   mu_pop_t0, mu_relative_tn, 
                                   sd_between_plots, 
                                   sd_between_trees, 
                                   sd_within_trees, autocor, 
                                   sd_error)
    newname <- paste("sim", sprintf("%04d", i), sep = "_")
    colnames(sim)[colnames(sim) == "nnv_measured"] <- newname
    base_data <- base_data %>% 
      inner_join(sim %>% select(plot, tree, year, all_of(newname)),
                 by = c("plot", "tree", "year"))  
    
    formula = paste(newname, "year", sep = " ~ " )
    
    model <- lme(data = base_data, 
                 fixed = eval(parse(text = formula)),
                 random = ~1|plot/tree, 
                 correlation = corAR1(form = ~year|plot/tree)) 
    fx <- summary(model)$tTable
    est <- fx[2,1]
    se <- fx[2,2]
    pval <- fx[2,4]
    rv[i, est] <- est
    rv[i, pval] <- pval
    
  }
  return(rv)
}


simconfig <- expand.grid(n_sims = 20, 
                         n_plots = 100, 
                         n_trees = 20, 
                         start_year = 1, 
                         end_year = 12, 
                         mu_pop_t0 = 0.25, 
                         mu_relative_tn = 0.88, 
                         sd_between_plots = 0.1, 
                         sd_between_trees = 0.1, 
                         sd_within_trees = 0.05, 
                         autocor = 0.735,
                         sd_error = 0.00)

result <- execute_simulation(simconfig, 1)

library(nlme)
model <- lme(data = result, 
             sim_0002 ~ year, 
             random = ~1|plot/tree, 
             correlation = corAR1(form = ~year|plot/tree))
summary(model)

view(result)

