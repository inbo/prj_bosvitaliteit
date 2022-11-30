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
  rv <- MASS::mvrnorm(n, mu =  mu, Sigma = Sigma)
  colnames(rv) <- 0:(ncol(rv) - 1)
  rv
}
test <- generate_autocor(10000, rep(0,11), rho = 0.6, sd = 2)
testdf <- as.data.frame(test) %>%
  mutate(id = 1:n()) %>% 
  pivot_longer(cols = -id) %>% 
  mutate(time = as.numeric(substring(name, 2)))
mod <- gls(data = testdf, value ~ 1, correlation = corAR1(form = ~time|id))
summary(mod)

#gemeten = (echte_waarde, sd_observer)
#echte_waarde = (real_expected_value, sd_natuur)
#modelschatting = (modelschatter, sd_model) --> sd_model en sd_natuur samenvooegen?
#variantie_model = var_ruis + jaar_tot_jaar_var_op_trend + var_tussen_plots + var_boom_in_plot + var_metingen_over_tijd_per_boom (autocorrelatie)


simulatie_bosvitaliteit <- function(n_plots=70, n_trees=20, 
                                    years = 12, #first year 0, last 12
                                    mu_pop_t0 = 0.25,
                                    mu_relative_tn = 0.88, 
                                    sd_trend = 0.01,
                                    sd_between_plots = 0.1, 
                                    sd_between_trees_nat = 0.05,
                                    sd_between_trees_obs = 0.05,
                                    sd_within_trees = 0.1, 
                                    autocor = 0.75, 
                                    sd_error = 0) {
  
  yearly_trend <- ((mu_relative_tn * mu_pop_t0)  - mu_pop_t0) / years
  start_year <- 0
  end_year <- years
  
  trend_modifier <- 
    data.frame(start = mu_pop_t0,
               year = 0:years, 
               base = yearly_trend * 0:years,
               random = c(0, rnorm(years, 0, sd_trend))) %>% 
    mutate (total_trend = base + random, 
            val_pop = start + total_trend)
  
  #init data
  data <- expand.grid(tree = 1:n_trees, plot = 1:n_plots, 
                      year = start_year:end_year, 
                      val_pop_t0 = mu_pop_t0)
  #population trend + observer effect
  #het observer effect is logischerwijs hetgeen de autocorrelatie mindert
  #omdat dit helemaal nergens mee linkt
  #aangezien we toch verwachten dat de fout meer boomafhankelijk is dan puur
  #onafhankelijk, zullen we dit observer effect om boomniveau toepassen
  #en niet willekeurig over alle metingen heen
  #dus hoe groter het observer effect hoe groter de interne autocorrelatie
  #moet zijn om tot de geobserveerde autocorrelatie van ca. 0.75 te geraken
  data <- data %>% 
    inner_join(trend_modifier %>% select(year, val_pop), by = "year") %>% 
    mutate(val_observer = rnorm(n(), 0, sd_error))
  
  #between plot variation
  between_plots <- 
    data.frame(plot = 1:n_plots, 
               val_plot = rnorm(n_plots, mean = 0, sd = sd_between_plots))
  
  #between trees in plot variation
  between_trees <- 
    expand.grid(plot = 1:n_plots, tree = 1:n_trees) %>% 
    mutate(val_tree = rnorm(n(), mean = 0, sd = sd_between_trees_nat) + 
                      rnorm(n(), mean = 0, sd = sd_between_trees_obs))

  #within tree variation over time (autocorrelated)
  epsilon <- data %>% 
    filter(year == start_year) %>% 
    select(-year, -val_pop_t0, -val_pop, -val_observer) %>% 
    bind_cols(generate_autocor(n_plots * n_trees, 
                               rep(0, length(start_year:end_year)),
                               rho = autocor, 
                               sd = sd_within_trees)) %>% 
    pivot_longer(cols = -c(tree, plot), 
                 names_to = "year", 
                 values_to = "val_eps") %>% 
    mutate(year = as.numeric(year) + start_year)
  
  data <- data %>% 
    inner_join(between_plots, by = "plot") %>% 
    inner_join(between_trees, by = c("plot", "tree")) %>% 
    inner_join(epsilon, by = c("plot", "tree", "year")) %>% 
    mutate(val_mod = val_pop + val_plot + val_tree, 
           val_observed = val_mod + val_eps + val_observer,
           val_observed = if_else(val_observed < 0, 0, val_observed),
           val_observed = if_else(val_observed > 1, 1, val_observed), 
           val_categorised = round(val_observed * 20)/20)
  
  data
}
test <- simulatie_bosvitaliteit(autocor = 0.80)

mod <- lme(val_categorised ~ year, 
           random = ~1|plot/tree, 
           correlation = corAR1(form = ~1|plot/tree),
           data = test); summary(mod)



execute_simulation <- function(configset, 
                               row, 
                               filter_years = NULL) {
  n_sims <- configset$n_sims[row]
  n_plots <- configset$n_plots[row]
  n_trees <- configset$n_trees[row]
  start_year <- configset$start_year[row]
  end_year <- configset$end_year[row]
  mu_pop_t0 <- configset$mu_pop_t0[row]
  mu_relative_tn <- configset$mu_relative_tn[row]
  sd_trend <- configset$sd_trend[row]
  sd_between_trees_nat <- configset$sd_between_trees_nat[row]
  sd_between_trees_obs <- configset$sd_between_trees_obs[row]
  sd_within_trees <- configset$sd_within_trees[row]
  autocor <- configset$autocor[row]
  sd_error <- configset$sd_error[row]
  
  if (!is.null(filter_years)) {
    years <- filter_years
  } else {
    years <- start_year:end_year
  }
  base_data <- expand.grid(plot = 1:n_plots, 
                           tree = 1:n_trees, 
                           year = years)
  print(dim(base_data))
  rv <- data.frame(sim = 1:n_sims, est = NA, end = NA, pval = NA, autocor = NA)
  for (i in 1:n_sims) {
    cat("Executing iteration", i, "of", n_sims, "\n")
    sim <- simulatie_bosvitaliteit(n_plots = n_plots, 
                                   n_trees = n_trees, 
                                   years = end_year - start_year, 
                                   mu_pop_t0 = mu_pop_t0,
                                   mu_relative_tn = mu_relative_tn, 
                                   sd_trend = sd_trend,
                                   sd_between_trees_nat = sd_between_trees_nat, 
                                   sd_between_trees_obs = sd_between_trees_obs,
                                   sd_within_trees = sd_within_trees, 
                                   autocor = autocor)
    newname <- paste("sim", sprintf("%04d", i), sep = "_")
    print(newname)
    colnames(sim)[colnames(sim) == "val_categorised"] <- newname
    base_data <- base_data %>% 
      inner_join(sim %>% select(plot, tree, year, all_of(newname)),
                 by = c("plot", "tree", "year"))  
    
    formula = paste(newname, "year", sep = " ~ " )
    if (!is.null(filter_years)) {
      base_data <- filter(base_data, year %in% filter_years)
      #base_data$year <- as.numeric(as.factor(base_data$year))
    }
    
    model <- lme(data = base_data, 
                 fixed = eval(parse(text = formula)),
                 random = ~1|plot/tree, 
                 correlation = corAR1(form = ~year|plot/tree)) 
    fx <- summary(model)$tTable
    est <- fx[2,1]
    se <- fx[2,2]
    itc <- fx[1,1]
    pval <- fx[2,5]
    ac_est <- coef(model$modelStruct$corStruct)
    ac_Phi <- (exp(ac_est) - 1) / (exp(ac_est) + 1)
    rv[i, "est"] <- est
    rv[i, "end"] <- (itc + 12 * est)/itc #12 en niet 11 want itc is t0, em data begint met t1
    rv[i, "pval"] <- pval
    rv[i, "autocor"] <- ac_Phi
    #print(summary(model))
    
  }
  return(rv)
}


simconfig <- expand.grid(n_sims = 3, 
                         n_plots = 100, 
                         n_trees = 20, 
                         start_year = 0, 
                         end_year = 12, 
                         mu_pop_t0 = 0.25, 
                         mu_relative_tn = 0.88, 
                         sd_trend = 0.0,
                         sd_between_trees_nat = 0.05, 
                         sd_between_trees_obs = 0.05,
                         sd_between_trees = 0.1, 
                         sd_within_trees = 0.1, 
                         autocor = 0.77,
                         sd_error = 0.1)

#corAR1 is misschien wat te streng, correlatie tussen jaren blijft vermoedelijk hoger
result <- execute_simulation(simconfig, 1)
result

#probleem met autocorrelatie (telkens als 0 gegenereerd)
result2 <- execute_simulation(simconfig, 1, 
                              filter_years = c(0,3,6,9,12))
result2
