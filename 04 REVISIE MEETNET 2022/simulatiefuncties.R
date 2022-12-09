
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

generate_ar1 <- function(n, mu, rho, sd) {
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



generate_cor_general <- function(n, mu, cor_per_lag, sd) {
  if (length(mu) != length(cor_per_lag)) 
    stop("mu and cor_per lag should be the same size, starting with lag 0")
    n_var <- length(mu)
  Sigma <- matrix(nrow = n_var, ncol = n_var, data = 0)
  for (i in 1:n_var) {
    for (j in i:n_var) {
        tdiff <- abs(i-j)
        Sigma[i,j] <- Sigma[j,i] <- cor_per_lag[tdiff + 1] * sd * sd
    }
  }
  rv <- MASS::mvrnorm(n, mu =  mu, Sigma = Sigma)
  colnames(rv) <- 0:(ncol(rv) - 1)
  rv
}

################################################

simulatie_bosvitaliteit <- function(n_plots=70, 
                                    n_trees=20, 
                                    years = 12, #first year 0, last 12
                                    mu_pop_t0 = 0.25,
                                    mu_relative_tn = 1.12, 
                                    sd_trend_year = 0.001,
                                    sd_trend_plot = 0.001,
                                    sd_trend_tree = 0.001,
                                    sd_between_plots = 0.001, 
                                    sd_between_trees_nat = 0.001,
                                    sd_between_trees_obs = 0.001,
                                    sd_within_trees = 0.001, 
                                    correlations = c(0.000^(0:(years))), 
                                    sd_error = 0.001,
                                    partitions = 20) {
  start_year <- 0
  end_year <- years
  
  #init data
  data <- expand.grid(tree = 1:n_trees, plot = 1:n_plots, 
                      year = start_year:end_year, 
                      val_pop_t0 = mu_pop_t0)
  
  #trend calculations
  trend_pop <- ((mu_relative_tn * mu_pop_t0)  - mu_pop_t0) / years
  
  trend_plot <- data.frame(plot = 1:n_plots, 
                           t_plt = rnorm(n_plots, 0, sd_trend_plot))
  
  trend_tree <- expand.grid(plot = 1:n_plots, tree = 1:n_trees) %>% 
    mutate(t_tree = rnorm(n(), 0, sd_trend_tree))
  
  trend_year <- data.frame(year = start_year:end_year) %>% 
    mutate(t_year = rnorm(n(), 0, sd_trend_year))
  
  trend <- data %>% 
    inner_join(trend_plot, by = "plot") %>% 
    inner_join(trend_tree, by = c("plot", "tree")) %>% 
    inner_join(trend_year, by = "year") %>% 
    mutate(t_pop = trend_pop, 
           trend_contribution = t_plt + t_tree + t_year + t_pop) 
  trendsum <- trend %>% group_by(tree, plot) %>% 
    do({
      year = .$year
      evolution = c(0, cumsum(.$trend_contribution[-1]))
      data.frame(year, evolution)
      })
  
  data <- data %>% 
    inner_join(trendsum, by = c("plot", "tree", "year")) %>% 
    mutate(val_trend = mu_pop_t0 + evolution)
  
  #voeg errorterm toe
  data <- data %>% 
    mutate(c_error = rnorm(n(), 0, sd_error))
  
  #between plot variation
  between_plots <- 
    data.frame(plot = 1:n_plots, 
               c_plot = rnorm(n_plots, mean = 0, sd = sd_between_plots))
  
  #between trees in plot variation
  between_trees <- 
    expand.grid(plot = 1:n_plots, tree = 1:n_trees) %>% 
    mutate(c_tree = rnorm(n(), mean = 0, sd = sd_between_trees_nat) + 
                      rnorm(n(), mean = 0, sd = sd_between_trees_obs))

  #within tree variation over time (autocorrelated)
  epsilon <- data %>% 
    filter(year == start_year) %>% 
    select(tree, plot) %>% 
    bind_cols(generate_cor_general(n = n_plots * n_trees, 
                               mu = rep(0, length(start_year:end_year)),
                               cor_per_lag = correlations, 
                               sd = sd_within_trees)) %>% 
    pivot_longer(cols = -c(tree, plot), 
                 names_to = "year", 
                 values_to = "c_eps") %>% 
    mutate(year = as.numeric(year) + start_year)
  
  data_final <- data %>% 
    inner_join(between_plots, by = "plot") %>% 
    inner_join(between_trees, by = c("plot", "tree")) %>% 
    inner_join(epsilon, by = c("plot", "tree", "year")) %>% 
    mutate(val_mod = val_trend  + c_tree +  c_plot, 
           val_observed = val_mod  + c_error + c_eps,
           val_observed = if_else(val_observed < 0, 0, val_observed),
           val_observed = if_else(val_observed > 1, 1, val_observed), 
           val_categorised = round(val_observed * partitions)/ partitions)
  
  data_final
}


##################################################

execute_simulation <- function(configset, 
                               row, 
                               filter_years = NULL,
                               correlations, 
                               modeltype = c("intercept", "slope", "nocorr"),
                               show_model = FALSE) {
  n_sims <- configset$n_sims[row]
  n_plots <- configset$n_plots[row]
  n_trees <- configset$n_trees[row]
  start_year <- configset$start_year[row]
  end_year <- configset$end_year[row]
  mu_pop_t0 <- configset$mu_pop_t0[row]
  mu_relative_tn <- configset$mu_relative_tn[row]
  sd_trend_tree <- configset$sd_trend_tree[row]
  sd_trend_plot <- configset$sd_trend_plot[row]
  sd_trend_year <- configset$sd_trend_year[row]
  sd_between_plots <- configset$sd_between_plots[row]
  sd_between_trees_nat <- configset$sd_between_trees_nat[row]
  sd_between_trees_obs <- configset$sd_between_trees_obs[row]
  sd_within_trees <- configset$sd_within_trees[row]
  correlations <- correlations
  sd_error <- configset$sd_error[row]
  partitions <- configset$partitions[row]
  
  if (!is.null(filter_years)) {
    years <- filter_years
  } else {
    years <- start_year:end_year
  }
  base_data <- expand.grid(plot = 1:n_plots, 
                           tree = 1:n_trees, 
                           year = years)
  print(dim(base_data))
  
  rv <- data.frame(sim = 1:n_sims, itc = NA, itc_se = NA, year = NA, 
                   year_se = NA, pval = NA, after_end = NA)

  for (i in 1:n_sims) {
    cat("Executing iteration", i, "of", n_sims, "\n")
    sim <- simulatie_bosvitaliteit(n_plots = n_plots, 
                                   n_trees = n_trees, 
                                   years = end_year - start_year, 
                                   mu_pop_t0 = mu_pop_t0,
                                   mu_relative_tn = mu_relative_tn, 
                                   sd_trend_plot = sd_trend_plot,
                                   sd_trend_tree = sd_trend_tree,
                                   sd_trend_year = sd_trend_year,
                                   sd_between_plots = sd_between_plots,
                                   sd_between_trees_nat = sd_between_trees_nat, 
                                   sd_between_trees_obs = sd_between_trees_obs,
                                   sd_within_trees = sd_within_trees, 
                                   correlations = correlations,
                                   sd_error = sd_error, 
                                   partitions = partitions)
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
    
    e <- try({
      if (modeltype[1] == "slope") {
        model <- lme(data = base_data, 
                   fixed = eval(parse(text = formula)),
                   random = ~year|plot/tree, 
                   correlation = corAR1(form = ~year|plot/tree))       
    }
      if (modeltype[1] == "intercept") {
        model <- lme(data = base_data, 
                   fixed = eval(parse(text = formula)),
                   random = ~1|plot/tree, 
                   correlation = corAR1(form = ~year|plot/tree)) 
    }
      if (modeltype[1] == "nocorr") {
        model <- lme(data = base_data, 
                   fixed = eval(parse(text = formula)),
                   random = ~year|plot/tree) 
    }
      if (modeltype[1] == "simple"){
        model <- lme(data = base_data, 
                   fixed = eval(parse(text = formula)),
                   random = ~1|plot/tree) 
      }
      if (show_model) print(summary(model))
    })
    if (inherits(e,  "try-error")) model <- NULL
    
    if (is.null(model)) {
      rv[i, 'itc'] <- rv[i, "itc_se"] <- rv[i, "year"] <- 
        rv[i, "year_se"] <- rv[i, "pval"] <- rv[i, "after_end"] <- NA
      next
    } 
    

    fx <- summary(model)$tTable
    est <- fx[2,1]
    se <- fx[2,2]
    itc <- fx[1,1]
    pval <- fx[2,5]
    ac_est <- coef(model$modelStruct$corStruct)
    #ac_Phi <- (exp(ac_est) - 1) / (exp(ac_est) + 1)
    rv[i, "itc"] <- itc
    rv[i, "itc_se"] <- fx[1,2]
    rv[i, "year"] <- est
    rv[i, "year_se"] <- se
    rv[i, "pval"] <- pval
    rv[i, "after_end"] <- itc + max(years) * est
  }
  return(rv)
}


