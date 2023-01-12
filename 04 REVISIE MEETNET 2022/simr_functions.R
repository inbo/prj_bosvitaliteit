simr_power_intervals <- function(obj, conf.level = 0.95) {
  require("binom")
  e <- try({
    pval <- obj$pval
    n <- obj$n
    ci <- as.data.frame(
      binom::binom.confint(sum(pval < conf.level), 
                           n = n, 
                           conf.level = conf.level,
                           methods = 'exact'))
  })
  if (inherits(e, "try-error")) {
    ci <- data.frame(method = NA, x = NA, n = NA, mean = NA, lower = NA, upper = NA)
  }
  return(ci)
}

mysummary <- function(x, 
                      probs = c(0,0.025,0.10,0.25,0.50,0.75,0.90,0.975,1),
                      digits = 4) {
  avg <- mean(x, na.rm = TRUE)
  quants <- quantile(x, probs = probs, na.rm = TRUE)
  n <- length(x)
  nas <- sum(is.na(x))
  c(n=n, 
    nas = nas, avg = round(avg, digits), 
    quants = round(quants, digits = 4))
}

#' Bulk simulation with simr and custom power_simulate_lmer
#'
#' @param nsim 
#' @param scenarios 
#' @param slice 
#'
#' @return
#' @export
#'
#' @examples
bulk_simulate <- function(nsim, scenarios, slice = NULL, outpath = "output") {
  library(simr)
  if (!is.null(slice)) {
    subset <- scenarios %>% slice(slice)    
  } else {
    subset <- scenarios
  }
  reslist <- NULL
  for (i in 1:nrow(subset)) {
    NR <- subset$NR[i]
    save_name = paste0("power_scenarios_01_NR_", NR, ".RDS")
    outfile <- file.path(outpath, save_name)
    e <- try({
      res <- 
        power_simulate_lmer(years = subset$years[i],
                            plots = subset$plots[i],
                            trees = subset$trees[i],
                            intercept = subset$intercept[i],
                            trend = subset$trend[i],
                            sd_trend = subset$sd_trend[i],
                            sd_intercept = subset$sd_intercept[i],
                            corr_itc_trend = subset$corr_itc_trend[i],
                            sd_tree = subset$sd_tree[i],
                            sd_resid = subset$sd_resid[i],
                            nsim = nsim)
    })
    if (inherits(e, "try-error")) res <- "Niet gelukt"
    cat("saving: ", outfile, "\n")
    saveRDS(res, file = outfile)
    reslist[[i]] <- res
  }
  reslist
}

########################

#' Simulate the model y ~ x + (x | p) + (1|g) with simr
#'
#' @param years 
#' @param plots 
#' @param trees 
#' @param intercept 
#' @param trend 
#' @param sd_trend 
#' @param sd_intercept 
#' @param corr_itc_trend 
#' @param sd_tree 
#' @param sd_resid 
#' @param nsim 
#'
#' @return
#' @export
#'
#' @examples
power_simulate_lmer <- function(years = 13, 
                                plots = 75,
                                trees = 20,
                                intercept = 0.25,
                                trend = 0.0025,
                                sd_trend = 0.01,
                                sd_intercept = 0.10,
                                corr_itc_trend = 0.75,
                                sd_tree = 0.10,
                                sd_resid = 0.10, 
                                nsim = 20) {
  years <- 0: (years-1)
  years <- years - floor(median(years))
  B <- c(intercept, trend)
  V_ITC = sd_intercept ** 2
  V_SLP = sd_trend  ** 2
  V_TREE = sd_tree ** 2
  COV = list(V_TREE, 
             rbind(c(V_ITC, 
                     corr_itc_trend * sqrt(V_ITC) * sqrt(V_SLP)),
                   c(corr_itc_trend * sqrt(V_SLP) * sqrt(V_ITC),
                     V_SLP)) 
  )
  S = sd_resid
  
  X <- expand.grid(x = years, p = 1:plots, t = 1:trees) %>% 
    mutate(g = interaction(p,t))
  model <- makeLmer(y ~ x + (x | p) + (1|g), 
                    fixef = B, VarCorr = COV, sigma = S, 
                    data = X )
  print(model)
  power <- powerSim(model, nsim = nsim)
  return(power)
}


######################################################

#' Get the residual covariance matrix of a model
#'
#' @param model 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}