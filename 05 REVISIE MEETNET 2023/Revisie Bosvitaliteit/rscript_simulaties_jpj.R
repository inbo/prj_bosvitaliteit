library(lme4)
library(simr)

X <- expand.grid(x = 0:1, p = 1:75)
sdbp <- 0.060
sdwp <- 0.030
COV = list(p = sdbp ** 2)
S <- sdwp
intercept <- 0.25
slopes <- seq(1,25)/1200
#sdt  <- 0.065 #
#cor  <- 0.400
nsim <-  200

PWR <- NULL
for (i in 1:length(slopes)) {
  print(i)
  B <- c(intercept, slopes[i])
  model <- makeLmer(y ~ x + (1 | p), 
                    fixef = B, VarCorr = COV, sigma = S, 
                    data = X )
  
  PWR[[i]] <- powerSim(model, nsim = nsim)    
}
datajpj <- 
  data.frame(
    jaarlijkse_trend = slopes, 
    jaarlijkse_power = sapply(PWR, function(obj) obj$x / obj$n))
write_excel_csv2(datajpj, file = "data/power_jpj.csv")
