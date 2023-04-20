library(tidyverse)
library(lme4)
library(simr)

dataset1 <- expand.grid(Mtype = 'Fix',
                        N_plots = c(20,40,60,80,100,120),
                        N_years = c(12, 20),
                        N_trees = 20,
                        trend = 0.0025, 
                        sd_bp = 0.07,
                        sd_wp = 0.175,
                        cor_is = 0.20,
                        sd_trend = 0.0065
)


dataset2 <- bind_rows(
  expand.grid(Mtype = 'Var_tree',
              N_plots = 80,
              N_years = c(12),
              N_trees = c(5,10,15,25),
              trend   = c(0.0025),
              sd_bp   = c(0.07),
              sd_wp   = 0.175,
              cor_is  =  c(0.20), 
              sd_trend = c(0.0065)),
  expand.grid(Mtype = 'Var_trend',
              N_plots = 80,
              N_years = c(12),
              N_trees = c(20),
              trend   = c(0.0050),
              sd_bp   = c(0.07),
              sd_wp   = 0.175,
              cor_is  =  c(0.20), 
              sd_trend = c(0.0065)),
  expand.grid(Mtype = 'Var_bp',
              N_plots = 80,
              N_years = c(12),
              N_trees = c(20),
              trend   = c(0.0025),
              sd_bp   = c(0.05,0.09),
              sd_wp   = 0.175,
              cor_is  =  c(0.20), 
              sd_trend = c(0.0065)),
  expand.grid(Mtype = 'Var_cor',
              N_plots = 80,
              N_years = c(12),
              N_trees = c(20),
              trend   = c(0.0025),
              sd_bp   = c(0.07),
              sd_wp   = 0.175,
              cor_is  =  c(0, 0.40), 
              sd_trend = c(0.0065)),
  expand.grid(Mtype = 'Var_sdt',
              N_plots = 80,
              N_years = c(12),
              N_trees = c(20),
              trend   = c(0.0025),
              sd_bp   = c(0.07),
              sd_wp   = 0.175,
              cor_is  =  c(0.20), 
              sd_trend = c(0.004, 0.0090))
)

insteldata <- bind_rows(dataset1, dataset2)
insteldata <- insteldata %>% mutate(modelnr = 1:nrow(.))
write_excel_csv2(insteldata, file = 'data/insteldata.csv')


insteldata <- read_csv2("data/insteldata.csv")
insteldata_fixed <- insteldata %>% filter(Mtype == "Fix")
insteldata_var   <- bind_rows(insteldata %>% slice(4), #4e is referentie
                              insteldata %>% filter(Mtype != "Fix"))

###

powers1 <- execute_simr(insteldata_fixed, 0.25, 100, 'data/modelsim_fixed.csv')

ggplot(powers1, 
       aes(x = N_plots, y = power, ymin = lcl, ymax = ucl,
           color = factor(N_years))) + 
  geom_point() + geom_line(linetype = "dashed") +  geom_errorbar() +
  geom_hline(yintercept = 0.8, color = "green4") + 
  labs(x = "Aantal plots", y = "Kracht", color = "Jaren") + 
  scale_y_continuous(labels = scales::percent)

###

powers2 <- execute_simr(insteldata_var, 0.25, 100, 'data/modelsim_var.csv')

ggplot(powers2 %>% slice(c(1,2:5)), 
        aes(x = N_trees, y = power, ymin = lcl, ymax = ucl)) + 
  geom_point() + geom_errorbar()

ggplot(powers2 %>% slice(c(1,6)), 
       aes(x = trend, y = power, ymin = lcl, ymax = ucl)) + 
  geom_point() + geom_errorbar()

ggplot(powers2 %>% slice(c(1, 7:8)), 
       aes(x = sd_bp, y = power, ymin = lcl, ymax = ucl)) + 
  geom_point() + geom_errorbar()

ggplot(powers2 %>% slice(c(1, 9:10)), 
       aes(x = cor_is, y = power, ymin = lcl, ymax = ucl)) + 
  geom_point() + geom_errorbar()

ggplot(powers2 %>% slice(c(1, 11:12)), 
       aes(x = sd_trend, y = power, ymin = lcl, ymax = ucl)) + 
  geom_point() + geom_errorbar()

# ggplot(powers2, 
#        aes(x = N_plots, y = power, ymin = lcl, ymax = ucl,
#            color = factor(N_years))) + 
#   geom_point() + geom_line(linetype = "dashed") +  geom_errorbar() +
#   geom_hline(yintercept = 0.8, color = "green4") + 
#   labs(x = "Aantal plots", y = "Kracht", color = "Jaren") + 
#   scale_y_continuous(labels = scales::percent)


execute_simr <- function(insteldata, nnv0, nsim = 50, logfile = "modelsim.csv") {
  PWR <- NULL
  rv <- NULL
  for (i in 1:nrow(insteldata)) {
    print(paste("Modelnummer: ", sprintf("%03d", i)))
    Nyears   <- slice(insteldata, i) %>% pull(N_years)
    Nplots   <- slice(insteldata, i) %>% pull(N_plots)
    Ntrees   <- slice(insteldata, i) %>% pull(N_trees)
    trend    <- slice(insteldata, i) %>% pull(trend)
    sdbp     <- slice(insteldata, i) %>% pull(sd_bp)
    sdwp     <- slice(insteldata, i) %>% pull(sd_wp)
    sdtr     <- slice(insteldata, i) %>% pull(sd_trend)
    coris    <- slice(insteldata, i) %>% pull(cor_is)
    Nsim     <- nsim
    nnv0     <- nnv0
    
    X <- expand.grid(x = 0:Nyears, p = 1:Nplots)
    COV = list(p = rbind(c(sdbp ** 2, coris * sdbp * sdtr), 
                         c(coris * sdbp * sdtr, sdtr ** 2)))
    S <- sdwp / sqrt(Ntrees)
    B <- c(0.25, trend)
    
    model <- makeLmer(y ~ x + (x | p), 
                      fixef = B, VarCorr = COV, sigma = S, 
                      data = X )
    powerres <- powerSim(model, nsim = nsim)
    propres <- prop.test(powerres$x, powerres$n)
    PWR[[i]] <- list(modelsimnr = i, 
                     power = powerres$x/powerres$n,
                     lcl = propres$conf.int[1],
                     ucl = propres$conf.int[2]) 
    rvdata <- bind_cols(slice(insteldata, i), 
                        data.frame(PWR[[i]]))
    if (i == 1) append = FALSE else append = TRUE
    write_excel_csv2(rvdata, file = logfile, append = append)
    rv <- bind_rows(rv, rvdata)
  }
  rv
}

