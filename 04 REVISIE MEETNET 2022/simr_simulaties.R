
### Get vereenvoudigde schattingen dfTrees (op plotniveau ipv boomniveau)

setwd("F:/INBO/BOSVITALITEIT/")
library(tidyverse)
#library(inbobosvitaliteit)
library(lme4)
library(simr)
source("simr_functions.R")

### INLEZEN EN KLAARZETTEN DATA

#Lees testdata in
df_trees <- readRDS("dfTrees_trend.RDS") %>% 
  mutate(JaarC = Jaar - 2000, 
         nnv = BladverliesNetto / 100,
         prbo = paste(PlotNr, BoomNr, sep = "."))

#aggregeer op plotniveau
df_plots <- df_trees %>% 
  group_by(PlotNr, Jaar) %>% 
  summarize(n_trees = n(), 
            nnv =  mean(BladverliesNetto)/100, 
            .groups = "drop") %>% 
  filter(Jaar >= 1995) %>% 
  mutate(PlotNr = as.factor(PlotNr),
         JaarC = Jaar - 2000) %>% 
  na.omit()

ggplot(df_plots, aes(x = Jaar, y = nnv, group = PlotNr)) + 
  geom_line() + geom_hline(yintercept = mean(df_plots$nnv), color = "green", size = 3)

mean(df_plots$nnv)

#autocorrelatie tussen bomen gaan we negeren, door het prbo effect is er wel impliciet al een constante correlatie tussen iedere meting per boom

### BEPALEN INSTELWAARDE RANDOM INTERCEPT TUSSEN PLOTS

#sd_intercept:
#worst case 0.095 ~ 0.10
#medium case 0.065 ~ 0.065

sd_between_plots <- 
  df_plots %>%  
  group_by(Jaar) %>% 
  summarise(sd_plot = sd(nnv)) %>% 
  pull(sd_plot) 
sd_between_plots

quantile(sd_between_plots, prob = c(0,0.10,0.25,0.50,0.75,0.90,1))
mean(sd_between_plots)
sd(sd_between_plots)
hist(sd_between_plots)  

### BEPALEN INSTELWAARDE RANDOM SLOPE OP PLOTNIVEAU

#sd_trend:
#worst case: 0.009 ~ 0.01
#mean case: 0.0076 ~ 0.0075

trendvals <- NULL
for (start in 1995:2010) {
  print(start)
  df_y <- df_plots %>% filter(Jaar >= start, Jaar <= start + 12)
  for (i in unique(df_plots$PlotNr)) {
    lmdata <<- df_y %>% filter(PlotNr == i)
    if (nrow(lmdata) >= 5) {
      testmodel <<- lm(nnv ~ JaarC, data = lmdata)
      res <- summary(testmodel)$coef[2,]
      names(res) <- c("Est", "se", "tval", "pval")
      trendvals = bind_rows(trendvals, 
                            c(start = start, plot = i, res))      
    } else {
      res <- rep(NA, 4)
      names(res) <- c("Est", "se", "tval", "pval")
      trendvals = bind_rows(trendvals, 
                            c(start = start, plot = i, res))
    } 
  }
}

trendvalssum <- trendvals %>%  
  group_by(start) %>%  
  summarise(sdev = sd(Est, na.rm = TRUE))

mean(trendvalssum$sdev)
sd(trendvalssum$sdev)
hist(trendvalssum$sdev)
quantile(trendvalssum$sdev, prob = c(0,0.10,0.25,0.50,0.75,0.90,1))


### UITTESTEN RANDOM SLOPE MODEL OP GEAGGREGEERD PLOT

lmermodel <- lmer(nnv ~ JaarC + (JaarC|PlotNr), 
                  data = df_plots %>% filter(Jaar %in% 2010:2022))
summary(lmermodel)
VarCorr(lmermodel)

### UITTESTEN RANDOM SLOPE MODEL OP VOLLEDIGE DATA

#sd_tree: 0.10053 ~ 0.10 (random intercept op prbo)
#sd_resid: 0.0787 ~ 0.08 (residual effect in model)

modellen <- simr_params <-  NULL
for (startjaar in 1995:2010) {
  jaren <- startjaar:(startjaar + 12)
  center <- median(jaren)
  print(startjaar)
  lmerdata <- df_trees %>% filter(Jaar %in% startjaar:(startjaar + 12)) %>% 
    mutate(JaarCC = Jaar - center)
  model_calc <- 
    lmer(nnv ~ JaarCC + (JaarCC|PlotNr) + (1|prbo), 
         data = lmerdata)
  
  sd_tree <- sqrt(VarCorr(model_calc)$prbo[1,1])
  sd_between_plots <- sqrt(VarCorr(model_calc)$PlotNr[1,1])
  sd_trend_between_plots <- sqrt(VarCorr(model_calc)$PlotNr[2,2])
  COV_plot <- VarCorr(model_calc)$PlotNr
  COR_plot <- attr(VarCorr(model_calc)$PlotNr, "correlation")
  correlation <- COR_plot[1,2]
  sd_resid <- attr(VarCorr(model_calc), "sc")
  simr <- list(sd_tree = sd_tree, 
               sd_plot = sd_between_plots,
               sd_trend = sd_trend_between_plots,
               cor_trend_plot = correlation, 
               sd_resid = sd_resid, 
               COV_tree = VarCorr(model_calc)$prbo,
               COV_plot = VarCorr(model_calc)$PlotNr)
  
  modellen[[startjaar-1994]] <- model_calc
  simr_params[[startjaar-1994]] <- simr
}

#sd_trees: 0.10 (median en max)
sd_trees_all <- sapply(simr_params, function(x) x$sd_tree) 
quantile(sd_trees_all);mean(sd_trees_all);sd(sd_trees_all);hist(sd_trees_all)

#sd_plots: 0.10 en 0.05 (stijgt hele tijd, de laatste jaren gaat dit naar de 0.10 toe)
sd_plots_all <- sapply(simr_params, function(x) x$sd_plot) 
quantile(sd_plots_all);mean(sd_plots_all);sd(sd_plots_all);hist(sd_plots_all)


#sd_trend: 0.01 (mediaan)
sd_trend_all <- sapply(simr_params, function(x) x$sd_trend) 
quantile(sd_trend_all);mean(sd_trend_all);sd(sd_trend_all);hist(sd_trend_all)

#cor_trend: 0.75 en 0.50 (0.75 laatste jaren, 0.50 tot bijna 0 in begintijd)
cor_trend_all <- sapply(simr_params, function(x) x$cor_trend_plot) 
quantile(cor_trend_all);mean(cor_trend_all);sd(cor_trend_all);hist(cor_trend_all)

#sd_resid: 0.07 (mediaan) heel stabiel over hele periode
sd_resid_all <- sapply(simr_params, function(x) x$sd_resid) 
quantile(sd_resid_all);mean(sd_resid_all);sd(sd_resid_all);hist(sd_resid_all)



#####################


#met 5 bomen gewerkt, omdat het model anders te moeilijk fit.
#voor 20 bomen is de sd_tree ongeveer 0.10
#equivalent voor 40 bomen zal sd_tree met sqrt verhoogd worden om ditzelfde effect van een sd van 0.10 te modelleren, dus als we willen simuleren met 5 bomen:
# 30 bomen sd_tree <- 0.10/sqrt(6)
# 20 bomen sd_tree <- 0.10/sqrt(4)
# 10 bomen sd_tree <- 0.10/sqrt(2)
# 5 bomen sd_tree <- 0.10/sqrt(1)


#scenarios beperken tot essentie
scenarios <- readr::read_csv2("scenarios_01_definition.csv")

run_power = FALSE
if (run_power) {
  tmp <- bulk_simulate(nsim = 200, scenarios = scenarios, slice = 1:18, outpath = "output")
  
}

files <- list.files("output",  pattern = "power", full.names = TRUE)
powers <- pwrlist <- NULL
for (file in files) {
  start <- regexpr("\\_NR\\_", file) + 4
  end <- regexpr("\\.RDS", file) - 1
  NR <- as.numeric(substring(file, start, end))
  pwrdata <- readRDS(file)
  pwr <- 100 * mean(pwrdata$pval <= 0.05)
  powers <- bind_rows(powers, 
                      cbind(NR = NR, 
                            power = binom::binom.confint(x = sum(pwrdata$pval < pwrdata$alpha), 
                                                         n = pwrdata$n, 
                                                         methods = "exact")))
  pwrlist[[NR]] <- pwrdata
}
scenario_power <- scenarios %>% 
  mutate(NR = as.numeric(NR)) %>%  
  left_join(powers, by = "NR")

#standard varying plots and sd_tree
ggplot(data = scenario_power %>% filter(NR %in% 1:12), 
       mapping = aes(x = plots, y=power.mean, color = factor(round(sd_tree,3)), 
                     ymin = power.lower, ymax = power.upper)) +
  geom_point() + geom_line() + geom_errorbar() + 
  labs(x = "aantal plots", y = "power", color = " stdev tussen bomen" )

#varying other variables, keeping plots fixed at 75
ggplot(data = scenario_power %>% filter(NR %in% c(1,2,13:50)), 
       mapping = aes(x = factor(NR), y=power.mean, color = paste(NR,Description, sep = ":"), 
                     ymin = power.lower, ymax = power.upper)) +
  geom_point() + geom_errorbar()


