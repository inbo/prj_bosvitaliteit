
library(DBI)
library(odbc)
library(tidyverse)

### >>> Inlezen data

source("01 JAARLIJKS RAPPORT/scripts/functies/functies_db.R")
conn <- bosvitaliteit_connect()
dfSoortInfo <- read.csv2("01 JAARLIJKS RAPPORT/data/tree_indeling.csv", 
                         stringsAsFactors = FALSE)
tree_sql <- readLines("01 JAARLIJKS RAPPORT/data/tree_info.SQL")
dfTrees <- get_treedata(conn, jaar = 1970:2024,
                        tree_indeling = dfSoortInfo,
                        sql = tree_sql)
names(dfTrees)

### >>> Variabiliteiten

## variabiliteit binnen een proefvlak, binnen een jaar

#Beperking: normale benadering, dus variatie onafhankelijk van gemiddelde
#sd blijft gemiddeld vrij gelijk over de tijd, maar de spreiding op de sd vergroot de laatste jaren
#de cv daarentegen blijft heel vat stabieler
sd_within_plot <- dfTrees %>% 
  group_by(Jaar, PlotKey) %>% 
  summarize(gemiddelde = mean(BladverliesNetto),
            sd = sd(BladverliesNetto),
            cv = sd/gemiddelde)

ggplot(sd_within_plot, aes(x = Jaar, y = gemiddelde)) + geom_point() + geom_smooth()
ggplot(sd_within_plot, aes(x = Jaar, y = sd)) + geom_point() + geom_smooth()
ggplot(sd_within_plot, aes(x = Jaar, y = cv)) + geom_point() + geom_smooth()

ggplot(sd_within_plot, aes(x = gemiddelde)) + geom_histogram()
ggplot(sd_within_plot, aes(x = sd)) + geom_histogram()
ggplot(sd_within_plot, aes(x = cv)) + geom_histogram()

mean(sd_within_plot$sd, na.rm = TRUE) #8.68

distri_estimated <- 
  MASS::fitdistr(sd_within_plot %>% pull(sd) %>% na.omit(),
                  densfun = "lognormal")
distri_estimated

distri_sample <- 
  data.frame(x = 0:300/10, 
             ests = dlnorm(0:300/10, 
                          meanlog = mean(log(sd_within_plot$sd), na.rm = TRUE), 
                          sdlog = sd(log(sd_within_plot$sd), na.rm = TRUE)),
             este = dlnorm(0:300/10,
                           meanlog = distri_estimated$estimate["meanlog"],
                           sdlog = distri_estimated$estimate["sdlog"]))

ggplot(sd_within_plot, aes(x = sd)) + geom_density(color = "blue") + 
  geom_line(distri_sample, mapping = aes(x = x, y = ests), color = "red") +
  geom_line(distri_sample, mapping = aes(x = x, y = este), 
            color = "green", linetype = 2)
#so sd_within_plot is distributed lognormally with pars lmean 2.16 and lsd 0.45

## variabiliteit tussen proefvlakken per jaar

sd_between_plots <- dfTrees %>% 
  group_by(Jaar, PlotKey) %>% 
  summarise(avg = mean(BladverliesNetto, na.rm = TRUE),
            med = median(BladverliesNetto, na.rm = TRUE)) %>% 
  group_by(Jaar) %>% 
  summarise(sd_avg = sd(avg, na.rm = TRUE),
             sd_med = sd(med, na.rm = TRUE))

ggplot(sd_between_plots, aes(x = sd_avg)) + geom_density() 
ggplot(sd_between_plots, aes(x = sd_med)) + geom_density() 

MASS::fitdistr(sd_between_plots %>% pull(sd_avg), densfun = "normal")

ggplot(sd_between_plots, aes(x = sd_avg)) + geom_density() +
  geom_line(data.frame(x = 350:900/100, fit = dnorm(350:900/100, 6.35, 1.4)),
               mapping = aes(x = x, y = fit), color = "red")
#so sd_between_plots is distributed normally with mean 6.34 en sd 1.4


## variabiliteit tussen observaties van 2 opeenvolgende jaren



## variabiliteit van treespecies binnen een plot

