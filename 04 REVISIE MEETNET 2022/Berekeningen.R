
library(DBI)
library(odbc)
library(tidyverse)
library(inbobosvitaliteit)

### >>> Inlezen data

get_from_db <- FALSE
if (get_from_db) {
  dfSoortInfo <- inbobosvitaliteit::read_species_information()
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "inbo-sql07-prd.inbo.be",
                        port = 1433,
                        Database = "D0004_00_Bosvitaliteit",
                        Trusted_Connection = "True")
  
  
  dfTrees <- inbobosvitaliteit::get_treedata(con, jaar = 1987:2022, 
                                             tree_indeling = dfSoortInfo)
  print(dim(dfTrees))
  dfSymptoms <- inbobosvitaliteit::get_symptomdata(con, jaar = 2022)
  print(dfSymptoms)
  saveRDS(dfTrees, file = "dfTrees_trend.RDS")  
  saveRDS(dfTrees, file = "dfSymptoms.RDS") 
} else {
  dfTrees <- readRDS("dfTrees_trend.RDS")
  dfSymptoms <- readRDS("dfSymptoms.RDS")
  print(dim(dfTrees))
}

names(dfTrees)

dfTrees %>% group_by(Jaar) %>% 
  summarise(aantal_plots = length(unique(PlotNr)),
            aantal_bomen = length(unique(paste(PlotNr, BoomNr, sep = ".")))) %>% 
  view()


hist(dfTrees$BladverliesNetto)
qqnorm(dfTrees$BladverliesNetto)
qqline(dfTrees$BladverliesNetto)
hist(log(dfTrees$BladverliesNetto+2.5))
car::powerTransform(dfTrees$BladverliesNetto)
car::powerTransform(log(dfTrees$BladverliesNetto+2.5))
qqnorm(log(dfTrees$BladverliesNetto+2.5))
qqline(log(dfTrees$BladverliesNetto+2.5))

hist(sqrt(dfTrees$BladverliesNetto))
qqnorm(sqrt(dfTrees$BladverliesNetto))
qqline(sqrt(dfTrees$BladverliesNetto))

plot(density(dfTrees$BladverliesNetto))
lines(density(dfTrees$BladverliesNetto[dfTrees$BladverliesNetto < 100]), col = "red")

quantile(dfTrees$BladverliesNetto, probs = seq(0,1, by = 0.05))

sd(dfTrees$BladverliesNetto)
sd(dfTrees$BladverliesNetto[dfTrees$BladverliesNetto<100])
mean(dfTrees$BladverliesNetto)
mean(dfTrees$BladverliesNetto[dfTrees$BladverliesNetto<100])

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
#!!!so sd_within_plot is distributed lognormally with pars lmean 2.16 and lsd 0.45

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
#!!!so sd_between_plots is distributed normally with mean 6.34 en sd 1.4


## variabiliteit tussen boomobservaties van 2 opeenvolgende jaren (= autocorrelatie)
df_compare <- dfTrees %>% 
  group_by(PlotKey, BoomKey, Jaar) %>% 
  summarise(NNV = BladverliesNetto) %>% 
  mutate(JaarPrev = Jaar-1)

df_comp_joined <- df_compare %>% 
  inner_join(df_compare %>% select(-JaarPrev, NNVP = NNV), 
             by = c("PlotKey", "BoomKey", "JaarPrev" = "Jaar")) %>% 
  mutate(NNVdiff = NNV - NNVP,
         NNVrel = NNV / (NNVP + 5))

cor(na.omit(df_comp_joined[c("NNV", "NNVP")], na.rm = TRUE))

#!!!Autocorrelatie van opeenvolgende metingen van een boom: 0.73


#variatie op de trend --> verschil tussen 2 opeenvolgende jaren?
dfTrees %>% group_by(Jaar) %>% 
  summarise(gemNNV = mean(as.numeric(BladverliesNetto)/100, na.rm = TRUE)) %>% 
  pull(gemNNV) -> trends
y2yv <- trends - lag(trends)
plot(y2yv)
sd(y2yv, na.rm = TRUE) #0.014

#check of het goed lukt autocorrelatie te simuleren
test <- MASS::mvrnorm(n=10000, mu = c(1,1), Sigma = rbind(c(1,0.73), c(0.73,1)))
test2 <- test
test2[,2] <- test[,2] + 1
cor(test2)

ggplot(df_comp_joined, aes(x = NNVdiff)) + geom_histogram(binwidth = 5)
ggplot(df_comp_joined, aes(x = NNVrel)) + geom_histogram(binwidth = 0.2)

mean(df_comp_joined$NNVdiff, na.rm = TRUE) #0.46
sd(df_comp_joined$NNVdiff, na.rm = TRUE) #8.60

generator <- expand.grid(Plot = 1:50, Boom = 1:20, NNV1 = NA, NNV2 = NA)





#TABEL MET WIJZIGINGEN

tabel <- table(df_comp_joined$NNVP, df_comp_joined$NNV)
tabeldf <- as.data.frame(tabel)
colnames(tabeldf) <- c("Current", "Prev", "Freq")

tabeldf <- tabeldf %>% 
  group_by(Prev) %>% 
  mutate(RelFreq = Freq / sum(Freq))

ggplot(tabeldf, aes(x = Current, y = Prev, z = RelFreq, fill = RelFreq)) + 
  geom_tile() + scale_fill_gradient(low = "blue", high = "red") + geom_abline(slope=1)


#mean(df_comp_joined$NNVrel, na.rm = TRUE) 
#sd(df_comp_joined$NNVrel, na.rm = TRUE)


## variabiliteit van treespecies binnen een plot
df_spec <- dfTrees %>% 
  group_by(Jaar, PlotKey, Soortnummer) %>% 
  summarise(avg = mean(BladverliesNetto) ) %>% 
  summarise(N = n(), 
            sdev = sd(avg))
ggplot(df_spec, aes(x = sdev)) + geom_histogram()
dist_spec <- MASS::fitdistr(df_spec %>% mutate(sdev = sdev + 0.1) %>% pull(sdev) %>% na.omit(), densfun = "lognormal")
 #variabiliteit gemiddeld 5.31 (log was 1.672) maar wel met heel veel ruis (sd= 0.897 in log-schaal)

