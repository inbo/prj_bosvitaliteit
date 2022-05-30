#TRENDANALYSE## >>> Trendanalyse

### >>> Verkennend plot met alle mogelijke data, bomen die sterven tellen slechts 1x nog mee in de data

out <- "outputEs_trend/"

unieke_bomen_trend <- dfEsMetingAlle %>% 
  group_by(ProefvlakNummer, BoomID) %>% 
  summarize(omtrek_start = min(Omtrek, na.rm = TRUE),
            sterftejaar = min(Jaar[Bladverlies == 100]))

dfEsMetingTrend <- dfEsMetingAlle %>% 
  left_join(unieke_bomen_trend) %>% 
  mutate(gestorven = Jaar > sterftejaar) %>% 
  filter(Jaar %in% es_jaren)

evolutie_per_boom <- dfEsMetingTrend %>% 
  group_by(Jaar, BoomID, ProefvlakNummer, `Textuur-gegroepeerd`, `Drainagegroepering`, 
           `Waterbeschikbaarheid`, sterftejaar, omtrek_start) %>%
  summarize(Bladverlies = mean(Bladverlies)) %>%
  filter(Jaar <= sterftejaar)

(p <- ggplot(evolutie_per_boom, aes(x = Jaar, y = Bladverlies, group = BoomID,
                                    color = `Textuur-gegroepeerd`,
                                    linetype = Drainagegroepering)) + 
    geom_line() + 
    scale_linetype_discrete(name = "drainage") + 
    scale_color_discrete(name = "textuur") + 
    facet_wrap(~ProefvlakNummer, ncol = 5) +  
    theme(axis.text.x = element_text(hjust = 1, angle = 90)))  %>%
  ggsave(filename = paste0(out, "trend_bladverlies_per_boom.png"), 
         height = fig_height * 1.5, width = fig_width) 


(p <- ggplot(evolutie_per_boom, aes(x = Jaar, y = Bladverlies, group = BoomID,
                                    color = `Textuur-gegroepeerd`,
                                    linetype = `Waterbeschikbaarheid`)) + 
    geom_line() + 
    scale_linetype_discrete(name = "drainage") + 
    scale_color_discrete(name = "textuur") + 
    facet_wrap(~ProefvlakNummer, ncol = 5) +  
    theme(axis.text.x = element_text(hjust = 1, angle = 90)))  %>%
  ggsave(filename = paste0(out, "trend_bladverlies_per_boom_2.png"), 
         height = fig_height * 1.5, width = fig_width) 


### >>> Invloed van huidig bladverlies op toekomstig bladverlies (is er een verhoogde verhoging bij meer bladverlies?)

dfCheckBladverlies <- dfEsMetingTrend %>%
  select(ProefvlakNummer, BoomNr, Bladverlies, Jaar) %>%
  arrange(Jaar) %>%
  group_by(ProefvlakNummer, BoomNr) %>% 
  do({
    data.frame(Jaar = .$Jaar, 
               Bladverlies = .$Bladverlies, 
               Bladverlies_verschil_yr = .$Bladverlies - lag(.$Bladverlies),
               Bladveriles_factor_yr = .$Bladverlies / lag(.$Bladverlies))
  })
(p <- ggplot(dfCheckBladverlies, aes(x = jitter(Bladverlies, factor = 2), y = Bladverlies_verschil_yr, color = factor(Jaar))) + 
    geom_point(pch = 1) + geom_smooth(aes(x = Bladverlies, y = Bladverlies_verschil_yr), inherit.aes = FALSE))
ggsave(p, filename = paste0(out, "trend_invloed_huidigbladverlies.png"), width = fig_width, height = fig_height)

(p <- ggplot(dfCheckBladverlies, aes(x = jitter(Bladverlies, factor = 2), y = Bladveriles_factor_yr, color = factor(Jaar))) + 
    geom_point(pch = 1) + geom_smooth(aes(x = Bladverlies, y = Bladveriles_factor_yr), inherit.aes = FALSE) + 
    ylim(0,7)) 
#ggsave(p, filename = paste0(out, "trend_invloed_huidigbladverlies_factor.png"), width = fig_width, height = fig_height)




### >>> Zaadjaren

(p <- ggplot(filter(dfEsMetingTrend, !is.na(ZaadzettingCode)), aes(x = Jaar, fill = ZaadzettingCode)) + 
    geom_bar(position = position_stack(reverse = TRUE)))
ggsave(p, filename = paste0(out, "trend_zaadzetting.png"), width = fig_width, height = fig_height)

(p <- 
    dfEsMetingTrend %>% 
      group_by(Zaadzetting, ZaadleeftijdCode, Jaar) %>% 
      filter(ZaadzettingCode > 0) %>%
      summarize(Aantal = n()) %>% 
  ggplot(aes(x = Jaar, y = Aantal, fill = ZaadleeftijdCode)) + geom_bar(stat = "identity") + facet_wrap(~Zaadzetting)) %>%
  ggsave(filename = paste0(out, "trend_zaadzetting_2.png"), width = fig_width, height = fig_height)

#############################################################################################

### >>> Data met als eenheidsniveau de individuele boom (dode bomen eruit wegens bias)

#1 boom verdwijnt (BoomID 32 van proefvlaknummer 721)
dfAllDead <- dfEsMetingTrend %>% 
  arrange(BoomID, Jaar) %>% 
  group_by(BoomID, ProefvlakNummer) %>% 
  summarize(Ndead = sum(Bladverlies >= 100), 
            fdead = Ndead / n(), 
            omtrek_start = Omtrek[!is.na(Omtrek)][1], 
            sterftejaar = Jaar[Bladverlies >= 100][1]) %>% 
  filter(fdead < 1) 

dfBladverlies <- dfEsMetingTrend %>% 
  inner_join(dfAllDead %>% select(-omtrek_start, -sterftejaar), by = c("BoomID", "ProefvlakNummer")) %>%
  rename(textuurgroep = `Textuur-gegroepeerd`) %>%
  rename(drainagegroep = `Drainagegroepering`) %>%
  rename(waterbeschikbaarheid = Waterbeschikbaarheid) %>%
  mutate(omtrek_start = omtrek_start / 100) %>% #omtrek omzetten naar meter voor betere schattingen brm
  filter(Jaar <= sterftejaar | is.na(sterftejaar)) %>% 
  group_by(BoomID, ProefvlakNummer, sterftejaar, omtrek_start,
           textuurgroep, drainagegroep, waterbeschikbaarheid) %>%
  mutate(JaarC = Jaar - es_firstyear, 
         logBladverlies = log(Bladverlies + 0.5),
         sqrtBladverlies = sqrt(Bladverlies))

### >>> Data met als eenheidsniveau het proefvlak

dfBladverliesPlot <-
  dfBladverlies %>%
  arrange(Jaar) %>% 
  group_by(Jaar, ProefvlakNummer, textuurgroep, drainagegroep, waterbeschikbaarheid) %>%
  summarize(Bladverlies = mean(Bladverlies), 
            logBladverlies = mean(logBladverlies), 
            sqrtBladverlies = mean(sqrtBladverlies),
            aantal_bomen = n())


### >>> Transformaties?

ggplot(dfBladverliesPlot, aes(x = Bladverlies)) + geom_histogram(bins = ceiling(sqrt(nrow(dfBladverlies))))
ggplot(dfBladverliesPlot, aes(x = Bladverlies)) + geom_histogram() + facet_wrap(~Jaar)

ggplot(dfBladverliesPlot, aes(x = sqrtBladverlies)) + 
  geom_histogram(bins = ceiling(sqrt(nrow(dfBladverlies))))

ggplot(dfBladverliesPlot, aes(x = Bladverlies)) + geom_histogram(binwidth = 5)
ggplot(dfBladverliesPlot, aes(x = Bladverlies)) + geom_histogram(binwidth = 5) + facet_wrap(~Jaar)

######################################################

### >>> LMER

#######################################################

table(dfBladverlies$textuurgroep,  dfBladverlies$drainagegroep) #interacties niet mogelijk
table(dfBladverlies$waterbeschikbaarheid, dfBladverlies$drainagegroep) #goede verdeling
table(dfBladverlies$waterbeschikbaarheid, dfBladverlies$textuurgroep) #interacties niet mogelijk

modelfull <- lmer(Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + textuurgroep + JaarC:textuurgroep + 
                    drainagegroep + waterbeschikbaarheid + drainagegroep:waterbeschikbaarheid + 
                    JaarC:drainagegroep + JaarC:waterbeschikbaarheid + JaarC:drainagegroep:waterbeschikbaarheid + 
                    (JaarC|ProefvlakNummer / BoomID), 
                  data = dfBladverlies)
summary(modelfull)
plot(modelfull)

#via drop1 en keuzes
modeldropped <- lmer(Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + 
                      waterbeschikbaarheid + 
                       (JaarC|ProefvlakNummer / BoomID), 
                     data = dfBladverlies)
summary(modeldropped)
drop1(modeldropped)

plot(modeldropped)

# 
# 
# modelfull01a <- lmer(Bladverlies ~ `Textuur-gegroepeerd` * JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# modelfull01b <- lmer(Bladverlies ~ `Textuur-gegroepeerd` * JaarC + (1|ProefvlakNummer/BoomID), data = dfBladverlies)
# anova(modelfull01a, modelfull01b)
# 
# modelfull02a <- lmer(Bladverlies ~ `Drainagegroepering` * JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# modelfull02b <- lmer(Bladverlies ~ `Drainagegroepering` * JaarC + (1|ProefvlakNummer/BoomID), data = dfBladverlies)
# anova(modelfull02a, modelfull02b)
# 
# modelfull03a <- lmer(Bladverlies ~ `Textuur-gegroepeerd` * JaarC + `Drainagegroepering` * JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# modelfull03b <- lmer(Bladverlies ~ `Textuur-gegroepeerd` * JaarC + `Drainagegroepering` * JaarC + (1|ProefvlakNummer/BoomID), data = dfBladverlies)
# anova(modelfull03a, modelfull03b)
# 
# modelfull04a <- lmer(Bladverlies ~ `Waterbeschikbaarheid` * JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# modelfull04b <- lmer(Bladverlies ~ `Waterbeschikbaarheid` * JaarC + (1|ProefvlakNummer/BoomID), data = dfBladverlies)
# anova(modelfull04a, modelfull04b)
# 
# modelfull04 <- lmer(Bladverlies ~ `Waterbeschikbaarheid` * JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# modelfull05 <- lmer(Bladverlies ~ `Waterbeschikbaarheid` + JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# modelfull06 <- lmer(Bladverlies ~  JaarC + (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
# 
# AIC(modelfull01a, modelfull02a, modelfull03a, modelfull04, modelfull05, modelfull06)
# BIC(modelfull01a, modelfull02a, modelfull03a, modelfull04, modelfull05, modelfull06)

###

model00 <- lmer(Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid + 
                  (JaarC|ProefvlakNummer/BoomID), data = dfBladverlies)
summary(model00)
plot(model00)
E <- resid(model00, type = "pearson")
qqnorm(E); qqline(E)
hist(E)
plot(density(E))
acf(E)

# newdata <- data.frame(Jaar = dfBladverlies$Jaar, JaarC = dfBladverlies$JaarC,  E = E, ProefvlakNummer = dfBladverlies$ProefvlakNummer, 
#                       Waterbeschikbaarheid = dfBladverlies$Waterbeschikbaarheid, 
#                       Textuur = dfBladverlies$`Textuur-gegroepeerd`,
#                       Drainage = dfBladverlies$`Drainagegroepering`,
#                       Fit = predict(model00),
#                       Fit0 = predict(model00, re.form = ~0))
# ggplot(newdata, aes(x = Fit, y = E)) + geom_point() + geom_smooth()
# ggplot(newdata, aes(x = Fit0, y = E)) + geom_point() + geom_smooth()
# ggplot(newdata, aes(x = Jaar, y = E, group = Jaar)) + geom_boxplot()
# ggplot(newdata, aes(x = factor(ProefvlakNummer), y = E)) + geom_boxplot()
# ggplot(newdata, aes(x = factor(ProefvlakNummer), y = E)) + geom_boxplot() + facet_wrap(~Jaar)
# ggplot(newdata, aes(x = Waterbeschikbaarheid, y = E)) + geom_boxplot() + facet_wrap(~Jaar)
# ggplot(newdata, aes(x = Textuur, y = E)) + geom_boxplot() + facet_wrap(~Jaar)
# ggplot(newdata, aes(x = Drainage, y = E)) + geom_boxplot() + facet_wrap(~Jaar)
# 
# predicties <- expand.grid(JaarC = es_firstyear:es_lastyear - es_firstyear, 
#                           Waterbeschikbaarheid = unique(dfBladverlies$Waterbeschikbaarheid))
# predicties$fit <- predict(model00, newdata = predicties, re.form = ~0)
# 
# ggplot(data = dfBladverliesPlot, aes(x = Jaar, y = Bladverlies, color = Waterbeschikbaarheid)) + 
#   geom_point(pch = 1) + geom_path(aes(group = ProefvlakNummer), linetype = "dashed") + 
#   geom_line(data = predicties, aes(x = JaarC + es_firstyear, y = fit, color = Waterbeschikbaarheid), size = 2)


###################################################################

### brms wegens autocorrelatie

###################################################################


# calc <- TRUE 
# if (calc) {
#   #STANDAARD MODEL ZONDER CORR
#   library(brms) #zorg dat rstan en Rcpp zeker goed up to date zijn, of je krijgt serialization founten
#   model_brm <- brms::brm(Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid + 
#                            (JaarC|ProefvlakNummer/BoomID),
#                          data = dfBladverlies, 
#                          cores = 3, chains = 3,
#                          control = list(max_treedepth = 15))
#   save(model_brm, file = paste0(out, "model_brm_basis.Rdata"))
# } else {
#   load(file = paste0(out, "model_brm_basis.Rdata"))
# }



# if (calc) {
#   #STANDAARD MODEL MET AUTOCORR
#   library(brms) #zorg dat rstan en Rcpp zeker goed up to date zijn, of je krijgt serialization founten
#   model_brm2 <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
#                             (JaarC|ProefvlakNummer/BoomID) + 
#                             ar(time = JaarC, gr = BoomID, p=1),                                                
#                           data = dfBladverlies, iter = 20000,
#                           cores = 3, chains = 3, 
#                           control = list(max_treedepth = 15))
#   save(model_brm2, file = paste0(out, "model_brm_autocor.Rdata"))
# } else {
#   load(file = paste0(out, "model_brm_autocor.Rdata"))
# }
# summary(model_brm2)
# plot(model_brm2)  


# 
# 
# calc <- TRUE 
# if (calc) {
#   #STANDAARD MODEL ZONDER CORR
#   library(brms) #zorg dat rstan en Rcpp zeker goed up to date zijn, of je krijgt serialization founten
#   model_brm3sr <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
#                             JaarC:waterbeschikbaarheid +
#                             (1|ProefvlakNummer/BoomID) + 
#                             ar(time = JaarC, gr = BoomID, p=1),
#                           data = dfBladverlies, 
#                           cores = 3, chains = 3,
#                           control = list(max_treedepth = 15))
#   save(model_brm3sr, file = paste0(out, "model_brm_autocor_xtravar_simpran.Rdata"))
# } else {
#   load(file = paste0(out, "model_brm_autocor_xtravar_simpran.Rdata"))
# }


calc <- TRUE 
if (calc) {
  #STANDAARD MODEL ZONDER AUTOCORR en enkel random intercept
  library(brms) #zorg dat rstan en Rcpp zeker goed up to date zijn, of je krijgt serialization founten
  model_brm3noar_ranicpt <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
                              JaarC:waterbeschikbaarheid +
                              (1|ProefvlakNummer/BoomID),
                            data = dfBladverlies, 
                            cores = 3, chains = 3,
                            control = list(max_treedepth = 15))
  loo_brm3noar_ranicpt <- loo(model_brm3noar_ranicpt)
  save(model_brm3noar_ranicpt, loo_brm3noar_ranicpt, file = paste0(out, "model_brm3noar_ranicpt.Rdata"))
} else {
  load(file = paste0(out, "model_brm3noar_ranicpt.Rdata"))
}

########################

if (calc) {
  #STANDAARD MODEL ZONDER AUTOCORR met random slope
  library(brms) 
  model_brm3noar_jran <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
                                     JaarC:waterbeschikbaarheid +
                                     (JaarC|ProefvlakNummer/BoomID),
                                   data = dfBladverlies, 
                                   cores = 3, chains = 3,
                                   control = list(max_treedepth = 15))
  loo_brm3noar_jran <- loo(model_brm3noar_jran)
  save(model_brm3noar_jran, loo_brm3noar_jran,  file = paste0(out, "model_brm3noar_jran.Rdata"))
} else {
  load(file = paste0(out, "model_brm3noar_jran.Rdata"))
}  


#########################

if (calc) {
  #STANDAARD MODEL MET AUTOCORR EN random intercept
  library(brms) #zorg dat rstan en Rcpp zeker goed up to date zijn, of je krijgt serialization founten
  model_brm3ar_ranicpt <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
                                JaarC:waterbeschikbaarheid + 
                                (1|ProefvlakNummer/BoomID) + 
                                ar(time = JaarC, gr = BoomID, p=1),                                                
                              data = dfBladverlies, 
                              cores = 3, chains = 3, 
                              control = list(max_treedepth = 15))
 
  loo_brm3ar_ranicpt <- loo(model_brm3ar_ranicpt)
  save(model_brm3ar_ranicpt, loo_brm3ar_ranicpt, file = paste0(out, "model_brm3ar_ranicpt.Rdata"))
} else {
  load(file = paste0(out, "model_brm3ar_ranicpt.Rdata"))
  
}

##########################

if (calc) {
  #STANDAARD MODEL MET AUTOCORR en random slope
  library(brms) 
  model_brm3ar_jran <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
                                 JaarC:waterbeschikbaarheid +
                                 (JaarC|ProefvlakNummer/BoomID) + 
                                   ar(time = JaarC, gr = BoomID, p=1),
                                 data = dfBladverlies, 
                               cores = 3, chains = 3,
                               control = list(max_treedepth = 15))
  loo_brm3ar_jran <- loo(model_brm3ar_jran)
  save(model_brm3ar_jran, loo_brm3ar_jran,  file = paste0(out, "model_brm3ar_jran.Rdata"))
} else {
  load(file = paste0(out, "model_brm3ar_jran.Rdata"))
}

#######################

if (calc) {
  #STANDAARD MODEL ZONDER AUTOCORR met random slope
  library(brms) 
  model_brm4noar_jran <- brms::brm(formula = Bladverlies ~ JaarC + omtrek_start + JaarC:omtrek_start + waterbeschikbaarheid +
                                     (JaarC|ProefvlakNummer/BoomID),
                                   data = dfBladverlies, 
                                   cores = 4, chains = 4,
                                   iter = 20000,
                                   control = list(max_treedepth = 20))
  loo_brm4noar_jran <- loo(model_brm4noar_jran)
  save(model_brm4noar_jran, loo_brm4noar_jran,  file = paste0(out, "model_brm4noar_jran.Rdata"))
} else {
  load(file = paste0(out, "model_brm4noar_jran.Rdata"))
}  
summary(model_brm4noar_jran)
model_brm4noar_jran_summary <- summary(model_brm4noar_jran)
sink(paste0(out, "model_brm4noar_jran_summary.txt"))
model_brm4noar_jran_summary
sink()

######################
  
loo_compare(loo_brm3noar_ranicpt, loo_brm3noar_jran, loo_brm3ar_ranicpt, loo_brm3ar_jran, loo_brm4noar_jran)


#########################

newdata2 <- expand.grid(JaarC = es_jaren - es_firstyear,
                        waterbeschikbaarheid = unique(dfBladverlies$waterbeschikbaarheid),
                        omtrek_start = c(1:5/2))
newdata2$ProefvlakNummer <- ifelse(newdata2$waterbeschikbaarheid == "nat", -1, -2)
newdata2$BoomID <- 0

newdata_brm4 <- cbind(newdata2, predict(model_brm4noar_jran, 
                                        newdata = newdata2 %>% mutate(JaarC = jitter(JaarC)), 
                                        re_formula = ~0))
ggplot(newdata_brm4, 
       aes(x = JaarC + es_firstyear, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = factor(omtrek_start))) + 
  geom_point() + geom_line() + geom_errorbar(position = position_dodge(width = 0.5)) + 
  facet_wrap(~waterbeschikbaarheid) + 
  labs(x = "Jaar", y = "Schatting: % Bladverlies ~ JaarC * omtrek + natheid", color = "omtrek (m) in 2014")
ggsave(filename = paste0(out, "trend_modelpredicties.png"),
       height = fig_height, width = fig_width, dpi = 300)


# summary(model_brm3)
# plot(model_brm3)  
# newdata3 <- expand.grid(JaarC = es_jaren - es_firstyear, Drainagegroepering = unique(dfBladverlies$Drainagegroepering))
# newdata3$ProefvlakNummer <- ifelse(newdata3$Drainagegroepering == "abcd", -1, -2)
# newdata3$BoomID <- 0
# 
# newdata_brm3 <- cbind(newdata3, predict(model_brm3, newdata = newdata3, re_formula = ~0))
# ggplot(newdata_brm3, aes(x = JaarC + es_firstyear, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Drainagegroepering)) + 
#   geom_point() + geom_line() + geom_errorbar(position = position_dodge(width = 0.1)) + 
#   labs(x = "Jaar", y = "Schatting")
# 
# kfold_brm3 <- kfold(model_brm3) #runt meer dan een uur     11091.6 DIC
# kfold_brm2 <- kfold(model_brm2) #runt meer dan een halfuur 11081.3 DIC
# 
# 
# summary(model00)$coef
# summary(model_brm)$fixed
# summary(model_brm2)$fixed
# 
# loo(model_brm)
# loo(model_brm2)
# loo(model_brm3)


####################################################################################

 ### NIET BIJGEWERKT 

#########################################

##SEN SLOPE op het geheel
# 
# set.seed(sen_seed)
# 
# #Maak een dataset met de gemiddeldes en standaardfouten: PER JAAR
# dfEsTrendSummary <-
#   dfEsMetingTrend %>% 
#   group_by(Jaar) %>% 
#   summarise(aantal = n(), 
#             mean_BV = mean(Bladverlies), 
#             serr_BV = sd(Bladverlies) / sqrt(aantal),
#             lcl_BV = mean_BV - 2 * serr_BV,
#             ucl_BV = mean_BV + 2 * serr_BV)
# 
# dfSenResult <- 
#   dfEsMetingTrend %>% 
#   group_by(Jaar, JaarC, ProefvlakID) %>% 
#   summarise(Bladverlies = mean(Bladverlies)) 
# 
# n_boot <- 1000
# n_plot <- length(unique(dfSenResult$ProefvlakID))
# senslope <- numeric(n_boot)
# senintercept <- numeric(n_boot)
# test <- with(dfSenResult, rkt::rkt(date = JaarC, y = Bladverlies, block = ProefvlakID))
# for (i in 1:n_boot) {
#   randplots <- sample(unique(dfSenResult$ProefvlakID), size = n_plot, replace = TRUE) 
#   simdata <- NULL
#   for (j in 1:length(randplots)){
#     simdata <- rbind(simdata, 
#                      filter(dfSenResult, ProefvlakID == randplots[j]) %>% mutate(ProefvlakID = j)) 
#   }
#   senslope[i] <- with(simdata, rkt::rkt(JaarC, y = Bladverlies, block = ProefvlakID))$B
#   senintercept[i] <- median(simdata$Bladverlies - senslope[i] * simdata$JaarC)
#   if (i %% 100 ==0 ) cat(i, "of", n_boot, "\n")
# }
# 
# mean(senslope)
# median(senslope)
# sd(senslope)
# mean(senintercept)
# median(senintercept)
# sd(senintercept)
# 
# hist(senslope)
# hist(senintercept)
# 
# 
# prednewdata0 <- data.frame(JaarC = 0:5)
# prednewdata0$avgBladverlies <- dfEsMetingTrend %>% 
#   group_by(JaarC, ProefvlakID) %>% 
#   summarize(Bladverlies = mean(Bladverlies)) %>% 
#   summarize(Bladverlies = mean(Bladverlies)) %>% pull(Bladverlies)
# prednewdata0$pred_lmer <- predict(model00, newdata = prednewdata0, re.form = ~0)
# prednewdata0$pred_loglmer <- exp(predict(modellog, newdata = prednewdata0, re.form = ~0)) - 2.5
# prednewdata0$pred_sen <- mean(senintercept) + prednewdata0$JaarC * mean(senslope)
# ggplot(prednewdata0 %>% gather(key = "model", value = "predictie", -JaarC), 
#        aes(x = JaarC, y = predictie, color = model)) + geom_point() + geom_line()


### >>> Wat met bomen die 100% bereiken

#Eens ze 100% bereiken tellen ze mee, maar daarna zouden ze als NA moeten beschouwd worden?
#Voor de status geeft dit een negatieve bias, 
#Voor de trend kan dit misschien geen kwaad?
#Daalt de vitaliteit sneller eens de bomen minder vitaal zijn geworden? --> autocorrelatie?


A <- 10
B <- 2
df <- data.frame(Jaar = 1:10) %>%
  mutate(Est = A + B * Jaar, 
         Respons = Est + rnorm(10, 0, 0.5), 
         Respons = ifelse(Respons > 24, 24, Respons))
summary(lm(Respons ~ Jaar, data = df))
summary(lm(Respons ~ Jaar, data = df[1:7,]))


#Impact van bomen die 100%  bereiken

dode_bomen <- dfEsMetingTrend %>%
  group_by(Jaar) %>%
  summarize(aantal_dood = sum(Bladverlies == 100))
ggplot(dode_bomen, aes(x = Jaar, y = aantal_dood)) + geom_line()




pop <- rnorm(100000, mean = 20, sd = )


# predictiessenslope <<- NULL
# (dfSenResult <-
#     bomen_calc(x = dfTreesTrend,
#                group = lapply(normal_groups, c, "PlotNr"),
#                respons = "BladverliesNetto") %>%
#     group_by(selectie) %>%
#     do({
#       print(.[["selectie"]][1])
#       test <- rkt::rkt(date = .[["Jaar"]], y = .[["mean_value"]], block = .[["PlotNr"]])
#       if (sen_boot > 0){
#         senslope <- numeric(sen_boot)
#         senintercept <- numeric(sen_boot)
#         allplots <- unique(.[["PlotNr"]])
#         for (i in 1:sen_boot){
#           randplots <- sample(allplots, size = length(allplots), replace = TRUE)
#           bd <- NULL
#           for (j in 1:length(randplots)){
#             bd <- rbind(bd, (filter(., PlotNr == randplots[j]) %>% mutate(PlotNr = j)))
#           }
#           senslope[i] <- rkt::rkt(bd[["Jaar"]], y = bd[["mean_value"]], block = bd[["PlotNr"]])$B
#           senintercept[i] <- median(bd[["mean_value"]] - senslope[i] * bd[["Jaar"]])
#         }
#       } else {
#         print(sen_boot)
#         print("HERE")
#         senslope <- NA
#       }
#       print(senslope)
#       intercept <- median(.[["mean_value"]] - test$B * .[["Jaar"]])
#       
#       lmerfit <- summary(lmer(mean_value ~ Jaar + (1|PlotNr), data = .))$coef[,1]
#       
#       rvdata <- data.frame(sen_slope = test$B,
#                            sen_lcl = quantile(senslope, probs = 0.025, na.rm = TRUE),
#                            sen_ucl = quantile(senslope, probs = 0.975, na.rm = TRUE),
#                            sen_intercept = intercept,
#                            tau = test$tau, pval = test$sl, N_obs = nrow(.),
#                            lmer_intercept = lmerfit[1], lmer_slope = lmerfit[2])
#       predicties <- data.frame(selectie = sort(unique(.[["selectie"]])), Jaar = sort(unique(.[["Jaar"]])))
#       for (i in 1:sen_boot){
#         predicties <- cbind(predicties, pred = senintercept[i] + senslope[i]*predicties$Jaar)
#       }
#       if (!is.null(predicties)) {
#         predictiessenslope <<- rbind(predictiessenslope, predicties)
#       }
#       rvdata
#     }) %>%
#     left_join(dfVolgorde) %>%
#     arrange(volgorde)) %>%
#   write.csv2(file = "output/trend_NNV_overzicht.csv")
# 
# dfSenTrend <- expand.grid(Jaar = unique(dfTrendSummary$Jaar),
#                           selectie = dfSenResult$selectie,
#                           stringsAsFactors = FALSE) %>%
#   left_join(select(dfSenResult, selectie, sen_intercept, sen_slope, lmer_intercept, lmer_slope), by = "selectie") %>%
#   left_join(select(dfTrendSummary, Jaar, selectie, mean_value, lcl, ucl), bu = c("selectie", "Jaar")) %>%
#   mutate(pred_sen = sen_intercept + Jaar * sen_slope,
#          pred_lmer = lmer_intercept + Jaar * lmer_slope)
# 
# predictiessummary <- apply(predictiessenslope[,-(1:2)], 1, quantile, probs = c(0.025,0.975))
# predictiessenslope <-cbind(predictiessenslope[,1:2], t(predictiessummary))
# 
# if(!is.null(predictiessenslope)) {
#   dfSenTrend <- left_join(dfSenTrend, predictiessenslope, by = c("selectie", "Jaar"))
#   write.csv2(dfSenResult, file = paste0("output/trend_NNV_overzicht_with_boot",sen_boot, ".csv"))
# }
# 
# ggplot(dfSenTrend, aes(x = Jaar, y = mean_value, ymin = lcl, ymax = ucl)) +
#   geom_point() + geom_errorbar() + geom_line() +
#   geom_line(aes(y = pred_sen), color = inbo.bruinrood) +
#   geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = inbo.bruinrood, alpha = 0.2) +
#   geom_line(aes(y = pred_lmer), color = inbo.donkergroen) +
#   facet_wrap(~selectie)
# 
# pp <- NULL
# for (sel in unique(dfSenTrend$selectie)) {
#   plotdata <- filter(dfSenTrend, selectie == sel)
#   pp[[sel]] <-
#     ggplot(plotdata,
#            aes(x = Jaar, y = mean_value, ymin = lcl, ymax = ucl)) +
#     geom_point() + geom_errorbar() + geom_line() +
#     geom_line(aes(y = pred_sen))
#   if (is.null(plotdata$`2.5%`)){
#     #do nothing
#   } else {
#     pp[[sel]] <- pp[[sel]] +
#       geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2)
#   }
#   pp[[sel]] <- pp[[sel]] +
#     # geom_line(aes(y = pred_lmer, color = "lmer slope")) +
#     ylab("Bladverlies (%)") + ggtitle(sel) + ylim(0, max(30, max(plotdata$ucl))) +
#     scale_color_manual(values = c(inbo.bruinrood, inbo.donkergroen))
# }
# 
# for (nm in names(pp)){
#   ggsave(pp[[nm]], file = paste0("output/trend_nnv_", nm, ".png"),
#          dpi = fig_dpi, width = fig_width, height = fig_height)
# }
# 
# 
# 
# ## SEN SLOPE PER PLOT
# 
# dfSenSourcePerPlot <- dfTreesTrend %>%
#   group_by(PlotNr, Jaar) %>%
#   summarize(gemNNV = mean(BladverliesNetto, na.rm = TRUE),
#             N = n(),
#             se = sd(BladverliesNetto, na.rm = TRUE) / sqrt(N),
#             lcl = gemNNV - 1.96 * se,
#             ucl = gemNNV + 1.96 * se)
# 
# #De fout wordt in blauw aangegeven, is geen echte fout, is gewoon omdat er een plot is met slechts 3 jaren met metingen
# dfSenPerPlot <- dfSenSourcePerPlot %>%
#   do({
#     test <- rkt::rkt(date = .[["Jaar"]], y = .[["gemNNV"]])
#     intercept <- median(.[["gemNNV"]] - test$B * .[["Jaar"]])
#     data.frame(sen_slope = test$B, sen_intercept = intercept,
#                tau = test$tau, pval = test$sl, N_obs = nrow(.))
#   })
# 
# ## SEN SLOPE PER PLOT EN PER SOORT
# 
# dfSenSourcePerSoortEnPlot <- dfTreesTrend %>%
#   group_by(PlotNr, SoortIndeling, Jaar) %>%
#   summarize(gemNNV = mean(BladverliesNetto, na.rm = TRUE),
#             N = n(),
#             se = sd(BladverliesNetto, na.rm = TRUE) / sqrt(N),
#             lcl = gemNNV - 1.96 * se,
#             ucl = gemNNV + 1.96 * se)
# 
# #De fout wordt in blauw aangegeven, is geen echte fout, is gewoon omdat er een plot is met slechts 3 jaren met metingen
# dfSenPerSoortEnPlot <- dfSenSourcePerSoortEnPlot %>%
#   do({
#     test <- rkt::rkt(date = .[["Jaar"]], y = .[["gemNNV"]])
#     intercept <- median(.[["gemNNV"]]) - median(test$B * .[["Jaar"]])
#     data.frame(sen_slope = test$B, sen_intercept = intercept,
#                tau = test$tau, pval = test$sl, N_obs = nrow(.))
#   })
# 
# dfSenAll <- left_join(dfSenSourcePerSoortEnPlot, dfSenPerSoortEnPlot) %>%
#   mutate(pred = sen_intercept + Jaar * sen_slope)
# 
# ggplot(dfSenAll, aes(x = Jaar, y = gemNNV, ymin = lcl, ymax = ucl, color = SoortIndeling)) +
#   geom_errorbar() +
#   geom_line(aes(y = pred, color = SoortIndeling)) +
#   facet_wrap(~PlotNr)  +
#   ggsave(file = "output/trend_nnv_persoortenplot.png", dpi = 300, width = 30, height = 20)
# 
# 
# #########################################################################
# 
# ### BEREKENINGEN BESCHADIGDE BOMEN (NOG VERDER UIT TE WERKEN, VIA BINOMIAL GLM IPV SEN)
# 
# 
# dfBeschadigdSource <-
#   bind_rows(
#     dfTreesTrend %>%
#       group_by(Jaar, PlotNr) %>%
#       summarize(n_beschadigd = sum(Beschadigd == "beschadigd"),
#                 totaal = n(),
#                 aandeel_beschadigd = n_beschadigd/totaal,
#                 selectie = "totaal"),
#     dfTreesTrend %>%
#       group_by(Jaar, PlotNr, SoortType) %>%
#       summarize(n_beschadigd = sum(Beschadigd == "beschadigd"),
#                 totaal = n(),
#                 aandeel_beschadigd = n_beschadigd/totaal,
#                 selectie = SoortType[1]),
#     dfTreesTrend %>%
#       group_by(Jaar, PlotNr, SoortIndeling) %>%
#       summarize(n_beschadigd = sum(Beschadigd == "beschadigd"),
#                 totaal = n(),
#                 aandeel_beschadigd = n_beschadigd/totaal,
#                 selectie = SoortIndeling[1])
#   ) %>%
#   group_by(selectie) %>%
#   mutate(JaarC = Jaar - meerjaarlijks[1],
#          n_onbeschadigd = totaal - n_beschadigd)
# 
# dfBeschadigdSummary <-
#   dfBeschadigdSource %>%
#   group_by(Jaar, selectie) %>%
#   summarize(gemiddeld_beschadigd = mean(aandeel_beschadigd),
#             Nplots = n(),
#             serr_beschadigd = sd(aandeel_beschadigd)/sqrt(Nplots),
#             lcl = gemiddeld_beschadigd - 1.96* serr_beschadigd,
#             ucl = gemiddeld_beschadigd + 1.96 * serr_beschadigd)
# 
# 
# set.seed(sen_seed)
# dfBeschadigdPred <-
#   dfBeschadigdSource %>%
#   do({
#     print(.[["selectie"]][1])
#     modelB <- glmer(cbind(n_beschadigd, n_onbeschadigd) ~ JaarC + (1|PlotNr),
#                     family = "binomial", data = .)
#     boot_modelB <-
#       bootMer(x  = modelB,
#               FUN = function(x) {
#                 predict(x, re.form = ~0, wts = 1,
#                         newdata = data.frame(JaarC = meerjaarlijks - meerjaarlijks[1],
#                                              Jaar = meerjaarlijks))
#               },
#               nsim = lmer_boot,
#               .progress = "txt")
#     predicties_modelB <-
#       data.frame(Jaar = meerjaarlijks,
#                  JaarC = meerjaarlijks - meerjaarlijks[1],
#                  Blink = apply(boot_modelB$t,2,mean),
#                  sd = apply(boot_modelB$t, 2, sd)) %>%
#       mutate(lcllink = Blink - 1.96 * sd,
#              ucllink = Blink + 1.96 * sd,
#              beschadigd = plogis(Blink),
#              lcl_lmer = plogis(lcllink),
#              ucl_lmer = plogis(ucllink))
#     predicties_modelB
#   })
# 
# dfBeschadigdPlot <- left_join(dfBeschadigdSummary, dfBeschadigdPred, by = c("Jaar", "selectie"))
# 
# 
# ggplot(dfBeschadigdPlot, aes(x = Jaar, y = gemiddeld_beschadigd, ymin = lcl, ymax = ucl)) +
#   geom_point() + geom_errorbar() +
#   geom_line(aes(y = beschadigd), color = inbo.bruinrood) +
#   geom_ribbon(aes(ymin = lcl_lmer, ymax = ucl_lmer), fill = inbo.bruinrood, alpha = 0.2) +
#   facet_wrap(~selectie)
# 
# pp <- NULL
# for (sel in unique(dfBeschadigdPlot$selectie)){
#   pp[[sel]] <-
#     ggplot(filter(dfBeschadigdPlot, selectie == sel),
#            aes(x = Jaar, y = gemiddeld_beschadigd, ymin = lcl, ymax = ucl)) +
#     geom_point() + geom_line() +  geom_errorbar() +
#     geom_line(aes(y = beschadigd), color = inbo.bruinrood) +
#     geom_ribbon(aes(ymin = lcl_lmer, ymax = ucl_lmer), fill = inbo.bruinrood, alpha = 0.2) +
#     ggtitle(sel) +
#     ylab("gemiddeld aandeel beschadigde bomen per plot") +
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# }
# 
# for (sel in names(pp)) {
#   ggsave(pp[[sel]], file = paste0("output/trend_beschadigd_", sel, ".png"),
#          width = fig_width, height = fig_height, dpi = fig_dpi)
# }
# 
# 
# ppp <-
#   dfBeschadigdPlot %>%
#   filter(selectie %in% c("totaal", "loofbomen", "naaldbomen")) %>%
#   ggplot(aes(x = Jaar, y = gemiddeld_beschadigd, color = selectie)) +
#   geom_point() + geom_line() + ylab("gemiddeld beschadigd per plot") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.4)) +
#   scale_color_manual(name = "", values = c(inbo.donkergroen, inbo.bruinrood, inbo.oranje))
# print(ppp)
# 
# ggsave(ppp, file = "output/trend_beschadigd_overzicht.png",
#        width = fig_width, height = fig_height, dpi = fig_dpi)
# 
# ################################################
