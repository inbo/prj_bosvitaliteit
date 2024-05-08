#TRENDANALYSE## >>> Trendanalyse

### >>> Verkennend plot met alle mogelijke data, bomen die sterven tellen slechts 1x nog mee in de data

out <- "outputEs_trend/"
inbo_palette_exp <- c(inbo_palette(), "#000000", "#CCCCCC")

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

(p <- ggplot(dfCheckBladverlies, 
             aes(x = jitter(Bladverlies, factor = 2), 
                 y = Bladverlies_verschil_yr, color = factor(Jaar))) + 
    geom_point(pch = 1) + 
    geom_smooth(aes(x = Bladverlies, 
                    y = Bladverlies_verschil_yr), inherit.aes = FALSE) +
  scale_color_manual(values = inbo_palette_exp)) +
ggsave(p, filename = paste0(out, "trend_invloed_huidigbladverlies.png"),
       width = fig_width, height = fig_height)

(p <- ggplot(dfCheckBladverlies, aes(x = jitter(Bladverlies, factor = 2), y = Bladveriles_factor_yr, color = factor(Jaar))) + 
    geom_point(pch = 1) + geom_smooth(aes(x = Bladverlies, y = Bladveriles_factor_yr), inherit.aes = FALSE) + 
    scale_color_manual(values = inbo_palette_exp)+
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

