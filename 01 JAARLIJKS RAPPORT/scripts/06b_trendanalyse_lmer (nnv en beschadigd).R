## Onderstaand bovenste blok is een kopie van 05a_Trendanalyse-Sen, zodat de scripts los van elkaar kunnen werken
## enkel 00_start.R moet gerund worden
## Voor Geert: dit script zal je zelf waarschijnlijk niet kunnen uitvoeren, omdat ik hiervoor brms gebruik, wat heel wat extra installatie vergt.

library(brms)
library(tidyverse)

set.seed(sen_seed) #gebruik hetzelfde seed als voor de sen

#Maak een dataset met de gemiddeldes en standaardfouten: PER JAAR
dfTrendSummary <-
  bomen_calc(x = dfTreesTrend, group = normal_groups, respons = "BladverliesNetto") %>%
  mutate(lcl = mean_value - 1.96 * se,
         ucl = mean_value + 1.96 * se)

dfTrendBeschadigdTot <-
  group_by(dfTreesTrend, PlotNr, Jaar) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100) %>%
  mutate(selectie = "totaal")

dfTrendBeschadigdType <-
  group_by(dfTreesTrend, PlotNr, Jaar, SoortType) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100) %>%
  mutate(selectie = SoortType)

dfTrendBeschadigdSoort <-
  group_by(dfTreesTrend, PlotNr, Jaar, SoortIndeling) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100) %>%
  mutate(selectie = SoortIndeling)

dfTrendBeschadigd <- bind_rows(dfTrendBeschadigdTot, dfTrendBeschadigdSoort, dfTrendBeschadigdType)

############################################################################################

## Analyse met de volledige dataset duurt te lang via brms, daarom gekozen om te werken met de aggregatie per plot


dfBRMSbasis  <- bomen_calc(x = dfTreesTrend,
                          group = lapply(normal_groups, c, "PlotNr"),
                          respons = "BladverliesNetto")

##############################################################################################

nnvmodels <- list()

for (sel in unique(dfBRMSbasis$selectie)) {
  dfBrms <- dfBRMSbasis %>%
    filter(selectie == sel) %>%
    ungroup() %>%
    transmute(JaarC = (Jaar - meerjaarlijks[1])/diff(range(meerjaarlijks)), PlotNr, logBladverlies = log(mean_value + 2.5))

  model <-  try(
    brm(logBladverlies ~ JaarC + (1|PlotNr), 
        data = dfBrms, family = gaussian(),
        autocor = cor_ar(~ JaarC|PlotNr, p = 1), #te brede intervallen
        iter = 10000, thin = 20, chains = 3, cores = 3))
  nnvmodels[[sel]] <- model
}

save(nnvmodels, file = file.path(outdir, "interim", "nnv_brms_models.Rdata"))
load(file = file.path(outdir, "interim", "nnv_brms_models.Rdata"))


### >>> Figuur maken
for (sel in names(nnvmodels)) {
  newdata <- data.frame(Jaar = min(meerjaarlijks):  max(meerjaarlijks)) %>%
    mutate(JaarC = (Jaar - meerjaarlijks[1])/diff(range(meerjaarlijks)), PlotNr = 0)
  plotdata <- dfBRMSbasis %>%
    filter(selectie == sel) %>%
    group_by(Jaar) %>%
    summarise(mean_nnv = mean(mean_value),
              se = sd(mean_value)/ sqrt(n())) %>%
    mutate(lcl = pmax(0, mean_nnv - 1.96 * se),
           ucl = pmin(100, mean_nnv + 1.96 * se))

  #predictie-intervallen
  preds <- cbind(newdata, predict(nnvmodels[[sel]], newdata = newdata, re.formula = NA, allow_new_levels = TRUE)) %>%
    mutate(fit = exp(Estimate) - 2.5, lcl = exp(Q2.5) - 2.5, ucl = exp(Q97.5) - 2.5)

  #conf-intervallen
  fixedsamps <- fixef(nnvmodels[[sel]], summary = FALSE)
  jaren <- cbind(1, newdata$JaarC)
  fixmat <- jaren %*% t(fixedsamps)
  confs <- cbind(newdata, fit = apply(fixmat, 1, mean), lcl = apply(fixmat, 1, quantile, prob = 0.025), ucl = apply(fixmat, 1, quantile, prob = 0.975)) %>%
    mutate(fit = exp(fit) - 2.5, lcl = exp(lcl) - 2.5, ucl = exp(ucl) - 2.5)

  p <-
    ggplot(plotdata, aes(x = Jaar, y = mean_nnv, ymin = lcl, ymax =  ucl)) +
    geom_point() +
    geom_errorbar() +
    geom_line(data =  confs, aes(x = Jaar, y = fit), inherit.aes = FALSE, color = inbo_groen) +
    geom_ribbon(data = confs, aes(x = Jaar, ymin = lcl, ymax = ucl), inherit.aes = FALSE, alpha = 0.3, fill = inbo_groen) +
      ylab("Bladverlies (%) brms") + ylim(0,40) + ggtitle(sel)
  ggsave(p, file = file.path(outdir, paste0("trend_nnv_",sel, "_brms.png")), dpi = fig_dpi, width = fig_width, height = fig_height)

}



############################################################################################
### >>> Aandeel beschadigde bomen (TREND)
############################################################################################


dfBRMS_schade  <- bomen_calc(x = dfTreesTrend,
                           group = lapply(normal_groups, c, "PlotNr", "Beschadigd"))


beschadigdmodels <- list()

for (sel in unique(dfBRMSbasis$selectie)) {
  dfBrmsschade <- dfBRMS_schade %>%
    filter(selectie == sel) %>%
    ungroup() %>%
    transmute(JaarC = (Jaar - meerjaarlijks[1])/diff(range(meerjaarlijks)), PlotNr, Beschadigd, AantalBomen) %>%
    spread(key = Beschadigd, value = AantalBomen, fill = 0) %>%
    mutate(totaal = beschadigd + onbeschadigd)

  model <- try(brm(beschadigd| trials(totaal) ~ JaarC + (1|PlotNr), data = dfBrmsschade, family = binomial(),
              iter = 10000, thin = 20, chains = 3, cores = 3))

  beschadigdmodels[[sel]] <- model

}

save(beschadigdmodels, file = file.path(outdir, "interim", "beschadigd_brms_models.Rdata"))
load(file = paste0(outdir, "interim", "beschadigd_brms_models.Rdata"))


### >>> Figuur maken

for (sel in names(beschadigdmodels)) {
  newdata <- data.frame(Jaar = min(meerjaarlijks):  max(meerjaarlijks)) %>%
    mutate(JaarC = (Jaar - meerjaarlijks[1])/diff(range(meerjaarlijks)), PlotNr = 0)
  plotdata <- dfBRMS_schade %>%
    filter(selectie == sel) %>%
    select(Jaar, PlotNr, Beschadigd, AantalBomen) %>%
    spread(key = Beschadigd, value = AantalBomen, fill = 0) %>%
    mutate(totaal = beschadigd + onbeschadigd,
           pctBeschadigd = beschadigd / totaal * 100) %>%
    group_by(Jaar) %>%
    summarize(meanBeschadigd = mean(pctBeschadigd),
              se = sd(pctBeschadigd) / sqrt(n())) %>%
    mutate(lcl = pmax(0, meanBeschadigd - 1.96 * se),
           ucl = pmin(100, meanBeschadigd + 1.96 * se))

  #predictie-intervallen
  preds <- cbind(newdata, predict(nnvmodels[[sel]], newdata = newdata, re.formula = NA, allow_new_levels = TRUE)) %>%
    mutate(fit = exp(Estimate) - 2.5, lcl = exp(Q2.5) - 2.5, ucl = exp(Q97.5) - 2.5)

  #conf-intervallen
  fixedsamps <- fixef(beschadigdmodels[[sel]], summary = FALSE)
  jaren <- cbind(1, newdata$JaarC)
  fixmat <- jaren %*% t(fixedsamps)
  confs <- cbind(newdata, fit = apply(fixmat, 1, mean), lcl = apply(fixmat, 1, quantile, prob = 0.025), ucl = apply(fixmat, 1, quantile, prob = 0.975)) %>%
    mutate(fit = 100 * plogis(fit), lcl = 100 * plogis(lcl), ucl = 100 * plogis(ucl))

  p <-
    ggplot(plotdata, aes(x = Jaar, y = meanBeschadigd, ymin = lcl, ymax =  ucl)) +
    geom_point() +
    geom_errorbar() +
    geom_line(data =  confs, aes(x = Jaar, y = fit), inherit.aes = FALSE, color = inbo_groen) +
    geom_ribbon(data = confs, aes(x = Jaar, ymin = lcl, ymax = ucl), inherit.aes = FALSE, alpha = 0.3, fill = inbo_groen) +
    ylab("Percentage beschadigd (brms)") + ylim(0,100) + ggtitle(sel)
  ggsave(p, file = file.path(outdir, paste0("trend_pctbeschadigd_",sel, "_brms.png")), dpi = fig_dpi, width = fig_width, height = fig_height)

}


############################################################################################
### >>> Aandeel beschadigde bomen (JAAR PER JAAR)
############################################################################################

dfBRMS_schade_yy  <- bomen_calc(x = dfTreesTrend,
                             group = lapply(normal_groups, c, "PlotNr", "Beschadigd")) %>%
  select(Jaar, PlotNr, Beschadigd, AantalBomen, selectie) %>%
  spread(key = Beschadigd, value = AantalBomen, fill = 0) %>%
  mutate(fJaar = factor(Jaar, levels = meerjaarlijks),
         totaal = onbeschadigd + beschadigd)

beschadigdyymodels <- NULL
for (sel in unique(dfBRMS_schade_yy$selectie)) {
  dfBrmsschade <- filter(dfBRMS_schade_yy, selectie == sel)
  model <- try(brm(beschadigd| trials(totaal) ~ 0 + fJaar + (1|PlotNr), data = dfBrmsschade, family = binomial(),
                   iter = 10000, thin = 20, chains = 3, cores = 3))
  beschadigdyymodels[[sel]] <- model
}

save(beschadigdyymodels, file = file.path(outdir, "interim", "beschadigd_brms_factorJaar.Rdata"))
load(file = file.path(outdir, "interim", "beschadigd_brms_factorJaar.Rdata"))

### Figuur maken

plotdata <- NULL
for (sel in names(beschadigdyymodels)) {
  modeldata <- cbind(data.frame(Jaar = meerjaarlijks, selectie = sel),
                    fixef(beschadigdyymodels[[sel]])) %>%
    mutate(fit = plogis(Estimate), lcl = plogis(Q2.5), ucl = plogis(Q97.5))
  tmp <- filter(dfTrendBeschadigd, selectie == sel) %>% group_by(Jaar, selectie) %>% summarize(Pctbeschadigd = mean(PctBeschadigd)) %>%
    inner_join(modeldata, by = c("Jaar", "selectie"))
  plotdata <- bind_rows(plotdata, tmp)
}

ggplot(plotdata, aes(x = Jaar, y = Pctbeschadigd)) + geom_point() + geom_line() + facet_wrap(~selectie) +
  geom_ribbon(aes(ymin = 100 * lcl, ymax = 100 * ucl), alpha = 0.3, color = NA, fill = inbo_groen) +
  ylab("Percentage Beschadigd (brms fit)")
ggsave(file = file.path(outdir, "trend_beschadigd_brmsfit.png"), dpi = fig_dpi, height = fig_height, width = fig_width)


