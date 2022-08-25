#Meerjaarlijkse Analyse

##############
### 2 jaar ###
##############

### >>> Toegevoegde en verdwenen maar levende bomen

#Toegevoegde bomen

dfTrees2 %>% as_tibble() %>%
  select(prbo, Jaar, PlotNr, PlotNaam, Gemeente, BoomNr, Soort) %>%
  filter(Jaar == tweejaarlijks[2]) %>%
  left_join(dfTrees2 %>%
              select(prbo, Jaar2 = Jaar) %>%
              filter(Jaar2 == tweejaarlijks[1]),
            by = "prbo") %>%
  filter(is.na(Jaar2)) %>%
  select(PlotNr, PlotNaam, Gemeente, BoomNr, Soort) %>%
  arrange(PlotNr, BoomNr) %>%
  write.csv2(file = paste0(outdir, "tweejaarlijks_toegevoegde_bomen.csv"))

#Verdwenen maar levende bomen

filter(dfTrees2, Jaar == tweejaarlijks[1]) %>%
  select(BoomKey,PlotNr, PlotNaam, Gemeente, BoomNr, Soort, VerwijderdReden, BladverliesNetto, SterfteJaar) %>%
  filter(!(BoomKey %in% (filter(dfTrees2, Jaar == tweejaarlijks[2]) %>% select(BoomKey) %>% .[[1]]))) %>%
  arrange(SterfteJaar, PlotNr, BoomNr) %>%
  write.csv2(file = paste0(outdir, "tweejaarlijks_verdwenen_bomen.csv"))

### >>> Beschadigde bomen

dfBeschadigd2j <-
  bind_rows(
    dfTrees2Gmsch %>%
      group_by(Jaar) %>%
      summarize(selectie = "totaal",
                pctBeschadigd = mean(BeschadigdNum, na.rm = TRUE),
                N = n(),
                serr = sd(BeschadigdNum/sqrt(N))),

    dfTrees2Gmsch %>%
      rename(selectie = SoortType) %>%
      group_by(Jaar, selectie) %>%
      summarize(pctBeschadigd = mean(BeschadigdNum, na.rm = TRUE),
                N = n(),
                serr = sd(BeschadigdNum/sqrt(N))),

    dfTrees2Gmsch %>%
      transmute(Jaar,
                selectie = paste(SoortType, LeeftijdsklasseEur),
                BeschadigdNum) %>%
      group_by(Jaar, selectie) %>%
      summarize(pctBeschadigd = mean(BeschadigdNum, na.rm = TRUE),
                N = n(),
                serr = sd(BeschadigdNum/sqrt(N))),


    dfTrees2Gmsch %>%
      rename(selectie = SoortIndeling) %>%
      group_by(Jaar, selectie) %>%
      summarize(pctBeschadigd = mean(BeschadigdNum, na.rm = TRUE),
                N = n(),
                serr = sd(BeschadigdNum/sqrt(N))) %>%
      filter(selectie %in% c("Amerikaanse eik", "beuk", "zomereik", "overige lbs.",
                             "Corsicaanse den", "grove den", "overige nbs."))
  ) %>%
  mutate(lcl = pctBeschadigd - 1.96 * serr,
         ucl = pctBeschadigd + 1.96 * serr)

selectie2 <- unique(arrange(dfSoortInfo, SoortVolgorde)[["SoortIndeling"]])

dfBeschadigd2j <-
  dfBeschadigd2j %>%
  mutate(selectie = factor(selectie, levels = selectie2[selectie2 %in% selectie]))


ggplot(filter(dfBeschadigd2j, !(selectie %in% c("overige nbs.", "loofbomen jong", "loofbomen oud", "naaldbomen jong", "naaldbomen oud"))),
              aes(x = selectie, y = pctBeschadigd, fill = factor(Jaar), ymin = pmax(0,lcl), ymax = ucl)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_discrete(name = "jaar") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("") + ylab("percentage beschadigde bomen") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_errorbar(position = "dodge")

ggsave(filename = file.path(outdir, "tweejaarlijks_04_aandeel_beschadigd.png"), width = fig_width, height = fig_height, dpi = fig_dpi)

dfBeschadigd2j %>%
  group_by(selectie) %>%
  do({
      test <- prop.test(x = .$pctBeschadigd *.$N, n = .$N)
      data.frame(p1 = test$estimate[1], p2 = test$estimate[2], pval_prop_test = test$p.value)
  }) %>%
  write.csv2(file = paste0(outdir, "tweejaarlijks_36_aandeel_beschadigd.csv"))

### >>> Evolutie blad/naaldverlies

bind_rows(
  cbind(selectie = "totaal", wilcox_table(dfTrees2Gmsch, BladverliesNetto ~ Jaar | prbo)),

  dfTrees2Gmsch %>%
    split(.$LeeftijdsklasseEur) %>%
    map_dfr(wilcox_table, formula = BladverliesNetto ~ Jaar | prbo, .id = "selectie") %>%
    mutate(selectie = paste("totaal", selectie)),

  dfTrees2Gmsch %>%
    split(.$SoortType) %>%
    map_dfr(wilcox_table, formula = BladverliesNetto ~ Jaar | prbo, .id = "selectie"),

  dfTrees2Gmsch %>%
    split(list(.$SoortType, .$LeeftijdsklasseEur), sep = " ") %>%
    map_dfr(wilcox_table, formula = BladverliesNetto ~ Jaar | prbo, .id = "selectie"),

  dfTrees2Gmsch %>%
    split(list(.$SoortIndeling), sep = " ") %>%
    map_dfr(wilcox_table, formula = BladverliesNetto ~ Jaar | prbo, .id = "selectie")
  ) %>%
  left_join(dfVolgorde) %>%
  arrange(volgorde) %>%
  select(-volgorde) %>%
  write.csv2(paste0(outdir, "tweejaarlijks_37_gemiddeld_bladverlies.csv"))

### Aantal gestegen/gedaalde klassen


tmpBVklasse <-
  dfTrees2Gmsch %>%
  group_by(prbo, SoortType, SoortIndeling) %>%
  mutate(BVKlasseEur = factor(BVKlasseEur, levels = c("0-10%",  "10+-25%", "25+-60%", "60+-99%", "100%" )),
         BVKlasseEurNum = as.numeric(BVKlasseEur)) %>%
  summarize(klasse_j1 = max(BVKlasseEurNum[Jaar == tweejaarlijks[1]]),
            klasse_j2 = max(BVKlasseEurNum[Jaar == tweejaarlijks[2]]),
            verschil = klasse_j2 - klasse_j1)

bind_rows(

  group_by(tmpBVklasse, verschil) %>%
    summarize(Aantal = length(verschil)) %>%
    mutate(selectie = "totaal"),

  group_by(tmpBVklasse, SoortType, verschil) %>%
    summarize(Aantal = length(verschil)) %>%
    mutate(selectie = SoortType),

  group_by(tmpBVklasse, SoortIndeling, verschil) %>%
    summarize(Aantal = length(verschil)) %>%
    mutate(selectie = SoortIndeling))  %>%

  select( - SoortType, - SoortIndeling) %>%
  left_join(dfTotaalBomen2J) %>%
  mutate(Pct = Aantal / TotaalAantalBomen * 100) %>%
  group_by(selectie, verschil) %>%
  summarize(Pct = max(Pct)) %>%
  spread(key = verschil, value = Pct, fill = 0.00) %>%
  left_join(dfVolgorde) %>%
  arrange(volgorde) %>%
  write.csv2(paste0(outdir, "tweejaarlijks_38_klassenverschuivingen.csv"))


### Overzichtstabel per proefvlak (gemNNV, verschilNNV, %verschilNNV)

plotNNVs <- dfTrees2Gmsch %>%
  group_by(PlotNr, JaarS2) %>%
  summarize(gemNNV = mean(BladverliesNetto, na.rm = TRUE),
            sdNNV = sd(BladverliesNetto, na.rm = TRUE),
            aantal_bomen = n(),
            serrNNV = sdNNV / sqrt(aantal_bomen),
            pctBeschadigd = sum(Beschadigd == "beschadigd")/ n() * 100)

dfpart_nnv <- plotNNVs %>%
  select(PlotNr, JaarS2, gemNNV) %>%
  spread(key = JaarS2, value = gemNNV) %>%
  mutate(gemNNV_J1 = J1,
         gemNNV_J2 = J2,
         verschilNNV = gemNNV_J2 - gemNNV_J1)

dfpart_aantal <- plotNNVs %>%
  select(PlotNr, JaarS2, aantal_bomen) %>%
  spread(key = JaarS2, value = aantal_bomen) %>%
  mutate(aantal_J1 = J1,
         aantal_J2 = J2)


dfpart_besch <- plotNNVs %>%
  select(PlotNr, JaarS2, pctBeschadigd) %>%
  spread(key = JaarS2, value = pctBeschadigd) %>%
  mutate(pctBeschadigd_J1 = J1,
         pctBeschadigd_J2 = J2,
         verschilbeschadigd = pctBeschadigd_J2 - pctBeschadigd_J1)

testgemNNV <- dfTrees2Gmsch %>%
  group_by(PlotNr) %>%
  do({
    tmp <- select(., BoomNr, JaarS2, BladverliesNetto) %>%
      spread(key = JaarS2, value = BladverliesNetto) %>%
      na.omit()
    if (nrow(tmp) >= 5){
      data.frame(pval_wilcox_nnv = wilcox.test(tmp[[2]], tmp[[3]], paired = TRUE)$p.value)
    } else {
      data.frame(pval_wilcox_nnv = NA)
    }
  })

testPctBeschadigd <- dfTrees2Gmsch %>%
  group_by(PlotNr) %>%
  do({
    tmp <- select(., BoomNr, JaarS2, Beschadigd) %>%
      mutate(Beschadigd = Beschadigd == "beschadigd") %>%
      spread(key = JaarS2, value = Beschadigd) %>%
      na.omit() %>%
      summarize(beschadigd_J1 = sum(J1),
                onbeschadigd_J1 = sum(J1 == FALSE),
                beschadigd_J2 = sum(J2),
                onbeschadigd_J2 = sum(J2 == FALSE))

      data.frame(pval_fisher_besch = fisher.test(cbind(c(tmp[[1]], tmp[[3]]),
                                               c(tmp[[2]], tmp[[4]])))$p.value)
  })

dfpart_aantal %>%
  select(PlotNr, aantal_J1, aantal_J2) %>%
  left_join(select(dfpart_nnv, PlotNr, gemNNV_J1, gemNNV_J2, verschilNNV), by = "PlotNr") %>%
  left_join(testgemNNV, by = "PlotNr") %>%
  left_join(select(dfpart_besch, PlotNr, pctBeschadigd_J1, pctBeschadigd_J2, verschilbeschadigd), by = "PlotNr") %>%
  left_join(testPctBeschadigd, by = "PlotNr") %>%
  write.csv2(file = paste0(outdir, "jaarlijks_appendix_cijfersperplot.csv"))

cat("ALL TWEEJAARLIJKS FINISHED\n")
