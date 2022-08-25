##########################
### JAARLIJKSE ANALYSE ###
##########################

#Ieder blokje zou onafhankelijk moeten zijn van elkaar
#Sommige worden weggeschreven als tabellen of figuren naar een bestand
#als je de volledige data wil zien kan je de regel "%>% View" toevoegen
#Je kan de tussentijdse resultaten zien, door enkel de eerste x regels van het blok te selecteren
#   zorg juist ervoor dat je de %>% op het einde van de laatste selectieregel niet meeneemt

###0.1 Samenstelling steekproef (Tabel 2 in rapport 2017)

bomen_calc(dfTrees, normal_groups) %>%
  select(selectie, AantalBomen, PctOfTotaalBomen) %>%
  filter(!duplicated(.)) %>%
  left_join(select(dfSoortInfo, SoortIndeling, Soort, SoortType), by = c(selectie = "Soort")) %>%
  arrange(SoortType, desc(AantalBomen)) %>%
  select(SoortIndeling, selectie, AantalBomen, PctOfTotaalBomen) %>%
  write.csv2(file = file.path(outdir, "jaarlijks_02_samenstellingsteekproef.csv"))

bomen_calc(filter(dfTrees, SoortIndeling %in% c("overige lbs.","overige nbs.")),
           list(c("Jaar", "Soort"))) %>%
  select(Jaar, Soort, AantalBomen) %>%
  left_join(select(dfSoortInfo, Soort, SoortIndeling), by = "Soort") %>%
  arrange(SoortIndeling, desc(AantalBomen)) %>%
  select(Jaar, SoortIndeling, Soort, AantalBomen) %>%
  write.csv2(file = file.path(outdir, "jaarlijks_02b_samenstellingsteekproefOverig.csv"))



###1.1 Aandeel beschadigde bomen leeftijd soort / verkleurde bomen leeftijd soort

#1.1a Beschadigde bomen

bomen_calc(dfTrees, normal_groups, "Beschadigd") %>%
  select(SoortType, SoortIndeling, Beschadigd, PctBomen) %>%
  spread(key = Beschadigd, value = PctBomen)

#1.1b Verkleuring

#Verkleuring is weggevallen in de dataset na 2006

###1.2 aandeel bomen 10% BV klassen / aandeel bomen EurBVklassen

bomen_calc(dfTrees, normal_groups, "BVKlasseEur")
bomen_calc(dfTrees, normal_groups, "BVKlasse5")
bomen_calc(dfTrees, normal_groups, "BVKlasse10")

###1.3 Gem-Med-Stdev (StdERR?) BV_Leeftijd_Soort

bomen_calc(dfTrees, normal_groups, respons = "BladverliesNetto")

###1.4 Samenstelling steekproef soort (eigen interpretatie)

bomen_calc(dfTrees, c("Jaar", "PlotNr"), "SPEC_DES", respons = "BladverliesNetto")

###1.5 Gem en Med Leeftijd soort

bomen_calc(dfTrees, group = c("Jaar"), group2 = "SPEC_DES", respons = "Leeftijd")

lft1 <- bomen_calc(dfTrees, normal_groups,  respons = "Leeftijd") %>% select(selectie, mean_value)
lft2 <- bomen_calc(dfTrees, normal_groups, group2 = "LeeftijdsklasseEur") %>%
  select(selectie, LeeftijdsklasseEur, PctBomen) %>% spread(key = LeeftijdsklasseEur, value = PctBomen)
left_join(lft1, lft2) %>% write.csv2(file.path(outdir, "jaarlijks_05_leeftijden.csv"))
rm(lft1, lft2)

###1.6 Aandeel bomen leeftijd soort

bomen_calc(dfTrees, normal_groups, group2 = "SPEC_DES")

bomen_calc(dfTrees, c("Jaar","SoortType", "LeeftijdsklasseEur"), group2 = c( "SoortIndeling")) %>%
  select(LeeftijdsklasseEur, SoortIndeling, PctBomen) %>%
  arrange(SoortType, LeeftijdsklasseEur, desc(PctBomen)) %>% spread(key = LeeftijdsklasseEur, value = PctBomen, fill = 0.00) %>%
  write.csv2(file.path(outdir, "jaarlijks_0607_soortensamenstellingleeftijdsgroepen.csv"))

###1.7 Aandeel soorten leeftijd soorttype

bomen_calc(dfTrees, c("Jaar", "LeeftijdsklasseEur"), "SoortIndeling")

###1.9 Aandeel bomen 5% BV klassen

bomen_calc(dfTrees, normal_groups, "BVKlasse5")

bomen_calc(dfTrees, normal_groups, "BVKlasseEur") %>%
  select(selectie, BVKlasseEur, PctBomen) %>%
  left_join(dfVolgorde) %>%
  spread(key = BVKlasseEur, value = PctBomen, fill = 0)  %>% arrange(volgorde) %>%
  rowwise() %>% mutate(Beschadigd = 100 - `0-10%` - `10+-25%`) %>%
  write.csv2(file.path(outdir, "jaarlijks_16_verdeling_per_bvklasse.csv"))

bomen_calc(dfTrees, normal_groups, "BVKlasse10") %>%
  select(selectie, BVKlasse10, PctBomen) %>%
  spread(key = selectie, value = PctBomen, fill = 0) %>%
  write.csv2(file.path(outdir, "jaarlijks_17_verdeling_per_10pctBVklasse.csv"))


bomen_calc(dfTrees, all_groups, c("Beschadigd")) %>%
  filter(Beschadigd == "beschadigd") %>%
  select(selectie, LeeftijdsklasseEur, PctBomen) %>%
  spread(key = LeeftijdsklasseEur, value = PctBomen) %>%
  left_join(dfVolgorde) %>% arrange(volgorde) %>%
  rename("totaal" = `<NA>`) %>% select(-volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_19_beschadigdperleeftijd.csv"))


# wilcoxon test (niet-gepaard) --> om wel gepaard te werken aan de formule " | paarvariabele" toevoegen

#Geeft een error maar die wordt via try() gegenereerd, mag genegeerd worden
bind_rows(
  cbind(selectie = "totaal", wilcox_table(dfTrees, BladverliesNetto ~ LeeftijdsklasseEur)),
dfTrees %>%
  split(.$SoortType)  %>%
  map_dfr(wilcox_table, formula = BladverliesNetto ~ LeeftijdsklasseEur, .id = "selectie"),
dfTrees %>%
  split(.$SoortIndeling) %>%
  map_dfr(wilcox_table, formula = BladverliesNetto ~ LeeftijdsklasseEur, .id = "selectie")
) %>% left_join(dfVolgorde) %>% arrange(volgorde) %>% select(-volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_20_bladverlieswilcoxtabelnietgepaard.csv"))


#beschadigdde proefvlakken

dfTrees %>%
  group_by(PlotNr, Gemeente) %>%
  summarize(gem_bladverlies = mean(BladverliesNetto)) %>%
  filter(gem_bladverlies >= 25) %>%
  arrange(PlotNr) %>%
  write.csv2(file.path(outdir, "jaarlijks_21_beschadigdeproefvlakken.csv"))

#schade per proefvlak, enkel voor de hoofdboomsoort (minstens 5 exemplaren, en slechts 1 hoofdboomsoort per plot)
#VRAAG Geert: Wat te doen met proefvlakken met meerdere hoofdboomsoorten (er zijn plots waarbij de 2 meest-voorkomende soorten gelijk zijn)
spp <-
  dfTrees %>%
  group_by(PlotNr, SoortIndeling) %>%
  filter(!(SoortIndeling %in% c("overige nbs.", "overige lbs."))) %>%
  summarize(gem_bladverlies = mean(BladverliesNetto),
            Aantal_bomen_in_klasse = n()) %>%
  filter(Aantal_bomen_in_klasse >= 5) %>%
  arrange(PlotNr, desc(Aantal_bomen_in_klasse)) %>% #nodig omdat de grootste dan eerste staat en niet duplicated is
  #filter(!duplicated(PlotNr)) %>% #we zullen dit nu wel toelaten
  mutate(schadeklasse = cut(gem_bladverlies, include.lowest = TRUE,
                            breaks = c(0,10,25,40,100),
                            labels = c("0 - 10", "10+ - 25", "25+ - 40", "40+ - 100"))) %>%
  group_by(SoortIndeling, schadeklasse) %>%
  summarize(Aantal_plots = n()) %>%
  mutate(Pct_plots = Aantal_plots / sum(Aantal_plots) * 100,
         Cumulatief = cumsum(Pct_plots),
         Positie = Cumulatief - Pct_plots/2) %>%
  mutate(SoortIndeling2 = factor(SoortIndeling,
                                levels = c(dfVolgorde$selectie[dfVolgorde$selectie %in% unique(.$SoortIndeling)])))

(p <- ggplot(spp, aes(x = "", y = Pct_plots, fill = schadeklasse)) +
   facet_wrap(~SoortIndeling2) +
   geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE))  +
   geom_text(aes(y = Positie, label = paste0(round(Pct_plots), "%"))) +
   scale_fill_manual(values = c("blue", "green4", "orange", "red"), drop = FALSE) +
   coord_polar("y", start = 0, direction = 1)  + ylab("") + xlab("") +
   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.title = element_text("schade (%)")))

ggsave(file.path(outdir, "jaarlijks_03_gemiddeld_aantal_beschadigde_proefvlakken_voor_hoofdboomsoort.png"), p,  width = fig_width, height = fig_height, dpi = 2 * fig_dpi)


###1.10 Aandeel bomen Slijmuitvloei Soort

#Is weggevallen na 2006

###1.11 Aandeel bomen Exploitatieschade soort

#Is weggevallen na 2006

###1.12 Aandeel bomen waterscheuten soort

bomen_calc(dfTrees, group = normal_groups, group2 = "Waterscheuten") %>%
  left_join(dfVolgorde) %>%
  select(volgorde, selectie, PctBomen, Waterscheuten) %>%
  spread(key = Waterscheuten, value = PctBomen, fill = 0.0) %>%
  mutate(`totaal (1-3)` = `1`+ `2` + `3`) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_35_waterscheuten.csv"))

###1.13 Aandeel bomen vorstscheuren soort

#Is weggevallen na 2006

###1.14 Aandeel bomen schimmelaantasting soort

#Is weggevallen na 2006

###1.15 Aandeel bomen insectenklasse soort

#Is weggevallen na 2006

###1.16 Aandeel bomen koonsterfteklasse soort

#Is weggevallen na 2006

###1.17 Aandeel bomen verkleuringsklasse soort

#Is weggevallen na 2006

###1.18 Aandeel bomen zaadzettingsklasse soort

bomen_calc(dfTrees, group = normal_groups, group2 = "Zaadzetting") %>%
  left_join(dfVolgorde) %>%
  select(volgorde, selectie, PctBomen, Zaadzetting) %>%
  spread(key = Zaadzetting, value = PctBomen, fill = 0.0) %>%
  mutate(`totaal (1-3)` = `1`+ `2` + `3`,
         `matig tot sterk (2-3)` = `2` + `3`) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_34_zaadzetting.csv"))

###1.19 Gemiddeld bladverlies zaadzetting soort

bomen_calc(dfTrees, group = normal_groups, group2 = "Zaadzetting", respons = "BladverliesNetto")

###3.1 aantal bomen proefvlak soort / vervangende bomen / Gemiddeld bladverlies soort /
###Gemiddeld bladverlies Proefvlak / Gemiddeld bladverlies proefvlak soort /
###aantalbomen omtrekklasse EURBV soort / Aandele beschadigde bomen omtrekklasse soort / Aandeel beschadigde bomen proefvlak

#3.1a aantal bomen per proefvlak

bomen_calc(dfTrees, group = normal_groups, group2 = "PlotNr")

#3.1b Vervangende bomen
#??

#3.1cde gemiddeld bladverlies

bomen_calc(dfTrees, group = extra_groups, respons = "BladverliesNetto") %>%
  left_join(dfVolgorde) %>% arrange(volgorde, LeeftijdsklasseEur) %>%
  select(selectie, LeeftijdsklasseEur, mean_value, median_value, sd, se) %>%
  write.csv2(file.path(outdir, "jaarlijks_18_gemiddeldbladverlies.csv"))

#alternatieve manier (test)
# bomen_calc_new(dfTrees, grouplist = extra_groups, response = "BladverliesNetto") %>%
#   left_join(dfVolgorde) %>% arrange(volgorde, LeeftijdsklasseEur) %>%
#   select(selectie, Jaar, LeeftijdsklasseEur, mean_response, median_response, sd_response, se_response)

#3.1f aantal bomen per omtrekklasse

bomen_calc(dfTrees, normal_groups, group2 = "OmtrekklasseEur")

#3.1fbis aantal per bladverliesklasse per omtrekklasse

bomen_calc(dfTrees, normal_groups, group2 = c("OmtrekklasseEur", "BVKlasseEur"))

#3.1g beschadigd per omtrekklasse

bomen_calc(dfTrees, normal_groups, group2 = c("OmtrekklasseEur", "Beschadigd"))

#3.1h beschadigd per proefvlak

bomen_calc(dfTrees, normal_groups, group2 = c("PlotNr", "Beschadigd"))

###3.2 Gemiddelde omtrek per proefvlak en soort

bomen_calc(dfTrees, c("Jaar", "PlotNr", "SPEC_DES"), respons = "Omtrek")

###3.3 Omtrek laatste 5 jaar (??)

#Is hier een lopend gemiddelde bedoeld? Wat is de zin hiervan, want bomen groeien vrij gelijkmatig dacht ik?

#####################################################################################################


cat("ALL JAARLIJKS FINISHED\n")
