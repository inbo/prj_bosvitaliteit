
### >>> Afgestorven bomen

dfEsMeting %>% 
  filter(Bladverlies == 100) %>% 
  select(ProefvlakNummer, BoomID, Jaar) %>%
  group_by(ProefvlakNummer, BoomID) %>%
  summarize(Sterftejaar = min(Jaar)) %>%
  arrange(ProefvlakNummer, Sterftejaar) %>%
  write_csv2(path = paste0(out, "jaarlijks_afgestorvenbomen.csv"))

### >>> Verdwenen bomen

dfEsMeting %>% 
  group_by(Jaar, ProefvlakNummer, BoomID) %>% 
  summarize(aantal = n()) %>% 
  spread(key = Jaar, value = aantal, fill = 0) %>%
  left_join(dfEsMeting %>% 
              group_by(ProefvlakNummer, BoomID) %>%
              summarize(jaren_aanwezig = n()) %>% 
              mutate(probleem = jaren_aanwezig < max(jaren_aanwezig)), 
            by = c("ProefvlakNummer", "BoomID")) %>%
  filter(probleem == TRUE) %>% 
  arrange(ProefvlakNummer, BoomID) %>% 
  write_csv2((path = paste0(out, "Niet_volledige_bomen.csv")))



### >>> Leeftijdsverdeling

dfEsMeting %>% 
  filter(Jaar == es_lastyear) %>%
  group_by(Jaar, Leeftijd) %>% 
  summarise(aantal = n()) %>%
  mutate(percentage = aantal / sum(aantal) * 100) %>%
  write_csv2(path = paste0(out, "jaarlijks_leeftijdsverdeling.csv"))

### >>> Omtrek Per Plot
dfEsMeting %>% 
  filter(Jaar == es_lastyear) %>% 
  group_by(ProefvlakID, Drainagegroepering, 
           Waterbeschikbaarheid, `Textuur-gegroepeerd`) %>% 
  summarize(aantal_bomen = n(), 
            gem_omtrek = mean(omtrek_start),
            se_gem_omtrek = sd(omtrek_start)/sqrt(n())) %>% 
  write_csv2(path = paste0(out, "jaarlijks_proefvlakeigenschappen.csv"))


### >>> Bladverliesklasses en beschadigd

#> Algemeen

dfBladverliesklasse <- 
  dfEsMeting %>% 
  group_by(Jaar, BladverliesklasseEur) %>% 
  summarise(aantal = n()) %>%
  mutate(percentage = aantal / sum(aantal) * 100) 

tab <- 
  dfBladverliesklasse %>% 
  select(-aantal) %>% 
  spread(key = Jaar, value = percentage, fill = 0)
tabb <- 
  filter(tab, BladverliesklasseEur %in% c("(25,60]", "(60,99]", "(99,100]")) %>%
  summarise_at(.vars = -1, .funs = sum) %>%
  mutate(BladverliesklasseEur = "beschadigd")

bind_rows(tab, tabb) %>% 
  write_csv2(path = paste0(out, "jaarlijks_bladverliesklasse.csv"))

(p <- ggplot(dfBladverliesklasse, 
             aes(x = Jaar, y = percentage, 
                 fill = fct_rev(BladverliesklasseEur))) + 
  geom_bar(stat = "identity") + ylab("percentage bomen") + 
  scale_x_continuous(breaks = es_jaren, labels = es_jaren) + 
  scale_fill_manual(name = "Bladverlies", values = colorscale_es5)) %>%
ggsave(filename = paste0(out, "jaarlijks_bladverliesklassen.png"), 
       height = fig_height, width = fig_width)
  
#> Beschadigd per leeftijdsklasse

dfBeschadigd <- 
  bind_rows(
  dfEsMeting %>% 
    mutate(Leeftijd = ifelse(is.na(Leeftijd), "NA in DB", Leeftijd)) %>%
    group_by(Jaar, Leeftijd) %>%
    summarise(pct_beschadigd = 100 * mean(Beschadigd == "beschadigd")), 
  dfEsMeting %>% 
    mutate(Leeftijd = "alles") %>%
    group_by(Jaar, Leeftijd) %>%
    summarise(pct_beschadigd = 100 * mean(Beschadigd == "beschadigd"))) 

(dfBeschadigdSpread <-  
  dfBeschadigd %>% 
  mutate(Leeftijd = factor(Leeftijd, levels = leeftijden_es)) %>%
spread(key = Leeftijd, value = pct_beschadigd) ) %>% 
  write_csv2(path = paste0(out, "jaarlijks_beschadigdebomenleeftijd.csv"))

(p <- ggplot(dfBeschadigd, 
             aes(x = Jaar, y = pct_beschadigd, color = Leeftijd)) + 
  geom_point() + geom_path() + ylab("beschadigde bomen (%)")) %>%
  ggsave(filename = paste0(out, "jaarlijks_beschadigdebomenleeftijd.png"),
         height = fig_height, width = fig_width)


### >>> Samenvattende statistieken

(es_gem <- 
  dfEsMeting %>% 
  group_by(Jaar) %>%
  summarize(gem_BV = mean(Bladverlies), 
            med_BV = median(Bladverlies),
            sd_BV = sd(Bladverlies),
            serr_BV = sd(Bladverlies) / sqrt(n()),
            lcl_BV = gem_BV - 2 * serr_BV,
            ucl_BV = gem_BV + 2 * serr_BV, 
            Leeftijd = "alles")) %>% 
  write_csv2(path = paste0(out, "jaarlijks_gemiddeldbladverlies.csv"))


(es_gem_lft <- 
  dfEsMeting %>% 
  group_by(Jaar, Leeftijd) %>%
  summarize(gem_BV = mean(Bladverlies), 
            med_BV = median(Bladverlies),
            sd_BV = sd(Bladverlies),
            serr_BV = sd(Bladverlies) / sqrt(n()),
            lcl_BV = gem_BV - 2 * serr_BV,
            ucl_BV = gem_BV + 2 * serr_BV) %>%
  mutate(Leeftijd = ifelse(is.na(Leeftijd), "NA in DB", Leeftijd))) %>%
  write_csv2(path = paste0(out, "jaarlijks_gemiddeldbladverlieslft.csv"))


(p <- ggplot(es_gem_lft , aes(x = Jaar, y = gem_BV, color = Leeftijd)) + 
  geom_path() + geom_point(position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes( ymin = lcl_BV, ymax = ucl_BV), width = 0.2, position = position_dodge(width = 0.2)) + 
  geom_path(data = es_gem, size = 1) + geom_point(data = es_gem, size = 1) + 
  geom_errorbar(data = es_gem, aes(ymin = lcl_BV, ymax = ucl_BV), width = 0.2, linewidth = 1) + 
  ylab("gemiddeld bladverlies (%)")) %>%
  ggsave(filename = paste0(out, "jaarlijks_gemiddelbladverlies.png"), 
         height = fig_height, width = fig_width)
  
### >>> Bladverlies per proefvlak

dfEsBV <- 
  dfEsMeting %>% 
  group_by(Jaar, ProefvlakNummer) %>% 
  summarise(aantal_bomen = n(),
            gem_BV = mean(Bladverlies)) %>%
  mutate(Beschadigd = gem_BV > 25)

dfEsBV %>% 
  select(Jaar, gem_BV, ProefvlakNummer) %>%
  spread(key = Jaar, value = gem_BV, fill = 0) %>%
  arrange(ProefvlakNummer) %>%
  write_csv2(path = paste0(out, "jaarlijks_beschadigdeproefvlakken.csv"))

(p <- ggplot(dfEsBV, 
             aes(x = factor(Jaar), 
                 y = gem_BV, 
                 group = factor(ProefvlakNummer), 
                         color = Beschadigd, shape = Beschadigd)) + 
  geom_point() + geom_path(aes(group = ProefvlakNummer)) + 
    scale_color_discrete("") + 
  xlab("Jaar") + ylab("gemiddeld bladverlies (%)")) %>%
  ggsave(filename = paste0(out, "jaarlijks_gemiddeldbladverlies.png"), 
         height = fig_height, width = fig_width)


dfEsBesch <- 
  dfEsBV %>% 
  mutate(
    schadeklasse = cut(gem_BV, c(0,10,25,40,100), include.lowest = TRUE)) %>% 
  group_by(Jaar, schadeklasse) %>% 
  summarize(aantal = n()) %>%
  mutate(percentage = aantal / sum(aantal) * 100)

(p <- ggplot(dfEsBesch, aes(x = "", y = percentage, fill = schadeklasse)) + 
  geom_bar(stat = "identity", 
           position = position_stack(reverse = TRUE), 
           width = 1) + 
  facet_wrap(~Jaar)  + 
  scale_fill_manual(values = c("blue", "green4", "orange", "red"), 
                    drop = FALSE) + 
  coord_polar("y", start = 0, direction = 1)  + xlab("") + ylab("") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        legend.title = element_text("schade (%)"))) %>%
  ggsave(filename = paste0(out, "jaarlijks_aantalplotsinschadeklasse.png"),
         height = fig_height, width = fig_width)


  
  