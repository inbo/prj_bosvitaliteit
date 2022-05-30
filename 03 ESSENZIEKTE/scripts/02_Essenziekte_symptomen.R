
### >>> Aantal bomen met symptomen en symptoomoorzaken

#doordat het percentage via tot_aantal_bomen_jr wordt berekend is er geen probleem met het weglaten van gezonde bomen (AantastingCode == "00")
#Update: symptomen gaan we niet meer gebruiken bij dode bomen, vandaar tot_aantal_bomen_jr_nodead

#opzoeken van onterechte NA of onterechte symptoomgraa,
#dfEsSymptoom %>% filter(SymptoomCode == "17", is.na(SymptoomGraad))

##Omdat symtomen het jaar van sterfte niet meer worden opgenomen, worden alle bomen weggelaten eens ze afgestorven zijn, dus het eerste jaar ze 100% bereiken in bladverlies vliegen ze er al uit

#Dataset zonder dode bomen
dfEsSymptoom <- dfEsSymptoomOrig %>% 
  left_join(unieke_bomen) %>% 
  filter(Jaar %in% es_jaren , Jaar < sterftejaar)

tot_aantal_bomen_jr_nodead <- dfEsMeting %>% group_by(Jaar) %>% summarize(tot_n_bomen = length(BoomID[Jaar < sterftejaar]))

#Overzicht alle bomen
symptomen_per_proefvlak <- dfEsSymptoomOrig %>% 
  group_by(Jaar, ProefvlakNummer) %>% 
  summarize(aantal_symptomen = length(SymptoomCode[!(SymptoomCode %in% c("00","99"))]),
            met_symptomen = n_distinct(BoomID[!(SymptoomCode %in% c("00","99"))]),
            dode_bomen = n_distinct(BoomID[SymptoomCode == "99"]),
            zonder_symptomen = n_distinct(BoomID[SymptoomCode == "00"])) %>%
  left_join(dfEsMeting %>% select(ProefvlakNummer, Jaar) %>% 
              group_by(ProefvlakNummer, Jaar) %>% 
              summarize(tot_aantal_bomen_jr = n())) %>%
  arrange(ProefvlakNummer, Jaar)

tmp <- 
  gather(symptomen_per_proefvlak %>% select(-aantal_symptomen, -tot_aantal_bomen_jr), 
         key = type, value = aantal, -Jaar, -ProefvlakNummer) %>% 
  mutate(type = factor(type, levels = c("dode_bomen", "met_symptomen", "zonder_symptomen")))
ggplot(tmp) + 
  geom_bar(aes(x = Jaar, y = aantal, fill = type),                        stat = "identity") + 
  facet_wrap(~ProefvlakNummer) + 
  scale_fill_manual(values = c(inbo_bruinrood, inbo_steun_blauw, inbo_groen) ) + 
  theme(legend.position = "right", axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename = paste0(out, "symptomen_bomen_proefvlak.png"), width = fig_width, height = fig_height)

  
(p <- dfEsSymptoom %>% group_by(SymptoomCode, SymptoomGraad, AantastingCode) %>% summarize(aantal = n()) %>% 
  ggplot(aes(x = SymptoomGraad, y = aantal, fill = AantastingCode)) + 
    scale_fill_manual(values = c(inbo_palette(), "#000000")) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~SymptoomCode, scales = "free_y", ncol = 3) + 
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90)) ) %>%
    ggsave(filename = paste0(out, "symptoomgraden_overzicht.png"), width = fig_width, height = fig_height)
  
symptoverzicht <- dfEsSymptoom %>%
  select(Jaar, AantastingCode, Aantasting, SymptoomCode, Symptoom, BoomID) %>%
  group_by(Jaar, AantastingCode, Aantasting, SymptoomCode, Symptoom) %>%
  summarize(aantal_bomen = n_distinct(BoomID)) %>%
  left_join(tot_aantal_bomen_jr) %>%
  select(Jaar, AantastingCode, Aantasting, SymptoomCode, Symptoom, aantal_bomen, tot_n_bomen) %>%
  mutate(percentage = aantal_bomen / tot_n_bomen * 100) %>%
  arrange(Aantasting, Jaar, SymptoomCode)

symptoverzicht %>% 
  select(Jaar, Aantasting, Symptoom, aantal_bomen) %>%
  spread(key = Jaar, value = aantal_bomen, fill = 0) %>%
  arrange(Aantasting, desc(.data[[as.character(es_lastyear)]])) %>%
  write_csv2(paste0(out, "symptomen_symptoomoverzichtaantal.csv"))

symptoverzicht %>% 
  select(Jaar, Aantasting, Symptoom, percentage) %>%
  spread(key = Jaar, value = round(percentage, 1), fill = 0) %>%
  arrange(Aantasting, desc(.data[[as.character(es_lastyear)]])) %>%
  write_csv2(paste0(out, "symptomen_symptoomoverzichtpercentage.csv"))

symptoorzaak <- 
  dfEsSymptoomOorz %>%
  group_by(Jaar, OorzaakType, OorzaakTypeCode) %>%
  summarize(aantal_bomen = n_distinct(BoomID)) %>%
  left_join(tot_aantal_bomen_jr, by = "Jaar") %>%
  mutate(percentage = aantal_bomen / tot_n_bomen * 100) %>% 
  arrange(Jaar, desc(aantal_bomen), OorzaakTypeCode)

symptoorzaak %>% 
  select(Jaar, OorzaakType, aantal_bomen) %>% 
  spread(key = Jaar, value = aantal_bomen, fill = 0) %>% 
  arrange(OorzaakType) %>% 
  write_csv2(path = paste0(out, "symptomen_symtoomoorzaakaantal.csv"))

symptoorzaak %>% 
  select(Jaar, OorzaakType, percentage) %>% 
  spread(key = Jaar, value = round(percentage, 1), fill = 0) %>% 
  arrange(OorzaakType) %>% 
  write_csv2(path = paste0(out, "symptomen_symtoomoorzaakpercentage.csv"))

### >>> Analyse per symptoom (te bespreken met Geert)

#> Verkleuring
verkleuring <- dfEsSymptoom %>% 
  filter(SymptoomCode%in% c("02", "03", "04", "05")) %>%
  mutate(SymptoomCode = factor(SymptoomCode, levels = c("02", "03", "04", "05"))) %>%
  select(ProefvlakNummer, BoomID, Jaar, SymptoomGraad, SymptoomCode, Aantasting) %>% 
  spread(key = SymptoomCode, value = SymptoomGraad, fill = 0, drop = FALSE) %>%
  mutate(Verkleuring = pmax(`02`, `03`, `03`, `04`, `05`, na.rm = TRUE)) %>%
  filter(Verkleuring > 0) %>%
  group_by(Aantasting, Jaar, Verkleuring) %>%
  summarize(aantal_bomen = length(Verkleuring)) %>% 
  left_join(tot_aantal_bomen_jr) %>%
  mutate(percentage = aantal_bomen / tot_n_bomen * 100)

ggplot(verkleuring, aes(x = Jaar, y = percentage, fill = Verkleuring)) + 
  geom_bar(stat = "identity", position = position_stack(reverse =  TRUE)) +
  geom_text(data = tot_aantal_bomen_jr, aes(x = Jaar, y = 1, label = tot_n_bomen), inherit.aes = FALSE) 
ggsave(filename = paste0(out, "symptoomgradenverkleuring_overzicht.png"), width = fig_width, height = fig_height)


(verkleuringsummary <- verkleuring %>%
  group_by(Jaar, Aantasting) %>%
  summarise(aantal_bomen = sum(aantal_bomen),
            percentage = aantal_bomen / mean(tot_n_bomen) * 100) %>%
  arrange(Aantasting, Jaar)) %>% 
  write_csv2(path = paste0(out, "symptomen_verkleuring.csv"))

(p <- ggplot(filter(verkleuringsummary, substring(Aantasting, 1, 8) == "bladeren"), 
       aes(x = Jaar, y = percentage)) + 
  geom_line() + geom_point() + 
  ylab("Percentage bomen met verkleuring op bladeren")) %>%
  ggsave(filename = paste0(out, "symptomen_verkleuring.png"),
         height = fig_height, width = fig_width)

#> Takbreuk (13) Hoe omgaan met vers en oud??? <<<NOG BEKIJKEN>>>

takbreuk <- dfEsSymptoom %>% 
  filter(SymptoomCode == "13",
         LeeftijdKlasse != "Oud") %>% 
  group_by(Jaar, Aantasting, AantastingCode, SymptoomGraad, LeeftijdKlasse) %>% 
  summarize(aantal_bomen = n_distinct(BoomID)) %>% 
  mutate(Aantasting = ifelse(AantastingCode == "22", 
                             "twijgen", 
                             ifelse(AantastingCode == "23", 
                                    "takken", 
                                    ifelse(AantastingCode == "24", "zware takken", "andere"))),
         Aantasting = factor(Aantasting, levels = c("twijgen", "takken", "zware takken", "andere"))) %>%
  left_join(tot_aantal_bomen_jr) %>%
  mutate(percentage = 100 * aantal_bomen/tot_n_bomen)
  #summarize(aantal_bomen = sum(aantal_bomen)) #drop leeftijdsklasse


takbreuk_tabel <- takbreuk %>% 
  ungroup() %>% 
  group_by(Jaar, Aantasting, LeeftijdKlasse) %>% 
  summarize(percentage = 100 * aantal_bomen / tot_n_bomen) %>% 
  group_by(Jaar, Aantasting) %>% 
  summarize(percentage = sum(percentage)) %>% 
  spread(key = Aantasting, value = percentage, fill = 0)
write_csv2(takbreuk_tabel, path = paste0(out, "symptomen_takbreuk_zonder_oud.csv"))


ggplot(takbreuk, aes(x = Jaar, y = percentage, fill = SymptoomGraad)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  ylab("percentage bomen met takbreuk") + 
  facet_grid(LeeftijdKlasse~Aantasting)
ggsave(filename = paste0(out, "symptomen_takbreuk.png"), height = fig_height, width = fig_width)

ggplot(takbreuk_tabel %>% gather(key = Aantasting, value = percentage, -Jaar), 
       aes(x=Jaar, y=percentage, color=Aantasting)) + 
  geom_line() + 
  ylab("percentage bomen met (deels) verse takbreuk")
ggsave(filename = paste0(out, "trend_takbreuk.png"), height = fig_height, width = fig_width)



#> Bladvraat (01)

(bladvraat <- dfEsSymptoom %>% 
  filter(SymptoomCode == "01") %>% 
  group_by(Jaar, SymptoomGraad) %>%
  summarize(aantal_bomen = n_distinct(BoomID)) %>% 
  left_join(tot_aantal_bomen_jr) %>% 
  mutate(percentage = 100 * aantal_bomen / tot_n_bomen)) %>%
write_csv2(path = paste0(out, "symptomen_bladvraat.csv"))


ggplot(bladvraat, aes(x = Jaar, y = percentage, fill = SymptoomGraad)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  ylab("percentage bomen met bladvraat")
ggsave(filename = paste0(out, "symptomen_bladvraat.png"), height = fig_height, width = fig_width)


#> Vervorming (08)
vervorming <- dfEsSymptoom %>% 
  filter(SymptoomCode == "08") %>% 
  group_by(Jaar, SymptoomGraad, AantastingCode, LeeftijdKlasse) %>%
  summarize(aantal_bomen = n_distinct(BoomID)) %>%
  left_join(tot_aantal_bomen_jr) %>% 
  mutate(percentage = 100 * aantal_bomen / tot_n_bomen)

ggplot(vervorming, aes(x = Jaar, y = percentage, fill = SymptoomGraad)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  ylab("percentage bomen met vervorming") + 
  facet_grid(AantastingCode ~ LeeftijdKlasse)
ggsave(filename = paste0(out, "symptomen_vervorming.png"), height = fig_height, width = fig_width)


#> Overzicht symptomen

#Algemene dataset met alle combinaties van boom, jaar en aantasting

#dataset met alle mogelijke combinaties van boom, jaar en aantasting
aantastingen <- distinct(dfEsSymptoom[c("AantastingCode", "Aantasting")])
boomids_aantasting <- crossing(alle_boomids_jr, aantastingen)
aantasting_jaar <- crossing(aantastingen, data.frame(Jaar = es_jaren))

for (symptcode in c("08", "10", "11", "12", "13", "14", "16", "17", "19", "20", "21")) {
 
  #filter de symptoomcode en steek in tmp alle bomen met de corresponderende symptoomcode
  tmp <- dfEsSymptoom %>% 
    filter(SymptoomCode == symptcode) %>%
    mutate(SymptoomAanw = TRUE)
  #indien de symptoomgraadcode altijd NA is, is dit een symptoom zonder gradatie, enkel 0, 1, 
  #en een NA betekent dan dat het symptoom aanwezig is
  #de aanwezigheid van de regel in de db wijst op aanwezigheid, maar de graad is NA omdat die niet ingeschat wordt
  if (all(is.na(tmp$SymptoomGraadCode))) {
    tmp$SymptoomGraadCode <- "100"
  }
  symptoomnaam <- unique(tmp$Symptoom); print(symptoomnaam)
  
  #maak de bestandsnamen voor tabellen en figuren aan
  pngname1 <- paste0("symptomen_graad_", make.names(symptoomnaam), ".png")
  pngname2 <- paste0("symptomen_symptoomtype_", make.names(symptoomnaam), ".png")
  txtname  <- paste0("symptomen_graad_", make.names(symptoomnaam), ".csv")
  
  #maak een dataset met alle bomen, jaren en aantastingen met een waarde als het symptoom er is, en NA indien niet
  evolutie <- tmp %>%
    right_join(boomids_aantasting, by = c("Jaar", "ProefvlakNummer", "BoomID", "AantastingCode", "Aantasting")) %>%
    #alle NA zijn lege rijen, dus combinaties waar het symptoom zich niet voordoet --> zet dit op 0 --> 0%
    mutate(SymptoomGraadCode = replace(SymptoomGraadCode, is.na(SymptoomAanw), 0),
           SymptoomGraad = replace(SymptoomGraad, 
                                   is.na(SymptoomAanw), 
                                   filter(symptoomgraden, Code == 0) %>% pull(Beschrijving))) %>%
    #groepeer en summarise per aantastingscode het aanstal unieke bomen
    group_by(Jaar, AantastingCode, Aantasting, SymptoomGraadCode, SymptoomGraad, SymptoomCode) %>%
    summarize(aantal_bomen = n_distinct(BoomID)) %>% 
    #hang de totaal aantal bomen per jaar eraan
    left_join(tot_aantal_bomen_jr, by = "Jaar") %>%
    #bereken statistieken + zet op beschadigd van wanneer een symptoom > 0% is
    mutate(percentage = aantal_bomen / tot_n_bomen * 100, 
           SymptoomGraad = factor(SymptoomGraad, levels = symptoomgraden$Beschrijving),
           beschadigd = ifelse(SymptoomGraadCode > 0, TRUE, FALSE))
  
  #kijk of meer dan all_zero_tolerance % van de symptoomgraden 0 i --> indien zo, maak geen plot
  checkdiversity <- evolutie %>% group_by(AantastingCode) %>% 
    do({
      zero_damage <- filter(.data, SymptoomGraadCode == 0)
      if (all(zero_damage$percentage > all_zero_tolerance)) {
        data.frame(keep = FALSE)
      } else {
        data.frame(keep = TRUE)
      }
  })
  
  #maak de figuur indien niet teveel 0-waarden
  if (any(checkdiversity$keep == TRUE)) {
    (p <- ggplot(evolutie %>% 
                   left_join(checkdiversity, by = "AantastingCode") %>% 
                   filter(keep == TRUE), 
                 aes(x = Jaar, y = aantal_bomen, fill = SymptoomGraad)) + 
       geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
       ylab(paste("symptoomgraad", symptoomnaam)) +
       facet_wrap(~Aantasting)) %>% 
      ggsave(filename = paste0(out, pngname1), width = fig_width, height = fig_height)    
  }

  #Bereken hoeveel % van de bomen dit symptoom hebben voor iedere aantasting
  beschadigd <- 
    evolutie %>% 
    filter(beschadigd == TRUE) %>%
    group_by(Aantasting, Jaar) %>% 
    summarize(pct_beschadigd = sum(percentage, na.rm = TRUE)) %>% 
    right_join(aantasting_jaar) %>% 
    mutate(pct_beschadigd = replace(pct_beschadigd, is.na(pct_beschadigd), 0)) %>%
    filter(!is.na(Aantasting))
  nonallzeros <- beschadigd %>% group_by(Aantasting) %>% summarize(som = sum(pct_beschadigd)) %>% filter(som > 0)
  beschadigd <- filter(beschadigd, 
                       Aantasting %in% (nonallzeros %>% pull(Aantasting)))
  
  #Maak een lijnplot die voor iedere aantasting de evolutie per jaar toont
  (p <- ggplot(beschadigd, 
               aes(x = Jaar, y = pct_beschadigd, color = Aantasting)) + 
      #geom_point() + 
      geom_line() + ylab(paste0("percentage bomen met ", symptoomnaam)) + 
      scale_color_manual(values = c(inbo_palette(), "#000000"))) %>%
    ggsave(filename = paste0(out, pngname2), height = fig_height, width = fig_width)
  
  evolutietabel <- evolutie %>% 
    select(Jaar, Aantasting, SymptoomGraad, percentage) %>% 
    spread(key = SymptoomGraad, value = percentage, fill = 0)
  evolutietabel$Beschadigd <- rowSums(evolutietabel[6:ncol(evolutietabel)])

    write_csv2(evolutietabel, path = txtname)
}


### Oorzaken (oorzaak 304,308,545,431)

oorzsel <- dfEsSymptoomOorz %>%
  group_by(OorzaakType, OorzaakTypeCode) %>% 
  summarise(aantal = n()) %>% 
  filter (aantal > es_min_oorz & OorzaakTypeCode != "999")


oorztab <- filter(dfEsSymptoomOorz, OorzaakTypeCode %in% oorzsel$OorzaakTypeCode) %>% 
  group_by(Jaar, OorzaakType, OorzaakTypeCode, BoomID) %>% 
  summarise(aantal = n()) %>% 
  summarize(aantal_bomen = sum(aantal >= 1)) %>% 
  left_join(tot_aantal_bomen_jr, by = "Jaar") %>% 
  mutate(percentage = aantal_bomen / tot_n_bomen * 100)

oorztab %>% 
  select(-aantal_bomen, -tot_n_bomen) %>%
  spread(key = Jaar, value = percentage, fill = 0) %>%
  write_csv2(path = paste0(out, "symptomen_overzicht_oorzaken.csv"))
  
(p <- ggplot(oorztab, aes(x = Jaar, y = percentage, color = OorzaakTypeCode)) + 
  geom_line() + ylab("percentage bomen met oorzaak")) %>% 
  ggsave(filename = paste0(out, "symptomen_overzicht_oorzaken.png"),
         width = fig_width, height = fig_height)

# code 04 aangetast deel --> oorzaak onderzoeken

(oorz_dood <- dfEsSymptoom %>% 
  left_join(dfEsSymptoomOorz, by = c("Jaar", "BoomID", "MetingID", "ProefvlakNummer")) %>%
  filter( AantastingCode == "04") %>% 
  select(Jaar, ProefvlakNummer, BoomID, OorzaakType, SymptOpm) %>%
  filter(!duplicated(BoomID, ProefvlakNummer)) %>%
  arrange(ProefvlakNummer, BoomID, Jaar)) %>%
  write_csv2(path = paste0(out, "symptomenoorzaak_dood_overzicht.csv"))

oorz_dood %>% 
  group_by(OorzaakType) %>%
  summarize(aantal = n()) %>%
  write_csv2(path = paste0(out, "symptomenoorzaak_dood_kort.csv"))

for (var in c("Taksterfte", "Necrose", "Waterscheuten", "Kroonverbossing", "Zaadzetting",
              "EssenMijt")) {
  print(var)
  code <- paste0(var, "Code")
  codes <- dfEsMeting %>% 
    select(.data[[var]], .data[[code]]) %>%
    mutate(code = as.numeric(.data[[var]])) %>%
      filter(!duplicated(.)) %>%
      arrange(.data[[code]])
  
  tmpsymp <- dfEsMeting %>% 
    mutate(sympvar = factor(.data[[var]], levels =  codes[[var]])) %>%
    group_by(Jaar, sympvar) %>%
    summarize(aantal_bomen = n()) %>%
    left_join(tot_aantal_bomen_jr, by = "Jaar") %>%
    mutate(pct = aantal_bomen / tot_n_bomen * 100) %>%
    select(Jaar, sympvar, pct)
  
  tmpsymp %>% spread(key = sympvar, value = pct, fill = 0) %>%
    write_csv2(path = paste0(out, paste0("symptomen_", var, ".csv")))  

  (p <- ggplot(filter(tmpsymp, !is.na(sympvar)), aes_string(x = "Jaar", y = "pct", fill = "sympvar")) + 
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
      scale_fill_discrete(name = var)) %>%
    ggsave(filename = paste0(out, paste0("symptomen_", var, ".png")),
           width = fig_width, height = fig_height)
  print(p)
}


# #> Hars- en slijmuitvloei  symptoom 19
# 
# harsslijm <- dfEsSymptoom %>% 
#   filter(SymptoomCode%in% c("19")) %>%
#   select(ProefvlakNummer, BoomID, Jaar, SymptoomGraad, SymptoomCode, Aantasting) %>% 
#   mutate(SymptoomGraad = replace(SymptoomGraad, SymptoomGraad == "1-10%", "01-10%"),
#          SymptoomGraad = replace(SymptoomGraad, SymptoomGraad == "100%", "volledig")) %>%
#   spread(key = SymptoomCode, value = SymptoomGraad, fill = 0, drop = FALSE) %>%
#   rename(harsslijm = `19`) %>%
#   ungroup() %>%
#   group_by(Aantasting, Jaar, harsslijm, BoomID) %>%
#   summarize(aantal_records = n()) %>% 
#   summarize(aantal_bomen = n()) %>%
#   left_join(tot_aantal_bomen_jr, by = "Jaar") %>%
#   mutate(percentage = aantal_bomen / tot_n_bomen * 100)
# 
# (harsslijmsummary <- harsslijm %>%
#     group_by(Jaar, Aantasting) %>%
#     summarise(aantal_bomen = sum(aantal_bomen),
#               percentage = aantal_bomen / mean(tot_n_bomen) * 100) %>%
#     arrange(Aantasting, Jaar)) %>% 
#   write_csv2(path = paste0(out, "symptomen_harsslijm.csv"))
# 
# (p <- ggplot(filter(harsslijmsummary), 
#              aes(x = Jaar, y = percentage, color = Aantasting)) + 
#     geom_line() + geom_point() + 
#     ylab("Percentage bomen met harsslijm")) %>%
#   ggsave(filename = paste0(out, "symptomen_harsslijm.png"),
#          height = fig_height, width = fig_width)
# 
# 
# #> Rot symptoom 20



