library(tidyverse)
library(INBOtheme)
library(readxl)
library(DBI)
library(odbc)
library(lme4)
options(dplyr.summarise.inform = FALSE) #geen messages als je .groups niet gebruikt
alleen_levende_bomen <- FALSE
if (alleen_levende_bomen) {
  out <- "outputEs_levend/lv_"
} else {
  out <- "outputEs_allebm/all_"  
}


con <- DBI::dbConnect(odbc::odbc(),
                 driver = "SQL Server",
                 server = "inbo-sql07-prd.inbo.be",
                 database = "D0027_00_Essen")
con
DBI::dbListTables(con, table_type = "TABLE", schema_name = "dbo")

sqlcodemeting <- "
select
  WaarnemingId = wn.Id
, wn.ProefvlakID
, pv.ProefvlakNummer
, wn.Jaar
, wn.Datum
, MetingID = m.Id
, m.BoomID
, m.Omtrek
, OmtrekklasseCode = ok.Code
, Omtrekklasse = ok.Beschrijving
, OmtrekklasseSort = ok.SortOrder
, m.Bladverlies
, TaksterfteCode = tst.Code
, Taksterfte = tst.Beschrijving
, NecroseCode = nt.Code
, Necrose = nt.Beschrijving
, WaterscheutenCode = wst.Code
, Waterscheuten = wst.Beschrijving
, KroonverbossingCode = kvt.Code
, Kroonverbossing = kvt.Beschrijving
, ZaadzettingCode = zzt.Code
, Zaadzetting = zzt.Beschrijving
, ZaadleeftijdCode = zlk.Code
, Zaadleeftijd = zlk.Beschrijving
, EssenMijtCode = ebm.Code
, EssenMijt = ebm.Beschrijving
, LeeftijdID = pv.Id
, Leeftijd = lft.Beschrijving
, BoomNr = b.Nummer
, GS = b.GemeenschappelijkeSteekproef
, b.VerwijderingJaar
, VerwijderingstypeCode = vwt.Code
, Verwijderingstype = vwt.Beschrijving
, b.Afwijkend
, OpmWN = wn.Opmerking
, OpmMt = m.Opmerking
, OpmBm = b.Opmerking
from Waarneming wn
left join Meting m on m.WaarnemingId = wn.Id
left join Boom b on m.BoomId = b.Id and wn.ProefvlakId = b.ProefvlakId
left join OmtrekKlasse ok on ok.Id = m.OmtrekKlasseId
left join TakSterfteType tst on tst.Id = m.TakSterfteTypeId
left join NecroseType nt on nt.Id = m.NecroseTypeId
left join WaterScheutenType wst on wst.Id = m.WaterscheutenTypeId
left join KroonVerbossingType kvt on kvt.Id = m.KroonVerbossingTypeId
left join ZaadzettingType zzt on zzt.Id = m.ZaadzettingTypeId
left join LeeftijdKlasse zlk on zlk.ID = m.LeeftijdKlasseId 
left join Essenbloesemmijt ebm on ebm.Id = m.EssenbloesemmijtId
left join VerwijderingType vwt on vwt.Id = b.VerwijderingTypeId
left join Proefvlak pv on pv.Id = wn.ProefvlakID
left join LeeftijdType lft on lft.Id = pv.LeeftijdTypeId
left join WaterBeschikbaarheidType wbt on wbt.Id = pv.WaterBeschikbaarheidTypeId
where b.GemeenschappelijkeSteekproef = 1"

sqlcodesymptoom <- "
select
  MetingSymptoomID = ms.Id
, MetingID
, BoomID = b.ID
, ProefvlakNummer = pv.ProefvlakNummer
, AantastingCode = atp.Code
, Aantasting = atp.BeschrijvingNL
, SymptoomCode = st.Code
, Symptoom = st.BeschrijvingNL
, SymptoomSpecCode = sst.Code
, SymptoomSpec = sst.BeschrijvingNL
, SymptoomGraadCode = sg.Code
, SymptoomGraad = sg.Beschrijving
, LeeftijdKlasseCode = lk.Code
, LeeftijdKlasse = lk.Beschrijving
, SymptOpm = ms.Opmerking
from MetingSymptoom ms
left join Meting met on met.ID = ms.metingID
left join Waarneming wrn on wrn.ID = met.WaarnemingId
left join Proefvlak pv on pv.ID = wrn.ProefvlakID
left join Boom b on b.ID = met.BoomID
left join AantastingType atp on atp.Id = ms.AantastingTypeId
left join SymptoomType st on st.Id = ms.SymptoomTypeId
left join SymptoomSpecificatieType sst on sst.Id = ms.SymptoomSpecificatieTypeId
left join SymptoomGraad sg on sg.Id = ms.SymptoomGraadId
left join LeeftijdKlasse lk on lk.Id = ms.LeeftijdKlasseId
where b.GemeenschappelijkeSteekproef = 1"

sqlcodeorganisme <- "
select
  mso.MetingSymptoomID
, OorzaakTypeCode = ot.Code
, OorzaakType = ot.BeschrijvingNL
, OorzaakOrganismeCode = oot.Code
, OorzaakOrganisme = oot.Beschrijving
from MetingSymptoomOorzaak mso
left join OorzaakType ot on ot.Id = mso.OorzaakTypeId
left join OorzaakOrganismeType oot on oot.Id = mso.OorzaakOrganismeTypeId"




colorscale_es5 <- c("red", "orange", "gold", "green4", "blue")
leeftijden_es <- c(">= 20 en <= 59", ">= 60", "Gemengd", "Onbekend", "NA in DB", "alles")

##############################################################################################

#Lees de data in op boomniveau per jaar
dfEsMetingAlle <-
  DBI::dbGetQuery(con, sqlcodemeting) %>%
  filter(GS == TRUE) %>% #niet meer nodig, is al in query verwerkt
  mutate(JaarC = Jaar - 2014,
         BladverliesklasseEur = cut(Bladverlies, c(0, 10, 25, 60, 99, 100), include.lowest = TRUE),
         Bladverliesklasse10 = cut(Bladverlies, 0:10*10, include.lowest = TRUE),
         Beschadigd = cut(Bladverlies, c(0, 25, 100), include.lowest = TRUE, label = c("ok", "beschadigd")))

#  --> vraagje voor Geert: bomen die het laatste jaar sterven, moeten die er ook uit, of worden daar wel symptomen voor bepaald?

### 

overlevende_bomen <- dfEsMetingAlle %>% 
  group_by(BoomID) %>% 
  summarize(MaxSchade = max(Bladverlies)) %>% 
  filter(MaxSchade < 100) %>% 
  pull(BoomID)

###

dfEsMetingLevend <- dfEsMetingAlle %>% filter(BoomID %in% overlevende_bomen)
if(alleen_levende_bomen) {
  dfEsMeting <- dfEsMetingLevend
} else {
  dfEsMeting <- dfEsMetingAlle
}

dfProefvlakEig <- 
  read_excel("data/PROEFVLAKKEN ES en TEXTUUR DRAINAGE.xlsx") %>% 
  rename( "Drainagegroepering" = `Drainage-gegroepeerd`) %>%
  mutate(Drainagegroepering = replace(Drainagegroepering, Nummer == "1121", "abcd"),
         Drainagegroepering = replace(Drainagegroepering, Nummer == "413019", "abcd"),
         Drainagegroepering = replace(Drainagegroepering, Nummer == "767", "efghi"),
         Drainagegroepering = replace(Drainagegroepering, Nummer == "302", "efghi"))

all(unique(dfEsMeting$ProefvlakNummer) %in% dfProefvlakEig$Nummer)

dfEsMetingAlle <- left_join(dfEsMetingAlle, dfProefvlakEig, by = c("ProefvlakNummer" = "Nummer"))
dfEsMetingLevend <- left_join(dfEsMetingLevend, dfProefvlakEig, by = c("ProefvlakNummer" = "Nummer"))
if(alleen_levende_bomen) {
  dfEsMeting <- dfEsMetingLevend
} else {
  dfEsMeting <- dfEsMetingAlle
}


#Lees de symptomendata in. Voeg kerngezonde bomen toe als symptoom "00" als ze niet in DB staan
symptomen_zonder_graad <- c("10", "11", "12", "21", "22")
symptomen_voorwaardelijk_graad <- "13"
symptomen_voorwaardelijk_aantasting <- c("31", "32", "33", "34")

dfEsSymptoomOrigAlle <- 
  DBI::dbGetQuery(con, sqlcodesymptoom) %>% 
  right_join(dfEsMetingAlle %>% 
               select(ProefvlakID, ProefvlakNummer, Jaar, BoomID, MetingID, GS), 
             by = c("MetingID", "BoomID", "ProefvlakNummer")) %>%
  mutate(AantastingCode = replace(AantastingCode, is.na(MetingSymptoomID), "00"),
         Aantasting = replace(Aantasting, is.na(MetingSymptoomID), "No symptoms on any part of tree"),
         SymptoomCode = replace(SymptoomCode, AantastingCode == "00", "00"),
         Symptoom = replace(Symptoom, AantastingCode == "00", "no symptoms"),
         SymptoomGraadCode = replace(SymptoomGraadCode, AantastingCode == "00", "0"),
         SymptoomGraad = replace(SymptoomGraad, AantastingCode == "00", "0%"),
         SymptoomCode = replace(SymptoomCode, AantastingCode == "04", "99"),
         SymptoomGraadCode = replace(SymptoomGraadCode, AantastingCode == "04", "7"),
         SymptoomGraad = replace(SymptoomGraad, AantastingCode == "04", "100%"),
         Symptoom = replace(Symptoom, AantastingCode == "04", "Dead Tree")) %>% 
  mutate(SymptoomGraad = ifelse(SymptoomCode %in% symptomen_zonder_graad & is.na(SymptoomGraad), "aanwezig", SymptoomGraad),
         SymptoomGraad = ifelse(SymptoomCode == symptomen_voorwaardelijk_graad & 
                                  AantastingCode %in% symptomen_voorwaardelijk_aantasting & 
                                  is.na(SymptoomGraad), 
                                "aanwezig",
                                SymptoomGraad),
         SymptoomGraad = replace(SymptoomGraad, SymptoomGraadCode == "7", "volle 100%"))

dfEsSymptoomOrigLevend <- dfEsSymptoomOrigAlle %>% filter(BoomID %in% overlevende_bomen)
if(alleen_levende_bomen) {
  dfEsSymptoomOrig <- dfEsSymptoomOrigLevend
} else {
  dfEsSymptoomOrig <- dfEsSymptoomOrigAlle
}



symptoomgraden <- 
  DBI::dbGetQuery(con, "select Code, Beschrijving from SymptoomGraad order by SortOrder")
symptoomgraden <- rbind(symptoomgraden, data.frame(Code = 100, Beschrijving = "aanwezig"))

#Data met enkel deze waarbij er een symptoomoorzaak is gedefinieerd
dfEsSymptoomOorzAlle <-
  DBI::dbGetQuery(con, sqlcodeorganisme) %>%
  dplyr::inner_join(select(dfEsSymptoomOrigAlle, ProefvlakID, ProefvlakNummer, Jaar, BoomID, MetingID, MetingSymptoomID, GS), by = "MetingSymptoomID") %>%
  filter(GS == TRUE) #niet meer relevant door de inner join

dfEsSymptoomOorzLevend <- dfEsSymptoomOorzAlle %>% filter(BoomID %in% overlevende_bomen)
if (alleen_levende_bomen) {
  dfEsSymptoomOorz <- dfEsSymptoomOorzLevend
} else {
  dfEsSymptoomOorz <- dfEsSymptoomOorzAlle
}


############################################

es_firstyear <- min(dfEsMeting$Jaar)
es_lastyear <- 2019
es_prevyear <- es_lastyear - 1
es_2yearsago <- es_lastyear - 2
es_jaren <- es_firstyear:es_lastyear
all_zero_tolerance <- 95
fig_width <- 7
fig_height <- 5
es_min_oorz <- 5 * length(es_jaren) #een oorzaak moet minstens gemiddeld 5 keer per jaar voorkomen voordat deze in de analyse komt

unieke_bomen <- dfEsMeting %>% 
  group_by(ProefvlakNummer, BoomID) %>% 
  summarize(omtrek_start = min(Omtrek, na.rm = TRUE),
            sterftejaar = min(Jaar[Bladverlies == 100]))
dfEsMeting <- dfEsMeting %>% left_join(unieke_bomen) %>% mutate(gestorven = Jaar > sterftejaar)

if (alleen_levende_bomen) {
  saveRDS(dfEsMeting, file = "data/interim/dfEsMeting_levend.Rds")
  saveRDS(dfEsSymptoomOrig, file = "data/interim/dfEsSymptoom_levend.Rds")
  saveRDS(dfEsSymptoomOorz, file = "data/interim/dfESymptoomOorz_levend.Rds")
  saveRDS(symptoomgraden, file = "data/interim/symptoomgraden_levend.Rds")
  saveRDS(unieke_bomen, file = "data/interim/unieke_bomen_levend.Rds") 
  
} else {
  saveRDS(dfEsMeting, file = "data/interim/dfEsMeting.Rds")
  saveRDS(dfEsSymptoomOrig, file = "data/interim/dfEsSymptoom.Rds")
  saveRDS(dfEsSymptoomOorz, file = "data/interim/dfESymptoomOorz.Rds")
  saveRDS(symptoomgraden, file = "data/interim/symptoomgraden.Rds")
  saveRDS(unieke_bomen, file = "data/interim/unieke_bomen.Rds")  
}

(tot_aantal_bomen_jr <- 
    dfEsMeting %>% 
    filter(Jaar %in% es_jaren) %>% 
    group_by(Jaar) %>% 
    summarise(tot_n_bomen = n_distinct(BoomID))) %>%
  write_csv2(path = paste0(out, "aantal_bomen.csv"))
tot_aantal_bomen_jr

(alle_boomids_jr <- 
  dfEsMeting %>% 
    filter(Jaar %in% es_jaren) %>% 
    select(Jaar, ProefvlakNummer, BoomID) %>% 
    arrange(ProefvlakNummer, BoomID, Jaar)) %>% 
  write_csv2(path = paste0(out, "alle_boomids.csv"))
alle_boomids_jr

(sterftejaar <- dfEsMeting %>% 
    filter(Jaar %in% es_jaren) %>% 
    group_by(ProefvlakNummer, BoomID) %>% 
    filter(Jaar %in% es_jaren) %>% summarize(Sterftejaar = min(Jaar[Bladverlies == 100]))) %>% 
  print(n = 260)

ggplot(alle_boomids_jr, aes(x = factor(Jaar))) + geom_bar() + facet_wrap(~ProefvlakNummer)
ggplot(dfEsMeting %>% group_by(ProefvlakNummer, Jaar) %>% summarize(gem_bladverlies = mean(Bladverlies)), 
       aes(x = Jaar, y = gem_bladverlies)) + facet_wrap(~ProefvlakNummer) + geom_point() + geom_line()

ggplot(dfEsMeting %>% filter(Jaar %in% es_jaren, gestorven == FALSE) %>% group_by(ProefvlakNummer, Jaar) %>% summarize(gem_bladverlies = mean(Bladverlies)), 
       aes(x = Jaar, y = gem_bladverlies)) + facet_wrap(~ProefvlakNummer) + geom_point() + geom_line()




#########################################################################################################
#########################################################################################################

# ### >>> Data cleaning (checks)
# 
# dfEsCheck <- dfEsSymptoom %>% 
#   select(ProefvlakNummer, BoomID, Jaar, AantastingCode, SymptoomCode, SymptoomGraad) %>%
#   arrange(ProefvlakNummer, BoomID, Jaar)
# 
# beschrijving <- "onterechte graad"
# sympt_probleem <- c("10", "11", "12", "21", "22")
# chk01 <- dfEsCheck %>%
#   filter(SymptoomCode %in% c(sympt_probleem), !is.na(SymptoomGraad)) %>%
#   mutate(beschrijving = beschrijving) 
# 
# beschrijving <- "onterechte graad"
# sympt_probleem <- c("13") 
# sympt_probleem_aantasting <- c("31", "32", "33", "34")
# chk02 <-  dfEsCheck %>%
#   filter(SymptoomCode %in% c(sympt_probleem), AantastingCode %in% c(sympt_probleem_aantasting), !is.na(SymptoomGraad)) %>% 
#   mutate(beschrijving = beschrijving) 
# 
# beschrijving = "ongeldige combinatie"
# sympt_probleem <- c("14") 
# sympt_probleem_aantasting <- c("31", "32", "33", "34")
# chk03 <- dfEsCheck %>% 
#   filter(SymptoomCode %in% c(sympt_probleem), AantastingCode %in% c(sympt_probleem_aantasting)) %>%
#   mutate(beschrijving = beschrijving) 
# 
# 
# beschrijving <- "graad niet ingevuld"
# sympt_probleem <- c("08", "17") 
# chk04 <- dfEsCheck %>% 
#   filter(SymptoomCode %in% c(sympt_probleem), is.na(SymptoomGraad)) %>% 
#   mutate(beschrijving = beschrijving)
# 
# beschrijving <- "klopt het dat dit oude schade is?"
# chk05 <- dfEsCheck %>% 
#   filter(SymptoomCode == 13) %>% 
#   group_by(ProefvlakNummer,  BoomID, SymptoomCode) %>%
#   summarise(Jaar = NA, SymptoomGraad = sum(is.na(SymptoomGraad)) == length(es_jaren), beschrijving = beschrijving) %>%
#   filter(SymptoomGraad == TRUE) %>%
#   mutate(SymptoomGraad = NA)
# 
# beschrijving = "inconsistent zaad"
# chk06 <- dfEsMeting %>%
#   select(ProefvlakNummer, BoomID, Jaar, ZaadzettingCode, Zaadzetting, ZaadleeftijdCode, Zaadleeftijd) %>% 
#   filter(ZaadzettingCode > 0 & (ZaadleeftijdCode == 0 | is.na(ZaadleeftijdCode))) %>% 
#            transmute(ProefvlakNummer, BoomID, Jaar, beschrijving = beschrijving)
#   
# inconsistenties <- bind_rows(chk01, chk02, chk03, chk04, chk05, chk06) %>% 
#   write.csv2(file = paste0(out, "inconsistenties.csv"))
  
