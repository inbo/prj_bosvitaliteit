

########################
### Inlezen Data uit DB
########################

### >>> Maak connectie met de databank en lees hulpbestanden in

if (connect_via_db) {
  #als onderstaande code fout geeft, dan is de connectie niet OK
  #dan best aan Pieter of IT vragen
  conn <- bosvitaliteit_connect()
  
  dfSoortInfo <- read.csv2("data/tree_indeling.csv", stringsAsFactors = FALSE)
  dim(dfSoortInfo)
  if (!is.null(dfSoortInfo) & length(dfSoortInfo))
    save(dfSoortInfo, file = "data/dfSoortInfo.Rdata")
  
  tree_sql <- readLines("data/tree_info.SQL")
  dfTrees <- get_treedata(conn, jaar = jaarkeuze,
                          tree_indeling = dfSoortInfo,
                          sql = tree_sql) #lees data in via functies_db.R
  dim(dfTrees)
  if (!is.null(dfTrees) & length(dfTrees))
    save(dfTrees, file = paste0("data/dfTrees.Rdata"))#bewaar als Rdata bestand (optioneel)
  
  
  sympt_sql <- readLines("data/tree_symptom_info.SQL")
  dfSymptoms <- get_symptomdata(conn, jaar = jaarkeuze, sql = sympt_sql ) #lees data in via functies_db.R
  dim(dfSymptoms)
  
  if (!is.null(dfSymptoms) & length(dfSymptoms))
    save(dfSymptoms, file = "data/dfSymptoms.Rdata")#bewaar als Rdata bestand (optioneel)
  print(dim(dfSymptoms))
  
  dfTrees2 <- get_treedata(conn, jaar = tweejaarlijks,
                           tree_indeling = dfSoortInfo,
                           sql = tree_sql)
  dim(dfTrees2)
  if (!is.null(dfTrees2) & length(dfTrees2))
    save(dfTrees2, file = "data/dfTrees2jaarlijks.Rdata")
  
  dfTrees3 <- get_treedata(conn, jaar = driejaarlijks,
                           tree_indeling = dfSoortInfo,
                           sql = tree_sql)
  dim(dfTrees3)
  if (!is.null(dfTrees3) & length(dfTrees3))
    save(dfTrees3, file = "data/dfTrees3jaarlijks.Rdata")
  
  
  dfTreesTrend <- get_treedata(conn, jaar = meerjaarlijks,
                               tree_indeling = dfSoortInfo,
                               sql = tree_sql) %>%
    select(Jaar, MetingKey, PlotNr, BoomNr, Soortnummer, Omtrek, Leeftijd, BladverliesNetto,
           Zaadzetting, Waterscheuten, Beschadigd, Soort, SoortType, SoortIndeling) %>%
    filter(!is.na(BoomNr)) #in 1996 PlotNr 508 waren er geen waarnemingen
  dim(dfTreesTrend)
  if (!is.null(dfTreesTrend) & length(dfTreesTrend))
    save(dfTreesTrend, file = "data/dfTreesTrend.Rdata")
  
  dbDisconnect(conn)
  
} else {
  load("data/dfSoortInfo.Rdata")
  load("data/dfTrees.Rdata")
  load("data/dfSymptoms.Rdata")
  load("data/dfTrees2jaarlijks.Rdata")
  load("data/dfTrees3jaarlijks.Rdata")
  load("data/dfNatuurindicatoren.Rdata")
  load("data/dfTreesTrend.Rdata")
}

####################################
### Basismanipulaties Data
####################################

### >>> Afgeleide datasets

#! SoortselectieVolgorde
dfVolgorde <- dfSoortInfo %>%
  select(selectie = SoortIndeling, volgorde = SoortVolgorde) %>%
  group_by(selectie) %>%
  summarise(volgorde = min(volgorde)) %>%
  arrange(volgorde)

#! Soortinfo en Treeinfo combineren
#Let op, er komen hier dubbele bomen voor, wegens verschillende oorzaken,
#maar er kunnen ook duplicaatrijen ontstaan door meerdere aangetaste delen,
#meerdere symptoomspecificaties en meerdere symptoomoorzaken

dfSA <- get_SymptomAnalysisdata(dfTrees, dfSymptoms)

#!Levend, met symptomen
#deze dataset heeft het nadeel dat er duplicaatrijen komen
#als een symptoom meerdere specificaties of oorzaken of organismeoorzaken krijgt)
dfLMS <- filter(dfSA,
                Jaar %in% jaarkeuze,
                !is.na(OnderdeelBoomCat),
                !(AangetastDeelCode %in% c(0,4))) #0 = geen symptoom, #4 is dood

#! Dode bomen
dfDead <- filter(dfSA, Jaar %in% jaarkeuze,  AangetastDeelCode == 4)

#! Voorbereidende totalentabel (gebruikt al een eerste keer bomen_calc)
dfTotaalBomen <-
  bomen_calc(dfTrees, normal_groups) %>%
  select(selectie, Jaar, TotaalAantalBomen = AantalBomen) %>%
  left_join(dfVolgorde, by = "selectie") %>%
  arrange(volgorde)

#! Tweejaarlijks
dfTrees2 <-
  dfTrees2 %>%
  mutate(prbo = paste0(PlotNr, BoomNr))

#!gemeenschappelijke bomen over de 2 jaar
dfTrees2Gmsch <-
  inner_join(dfTrees2,
             dfTrees2 %>%
               group_by(prbo) %>%
               summarize(aantal = n()) %>%
               filter(aantal == 2),
             by = "prbo")

#!Berekening totalen 2-jaarlijks gemeenschappelijk
dfTotaalBomen2J <-
  bomen_calc(dfTrees2Gmsch, normal_groups) %>%
  select(selectie, Jaar, TotaalAantalBomen = AantalBomen) %>%
  left_join(dfVolgorde, by = "selectie") %>%
  arrange(volgorde)

#! Toevoegen prbo aan driejaarlijkse data
dfTrees3 <-
  dfTrees3 %>%
  mutate(prbo = paste0(PlotNr, BoomNr))

#gemeenschappelijke bomen over 3 jaar
gemeenschappelijk3j <-
  
  dfTrees3Gmsch <-
  inner_join(dfTrees3,
             dfTrees3 %>% group_by(prbo) %>%
               summarize(aantal = n()) %>%
               filter(aantal == 3),
             by = "prbo")

#Berekening totalen 3-jaarlijks gemeenschappelijk
dfTotaalBomen3J <-
  bomen_calc(dfTrees3Gmsch, normal_groups) %>%
  select(selectie, Jaar, TotaalAantalBomen = AantalBomen) %>%
  left_join(dfVolgorde, by = "selectie") %>%
  arrange(volgorde)

cat("IMPORTSCRIPT VOLLEDIG UITGEVOERD\n")
