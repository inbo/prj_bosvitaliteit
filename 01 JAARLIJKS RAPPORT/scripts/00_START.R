#startscript

#######################################
### Benodigde libraries en instellingen
#######################################

#Gebruik deze scripts zoals hier beschreven.
#In de hoofddirectory vind je volgende bestanden terug
#   1) "inbobosvitaliteit.Rproj" projectbestand van Rstudio

#Voor ieder jaar maak je een nieuwe map in deze hoofddirectory
#zorg dat in deze directory de folders "data", "R", "output" bestaan

#!! DE WERKDIRECTORY IN R moet overeenkomen met deze hoofddirectory. !! te vinden via: getwd()

#data: bevat volgende bestanden (de inhoud van deze bestanden staan ook in de scripts moesten deze verloren gaan)
#  1) "tree_indeling.csv" dataset die de codes met de namen van de bomen koppelt
#  2) "tree_info.SQL" de sql-code die gebruikt wordt om de boominfo uit de databank te halen
#  3) "tree_symptom_info.SQL de sql-code die gebruikt wordt om de symptoominfo uit de databank te halen
#  4) "natuurindicatoren.SQL" de sql code die gebruikt wordt om de natuurindicatordata op te halen
   #--> er worden in deze directory ook .Rdata bstanden gemaakt, zodat deze kunnen gebruikt worden ipv een databankconnectie

#output: mag nog een lege directory zijn (maar deze moet wel bestaan) om de resultaten in weg te schrijven

#R : directory die de R-scripts bevat. Dit zijn:
#  1) 00_START.R (dit script)
#  2) 01_JaarlijkseAnalyse.R
#  3) 02_SymptomenAnalyse.R
#  4) 03_TweejaarlijkseAnalyse.R
#  5) 04_DriejaarlijkseAnalyse.R
#  6) 05_Trendanalyse.R
#  8) functies_db.R
#  9) functies_samenvatting.R
# 10) functies_trend.R

##########################################Y
### INSTELVARIABELEN
##########################################

#vul deze cijfers in de volgende vorm in zoals van toepassing is
jaarkeuze     <- c(2021) #datajaar
pathkeuze <- paste0(jaarkeuze + 1, "/") #de rapportering gebeurt een jaar later
tweejaarlijks <- c(jaarkeuze-1, jaarkeuze)
driejaarlijks <- c(jaarkeuze-2, jaarkeuze-1, jaarkeuze)
meerjaarlijks <- 1995:jaarkeuze
jaren_natuurindicatoren <- 1987:jaarkeuze
connect_via_db <- TRUE #zet op TRUE als je de gegevens uit de db wil halen, op FALSE als je de gegevens uit de Rdata bestanden wil halen van een vroegere import
outdir <- paste0(pathkeuze, "output/")

fig_width  <- 7 #standaardbreedte figuur (in inch)
fig_height <- 5 #standaardhoogte figuur (in inch)
fig_dpi <- 300 #standaard dpi figuur
sen_boot <- 200 #aantal bootstrap samples voor de sen slope (liefst 200 of 1000), let op hoe hoger hoe langer het script zal runnen, zet dit op 0 dan zal de bootstrap niet uitgevoerd worden
lmer_boot <- 200 #idem maar voor de lmer bootstrap voor aandeel beschadigde bomen
sen_seed <- 176 #seed voor de bootstrap voor reproduceerbare resultaten (willekeurig getal)

#Tot hier is er USER interactie, de rest zou altijd identiek moeten blijven
#---------------------------------------------------------------------------------------------

###############################
### Bibliotheken en hulpscripts
###############################

library(RODBC)      #connectie leggen met SQL databank
library(dplyr)      #R code basis voor simpele aggregaties
library(tidyr)      #Data wrangling
library(rlang)      #
library(purrr)      #Ingewikkelde aggregaties
library(ggplot2)    #Figuren
library(stringr)    #functies om tekst te manipuleren
library(lme4)       #
#library(gamm4)      #
library(INBOtheme)  #
library(rkt)        #Sen slope en MannKendall test
library(readr)      #Lezen van en wegschrijven naar tekstbestanden

theme_set(theme_inbo(10))

### >>> Hulpscripts

source(paste0(pathkeuze,"/R/functies_db.R"))           #data-inleesfuncties
source(paste0(pathkeuze,"/R/functies_samenvatting.R")) #tabelleringsfuncties
source(paste0(pathkeuze,"/R/functies_trend.R")) #functies voor de sen en lmer trends

#connectiegegevens databank (zelf je login en pwd aanpassen)
#Maak deze file zelf aan als die nog niet bestaat met onderstaande code
#vul zelf wel je windows passwoord in (in plaats van ***windowsAD***)
#in de file zelf zodat deze niet in de code staat
#De data-source moet je zelf nog aanmaken als deze nog niet bestaat:
   #open windows startmenu en zoek naar gegevensbronnen (odbc) en open die
   #maak de gegevensbron aan via toevoegen ...
   #kies SQL-server en druk op voltooien
   #1) in het volgende venster vul de naam van de gegensbron in (kan je zelf kiezen,
   #   maar zorg dat deze overeenkomt met je dbcredentials.txt)
   #   standaard raadt ik D0004_00_Bosvitaliteit aan in de huidige omgeving
   #2) geef een beschrijving (zoals bosvitaliteit)
   #3) vul de server en poort in. In de huidige omgeving is dit:
   #  inbo-sql07-prd.inbo.be,1433 (mogelijks is de ,1433 niet nodig)
   #4) druk op volgende en in dit venster kies windows NT verificatie
   #   en vink het vinkje onderaan aan en kies je aanmeldingsid geert_sioen
   #5) druk op volgende en kies in dit venster een andere standaarddatabase,
   #   namelijk D0004_00_Bosvitaliteit
   #6) de rest kan je laten staan zoals het is, en je kan ,naar volgende en voltooien
   #7) Nu krijg je de gelegenheid de gegevensbron te testen en dat zou moeten werken

#let op, de tekst niet laten inspringen. Op zich is het voldoende enkel de regels
#name;value en data-sorce;D004_00_Bosvitaliteit in te vullen,
#als je de data source met windows NT authentificatie hebt aangemaakt
create_new_credentials <- FALSE
if (create_new_credentials) {
  cat("
name;value
server;inbo-sql07-prd.inbo.be
port;1433
login;pieter.verschelde@inbo.be
pwd;***windowsAD***
data-source;D0004_00_Bosvitaliteit",
  file = "_DO_NOT_COPY_/dbcredentials.txt", append = FALSE )
}

#onderstaande variabelen worden gebruikt om de tabellen te aggregeren
#    dit is een lijst met de afzonderlijke aggregaties
#    normal_groups bestaat uit 3 afzonderlijke aggregaties,
#    op jaarbasis, per jaar per soorttype, en per jaar per soortindeling

normal_groups    <- list(c("Jaar"),
                         c("Jaar", "SoortType"),
                         c("Jaar", "SoortIndeling"))
all_groups       <- list(c("Jaar"),
                         c("Jaar", "LeeftijdsklasseEur"),
                         c("Jaar", "SoortType"),
                         c("Jaar", "SoortIndeling"),
                         c("Jaar", "LeeftijdsklasseEur", "SoortType"),
                         c("Jaar", "LeeftijdsklasseEur", "SoortIndeling"))
extended_groups <-  list(c("Jaar"),
                         c("Jaar", "SoortType"),
                         c("Jaar", "SoortIndeling"),
                         c("Jaar", "Soort"))
groups_multiyear <- list(c("Jaar"),
                         c("Jaar", "LeeftijdsklasseEur"),
                         c("Jaar", "SoortType"),
                         c("Jaar", "SoortType", "LeeftijdsklasseEur"),
                         c("Jaar", "SoortIndeling"))

extra_groups     <- list(c("Jaar"),
                         c("Jaar", "LeeftijdsklasseEur"),
                         c("Jaar", "SoortType"),
                         c("Jaar", "SoortType", "LeeftijdsklasseEur"),
                         c("Jaar", "SoortIndeling"))



########################
### Inlezen Data uit DB
########################

### >>> Maak connectie met de databank en lees hulpbestanden in

if (connect_via_db) {
  #als onderstaande code fout geeft, dan is de connectie niet OK
  #dan best aan Pieter of IT vragen
  conn <- bosvitaliteit_connect("_DO_NOT_COPY_/dbcredentials.txt")
  tree_sql <- readLines(paste0(pathkeuze, "/data/tree_info.SQL"))

  dfSoortInfo <- read.csv2(file = paste0(pathkeuze, "/data/tree_indeling.csv"), stringsAsFactors = FALSE)
  dim(dfSoortInfo)
  if (!is.null(dfSoortInfo) & length(dfSoortInfo))
    save(dfSoortInfo, file = paste0(jaarkeuze, "/data/dfSoortInfo.Rdata"))

  dfTrees <- get_treedata(conn, jaar = jaarkeuze,
                          tree_indeling = dfSoortInfo,
                          sql = tree_sql) #lees data in via functies_db.R
  dim(dfTrees)
  if (!is.null(dfTrees) & length(dfTrees))
    save(dfTrees, file = paste0(pathkeuze, "/data/dfTrees.Rdata"))           #bewaar als Rdata bestand (optioneel)

  dfSymptoms <- get_symptomdata(conn, jaar = jaarkeuze, sql = readLines(paste0(pathkeuze, "/data/tree_symptom_info.SQL"))) #lees data in via functies_db.R
  dim(dfSymptoms)
  if (!is.null(dfSymptoms) & length(dfSymptoms))
    save(dfSymptoms, file = paste0(pathkeuze, "/data/dfSymptoms.Rdata"))           #bewaar als Rdata bestand (optioneel)

  dfTrees2 <- get_treedata(conn, jaar = tweejaarlijks,
                           tree_indeling = dfSoortInfo,
                           sql = tree_sql)
  dim(dfTrees2)
  if (!is.null(dfTrees2) & length(dfTrees2))
    save(dfTrees2, file = paste0(pathkeuze, "/data/dfTrees2jaarlijks.Rdata"))

  dfTrees3 <- get_treedata(conn, jaar = driejaarlijks,
                           tree_indeling = dfSoortInfo,
                           sql = tree_sql)
  dim(dfTrees3)
  if (!is.null(dfTrees3) & length(dfTrees3))
    save(dfTrees3, file = paste0(pathkeuze, "/data/dfTrees3jaarlijks.Rdata"))


  dfTreesTrend <- get_treedata(conn, jaar = meerjaarlijks,
                               tree_indeling = dfSoortInfo,
                               sql = tree_sql) %>%
    select(Jaar, MetingKey, PlotNr, BoomNr, Soortnummer, Omtrek, Leeftijd, BladverliesNetto,
           Zaadzetting, Waterscheuten, Beschadigd, Soort, SoortType, SoortIndeling) %>%
    filter(!is.na(BoomNr)) #in 1996 PlotNr 508 waren er geen waarnemingen
  dim(dfTreesTrend)
  if (!is.null(dfTreesTrend) & length(dfTreesTrend))
    save(dfTreesTrend, file = paste0(pathkeuze, "/data/dfTreesTrend.Rdata"))

  odbcCloseAll()

} else {
  load(paste0(pathkeuze, "/data/dfSoortInfo.Rdata"))
  load(paste0(pathkeuze, "/data/dfTrees.Rdata"))
  load(paste0(pathkeuze, "/data/dfSymptoms.Rdata"))
  load(paste0(pathkeuze, "/data/dfTrees2jaarlijks.Rdata"))
  load(paste0(pathkeuze, "/data/dfTrees3jaarlijks.Rdata"))
  load(paste0(pathkeuze, "/data/dfNatuurindicatoren.Rdata"))
  load(paste0(pathkeuze, "/data/dfTreesTrend.Rdata"))
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
