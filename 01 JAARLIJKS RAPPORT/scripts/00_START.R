#startscript

#######################################
### Benodigde libraries en instellingen
#######################################

#Gebruik deze scripts zoals hier beschreven.
#In de hoofddirectory vind je volgende bestanden terug
#   1) "inbobosvitaliteit.Rproj" projectbestand van Rstudio

#Voor ieder jaar maak je een nieuwe map in deze hoofddirectory
#zorg dat in deze directory de folders "data", "R", "output" bestaan

#!! DE WERKDIRECTORY IN R moet overeenkomen met deze hoofddirectory. !! te vinden via: getwd() en via het menu session > set working directory aan te passen (of via setwd)

#data: bevat volgende bestanden (de inhoud van deze bestanden staan ook in de scripts moesten deze verloren gaan)
#  1) "tree_indeling.csv" dataset die de codes met de namen van de bomen koppelt
#  2) "tree_info.SQL" de sql-code die gebruikt wordt om de boominfo uit de databank te halen
#  3) "tree_symptom_info.SQL de sql-code die gebruikt wordt om de symptoominfo uit de databank te halen
#  4) "natuurindicatoren.SQL" de sql code die gebruikt wordt om de natuurindicatordata op te halen
#--> er worden in deze directory ook .Rdata bstanden gemaakt, zodat deze kunnen gebruikt worden ipv een databankconnectie

#output: mag nog een lege directory zijn (maar deze moet wel bestaan) om de resultaten in weg te schrijven

#R : directory die de R-scripts bevat. Dit zijn:
#  1) 00_START.R (dit script)
#  2) 01_data_import.R
#  3) 02_JaarlijkseAnalyse.R
#  4) 03_SymptomenAnalyse.R
#  5) 04_TweejaarlijkseAnalyse.R
#  6) 05_DriejaarlijkseAnalyse.R
#  7) 06a_Trendanalyse_Sen
#  8) 06b_Trendanalyse_lmer.R
#  9) functies_db.R
# 10) functies_samenvatting.R
# 11) functies_trend.R

##########################################Y
### INSTELVARIABELEN
##########################################

#zorg dat de root directory juist staat (aanpassen indien nodig)
rootdir <- file.path(here::here(), "01 JAARLIJKS RAPPORT")
setwd(rootdir)

#zorg dat alle functies gekend zijn
source("scripts/functies/functies_db.R")
source("scripts/functies/functies_samenvatting.R")
source("scripts/functies/functies_trend.R")

#maak een connectie met de db (indien relevant)
conn <- bosvitaliteit_connect()

#maak globale variabelen aan die gebruikt worden door de scripts
init_sessie(jaar = 2021, connect_via_db = TRUE) 

#kies het thema voor de figuren
theme_set(theme_inbo(10))

#lees de data in 
source("scripts/01_data_import.R")

#jaarlijkse analyse
source("scripts/02_jaarlijkseAnalyse.R")

#symptomen analyse
source("scripts/03_SymptomenAnalyse.R")

#tweejaarlijkse analyse
source("scripts/04_TweejaarlijkseAnalyse.R")

#driejaarlijkse analyse
source("scripts/05_DriejaarlijkseAnalyse.R")

#langere termijn analyse (Sen slope)
source("scripts/06a_Trendanalyse_Sen (nnv en beschadigd).R")

#langere termijn analyse (Lineair model)
source("scripts/06b_trendanalyse_lmer (nnv en beschadigd).R")

