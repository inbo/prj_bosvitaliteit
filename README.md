# Omschrijving

Deze repository combineert verschillende projecten rond bosvitaliteit op het INBO.

1. Scripts voor jaarlijkse rapportage bosvitaliteit
2. Scripts voor evaluatie van de evolutie van de essenziekte
3. Scripts voor de aanmaak van de INBO indicator "beschadigde bosbomen"
4. Revisie bosvitaliteitsmeetnet 2022

De data van de projecten is niet publiek beschikbaar in deze repository en moet aangevraagd worden bij het INBO.

# Jaarlijkse rapportage bosvitaliteit

Scripts ter ondersteuning van de jaarlijkse rapportage van de bosvitaliteit in Vlaanderen

Dit maakt gebruik van interne databronnen op het INBO. 


## Hoe te werk gaan

Er is een startscript waarmee je de volledige analyse kan doorlopen:

````
#zorg dat de root directory juist staat (aanpassen indien nodig)
rootdir <- file.path(here::here(), "01 JAARLIJKS RAPPORT")
setwd(rootdir)

#zorg dat alle functies gekend zijn
source("scripts/functies/functies_db.R")
source("scripts/functies/functies_samenvatting.R")
source("scripts/functies/functies_samenvatting.R")

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


````

# Evolutie van de essenziekte

# Indicator beschadigde bosbomen

# revisie bosvitaliteitsmeetnet 2022

Op vraag van het managementteam wordt het bosvitaliteitsmeetnet onder de loep genomen, om te zien of optimalisaties mogelijk zijn.

