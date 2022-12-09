#Keuze van trend per jaar (relatief percentage)
#bv 12% reductie op 12 jaar: 0.25 * (1-0.12) (of moet je hier 11 nemen?)
#(1+x)^12 = 0.88
#1+x = 0.88^(1/12)
#x = 0.88^(1/12) - 1

#keuze van aantal plots
#keuze van aantal jaar
#keuze van aantal bomen per plot
#keuze range effectief bladverlies
#keuze van betrouwbaarheids van telproces
#keuze van correlatie van een boom naar volgend jaar toe
#keuze van correlatie tussen bomen binnen een plot
#keuze van correlatie tussen plots (misschien niet nodig, want volgt uit bomen)


#OPMERKINGEN
# Eigenlijk is het een stijging van 12% die we willen detecteren
# Het correcte model zou een random slope moeten gebruiken
#de sd_slope moet berekend worden zonder outliers anders zal het model zelden betekenis hebben
#4 paden
# - minder metingen per boom (nadeel:veel informatieverlies voor weinig tijdswinst)
# - minder bomen per proefvlak (nadeel: beoordeling is niet heel nauwkeurig, en het wordt gevraagd door het ICP forests protocol)
# - minder proefvlakken (grootste potentieel, ook nadenken over vervangingsstrategie)
# - minder regelmatig meten (nadeel; bladverlies is heel variabel, dus kans op foute conclusies, het gaat in tegen het ICP forests protocol, misschien suggestie enkele proefvlakken jaarlijks en andere minder regelmatig bezoeken?) 
#grote zwakte: geen representatief beeld door keuze gewenste boomsoorten en geen gegevens om stratificatiecorrecties uit te voeren, suggestie terug te gaan naar een random proces + extra proefvlakken ter informatie maar niet voor de algemene trend, maar eventueel wel voor soortspecifieke trends.)
#2 proefvlakken per dag, 120km per proefvlak, tijd voor rapportage?
#Vraagstelling, Communicatieplan?

#DMP in ontwerp, zenodo publicatie mogelijk
#Vrij bruibaar en gebruikt door doctoraatstudenten
#8 proefvlakken verplicht maar niet meer opgevolgd, de rest wel voor ICP forests statistieken rapport.


##structuur: 
#beschrijving meetnet inclusief historiek
#Korte vergelijking met leidraad voor meetnetten (verschillende fases)
##fase1: protocolgestuurd en interessegestuurd ipv vraaggestuurd, geen echte vraagstelling, aanbod heel langlopend meetnet met enkele beperkingen
##fase2: gegevensinzameling plannen (tekortkomingen, opleiding?, goedkoop)
##fase3: analysestrategie: niet duidelijk, sen-slope, lmer, mnaar welke vorm
##fase3: poweranalyse:  historische revisie, maar nog niet echt uitgevoerd?
##fase4: communicatiestrategie: jaarlijks rapport, ICP forest communicatie, persartikels
##fase5: implementatie: reeds lopend meetnet
#SWOT analyse
#potentiele winsten in kosten en verbetersuggesties
#powersimulaties
#conclusies

####################################
### INSTELWAARDEN OP BASIS VAN DATA
####################################

correlations <- c(1, 0.672, 0.591, 0.525, 0.491, 0.465, 0.443, 
                  0.421, 0.397, 0.388, 0.366, 0.343, 0.317)
sd_between_plots <- 0.07 #op basis van de data, afgerond naar boven
sd_between_trees <- 0.10 #op basis van data (0.11 en model 0.08)
sd_within_trees <- 0.08 #op basis van correlatiedata
sd_trend_year <- 0.002 #klopt vermoedelijk niet volledig (op 2 methoden gedaan)
sd_trend_plot <- 0.02
sd_trend_tree <- 0.07


##########
### TEST
##########


test <- generate_cor_general(100000, rep(0,12), correlations[1:12], sd = 2)
cor(test)
apply(test, 2, mean)

test <- simulatie_bosvitaliteit()

mod <- lme(val_categorised ~ year, 
           random = ~year|plot/tree, 
           correlation = corAR1(form = ~1|plot/tree),
           data = test); summary(mod)


#####################
### TEST SIMULATIES
#####################

simconfig <- expand.grid(n_sims = 10, 
                         n_plots = c(100, 75, 50, 25),
                         n_trees = c(20, 10, 5), 
                         start_year = 0, 
                         end_year = 12, 
                         mu_pop_t0 = 0.25, 
                         mu_relative_tn = c(1.12, 1.24), 
                         sd_trend_plot = sd_trend_plot,
                         sd_trend_tree = sd_trend_tree,
                         sd_trend_year = sd_trend_year,
                         sd_between_plots = sd_between_plots,
                         sd_between_trees_nat = sd_between_trees/2, 
                         sd_between_trees_obs = sd_between_trees/2,
                         sd_within_trees = sd_within_trees,
                         sd_error = 0,
                         partitions = 20)

#corAR1 is misschien wat te streng, correlatie tussen jaren blijft vermoedelijk hoger
#random slope is onmogelijk te gebruiken door convergentiefouten, 
#daarom het minder correcte random intercept als keuze
#met al de extra variatie is er toch een groot probleem met de simulatie
#misschien vereenvoudigen, door met vastgelegde trends per plot en per  boom te werken
#in plaats van dit telkens ieder jaar aan te passen
result <- execute_simulation(simconfig, 1, correlations = correlations,
                             show_model = TRUE, modeltype = "intercept")
result

result2 <- execute_simulation(simconfig, 1, 
                              filter_years = c(0,3,6,9,12))
result2

