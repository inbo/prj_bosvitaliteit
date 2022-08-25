
create_new_treeindeling <- FALSE
if (create_new_treeindeling){
  cat("

SPEC_EUR_CDE;SPEC_DES;Soort;SoortType;SoortIndeling;SoortEngels;SoortVolgorde
      999;;totaal;totaal;totaal;total;0
      9998;;totaal jong;totaal jong;totaal jong;total young;1
      9999;;totaal oud;totaal oud;totaal oud;total old;2
      948;;loofbomen;loofbomen;loofbomen;deciduous trees;11
      9997;;jonge loofbomen;jonge loofbomen;loofbomen jong;deciduous young;12
      9996;;oude loofbomen;oude loofbomen;loofbomen oud;deciduous old;13
      998;;naaldbomen;naaldbomen;naaldbomen;conifers;211
      9995;;jonge naaldbomen;jonge naaldbomen;naaldbomen jong;conifers young;212
      9994;;oude naaldbomen;oude naaldbomen;naaldbomen oud;conifers old;213
      51;Quercus robur;zomereik;loofbomen;zomereik;oak;101
      20;Fagus sylvatica;beuk;loofbomen;beuk;beech;102
      53;Quercus rubra;Amerikaanse eik;loofbomen;Amerikaanse eik;northern red oak;103
      947;;overige lbs.;loofbomen;overige lbs.;other deciduous;109
      33;Populus hybrides;populier;loofbomen;overige lbs.;poplar;111
      7;Alnus glutinosa;zwarte els;loofbomen;overige lbs.;black alder;112
      15;Castanea sativa;tamme kastanje;loofbomen;overige lbs.;sweet chestnut;113
      48;Quercus petraea;wintereik;loofbomen;overige lbs.;sessile oak;114
      22;Fraxinus excelsior;es;loofbomen;overige lbs.;ash;115
      5;Acer pseudoplatanus;esdoorn;loofbomen;overige lbs.;sycamore maple;116
      10;Betula pendula;ruwe berk;loofbomen;overige lbs.;silver berch;117
      56;Robinia pseudoacacia;valse acacia;loofbomen;overige lbs.;black locust (false acacia);118
      32;Populus canescens;grauwe abeel;loofbomen;overige lbs.;grey poplar;119
      13;Carpinus betulus;haagbeuk;loofbomen;overige lbs.;hornbeam;120
      8;Alnus incana;witte els;loofbomen;overige lbs.;grey alder;121
      36;Prunus avium;zoete kers;loofbomen;overige lbs.;sweet cherry;122
      11;Betula pubescens;zachte berk;loofbomen;overige lbs.;downy (moor, white) birch;123
      72;Ulmus minor;gladde iep;loofbomen;overige lbs.;field elm;124
      35;Populus tremula;ratelpopulier;loofbomen;overige lbs.;aspen;125
      134;Pinus sylvestris;grove den;naaldbomen;grove den;Scots pine;301
      129;Pinus nigra;Corsicaanse den;naaldbomen;Corsicaanse den;black pine;302
      997;;overige nbs.;naaldbomen;overige nbs.;other conifers;309
      117;Larix kaempferi;Japanse lork;naaldbomen;overige nbs.;Japanese larch;311
      118;Picea abies;fijnspar;naaldbomen;overige nbs.;Norway spruce;312
      136;Pseudotsuga menziesii;douglas;naaldbomen;overige nbs.;douglas fir;313",
      file = "data/tree_indeling.csv", append = FALSE)
}



create_sql <- FALSE
if (create_sql) {

  #tree_info.SQL
  cat("
select
  p.PLOT_ID as PlotKey
      , b.BOOM_ID as BoomKey
      , w.WRNG_ID as WaarnemingKey
      , m.WRME_ID as MetingKey
      , p.PLOT_NUM as PlotNr
      , p.PLOT_NAM as PlotNaam
      , g.GMTE_NAM as Gemeente
      , b.BOOM_BNR as BoomNr
      , w.WRNG_DTE as Datum
      , w.WRNG_JAA as Jaar
      , s.SPEC_EUR_CDE as Soortnummer
      , m.WRME_OMT as Omtrek
      , m.WRME_LFT as Leeftijd
      , m.WRME_UCLT_CDE AS Leeftijdsklasse
      , m.WRME_UCBL_CDE as BladverliesNetto
      , m.WRME_OPM as MetingOpmerking
      , m.WRME_UCNJ_CDE AS AantalNaaldjaargangen
      , m.WRME_UCZZ_CDE AS Zaadzetting
      , m.WRME_UCWS_CDE AS Waterscheuten
      , b.BOOM_JDO as SterfteJaar
      , b.BOOM_REMO_CDE VerwijderdCode
      , cdem.REMO_DES as VerwijderdReden
      from
      tblProefvlak p left join
      tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
      tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
      tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
      tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID left join
      cdeRemovalsMortality cdem on b.BOOM_REMO_CDE = cdem.REMO_CDE left join
      tblGemeente g on g.GMTE_NIS_CDE = p.PLOT_GMTE_NIS_CDE
      ", file = "data/tree_info.SQL", append = FALSE)

  #tree_symptom_info.SQL
  cat("
select
  m.WRME_ID as MetingKey
      , sym.WMSY_ATDE_CDE AangetastDeelCode
      , cad.ATDE_DES AangetastDeel
      , sym.WMSY_SYSP_CDE SymptoomSpecCode
      , css.SYSP_DES SymptoomSpecificatie
      , sym.WMSY_SYTO_CDE SymptoomCode
      , csym.SYTO_DES Symptoom
      , sym.WMSY_UCME_CDE SymptoomMeting
      , sym.WMSY_LEFT_CDE LeeftijdSchadeCode
      , clft.LEFT_DES as LeeftijdSchade
      , sym.WMSY_OPM SymptoomOpmerking
      , oorz.OORZ_CDE SymptoomOorzaakCode
      , oorz.OORZ_DES SymptoomOorzaak
      , ozor.OZOR_CDE SymptoomOrganismeCode
      , ozor.OZOR_NAM SymptoomOrganisme
      from
      tblProefvlak p left join
      tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
      tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
      tblWaarnemingMetingSymptoom sym on sym.WMSY_WRME_ID = m.WRME_ID left join
      tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
      tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID  left join
      cdeAangetastDeel cad on cad.ATDE_CDE = sym.WMSY_ATDE_CDE left join
      cdeSymptoomSpecificatie css on css.SYSP_CDE = sym.WMSY_SYSP_CDE left join
      cdeSymptoom csym on csym.SYTO_CDE = sym.WMSY_SYTO_CDE left join
      cdeLeeftijdSchade clft on clft.LEFT_CDE = sym.WMSY_LEFT_CDE left join
      tblMetingSymptoomOorzaak syo on syo.MSYO_WMSY_ID = sym.WMSY_ID left join
      tblOorzaak oorz on oorz.OORZ_ID = syo.MSYO_OORZ_ID left join
      tblOorzaakOrganisme ozor on ozor.OZOR_ID = syo.MSYO_OZOR_ID",
      file = "data/tree_symptom_info.SQL", append = FALSE )

  #natuurindicatoren.sql
  cat("
select
p.PLOT_NUM as PlotNr
, b.BOOM_BNR as BoomNr
, w.WRNG_JAA as Jaar
, s.SPEC_EUR_CDE as Soortnummer
, m.WRME_OMT as Omtrek
, m.WRME_LFT as Leeftijd
, m.WRME_UCBL_CDE as BladverliesNetto

from
tblProefvlak p left join
tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID",
      file = "data/natuurindicatoren.sql", append = FALSE)
}

##########################
##########################

###################################################################

#' Functie om de instelvariabelen te zetten om de scripts te kunnen runnen
#'
#' @param jaar laatste datajaar van de analyse
#' @param connect_via_db indien TRUE maak een connectie via DB, indien niet haal dit uit een bestaand .Rdata bestand die alle data bevat
#' @param beginjaar Eerste jaar die meegenomen wordt de heel globale figuren zoals voor de natuurindicatoren
#' @param beginjaar_meerjaarlijks Eerste jaar die meegenomen wordt voor de meerjaarlijkse analyses
#' @param fig_width standaard figuurbreedte in inch
#' @param fig_height standaard figuurhoogote in inch
#' @param fig_dpi standaard resolutie voor de figuren
#' @param sen_boot aantal bootstrap samples om betrouwbaarheidsintervallen op de sen slope te bepalen, indien 0 dan wordt geen bootstrap uitgevoerd
#' @param lmer_boot aantal bootstrap samples om  betrouwbaarheidsintervallen voor de lineaire modellen te bepalen, indien 0 dan wordt geen bootstrap uitgevoerd
#'
#' @return maakt verschillende golbale variabelen aan: jaarkeuze, pathkeuze, tweejaarlijks, driejaarlijks, meerjaarlijks, jaren_natuurindicatoren, outdir, connect_via_db, normal_groups, all_groups, extended_groups, groups_multiyear, extra_groups
#' @export
#'
#' @examples
#' {
#' definieer_inputvariabelen(jaar = 2021)
#' }

init_sessie <- 
  function(jaar, 
           connect_via_db = TRUE,
           beginjaar = 1987, 
           beginjaar_meerjaarlijks = 1995,
           fig_width = 7, 
           fig_height = 5, 
           fig_dpi = 300, 
           sen_boot = 200, 
           lmer_boot = 200) {
    
    #globale variabelen
    fig_width <<- fig_width
    fig_height <<- fig_height
    fig_dpi <<- fig_dpi
    jaarkeuze <<- jaar
    pathkeuze <<- paste0(jaarkeuze + 1, "/") 
    tweejaarlijks <<- c(jaarkeuze-1, jaarkeuze)
    driejaarlijks <<- c(jaarkeuze-2, jaarkeuze-1, jaarkeuze)
    meerjaarlijks <<- 1995:jaarkeuze
    jaren_natuurindicatoren <<- 1987:jaarkeuze
    if (!dir.exists(pathkeuze)) try(dir.create(pathkeuze))
    outdir <<- paste0(pathkeuze, "output")
    if(!dir.exists(outdir)) try(dir.create(outdir))
    connect_via_db <<- connect_via_db
    
    #extra variabelen
    
    normal_groups    <<- list(c("Jaar"),
                             c("Jaar", "SoortType"),
                             c("Jaar", "SoortIndeling"))
    all_groups       <<- list(c("Jaar"),
                             c("Jaar", "LeeftijdsklasseEur"),
                             c("Jaar", "SoortType"),
                             c("Jaar", "SoortIndeling"),
                             c("Jaar", "LeeftijdsklasseEur", "SoortType"),
                             c("Jaar", "LeeftijdsklasseEur", "SoortIndeling"))
    extended_groups <<-  list(c("Jaar"),
                             c("Jaar", "SoortType"),
                             c("Jaar", "SoortIndeling"),
                             c("Jaar", "Soort"))
    groups_multiyear <<- list(c("Jaar"),
                             c("Jaar", "LeeftijdsklasseEur"),
                             c("Jaar", "SoortType"),
                             c("Jaar", "SoortType", "LeeftijdsklasseEur"),
                             c("Jaar", "SoortIndeling"))
    
    extra_groups     <<- list(c("Jaar"),
                             c("Jaar", "LeeftijdsklasseEur"),
                             c("Jaar", "SoortType"),
                             c("Jaar", "SoortType", "LeeftijdsklasseEur"),
                             c("Jaar", "SoortIndeling"))
    
    #benodigde libraries
    packages <- c("here", "odbc", "DBI", "tidyverse", "rkt", "rlang", "lme4", "remotes")
    install.packages(setdiff(packages, rownames(installed.packages())))
    
    library(here)
    library(odbc)
    library(DBI)
    library(tidyverse)
    library(rkt)
    library(rlang)
    library(lme4)
    
    if(!("INBOtheme" %in% rownames(installed.packages()))) 
      remotes::install_github("INBO/INBOtheme")
    library(INBOtheme)

    invisible()  
  }

#########################################################################

# bosvitaliteit_connect <- function(odbc_txt = "_DO_NOT_COPY_/dbcredentials.txt"){
#   require("RODBC")
#   creds <- read.csv2(odbc_txt, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
#   dsn <- as.character(creds[creds$name == "data-source","value"])
#   odbcConnect(dsn)
# }

bosvitaliteit_connect <- function(){
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server", 
                        Server = "inbo-sql07-prd.inbo.be", 
                            port = 1433, 
                            Database = "D0004_00_Bosvitaliteit", 
                            Trusted_Connection = "True")   
  if(class(con) != "Microsoft SQL Server") 
        print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator")
  con
}
    
###################################################################

get_treedata <- function(channel, jaar, tree_indeling, sql, show_query = TRUE, 
                         tweejaarlijks = NULL, driejaarlijks = NULL) {
  if(is.null(tweejaarlijks)) tweejaarlijks <- c(jaar[length(jaar) - 1:0])
  if(is.null(driejaarlijks)) driejaarlijks <- c(jaar[length(jaar) - 2:0])
  jaarstring <- paste(jaar, collapse = ",")
  whereClause <-  paste0(" where w.WRNG_JAA in (", jaarstring, ")")
  dfTreeIndeling <- tree_indeling
  sql <- c(sql, whereClause)
  sql <- paste(sql, collapse = "\n")
  if (show_query) cat(sql, "\n")
  
  df <- dbGetQuery(channel, sql)
  print(head(df))
  maxOmtrekKlasse <- ceiling(max(df$Omtrek, na.rm = TRUE)/50)
  df <-
    mutate(df,
           BladverliesNetto = as.numeric(BladverliesNetto),
           Soortnummer = as.integer(Soortnummer),
           BVKlasseEur = cut(BladverliesNetto,
                             breaks = c(0, 10, 25, 60, 99, 100),
                             include.lowest = TRUE,
                             labels = c("0-10%", "10+-25%", "25+-60%", "60+-99%", "100%")),
           Beschadigd = cut(BladverliesNetto,
                            breaks = c(0,25,100),
                            include.lowest = TRUE,
                            labels = c("onbeschadigd", "beschadigd")),
           BeschadigdNum = as.numeric(Beschadigd) - 1,
           Dood = cut(BladverliesNetto,
                      breaks = c(0, 99.9, 100),
                      include.lowest = TRUE,
                      labels = c("levend", "dood")),
           LeeftijdsklasseEur = cut(Leeftijd,
                                    breaks = c(0, 59, Inf),
                                    labels = c("jong", "oud"),
                                    include.lowest = TRUE),
           OmtrekklasseEur = cut(Omtrek,
                                  breaks = 0:maxOmtrekKlasse * 50,
                                  include.lowest = TRUE,
                                  labels = paste0(0:(maxOmtrekKlasse - 1)*50, "-", 1:maxOmtrekKlasse*50, "cm")),
           BVKlasse5 = cut(BladverliesNetto,
                           breaks = c(0, 1, seq(5,95,by = 5), 99, 100),
                           include.lowest = TRUE,
                           labels = paste0(c(0,1 + 0:19*5, 100),
                                           c("", rep("-", 20), ""),
                                           c("", 1:19*5, 99, ""),
                                           c(rep("%", 21), "% dode boom")
                                           )),
           BVKlasse10 = cut(BladverliesNetto,
                            breaks = c(0:10*10), include.lowest = TRUE,
                            labels = paste0(c(0, seq(11,91, by = 10)), "-",
                                            c(seq(10,100,by = 10)), "%")),
           BVKlassePiechart = cut(BladverliesNetto,
                                  breaks = c(0,10,25,40,100),
                                  include.lowest = TRUE,
                                  labels = paste(c(0,11,26,41), c(10,25,40,100), sep = "-")),
           JaarS2 = ifelse(Jaar == tweejaarlijks[1], "J1",
                           ifelse(Jaar == tweejaarlijks[2], "J2", NA)),
           JaarS3 = ifelse(Jaar == driejaarlijks[1], "J1",
                           ifelse(Jaar == driejaarlijks[2], "J2",
                                  ifelse(Jaar == driejaarlijks[3], "J3", NA))),
           prbo = paste0(PlotNr, BoomNr)) %>%
    left_join(tree_indeling, by = c("Soortnummer" = "SPEC_EUR_CDE"))
  df
}

#######################################################################

get_symptomdata <- function(channel, jaar, sql, show_query = FALSE) {
  jaarstring <- paste(jaar, collapse = ",")
  whereClause = paste0(" where w.WRNG_JAA in (", jaarstring, ")")
  sql <- c(sql, whereClause)
  sql <- paste(sql, collapse = "\n")
  if (show_query) cat(sql)

  df <-
    dbGetQuery(channel, sql, stringsAsFactors = FALSE, nullstring = -1) %>% 
    mutate(SymptoomCode = as.numeric(SymptoomCode), 
           SymptoomOorzaakCode = as.numeric(SymptoomOorzaakCode), 
           AangetastDeelCode = as.numeric(AangetastDeelCode))
  
  print(str(df))
  
  df <- df %>%
    mutate(OnderdeelBoomCat = if_else(AangetastDeelCode < 0, "Unknown",
                                     if_else(AangetastDeelCode < 20,
                                     "Bladeren",
                                     if_else(AangetastDeelCode < 30,
                                            "Takken", "Stam"))),
           SymptoomExtent = if_else(SymptoomMeting < 0, "Unknown",
                                   if_else(SymptoomMeting > 1, "Zware schade", "Beperkte schade")),
           Symptoom = replace(Symptoom, Symptoom == "-1", "NVT"),
           SymptoomSpecificatie = replace(SymptoomSpecificatie, SymptoomSpecificatie == "-1", "Unknown"),
           SymptoomExtent = if_else(SymptoomMeting < 0, "Unknown",
                                    if_else(SymptoomMeting < 2, "Beperkte schade", "Zware schade")),
           SymptoomVerkleurd = ifelse(SymptoomCode %in% c(2,3), TRUE, FALSE), #nodig omdat bomen beide verkleuringen samen kunnen hebben
           SymptoomAbnormaalVerkleurd = ifelse(SymptoomCode %in% c(2,3) & SymptoomMeting > 1, TRUE, FALSE ),
           LeeftijdSchade = replace(LeeftijdSchade, LeeftijdSchade == "-1", "Unknown"),
           SymptoomOorzaak = replace(SymptoomOorzaak, SymptoomOorzaak == "-1", "Unknown"),
           SymptoomOrganisme = replace(SymptoomOrganisme, SymptoomOrganisme == "-1", "Unknown"),
           SymptoomOorzaakGroep = ifelse(SymptoomOorzaakCode < 1000,
                                         floor(SymptoomOorzaakCode/100)*100,
                                         floor(SymptoomOorzaakCode/10000)*100),
           SymptoomOorzaakGroepNaam = factor(SymptoomOorzaakGroep,
                                             levels = c(100,200,300,400,500,800,900),
                                             labels = c("vraat (wild, vee)", "insecten", "schimmels", "abiotisch",
                                                        "mens", "andere", "onbekend")),
           AantastingsKey = paste(MetingKey, AangetastDeelCode, SymptoomCode, SymptoomSpecCode, sep = "_"))
  df
}



############################################################

get_SymptomAnalysisdata <- function(trees, symptoms) {
  left_join(trees, symptoms, by = "MetingKey")
}

###

get_symtomsummary <- function(trees, symptoms) {
  str(trees$MetingKey)
  str(symptoms$MetingKey)
  df <-
    left_join(trees, symptoms, by = c("MetingKey")) %>%
    group_by(WaarnemingKey, Jaar, PlotNr, BoomNr)

  dfsum <-
   summarize(df,
             AantalSymptomen = sum(!is.na(SymptoomCode)))
  dfsum
}



