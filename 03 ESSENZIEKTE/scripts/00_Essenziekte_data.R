library(tidyverse)
library(INBOtheme)
library(readxl)
library(DBI)
library(odbc)
library(lme4)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(brms::ar)
conflicted::conflicts_prefer(Matrix::expand)
conflicted::conflicts_prefer(brms::lognormal)
conflicted::conflicts_prefer(brms::ngrps)
conflicted::conflicts_prefer(Matrix::pack)
conflicted::conflicts_prefer(Matrix::unpack)

### instelvariabelen
es_firstyear <- 2014
es_lastyear <- 2025
es_prevyear <- es_lastyear - 1
es_2yearsago <- es_lastyear - 2
es_jaren <- es_firstyear:es_lastyear
all_zero_tolerance <- 95
fig_width <- 7
fig_height <- 5
es_min_oorz <- 5 * length(es_jaren) # een oorzaak moet minstens gemiddeld 5 keer per jaar voorkomen voordat deze in de analyse komt


# if (!dir.exists("outputEs_levend")) dir.create("outputEs_levend")
# if (!dir.exists("outputEs_allebm")) dir.create("outputEs_allebm")
# if (!dir.exists("outputEs_trend")) dir.create("outputEs_trend")

if (!dir.exists("output_es")) dir.create("output_es")
if (!dir.exists("output_es_trend")) dir.create("output_es_trend")

options(dplyr.summarise.inform = FALSE) # geen messages als je .groups niet gebruikt
alleen_levende_bomen <- FALSE # LET OP, beiden runnen
# if (alleen_levende_bomen) {
#   out <- "outputEs_levend/lv_"
# } else {
#   out <- "outputEs_allebm/all_"
# }
out <- "output_es"


con <- DBI::dbConnect(odbc::odbc(),
  driver = "SQL Server",
  server = "inbo-sql07-prd.inbo.be",
  database = "D0027_00_Essen"
)
con
DBI::dbListTables(con, table_type = "TABLE", schema_name = "dbo")

prv_es_textuur_path <-
  "03 ESSENZIEKTE/data/PROEFVLAKKEN ES en TEXTUUR DRAINAGE.xlsx"

sqlcodemeting <- "
select
  waarneming_id = wn.Id
, proefvlak_id = wn.ProefvlakID
, proefvlak_nummer = pv.ProefvlakNummer
, jaar = wn.Jaar
, datum = wn.Datum
, meting_id =  m.Id
, boom_in_proevlak_id = m.BoomID
, boom_id = concat(pv.ProefvlakNummer, '_', b.ID)
, omtrek = m.Omtrek
, omtrekklasse_code = ok.Code
, omtrekklasse = ok.Beschrijving
, omtrekklasse_sorteerorde = ok.SortOrder
, bladverlies = m.Bladverlies
, taksterfte_code = tst.Code
, taksterfte = tst.Beschrijving
, necrose_code = nt.Code
, necrose = nt.Beschrijving
, waterscheuten_code = wst.Code
, waterscheuten = wst.Beschrijving
, kroonverbossing_code = kvt.Code
, kroonverbossing = kvt.Beschrijving
, zaadzetting_code = zzt.Code
, zaadzetting = zzt.Beschrijving
, zaadleeftijd_code = zlk.Code
, zaadleeftijd = zlk.Beschrijving
, essenmijt_code = ebm.Code
, essenmijt = ebm.Beschrijving
, leeftijd_id = pv.Id
, leeftijd = lft.Beschrijving
, boom_nr = b.Nummer
, gemeenschappelijk = b.GemeenschappelijkeSteekproef
, verwijderingsjaar = b.VerwijderingJaar
, verwijderingstype_code = vwt.Code
, verwijderingstype = vwt.Beschrijving
, afwijkend = b.Afwijkend
, opm_waarneming  = wn.Opmerking
, opm_meting =  m.Opmerking
, opm_boom = b.Opmerking
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
left join WaterBeschikbaarheidType wbt on wbt.Id = pv.WaterBeschikbaarheidTypeId"
# where b.GemeenschappelijkeSteekproef = 1"

sqlcodesymptoom <- "
select
  meting_symptoom_id = ms.Id
, meting_id = MetingID
, boom_in_proefvlak_id = b.ID
, boom_id = concat(pv.ProefvlakNummer, '_', b.ID)
, proefvlak_nummer = pv.ProefvlakNummer
, aantasting_code = atp.Code
, aantasting = atp.BeschrijvingNL
, symptoom_code = st.Code
, symptoom = st.BeschrijvingNL
, symptoom_spec_code = sst.Code
, symptoom_spec = sst.BeschrijvingNL
, symptoomgraad_code = sg.Code
, symptoomgraad = sg.Beschrijving
, leeftijdklasse_code = lk.Code
, leeftijdKlasse = lk.Beschrijving
, opm_symptoom = ms.Opmerking
from MetingSymptoom ms
left join Meting met on met.ID = ms.metingID
left join Waarneming wrn on wrn.ID = met.waarnemingId
left join Proefvlak pv on pv.ID = wrn.ProefvlakID
left join Boom b on b.ID = met.BoomID
left join AantastingType atp on atp.Id = ms.AantastingTypeId
left join SymptoomType st on st.Id = ms.SymptoomTypeId
left join SymptoomSpecificatieType sst on sst.Id = ms.SymptoomSpecificatieTypeId
left join SymptoomGraad sg on sg.Id = ms.SymptoomGraadId
left join LeeftijdKlasse lk on lk.Id = ms.LeeftijdKlasseId"
# where b.GemeenschappelijkeSteekproef = 1"

sqlcodeorganisme <- "
select
  meting_symptoom_id = mso.MetingSymptoomID
, oorzaaktype_code = ot.Code
, oorzaaktype = ot.BeschrijvingNL
, oorzaakorganisme_code = oot.Code
, oorzaakorganisme = oot.Beschrijving
from MetingSymptoomOorzaak mso
left join OorzaakType ot on ot.Id = mso.OorzaakTypeId
left join OorzaakOrganismeType oot on oot.Id = mso.OorzaakOrganismeTypeId"


colorscale_es5 <- c("red", "orange", "gold", "green4", "blue")
leeftijden_es <- c(">= 20 en <= 59", ">= 60", "Gemengd", "Onbekend", "NA in DB", "alles")

##############################################################################################

# Lees de data in op boomniveau per jaar waarvoor een meting_id gekend is
# 5 waarningen verdwijnen wegens ontbreken meting_id
df_esmeting_all <-
  DBI::dbGetQuery(con, sqlcodemeting) %>%
  # filter(!is.na(meting_id)) |>
  # filter(GS == TRUE) %>% # niet meer gebruiken, want teveel bomen gestorven
  mutate(
    jaar_c = jaar - 2014,
    bladverliesklasse_euro = cut(bladverlies, c(0, 10, 25, 60, 99, 100),
      include.lowest = TRUE
    ),
    bladverliesklasse_10 = cut(bladverlies, 0:10 * 10,
      include.lowest = TRUE
    ),
    beschadigd = cut(bladverlies, c(0, 25, 100),
      include.lowest = TRUE, label = c("ok", "beschadigd")
    )
  )

#  --> vraagje voor Geert: bomen die het laatste jaar sterven, moeten die er ook uit, of worden daar wel symptomen voor bepaald?

###

alle_bomen <- df_esmeting_all |>
  group_by(proefvlak_nummer, boom_id) |>
  summarise(
    aantal = n(),
    min_bladverlies = min(bladverlies, na.rm = TRUE),
    max_bladverlies = max(bladverlies, na.rm = TRUE),
    is_gemeenschappelijk = sum(gemeenschappelijk, na.rm = TRUE) > 0
  )


# een boom die geen enkel record bladverlies 100 heeft, is nog in leven
overlevende_bomen <- df_esmeting_all %>%
  group_by(proefvlak_nummer, boom_id) %>%
  summarize(max_schade = max(bladverlies, na.rm = TRUE)) %>%
  filter(max_schade < 100) |>
  pull(boom_id)

dode_bomen <- df_esmeting_all %>%
  group_by(proefvlak_nummer, boom_id) %>%
  summarize(max_schade = max(bladverlies, na.rm = TRUE)) %>%
  filter(max_schade == 100) %>%
  pull(boom_id)

###

df_esmeting_levend <- df_esmeting_all %>% filter(boom_id %in% overlevende_bomen)

df_esmeting <- df_esmeting_all

df_esmeting_pivot <- df_esmeting |>
  summarise(aantal_bomen = n(), .by = c(proefvlak_nummer, jaar)) %>%
  mutate(jaar = as.numeric(jaar)) |>
  arrange(jaar) |>
  pivot_wider(
    id_cols = proefvlak_nummer,
    names_from = jaar,
    values_from = aantal_bomen,
    values_fill = 0
  )

write_csv2(df_esmeting_pivot,
  file = paste0(out, "aantal_bomen_proefvlak_jaar.csv")
)

# lezen proefvlakeigenschappen
df_proefvlak_eig <- read_excel(prv_es_textuur_path) %>%
  rename(
    proefvlak_nummer = Nummer,
    naam = Naam,
    plaats = Plaats,
    bodemserie = Bodemserie,
    textuur = Textuur,
    drainage = Drainage,
    textuurgroep = `Textuur-gegroepeerd`,
    drainagegroep = `Drainage-gegroepeerd`,
    waterbeschikbaarheid = Waterbeschikbaarheid
  ) |>
  mutate(
    drainagegroep =
      case_match(drainagegroep,
        c("1121", "413019") ~ "abcd",
        c("767", "302") ~ "efghi",
        .default = drainagegroep
      )
  )

all(unique(df_esmeting$proefvlak_nummer) %in% df_proefvlak_eig$proefvlak_nummer)

df_esmeting <- df_esmeting |>
  left_join(df_proefvlak_eig, by = "proefvlak_nummer")

df_esmeting_levend <- df_esmeting_levend |>
  left_join(df_proefvlak_eig, by = "proefvlak_nummer")


# Lees de symptomendata in. Voeg kerngezonde bomen toe als symptoom "00" als ze niet in DB staan
symptomen_zonder_graad <- c("10", "11", "12", "21", "22")
symptomen_voorwaardelijk_graad <- "13"
symptomen_voorwaardelijk_aantasting <- c("31", "32", "33", "34")

df_essymptoom_all <- DBI::dbGetQuery(con, sqlcodesymptoom) |>
  right_join(
    df_esmeting_all |>
      select(
        "proefvlak_id",
        "proefvlak_nummer",
        "jaar",
        "boom_id",
        "meting_id",
        "gemeenschappelijk"
      ),
    by = c("meting_id", "boom_id", "proefvlak_nummer")
  ) |>
  mutate(
    aantasting_code = case_when(
      is.na(meting_symptoom_id) ~ "00",
      TRUE ~ aantasting_code
    ),
    aantasting = case_when(
      is.na(meting_symptoom_id) ~ "No symptoms on any part of tree",
      TRUE ~ aantasting
    ),
    symptoom_code = case_when(
      aantasting_code == "00" ~ "00",
      aantasting_code == "04" ~ "99",
      TRUE ~ symptoom_code
    ),
    symptoom = case_when(
      aantasting_code == "00" ~ "no symptoms",
      aantasting_code == "04" ~ "Dead Tree",
      TRUE ~ symptoom
    ),
    symptoomgraad_code = case_when(
      aantasting_code == "00" ~ "0",
      aantasting_code == "04" ~ "7",
      TRUE ~ symptoomgraad_code
    ),
    symptoomgraad = case_when(
      symptoomgraad_code == "7" ~ "volle 100%",
      aantasting_code == "04" ~ "100%",
      aantasting_code == "00" ~ "0%",
      is.na(symptoomgraad) &
        (symptoom_code %in% symptomen_zonder_graad) ~ "aanwezig",
      is.na(symptoomgraad) &
        (symptoom_code == symptomen_voorwaardelijk_graad) &
        (aantasting_code %in% symptomen_voorwaardelijk_aantasting) ~ "aanwezig",
      TRUE ~ symptoomgraad
    )
  )


df_essymptoom_levend <- df_essymptoom_all %>%
  filter(boom_id %in% overlevende_bomen)

df_essymptoom_orig <- df_essymptoom_all

symptoomgraden <-
  DBI::dbGetQuery(
    con,
    "select code, beschrijving from SymptoomGraad order by SortOrder"
  ) |>
  rbind(data.frame(code = 100, beschrijving = "aanwezig"))


# Data met enkel deze waarbij er een symptoomoorzaak is gedefinieerd
df_essymptoomoorzaak_all <-
  DBI::dbGetQuery(con, sqlcodeorganisme) %>%
  dplyr::inner_join(
    df_essymptoom_all |>
      select(
        "proefvlak_id",
        "proefvlak_nummer",
        "jaar",
        "boom_id",
        "meting_id",
        "meting_symptoom_id",
        "gemeenschappelijk"
      ),
    by = "meting_symptoom_id"
  )
# filter(GS == TRUE) # niet meer relevant door de inner join

df_essymptoomoorzaak_levend <- df_essymptoomoorzaak_all |>
  filter(boom_id %in% overlevende_bomen)

df_essymptoomoorzaak <- df_essymptoomoorzaak_all

############################################


unieke_bomen <- df_esmeting %>%
  group_by(proefvlak_nummer, boom_id) %>%
  summarize(
    omtrek_start = min(omtrek, na.rm = TRUE),
    sterftejaar = min(jaar[bladverlies == 100]),
    aantal_metingen = n()
  )

df_esmeting <- df_esmeting %>%
  left_join(unieke_bomen) %>%
  mutate(gestorven = jaar > sterftejaar)

# if (alleen_levende_bomen) {
#   saveRDS(dfEsMeting, file = "data/interim/dfEsMeting_levend.Rds")
#   saveRDS(dfEsSymptoomOrig, file = "data/interim/dfEsSymptoom_levend.Rds")
#   saveRDS(dfEsSymptoomOorz, file = "data/interim/dfESymptoomOorz_levend.Rds")
#   saveRDS(symptoomgraden, file = "data/interim/symptoomgraden_levend.Rds")
#   saveRDS(unieke_bomen, file = "data/interim/unieke_bomen_levend.Rds")
# } else {
#   saveRDS(dfEsMeting, file = "data/interim/dfEsMeting.Rds")
#   saveRDS(dfEsSymptoomOrig, file = "data/interim/dfEsSymptoom.Rds")
#   saveRDS(dfEsSymptoomOorz, file = "data/interim/dfESymptoomOorz.Rds")
#   saveRDS(symptoomgraden, file = "data/interim/symptoomgraden.Rds")
#   saveRDS(unieke_bomen, file = "data/interim/unieke_bomen.Rds")
# }

(tot_aantal_bomen_jr <-
  df_esmeting %>%
  filter(jaar %in% es_jaren) %>%
  group_by(jaar) %>%
  summarise(tot_n_bomen = n_distinct(boom_id))) %>%
  write_csv2(path = paste0(out, "aantal_bomen.csv"))
tot_aantal_bomen_jr

(alle_boomids_jr <-
  df_esmeting %>%
  filter(jaar %in% es_jaren) %>%
  select(jaar, proefvlak_nummer, boom_id) %>%
  arrange(proefvlak_nummer, boom_id, jaar)) %>%
  write_csv2(path = paste0(out, "alle_boomids.csv"))
alle_boomids_jr


ggplot(alle_boomids_jr, aes(x = factor(jaar))) +
  geom_bar() +
  facet_wrap(~proefvlak_nummer)

ggplot(
  df_esmeting %>% group_by(proefvlak_nummer, jaar) %>% summarize(gem_bladverlies = mean(bladverlies)),
  aes(x = jaar, y = gem_bladverlies)
) +
  facet_wrap(~proefvlak_nummer) +
  geom_point() +
  geom_line()

ggplot(
  df_esmeting %>% filter(jaar %in% es_jaren, gestorven == FALSE) %>% group_by(proefvlak_nummer, jaar) %>% summarize(gem_bladverlies = mean(bladverlies)),
  aes(x = jaar, y = gem_bladverlies)
) +
  facet_wrap(~proefvlak_nummer) +
  geom_point() +
  geom_line()
