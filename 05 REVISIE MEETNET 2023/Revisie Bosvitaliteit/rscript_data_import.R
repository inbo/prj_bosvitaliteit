#data_opslag.R

remotes::install_github("inbo/inbobosvitaliteit")
install.packages("webshot")
webshot::install_phantomjs(force = TRUE)
library(inbobosvitaliteit)
library(tidyverse)
library(sf)
library(leaflet)
library(webshot)
library(htmlwidgets)

#Inlezen data (enkel indien toegang tot INBO databases)

conn <- bosvitaliteit_connect() #enkel wanneer toegang tot INBO databases

df_soortinfo <- read.csv2("data/tree_indeling.csv", stringsAsFactors = FALSE)
tree_sql <- readLines("data/tree_info.SQL")

df_trend <- get_treedata(conn, jaar = 1995:2022,
                             tree_indeling = df_soortinfo,
                             sql = tree_sql) 
#df_trend <- readRDS("data/dfTrees_trend.RDS") #uit lokale save

df_trend <- df_trend %>%
  transmute(jaar = Jaar, 
            plot = PlotNr, 
            tree = BoomNr, 
            soort = Soort, 
            nnv = BladverliesNetto/100) %>%
  filter(!is.na(tree))
write_excel_csv2(df_trend, "data/inputdata.csv")

# Kaartje met locaties


startyears <- df_trend %>% 
  group_by(plot) %>% 
  summarise(start = min(jaar))

proefvlakken <- readr::read_csv2("data/coords_proefvlakken.csv") %>% 
  left_join(startyears, by = c('PROEFVLAK'= 'plot') ) %>% 
  mutate(startperiode = ifelse(start == 1987, 
                               '1987 (34)',
                               ifelse(start < 1995,
                                      '1988-1992 (5)',
                                      ifelse (start < 2018, 
                                              '1995 (30)', 
                                              '2019-2022 (9)'))))
proefvlakken <- sf::st_as_sf(proefvlakken, 
                             coords=c("LONGITUDE (X)","LATITUDE (Y)"), 
                             crs=31370)
proefvlakken_wgs84 <- st_transform(proefvlakken, 4326)

startperiode_colors <- colorFactor(palette = "viridis", 
                                   domain = proefvlakken_wgs84$startperiode)

m <- leaflet(proefvlakken_wgs84) %>%
  addTiles(options = tileOptions(opacity = 0.5)) %>%
  setView(lng = mean(st_coordinates(proefvlakken_wgs84)[, "X"])-0.2,
          lat = mean(st_coordinates(proefvlakken_wgs84)[, "Y"]),
          zoom = 8) %>%
  addCircleMarkers(data = proefvlakken_wgs84,
                   lng = ~st_coordinates(geometry)[, "X"],
                   lat = ~st_coordinates(geometry)[, "Y"],
                   color = ~startperiode_colors(startperiode),
                   radius = 5,
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   popup = ~paste("Number:", PROEFVLAK, "<br>", "Name:", NAAM, "<br>", "Startperiode:", startperiode))
m

saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "data/punten_in_vlaanderen.png",
        cliprect = c(150,150,640,400))
