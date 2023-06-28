library(tidyverse)
library(sf)
library(leaflet)
library(n2khab)
library(mapview)
library(units)
library(kableExtra)
library(grtsdb)
library(crosstalk)

meetpunten <- read_csv2("../data/coördinaten_proefvlakken_bosvitaliteitsmeetnet_2022.csv")

meetpunten_sf <- st_as_sf(meetpunten,
                          coords = c("x", "y"), crs = 31370)


grid_orig <- read_csv2("../data/gridpunten.csv")

grid_orig_sf <- st_as_sf(grid_orig, coords = c("LambertX", "LambertY"),
                         crs = 31370)

grid_4 <- grid_orig_sf %>%
  filter(Grid == "4x4") %>%
  dplyr::select(id_grid = ID)


# poging om het grid te transformeren naar de oorspronkelijke projectie --> zou dan een perfect grid moeten vormen
# maar oorspronkelijke projectie is onvoldoende gedocumenteerd om te kunnen reconstrueren
# verschillende projecties worden hieronder uitgeprobeerd, zonder resultaat

try_projection <- function(crs_select, grid) {
  
  grid_transform <- grid %>%
    st_transform(crs = crs_select)
  
  grid_coordinates <- grid_transform %>%
    mutate(x = st_coordinates(grid_transform)[, 1],
           y = st_coordinates(grid_transform)[, 2]) %>%
    st_drop_geometry() %>%
    mutate(verschil_x = x - dplyr::lag(x),
           verschil_y = y - dplyr::lag(y),
           crs = crs_select)
  
}

grid_test <- grid_orig_sf %>%
  head(15)

search_projection <- bind_rows(
  try_projection(3035, grid_test),
  try_projection(3034, grid_test),
  try_projection(3043, grid_test),
  try_projection(3395, grid_test),
  try_projection(3857, grid_test),
  try_projection(4087, grid_test),
  try_projection(5649, grid_test),
  try_projection(5651, grid_test),
  try_projection(8857, grid_test),
  try_projection(23031, grid_test),
  try_projection(25831, grid_test),
  #try_projection(32600, grid_test),
  try_projection(32631, grid_test),
)


search_projection %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~crs, scales = "free")


################################################################
#INPUT FILES MISSING
################################################################

#kaartbladen samenvoegen (stafkaart )

# # read different files and merge them together
# 
# for (i in 1:43) {
#   
#   bos_i <- terra::rast(str_c("../data/Boswijzer_Vlaanderen_2021/Boswijzer_2021/GeoTIFF/BOS_",
#                              ifelse(i < 10, "0", ""), 
#                              i, 
#                              ".tif"))
#   
#   if (i == 1) {
#     
#     bos_vlaanderen <- bos_i  
#     
#   } else {
#     
#     bos_vlaanderen <- merge(bos_vlaanderen, bos_i)
#     
#   }
#   bos_vlaanderen <- bos_vlaanderen %>%
#     terra::merge(bos_i)
#   
# }
# 
# terra::writeRaster(bos_vlaanderen, "../data/boswijzer_vl_2021.tif")
# 
# plot(bos_vlaanderen)

####################################################################


bos_vl <- raster::raster("../data/boswijzer_vl_2021.tif")

meetpunten_grid <- meetpunten_sf %>%
  st_join(grid_4, join = st_nearest_feature) %>%
  group_by(geometry) %>%
  mutate(distance_to_meetpunt = min(st_distance(geometry, grid_4))) %>%
  ungroup() %>%
  st_drop_geometry()

grid_4_bos <- grid_4 %>%
  mutate(bos_extract = bos_vl[as(., "Spatial")]) %>%
  mutate(legacy_site = id_grid %in% meetpunten_grid$id_grid)

grid_4_bos %>%
  mutate(show_fillcolor = ifelse(bos_extract == 1, "green",
                                 ifelse(bos_extract == 0, "blue", "grey")),
         show_strokecolor = ifelse(legacy_site, "black", show_fillcolor)) %>%
  st_transform(4326) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(label = ~bos_extract, fillColor = ~show_fillcolor, color = ~show_strokecolor, fillOpacity = 0.5, opacity = 0.9) %>%
  addMarkers(data = st_transform(meetpunten_sf, 4326), label = ~naam)

overzicht <- grid_4_bos %>%
  st_drop_geometry() %>%
  group_by(bos_extract, legacy_site) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(bos_extract = ifelse(bos_extract == 0, "nee",
                              ifelse(bos_extract == 1, "ja", "geen data")),
         legacy_site = ifelse(legacy_site, "ja", "nee")) %>%
  dplyr::select(bos = bos_extract, "bestaand meetpunt" = legacy_site, "aantal girdpunten" = n)

overzicht %>%
  kable() %>%
  kable_styling()

### GRTS


#voorbeeld download zenodo
#Voorbereidingen (let op eenmalig)
#n2khab_data_path <- fileman_folders()
#soilmap_simple_path <- file.path(n2khab_data_path, "10_raw/flanders")
#dir.create(soilmap_simple_path)
#download_zenodo(doi = "10.5281/zenodo.3732903",
#                path = soilmap_simple_path)



flanders <- read_admin_areas(dsn = "flanders") #welke data?

flanders_buffer <- flanders %>%
  st_buffer(dist = 5000)

bbox_flanders_buffer <- st_bbox(flanders_buffer)

xmin <- floor(bbox_flanders_buffer$xmin/1000)*1000
ymin <- floor(bbox_flanders_buffer$ymin/1000)*1000
xmax <- ceiling(bbox_flanders_buffer$xmax/1000)*1000
ymax <- ceiling(bbox_flanders_buffer$ymax/1000)*1000


# we add a row or a column when we have an odd number of rows or columns
# otherwise the grid created with grtsdb package is not correct (point coordinates are not in the center of the cell)

xres <- (xmax - xmin)/1000
yres <- (ymax - ymin)/1000

is_odd <- function(x){round(x/2) != x/2}

if (is_odd(xres)) {
  xmin = xmin - 1000
}

if (is_odd(yres)) {
  ymin = ymin - 1000
}

db <- grtsdb::connect_db()

bbox <- rbind(
  x = c(xmin, xmax),
  y = c(ymin, ymax)
)

cellsize <- 4000

add_level(bbox = bbox, cellsize = cellsize, grtsdb = db)


grts_sample <- extract_sample(
  grtsdb = db, samplesize = 10000, bbox = bbox, cellsize = cellsize
)

grts_sample_sf <- grts_sample %>%
  st_as_sf(coords = c("x1c", "x2c"), crs = 31370) %>%
  st_join(flanders_buffer) %>%
  filter(!is.na(name))

grts_sample_polygons_sf <- grts_sample %>%
  mutate(geom = str_c("POLYGON ((", x1c - 2000, " ", x2c - 2000, 
                      ",", x1c + 2000, " ", x2c - 2000,
                      ",", x1c + 2000, " ", x2c + 2000,
                      ",", x1c - 2000, " ", x2c + 2000,
                      ",", x1c - 2000, " ", x2c - 2000, "))")) %>%
  st_as_sf(wkt = "geom", crs = 31370) %>%
  st_join(flanders_buffer) %>%
  filter(!is.na(name))

sample_grid <- grid_4_bos %>%
  st_join(grts_sample_polygons_sf) %>%
  mutate(grts_type = "no legacy")

legacy_sites <- grid_4_bos %>%
  st_drop_geometry() %>%
  mutate(x = st_coordinates(grid_4_bos)[,1],
         y = st_coordinates(grid_4_bos)[,2]) %>%
  filter(legacy_site) %>%
  dplyr::select(x, y) %>%
  as.matrix()

add_legacy_sites(legacy = legacy_sites, bbox = bbox, cellsize = cellsize)

grts_sample_legacy <- extract_legacy_sample(samplesize = 10000, bbox = bbox, cellsize = cellsize)

grts_sample_legacy_sf <- grts_sample_legacy %>%
  st_as_sf(coords = c("x1c", "x2c"), crs = 31370) %>%
  st_join(flanders_buffer) %>%
  filter(!is.na(name))

grts_sample_legacy_polygons_sf <- grts_sample_legacy %>%
  mutate(geom = str_c("POLYGON ((", x1c - 2000, " ", x2c - 2000, 
                      ",", x1c + 2000, " ", x2c - 2000,
                      ",", x1c + 2000, " ", x2c + 2000,
                      ",", x1c - 2000, " ", x2c + 2000,
                      ",", x1c - 2000, " ", x2c - 2000, "))")) %>%
  st_as_sf(wkt = "geom", crs = 31370) %>%
  st_join(flanders_buffer) %>%
  filter(!is.na(name)) 

sample_grid_legacy <- grid_4_bos %>%
  st_join(grts_sample_legacy_polygons_sf) %>%
  mutate(grts_type = "legacy")

overzicht <- sample_grid %>%
  bind_rows(sample_grid_legacy) %>%
  st_drop_geometry() %>%
  group_by(grts_type, legacy_site) %>%
  summarise(mean_ranking = mean(ranking)) %>%
  ungroup()

sample_select <- sample_grid_legacy %>%
  filter(bos_extract == 1 | legacy_site) %>%
  group_by(legacy_site) %>%
  mutate(ranking_rel = rank(ranking)) %>%
  ungroup()

sample_new <- sample_select %>%
  filter(!legacy_site)

sample_legacy <- sample_select %>%
  filter(legacy_site)

sd <- SharedData$new(st_transform(sample_new, crs = 4326))

crosstalk::filter_slider("ranking_rel", "Relatieve ranking", sd, column = ~ranking_rel)

sd %>%
  leaflet() %>%
  addTiles(group = "Open streetmap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI luchtfoto") %>%
  addCircleMarkers(color = ~ifelse(legacy_site, "blue", "yellow"), label = ~ranking_rel, group = "Nieuwe meetpunten op 4 km grid") %>%
  addCircleMarkers(data = st_transform(sample_legacy, 4326),color = ~ifelse(legacy_site, "blue", "yellow"), group = "Bestaande meetpunten op 4 km grid") %>%
  addMarkers(data = st_transform(meetpunten_sf, 4326), label = ~naam, group = "Bestaande meetpunten") %>%
  addLayersControl(
    baseGroups = c("Open streetmap", "ESRI luchtfoto"),
    overlayGroups = c("Nieuwe meetpunten op 4 km grid", "Bestaande meetpunten op 4 km grid", "Bestaande meetpunten"),
    options = layersControlOptions(collapsed = FALSE)
  )

sample_new %>%
  st_drop_geometry() %>%
  dplyr::select(id_grid, ranking, ranking_rel, x = x1c, y = x2c) %>%
  arrange(ranking_rel) %>%
  write_csv2("../output/meetpunten_potentieel.csv")
