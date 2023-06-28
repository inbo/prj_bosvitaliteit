key = "1bXNbrrgQFB0nKhP69-Nc_lFMI-Rg-1axCGoPEIx1Po4"
library(googlesheets4)
library(tidyverse)
data_all <- read_sheet(key)
data_meetnet <- read_sheet(key, sheet = 2)
data16 <- data_all %>% filter(Grid == "16x16")
data04 <- data_all %>% dplyr::filter(Grid == "4x4")

p1 <- ggplot(data_all, aes(x = LambertX, y = LambertY, label = ID)) + 
  geom_text(size = 2) + coord_fixed() + facet_wrap(~Grid, ncol = 1)
ggsave(p1, filename = "gridpuntenLambert.png", width = 14, height = 14)

p2 <- ggplot(data_all, aes(x = Longitude, y = Latitude, label = ID)) + 
  geom_text(size = 2) + coord_fixed() + facet_wrap(~Grid, ncol = 1)
ggsave(p2, filename = "gridpuntenLatLon.png", width = 7, height = 7)

p3 <- ggplot(data04, aes(x = LambertX, y = LambertY)) + 
  geom_point(data = data_meetnet, color = "red", size = 1) + 
  geom_text(aes(label = ID), size = 1) + coord_fixed()
ggsave(p3, filename = "Meetnetpuntenopgrid.png", width = 7, height = 7)

write_excel_csv2(data_all, file = "gridpunten.csv")


ggplot(data16, aes(x = LambertX, y = LambertY, label = ID)) + geom_text(size = 2) + coord_fixed()
ggplot(data04, aes(x = LambertX, y = LambertY, label = ID)) + geom_text(size = 2) + coord_fixed()

diff(data16 %>% filter(ID %in% c(13, 21)) %>% pull(X))
diff(data16 %>% filter(ID %in% c(13, 29)) %>% pull(X))/2

# library(sf)
# data_sf = st_as_sf(data_all[c("LambertX", "LambertY")], coords = c("LambertX", "LambertY"), crs = 31370)
# data_latlon = st_transform(data_sf, 4326)
# 
# # Extract latitude and longitude and add to the original data frame
# data_all$Longitude = st_coordinates(data_latlon)[, "X"]
# data_all$Latitude = st_coordinates(data_latlon)[, "Y"]
# write_excel_csv2(data_all, file = "coordinates_transformed.csv")
# write_excel_csv(data_all, file = "coordinates_transformed2.csv", delim = '\t')
# 
# view(data_all)