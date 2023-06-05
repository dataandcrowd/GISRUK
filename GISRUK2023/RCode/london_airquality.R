library(openair)
library(tidyverse)
library(purrr)
options(warn = -1)

london_meta <- importMeta(source = "kcl", all = T) %>% 
  filter(la_id %in% c(1:33),
         is.na(ClosingDate),
         !is.na(latitude),
         latitude > 51) # excluding one station near brighton

london_meta %>% 
  ggplot(aes(longitude, latitude, colour = site_type)) +
  geom_point()


unique(london_meta$site_type)

london_meta %>% filter(site_type %in% c("Roadside", "Kerbside")) -> london_road
london_meta %>% filter(site_type %in% c("Suburban", "Urban Background", "Industrial")) -> london_back

#############
library(mapboxapi)
library(leaflet)

leaflet() %>% 
  addMapboxTiles(style_id = "light-v10",
                 username = "mapbox") %>%
  addCircleMarkers(data = london_back,
                   label = london_back$site,
                   color = "blue",
                   radius = 1.5) %>% 
  addCircleMarkers(data = london_road,
                   label = london_road$site,
                   color = "red",
                   radius = 1.5) 


##############
no2_road_raw <- lapply(1:nrow(london_road), function(i){
importKCL(london_road$code[i], year = 2019) 
})

no2_road_raw[lengths(no2_road_raw) != 0] -> no2_road_new
which(map_lgl(no2_road_new, ~ any(names(.) == "no2")))
no2_road_new[-c(28, 49)] -> no2_road

map(no2_road, ~ .x %>% dplyr::select(date, code, site, no2)) %>% 
  map_df(tibble::as_tibble) -> no2_road_tib

unique(no2_road_tib$site)
#

no2_back_raw <- lapply(1:nrow(london_back), function(i){
  importKCL(london_back$code[i], year = 2019) 
})
no2_back_raw[lengths(no2_back_raw) != 0] -> no2_back_new
which(map_lgl(no2_back_new, ~ any(names(.) == "no2"))) -> findmissingindex
seq2 <- min(findmissingindex):max(findmissingindex)
seq2[!seq2 %in% findmissingindex]


no2_back_new[-c(seq2[!seq2 %in% findmissingindex])] -> no2_back

map(no2_back, ~ .x %>% dplyr::select(date, code, site, no2)) %>% 
  map_df(tibble::as_tibble) -> no2_back_tib


########

no2_back_tib %>% 
  rename(timestamp = date) %>% 
  mutate(date = lubridate::as_date(timestamp),
         hour = lubridate::hour(timestamp), 
         dn = case_when(hour >= 8 & hour <= 17 ~ "Work", 
                        TRUE ~ "Home")) %>% 
  filter(date == "2019-04-18") %>% 
  drop_na() %>% 
  group_by(code, site, dn) %>% 
  summarise(no2 = mean(no2, na.rm = T)) %>% 
  arrange(dn) %>% 
  ungroup() %>% 
  left_join(london_back, by = c("code", "site")) -> no2_back_dn

no2_back_dn %>% 
  ggplot(aes(x= no2, fill= dn)) +
  geom_histogram(binwidth=10, colour = "black") +
  facet_wrap(~dn)

#
no2_road_tib %>% 
  rename(timestamp = date) %>% 
  mutate(date = lubridate::as_date(timestamp),
         hour = lubridate::hour(timestamp), 
         dn = case_when(hour >= 8 & hour <= 17 ~ "Work", 
                        TRUE ~ "Home")) %>% 
  filter(date == "2019-04-18") %>% 
  drop_na() %>% 
  group_by(code, site, dn) %>% 
  summarise(no2 = mean(no2, na.rm = T)) %>% 
  arrange(dn) %>% 
  ungroup() %>% 
  left_join(london_road, by = c("code", "site")) -> no2_road_dn


no2_road_dn %>% 
  group_by(site_type) %>% 
  summarise(no2 = mean(no2),
            n= n())

##########

no2_back_dn %>% select(dn, no2) %>% group_by(dn) %>% summarise(no2 = mean(no2)) -> ratio_back
no2_road_dn %>% select(dn, site_type, no2) %>% group_by(dn, site_type) %>% summarise(no2 = mean(no2)) -> ratio_road

ratio_road %>% 
  left_join(ratio_back, by = "dn") %>% 
  rename(no2_rd = no2.x,
         no2_bk = no2.y) %>% 
  mutate(ratio = no2_rd / no2_bk)


########
library(gstat)
library(sf)
#library(raster)

london_gis_admin <- read_sf("GIS/London_Boundary_cleaned.shp")
london_gis_road <- read_sf("GIS/laei-2019-major-roads-final-unique-toids-flows-speeds-osgb.shp")

unique(london_gis_road$Road_Class)

ggplot() +
  geom_sf(data = london_gis_road, aes(color = Road_Class))


### Ratio
london_gis_road %>% 
  # mutate(X = unlist(map(geometry,1)),
  #        Y = unlist(map(geometry,2))) %>% 
  # dplyr::select(X, Y, Road_Class) %>% 
  st_drop_geometry() %>% 
  group_by(Road_Class) %>% 
  summarise() %>%   
  mutate(ratio_home = case_when(Road_Class == "A Road"~ 1.31,
                                Road_Class == "B Road"~ 1.20,
                                .default = 1.10),
         ratio_work = case_when(Road_Class == "A Road"~ 1.79,
                                Road_Class == "B Road"~ 1.37,
                                .default = 1.20)) -> gis




## Kriging
# no2_back_dn %>% 
#   st_as_sf(coords = c("os_grid_x", "os_grid_y"), 
#            crs = 27700, agr = "constant") %>% 
#   filter(dn == "Home") %>% 
#   as_Spatial() -> no2_sp_home
# 
# no2_back_dn %>% 
#   st_as_sf(coords = c("os_grid_x", "os_grid_y"), 
#            crs = 27700, agr = "constant") %>% 
#   filter(dn == "Work") %>% 
#   as_Spatial() -> no2_sp_work


no2_back_dn %>% 
  st_as_sf(coords = c("os_grid_x", "os_grid_y"), 
           crs = 27700, agr = "constant") %>% 
  filter(dn == "Work") %>% 
  as_Spatial() -> no2_sp


myVario <- automap::autofitVariogram(no2 ~ 1, no2_sp)
#myVario <- automap::autofitVariogram(no2 ~ dn, no2_sp)
class(myVario)
plot(myVario)

grid <- read_csv("GIS/London_Grid.csv") ## 500m x 500m resolution

grid
sp::gridded(grid) <- ~ right+top
kriging_result <- automap::autoKrige(no2~1, no2_sp, grid)
plot(kriging_result)


kriging_result$krige_output@coords %>% 
  as_tibble() -> kriging_df
  
kriging_result$krige_output@data %>% 
  as_tibble() -> kriging_data

bind_cols(kriging_df, kriging_data) %>% 
  rename(X = right,
         Y = top,
         no2 = var1.pred) %>% 
  select(X, Y, no2) -> kriging_df_combined


kriging_df_combined %>% 
  ggplot() +
  geom_tile(aes(x = X, y = Y, fill = no2)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA) +#, limits = c(10,70), breaks = c(20,40,60)) +
  geom_sf(data = london_gis_admin, color = 'black', size = 2, fill=NA) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 20),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15)
  ) 

ggsave("London_NO2_kriged.jpg", width = 5, height = 5, dpi = 300)

###
library(sp)
library(raster)
kriging_df_combined %>% 
  raster::rasterFromXYZ(crs="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs",
                digits=5) -> ras_krige

ras_road <- raster::raster("GIS/London_Road2.tif") 
resample <- raster::resample(ras_krige, ras_road, method = "bilinear") # resample 


#resample1 <- raster::merge(ras_road, resample) # merge

#writeRaster(resample, 'resample.tif', overwrite=T)

values(ras_road)[values(ras_road) == 1] <- 1.79
values(ras_road)[values(ras_road) == 2] <- 1.2
values(ras_road)[values(ras_road) == 3] <- 1.1

#writeRaster(ras_road, 'road.tif', overwrite=T)


gis_kr <- overlay(resample, ras_road, fun = function(x,y){ifelse(y != 0, x*y, x)})

plot(gis_kr)


### road
ras <- xyFromCell(gis_kr, 1:ncell(gis_kr))
krige.df <- as.data.frame(gis_kr) 

ras_krige_df <- 
  data.frame(ras, krige.df) %>% 
  pivot_longer(!c("x", "y"), names_to = "Day", values_to = "NO2") 
  


###
ras1 <- xyFromCell(resample, 1:ncell(resample))
krige.df1 <- as.data.frame(resample) 

ras_krige_df1 <- 
  data.frame(ras1, krige.df1) %>% 
  pivot_longer(!c("x", "y"), names_to = "Day", values_to = "NO2") 




###

ras_krige_df1 %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = NO2)) +
  geom_tile(data= ras_krige_df, aes(x = x, y = y, fill = NO2)) +
  geom_sf(data = london_gis_admin, color = 'grey50', alpha = 0.1, fill=NA) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = c(0, 90), breaks = c(0, 30, 60, 90)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 20),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15)
  ) 

ggsave("London_NO2_kriged_final.jpg", width = 5, height = 5, dpi = 300)
