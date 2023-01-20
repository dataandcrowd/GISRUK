options(scipen = 100, "rgdal_show_exportToProj4_warnings"="none")
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(gstat)
library(moments)

load("../data/no2.RData")
stations <- read_sf("../data/stations_10km.shp")
stations_df <- stations %>% filter (F.R == "Fixed")%>% st_set_geometry(NULL)
seoul <- read_sf("../data/Seoul_City.shp") %>% as('Spatial') %>% fortify()
no2.winter <- merge(no2.win.bk, stations_df, by.x = c("Station.ID", "X", "Y"), by.y = c("Station", "X", "Y"))
coordinates(no2.winter) <- ~X+Y
proj4string(no2.winter) <- CRS("+init=epsg:5181")


mid.feb <- no2.winter[153:158]

sk <- data.frame(Date = mid.feb@data %>% 
                   reshape2::melt(id.vars = NULL, variable.name = "Date", value.name = "no2") %>% 
                   dplyr::select(1) %>% unique(),
                 mean = colMeans(mid.feb@data) %>% round(2),
                 median = apply(mid.feb@data, 2, FUN = median) %>% round(2),
                 skewness = skewness(mid.feb@data) %>% round(2),
                 kurtosis = kurtosis(mid.feb@data) %>% round(2))


mid.feb@data %>% 
  reshape2::melt(id.vars = NULL, variable.name = "Date", value.name = "no2") %>% 
  ggplot(aes(x= no2, fill= Date)) +
  geom_histogram(binwidth=10, colour = "black")+
  geom_text(data = sk, aes(-Inf, Inf, label = paste0("mean = " , mean)), hjust = -0.05, vjust = 1.1, size = 3.5) +
  geom_text(data = sk, aes(-Inf, Inf, label = paste0("median = " , median)), hjust = -0.05, vjust = 2.1, size = 3.5) +
  geom_text(data = sk, aes(-Inf, Inf, label = paste0("skewness = " , skewness)), hjust = -0.05, vjust = 3.1, size = 3.5) +
  geom_text(data = sk, aes(-Inf, Inf, label = paste0("kurtosis = " , kurtosis)), hjust = -0.05, vjust = 4.1, size = 3.5) +
  facet_wrap(~Date) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 15)) -> hist_poll


ggsave("../results/Hist_no2_hist_02_S2.png", hist_poll, width = 10, height = 8)#, scale = 1.5)



##########
options(warn = -1) # don't print warnings
myVario <- list()
myList <- list()

for(i in 1:6){
  myVario[[length(myVario)+1]] <- variogram(mid.feb[[i]] ~ 1, mid.feb, cutoff = 30000, width = 3000)
  myList[[length(myList) + 1]]  <- fit.variogram(myVario[[i]], 
                                                 vgm(psill = 160,
                                                     nugget= 10,
                                                     model="Ste",
                                                     kappa = 50),
                                                 fit.kappa = TRUE, fit.method = 6) 
}

library(gridExtra)

p01 <- plot(myVario[[1]], myList[[1]], main = "Feb 15th\nDay hours")
p02 <- plot(myVario[[2]], myList[[2]], main = "Feb 15th\nNight hours")
p03 <- plot(myVario[[3]], myList[[3]], main = "Feb 16th\nDay hours")
p04 <- plot(myVario[[4]], myList[[4]], main = "Feb 16th\nNight hours")
p05 <- plot(myVario[[5]], myList[[5]], main = "Feb 17th\nDay hours")
p06 <- plot(myVario[[6]], myList[[6]], main = "Feb 17th\nNight hours")

varplot <- grid.arrange(p01, p02, p03, p04, p05, p06, 
                        nrow = 3)

ggsave("../results/Semivariogram_no2_semvario_02_S2.png",varplot, width = 6, height = 8)#, scale = 1.5)


### Data Frame
seoul_grid <- data.frame(expand.grid(X = seq(min(no2.winter$X), max(no2.winter$X), length=200),
                                     Y = seq(min(no2.winter$Y), max(no2.winter$Y), length=200)))
coordinates(seoul_grid) <- ~X+Y
proj4string(seoul_grid) <- CRS("+init=epsg:5181")
#https://gis.stackexchange.com/questions/157279/saving-results-in-automap-r-package-for-time-series-data


##############
#--Kriging--##
##############
pred.model <- seoul_grid@coords
var.model <- seoul_grid@coords


for(i in 1:6) {
  kriging_new <- krige(mid.feb@data[,i]~ X + Y,
                       mid.feb, 
                       seoul_grid,
                       #nmin = 20,
                       #nmax = 40,
                       model = myList[[i]])
  kriging_new$var_model <- data.frame(kriging_new$var1.var)
  var.model <- cbind(var.model, kriging_new$var_model)
  xyz <- as.data.frame(kriging_new$var1.pred)
  colnames(xyz) <- colnames(mid.feb@data)[i]
  pred.model <- cbind(pred.model, xyz)
} 


##-- Add ColNames
colnames(pred.model) <- c("X", "Y", "feb15d", "feb15n", "feb16d", "feb16n", "feb17d", "feb17n")
colnames(var.model) <- c("X", "Y", "feb15d", "feb15n", "feb16d", "feb16n", "feb17d", "feb17n")


#############
#- Plotting-#
#############

ras.krige.df <- pred.model %>% 
  reshape2::melt(id = c("X", "Y"), variable.name = "Hour", value.name = "NO2") 

ras.krige.df %>% 
  ggplot() +
  geom_tile(aes(x = X, y = Y, fill = NO2)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = c(10,70), breaks = c(20,40,60)) +
  geom_contour(aes(x = X, y = Y, z = NO2),bins = 30, colour = "grey40", alpha = 0.7) +
  geom_path(data = seoul, aes(x = long, y = lat), color = 'black', size = 1) +
  geom_text(data = stat, aes(-Inf, -Inf, label = paste0("mean = " , mean)), hjust = -.1, vjust = -2, size = 3.5) +
  geom_text(data = stat, aes(-Inf, -Inf, label = paste0("sd = " , sd)), hjust = -.15, vjust = -1, size = 3.5) +
  #geom_text(data = stat, aes(-Inf, Inf, label = paste0("RMSE=" , rmse)), hjust = -.1, vjust = 1.2, size = 3.5) +
  facet_wrap(~ Hour, ncol = 8) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 20),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15)                                  
  ) -> kriged # 1200 x 550 

# Export PNG
ggsave("../results/NO2_kriged_pred_02_S2.png", kriged, width = 10, height = 2, dpi = 300)

# convert to Raster Bricks
krige <- rasterFromXYZ(pred.model, 
                       crs="+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                       digits=5)

ras.road <- raster("../data/road_10km_re.tif")  # Import raster
res.mgcv <- resample(krige, ras.road, method = "bilinear") # resample 
res.mgcv <- merge(ras.road, res.mgcv) # merge

# assign road
road_01 = road_02 = road_03 = road_04 = road_05 = road_06 = ras.road

# stack raster and remove individual raster files
road.stack <- stack(road_01, road_02, road_03, road_04, road_05, road_06)
rm(road_01, road_02, road_03, road_04, road_05, 
   road_06)

# add road ratio values to GAM raster
ratio.mid.feb <- no2.win.ratio[149:154,]

for(i in 1:6){
  #road.stack[[i]] <- road.stack[[i]] * ratio.no2.sum$ratio[i]
  values(road.stack)[values(road.stack[[i]]) == 1] <- ratio.mid.feb$Back.Road.Ratio[i]
  values(road.stack)[values(road.stack[[i]]) == 2] <- ratio.mid.feb$Back.High.Ratio[i]
}

# add no2 and road values
r.poll.rd <- overlay(res.mgcv, road.stack, fun = function(x,y){ifelse(y != 0, x*y, x)})
names(r.poll.rd) <- c("feb15d", "feb15n", "feb16d", "feb16n", "feb17d", "feb17n")

#writeRaster(r.poll.rd, filename="../results/no2_02_S2_kr_final.tif", format="GTiff", overwrite=TRUE)

#####################
#ras.mgcv.df <- as.data.frame(r.poll.rd, xy = TRUE) # easy way
# however, since we resampled and changed our data
# with different resolution imamges and extent, the easier way doesn't work
ras <- xyFromCell(r.poll.rd, 1:ncell(r.poll.rd))
krige.df <- as.data.frame(r.poll.rd) 

##-- Find Mean and variance

ras.krige.stat <- data.frame(ras, krige.df)

stat1 <- ras.krige.stat %>% dplyr::select(-c(x,y)) %>% 
  gather(factor_key = T) %>% 
  group_by(key) %>% summarise(mean= round(mean(value),1), sd= round(sd(value),1), max = max(value),min = min(value)) %>% 
  rename(Hour = key)

#####
ras.krige.df <- data.frame(ras, krige.df) %>% 
  reshape2::melt(id = c("x", "y"), variable.name = "Hour", value.name = "NO2") 

ras.krige.df %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = NO2)) +
  scale_fill_distiller(palette = "Spectral", na.value = NA, limits = c(10,100), breaks = c(0,25,50,75,100)) +
  geom_text(data = stat1, aes(-Inf, -Inf, label = paste0("mean = " , mean)), hjust = -.1, vjust = -2, size = 3.5) + 
  geom_text(data = stat1, aes(-Inf, -Inf, label = paste0("sd = " , sd)), hjust = -.1, vjust = -1, size = 3.5) + 
  geom_path(data = seoul, aes(x = long, y = lat), color = 'black', size = 1) +
  facet_wrap(~ Hour, ncol = 8) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 20),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15)                                  
  ) -> final


# Export PNG
ggsave("../results/NO2_kriged_final_02_S2.png", final, width = 10, height = 2, dpi = 300)


