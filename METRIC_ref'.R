library(water)
library(tiff)
library(ape)
library(raster)
library(exiftoolr)
library(exifr)
library(rgdal)
library(lattice)
library(sp)
library(rasterVis)
library(sf)
library(ggplot2)
library(maptools)
library(latticeExtra)

proj4string <- "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"

# Directory 
setwd("C:/1Image Processing/Henley/08-20/QGIS/Soybean_METRIC")
options(warn=-1)

ROI <- readOGR("aoi.shp")
sensor_aoi<- readOGR("sensor_pol.shp") 
plot(ROI,
     main = "Shapefile imported into R - ROI",
     axes = TRUE,
     border = "blue")
aoi <-ROI

#  steps I followed:
# 1- Geo-reference in QGIS: georeference the thermal map by the blue band then save the georefereced thermal image 
# 2- Read Geo-referenced thermal image here for re-sampling
#T <- raster("corn_thermal.TIF")
T<- raster("thermal_final.tif")
plot(T, main = "Georef_Thermal")

# 3- Read Blue band to resample the thermal image based on the resolution of B band 
B <- raster ("blue.tif")
plot(B, main = "Blue Band")

# 4 Re-sample Thermal Image
# Resampling could be done in QGIS too but here was running two codes and faster.
T_r <- resample(T, B, method ="ngb")
plot(T_r, main = "Resample Thermal")

# 5- Write Resampled Thermal Image in the file
#writeRaster(T_r, filename="Roza_Vineyard_T50m_9_6_22_noalpha_reflectance_grayscale_1_1_Geo_Resample", format="GTiff", overwrite=TRUE)


#  Crop Blue Band with ROI to see if ROI cover the image or not
# I skipped this part, and used the QGIS, Raster, Extraction, Clip Raster by mask layer and crop all bands with same resolution using the ROI that we have
#B <- crop(B, ROI)
#plot(B, main = "Blue band")

# Correcting edges of ROI

#aoi <- createAoi(topleft = c(288925, 5125670), bottomright = c(289000, 5125510), EPSG = 32611) # Prosser : 32611, TOpennish : 32610

#B <- crop(B, aoi)
#plot(B, main = "Blue band")

#T <- crop(T, aoi)
#plot(T, main = "Cropped Thermal")

# I need to read the EXIF of the Blue band to know time of flight and date exactly to create our MTL file
#B_exif <- "Roza_Vineyard_M50m_9_6_22_noalpha_reflectance_blue_1_1.tif" # When you crop the bands with ROI and then enter it in R, you lose the exif information. Use the origical blue band to read exif
#exif <- exif_read(B_exif)
# use "DATE TIME ORIGINAL" for SCENE_CENTER_TIME in the MTL file
# MTL file needs to modify based on time of data collection from exif file and then use the suncalc page to find sun elevation
MTLfile <- "Soybean_MTL.txt"   #https://www.suncalc.org/#/46.3713,-120.4558,3/2021.01.27/19:27/1/1
csvfile <- "Weather_data_08_20.csv"

# Download 15 minutes interwal data from AgWeatherNet and organize it based on the file (Lat and Long can be find from station AWN)
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = "%d/%m/%Y", 
                              lat= 35.2832337, long= -85.9263302, elev=306.0, height= 2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 3,
                                        "wind" = 7, "RH" = 5, "temp" = 4, "rain" = 6),
                              tz="America/Chicago",
                              cf = c(1, 1, 1),
                              MTL = MTLfile)
plot(WeatherStation)

#Thermal
image.DN<- raster()
image.DN$B <- crop(raster("blue.tif"), extent(aoi)) # read raw data files from folder 
image.DN$B <- mask(image.DN$B,aoi)
image.DN$G <- crop(raster("green.tif"), extent(aoi))
image.DN$G <- mask(image.DN$G,aoi)
image.DN$R <- crop(raster("red.tif"), extent(aoi))
image.DN$R <- mask(image.DN$R,aoi)
image.DN$NIR <- crop(raster("nir.tif"), extent(aoi))
image.DN$NIR <- mask(image.DN$NIR,aoi)
image.DN$RE <- crop(raster("red edge.tif"), extent(aoi))
image.DN$RE <- mask(image.DN$RE,aoi)
image.DN$Thermal <- crop(T_r, extent(aoi))
image.DN$Thermal<- mask(image.DN$Thermal,aoi)

plot(image.DN) 

# Removing the NA
image.DN$B[is.na(image.DN$B[])] <- 1
image.DN$G[is.na(image.DN$G[])] <- 1
image.DN$R[is.na(image.DN$R[])] <- 1
image.DN$NIR[is.na(image.DN$NIR[])] <- 1
image.DN$RE[is.na(image.DN$RE[])] <- 1
#image.DN$Thermal[is.na(image.DN$Thermal[])] <- 1
image.DN$Thermal
plot(image.DN)

#checkSRTMgrids(image.DN) # Check if you have a DEM in your folder

DEM <- crop(raster("Soybean_multispectral_dtm_1_1.tif"), extent(aoi)) # I use Drone DEM from Multispectral images_Chnage the name on the file
DEM <- resample(DEM, image.DN, method ="ngb")
plot(DEM)
# Crop the drone DEM in QGIS using Raster, extraction clip. tbring same crop DEM in here.
surface.model <-METRICtopo(DEM)
surface.model$Slope[]<-0
surface.model$Aspect[]<-0
surface.model$DEM <- cellStats(surface.model$DEM, mean)
surface.model[[1]]@crs<- crs(DEM)


solar.angles.r <- solarAngles(surface.model = surface.model,
                              WeatherStation = WeatherStation, MTL = MTLfile)

Rs.inc <- incSWradiation(surface.model = surface.model,
                         solar.angles = solar.angles.r,
                         WeatherStation = WeatherStation)  # or mean (WeatherStation$alldata$radiation)
#Rs <- writeRaster(Rs.inc, filename="rsinc_test", format="GTiff", overwrite=TRUE)


# for red edge
albedo <- (image.DN$B)*0.28 + (image.DN$G)*0.246 + (image.DN$R)*0.19 + (image.DN$NIR)*0.123 + (image.DN$RE)*0.166
albedo
#albedo <- albedo(image.SR=image.SR, coeff="Tasumi", sat="L8")

#LAI <- LAI(method="metric2010", image=image.DN, L=0.5)  #L=0.5 for Vineyard
LAI <- LAI(method="metric2010", image=image.DN, L=0.1)
LAI

#Ts <- image.DN$Thermal+273.17
Ts <- image.DN$Thermal
plot(Ts)
Rl.out <- outLWradiation(LAI = LAI, Ts = Ts)
#plot(Rl.out)

Rl.inc <- incLWradiation(WeatherStation,DEM = surface.model$DEM, 
                         solar.angles = solar.angles.r, Ts= Ts)

Rn <- netRadiation(LAI, albedo, Rs.inc, Rl.inc, Rl.out)
G <- soilHeatFlux(image = image.DN, Ts = Ts, albedo = albedo,
                  Rn = Rn, LAI = LAI)
#FOR THE VINEYARD
#Z.om <- momentumRoughnessLength(LAI=LAI, mountainous = TRUE, fLAI = 1, h = 1.8, method = "Perrier", surface.model=surface.model)
#Z.om[Z.om <0.005] <- 0.005 #Bare soil
Z.om <- momentumRoughnessLength(LAI = LAI, mountainous = TRUE,
                                method = "short.crops",
                                surface.model = surface.model)
#Z.om <- momentumRoughnessLength(LAI = LAI, mountainous = TRUE,
    #                            method = "short.crops",
     #                           surface.model = surface.model)

# METRIC model hot cold pixel method
hot.and.cold <- calcAnchors(image = image.DN, Ts = Ts,
       LAI = LAI, plots = FALSE, albedo = albedo,
    Z.om = Z.om, n = 5,
   anchors.method = "flexible",
 deltaTemp = 1, verbose = FALSE)
hot.and.cold

#point_pixel<- st_as_sf(hot.and.cold, coords = c("X", "Y"), crs= proj4string)
#pixeltype<- as.character(hot.and.cold$type)
#plot(image.DN$Thermal)
#points(plot(st_geometry(point_pixel), add=TRUE, col=ifelse(pixeltype=="hot","red","blue"), pch=16)) #red= hot and blue= cold
#st_write(point_pixel,"anchorpixel.shp")

#hot.and.cold
#write.csv(hot.and.cold,"C:/Users/behnaz.molaei/OneDrive - Washington State University (email.wsu.edu)/Behnaz/Projects/Mint Project/3-IAREC mint project/2021/Mint Maps/9-1-2021/METRIC.csv")

### Behnaz Method: I creat a CSV file and define hot pixel and cold pixel here and then run H for it.
#hot.and.cold <- read.csv(file.choose())
hot.and.cold

H <- calcH(anchors = hot.and.cold, Ts = Ts, Z.om = Z.om,
           WeatherStation = WeatherStation, ETp.coef = 1.05,
           Z.om.ws = 0.003, DEM = DEM, Rn = Rn, G = G, verbose = TRUE, method = "mean")

ET_WS <- dailyET(WeatherStation = WeatherStation, MTL=MTLfile, ET = "ETr")  ## ETo for grass

ET.24 <- ET24h(Rn, G, H$H, Ts, WeatherStation = WeatherStation, ETr.daily=ET_WS)

ET.24_aoi<- crop(ET.24, extent(sensor_aoi))
plot(ET.24_aoi)

mean_value<- cellStats(ET.24_aoi,stat = 'mean')
mean_value



LE <- Rn - G - H$H

ET.inst <- 3600 * LE/((2.501 - 0.00236 * (Ts - 273.15)) * (1e+06))
plot(ET.inst)


#levelplot(ET.24 , margin=FALSE, at=c(seq(0,10,0.2)), par.settings = RdBuTheme)+layer(sp.points(hot.and.cold, pch=3, lwd=1, col='black'))

writeRaster(ET.24, filename="ET_24", format="GTiff", overwrite=TRUE)


#Bikalpa ET map print
point_pixel<- st_as_sf(hot.and.cold, coords = c("X", "Y"), crs= proj4string)
pixeltype<- as.character(hot.and.cold$type)
plot(ET.24)
points(plot(st_geometry(point_pixel), add=TRUE, col=ifelse(pixeltype=="hot","red","blue"), pch=16)) #red= hot and blue= cold
#st_write(point_pixel,"anchorpixel.shp")
rgb.palette <- grDevices::colorRampPalette(c("red3", "snow2", "blue"), space = "rgb")

# Convert the raster to a data frame for ggplot
et_df <- as.data.frame(rasterToPoints(ET.24))
colnames(et_df) <- c("x", "y", "value")
p <- ggplot() +
  geom_raster(data = et_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = rgb.palette(50), name = "24-Hour Evapotranspiration Soybean (mm/day)") +
  labs(x = "Easting", y = "Northing", title = "24-Hour Evapotranspiration Soybean (mm/day)") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(),
        axis.title.y = element_text(),
        axis.text.x = element_text(),
        axis.text.y = element_text(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
p <- p + geom_sf(data = point_pixel, aes(color = pixeltype), shape = 3, size=6, stroke=3) +
  scale_color_manual(values = c("hot" = "green", "cold" = "yellow"), name = "Pixel Type")
print(p)

NDVI= (image.DN$NIR - image.DN$R) / (image.DN$NIR + image.DN$R)
NDVI <- writeRaster(NDVI, filename="NDVI", format="GTiff", overwrite=TRUE)
plot(NDVI)


########################Transpiration#############################
NDVI_mask<- NDVI < 0.6
masked_ET24 <- mask(ET.24, NDVI_mask, maskvalue= TRUE)
plot(masked_ET24)
cropped_ET24<- crop(masked_ET24, sensor_aoi)
plot(cropped_ET24)
final_ET24<- mask(cropped_ET24, sensor_aoi)
plot(final_ET24)
transpiration_value<- cellStats(final_ET24,stat = 'mean')
transpiration_value





#######################################
#hot.and.cold2 <- hot.and.cold[,-c(1,5,6,8)]
### Behnaz Method: I think I can define hot pixel and cold pixel here and then run H for it.
#hot.and.cold <- read.csv(file.choose())
#hot.and.cold
coordinates(hot.and.cold) <- ~ X + Y
hot.and.cold

T <- raster("Roza_09012022_B6.tif")
#plot(T)
#points(hot.and.cold[,-c(1,4,5,8)], col='blue', pch=20)

levelplot(T , margin=FALSE, at=c(seq(10,70,2)))+layer(sp.points(hot.and.cold, pch=3, lwd=2, col='green')) #cex = 2

#plotting points on raster
ET <- raster("ET_ColdlvinesHotSoil_CorrectedMTL.tif")
levelplot(ET , margin=FALSE, at=c(seq(0,10,0.2)), par.settings = RdBuTheme)+layer(sp.points(hot.and.cold, pch=3, lwd=1, col='black')) # if you want points instead of + use pch=20 instead of cex=2###########################################################################################
##############################################
