setwd("C:/Users/bikal/OneDrive - Tennessee State University/Research Assistantship/20240604/Henley/February 26, 2024")
proj4string <- "+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs"

#Input Package
library(rgdal)
library(raster)
library(sp)
library(water)
library(tiff)
library(sf)
library(terra)

#Shape file
shapefile<- st_read("C:/Users/bikal/OneDrive - Tennessee State University/Research Assistantship/20240604/Henley/henley_field.shp")
shapefile <- st_zm(shapefile, drop =TRUE)
geom <- st_geometry(shapefile[1])
geom_shp <- st_as_sf(geom)

extent(shapefile)
plot(shapefile)

#get location for water package just for reference
system.file(package = "water")

#MTL & CSV
MTLfile <- "LC09_L2SP_020036_20240226_20240228_02_T1_MTL.txt"
csvfile <- "Weather_data_Feb_26.csv"

WeatherStation <- read.WSdata(WSdata = csvfile, date.format = "%d/%m/%Y", 
                              lat= 35.2832473, long= -85.9263288, elev=307, height= 2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 3,
                                        "wind" = 7, "RH" = 5, "temp" = 4, "rain" = 6),
                              tz="America/Chicago",
                              cf = c(1, 1, 1),
                              MTL = MTLfile)
plot(WeatherStation)

#Satellite Image
#image.DN<- loadImage(path="C:/Users/bikal/OneDrive - Tennessee State University/Research Assistantship/Test_2/LC08_L2SP_019035_20180501_20201015_02_T1" ,aoi, sat = "L8")
k<- raster("LC09_L2SP_020036_20240226_20240228_02_T1_ST_B10.tif")
image.DN<- crop(k, extent(shapefile))
image.DN <- mask(image.DN, geom_shp)
image.DN$Thermal1<- image.DN
#image.DN$Thermal1 <-crop(k, extent(shapefile)) 
plot(image.DN)

image.DN@crs<- CRS(proj4string)

#Ts<- image.DN$Thermal1*0.00341802+149
#Ts

#DEM image
DEM <- prepareSRTMdata(format= "tif", extent=image.DN)
DEM<- mask(DEM, geom_shp)
plot(DEM)
DEM


surface.model <-METRICtopo(DEM)
surface.model[[1]]@crs<- CRS(proj4string)
solar.angles.r <- solarAngles(surface.model = surface.model,
                              WeatherStation = WeatherStation, MTL = MTLfile)


Rs.inc <- incSWradiation(surface.model = surface.model,
                         solar.angles = solar.angles.r,
                         WeatherStation = WeatherStation)  # or mean (WeatherStation$alldata$radiation)

plot(Rs.inc)
#Rs <- writeRaster(Rs.inc, filename="rsinc_test", format="GTiff", overwrite=TRUE)

image.SR <- loadImageSR(getwd(), aoi= extent(shapefile))
image.SR<- mask(image.SR, geom_shp)
image.SR<- image.SR * 10000
image.SR<- image.SR*0.0000275 - 0.2
plot(image.SR)

albedo <- albedo(image.SR=image.SR, coeff="Tasumi", sat="L8", aoi = extent(shapefile))
albedo<- mask(albedo, geom_shp)
albedo
#writeRaster(albedo, filename="albedo.tiff", overwrite=TRUE)

LAI <- LAI(method="metric2010", image=image.SR, L=0.1)
LAI
#writeRaster(LAI, filename="LAI.tiff", overwrite=TRUE)


Ts <- surfaceTemperature(image.DN=image.DN, LAI=LAI, sat = "L8", 
                         WeatherStation = WeatherStation)
Ts
plot(Ts)

Rl.out <- outLWradiation(LAI = LAI, Ts = Ts)
Rl.inc <- incLWradiation(WeatherStation,DEM = surface.model$DEM, 
                         solar.angles = solar.angles.r, Ts= Ts)

Rn <- netRadiation(LAI, albedo, Rs.inc, Rl.inc, Rl.out)

G <- soilHeatFlux(image = image.SR, Ts = Ts, albedo = albedo,
                  Rn = Rn, LAI = LAI)

Z.om <- momentumRoughnessLength(LAI = LAI, mountainous = TRUE,
                                method = "short.crops",
                                surface.model = surface.model)
#writeRaster(Z.om, filename="Z_om.tiff", overwrite=TRUE)

hot.and.cold <- calcAnchors(image = image.SR, Ts = Ts,
                            LAI = LAI, plots = FALSE, albedo = albedo,
                            Z.om = Z.om, n = 5,
                            anchors.method = "flexible",
                            deltaTemp = 5, verbose = FALSE)
hot.and.cold

H <- calcH(anchors = hot.and.cold, Ts = Ts, Z.om = Z.om,
           WeatherStation = WeatherStation, ETp.coef = 1.05,
           Z.om.ws = 0.003, DEM = DEM, Rn = Rn, G = G, verbose = TRUE, method = "mean")

plot(H$H)
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL=MTLfile, ET = "ETo")
plot(ET_WS)

ET.24 <- ET24h(Rn, G, H$H, Ts, WeatherStation = WeatherStation, ETr.daily=ET_WS)
writeRaster(ET.24, filename="Evapotranspirationplot.tiff", overwrite=TRUE)

#Plotting hot and cold pixels 
point_pixel<- st_as_sf(hot.and.cold, coords = c("X", "Y"), crs= proj4string)
pixeltype<- as.character(hot.and.cold$type)
plot(ET.24)
points(plot(st_geometry(point_pixel), add=TRUE, col=ifelse(pixeltype=="hot","red","blue"), pch=16)) #red= hot and blue= cold
st_write(point_pixel,"anchorpixel.shp")

#extracting ET value at hot and cold pixel
weatherstation_point<- data.frame(X=c(597637.386), Y=c(3904983.3))
st_weather<- st_as_sf(weatherstation_point, coords =c("X","Y")  ,crs= proj4string)
plot(st_geometry(st_weather), add=TRUE, col="orange", pch=16)
extract(ET.24, point_pixel)
extract(ET.24, st_weather)



NDVI= (image.SR$NIR - image.SR$R) / (image.SR$NIR + image.SR$R)
plot(NDVI)
NDVI #cold: > (max val-0.15), hot: 0.1-0.28
albedo #cold: 0.18 - 0.25, hot: 0.13-0.15
LAI #cold: 3-6
Z.om #cold: 0.03 - 0.08, hot: <0.005

