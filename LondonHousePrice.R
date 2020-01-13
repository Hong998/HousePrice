library(tidyverse)
library(tmap)
library(downloader)
library(highcharter)
library(ggplot2)
library(geojsonio)
library(reshape2)
library(spatialreg)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(corrplot)
library(sf)
library(sp)
library(spdep)
library(car)
library(lwgeom)
library(readxl)
#download the MSOA house price csv file
MSOAHousePrice <- read_csv("https://data.london.gov.uk/download/average-house-prices/bdf8eee7-41e1-4d24-90ce-93fe5cf040ae/land-registry-house-prices-MSOA.csv", col_names = TRUE, locale = locale(encoding = 'Latin1'))
colnames(MSOAHousePrice)[5] <- "Price"
#extract the England house price by regional and select the time and measure type
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", destfile="statistical-gis-boundaries-london.zip")
unzip("statistical-gis-boundaries-london.zip", exdir="LondonMap")
LondonMSOA <- st_read("LondonMap/statistical-gis-boundaries-london/ESRI/MSOA_2011_London_gen_MHW.shp")
BNG = "+init=epsg:27700"
MSOAHousePrice <- MSOAHousePrice[MSOAHousePrice$Year=="Year ending Dec 2011"&MSOAHousePrice$Measure=="Mean",]
MSOAHousePrice$Price <- as.numeric(MSOAHousePrice$Price)

#download the MSOA data
MSOAdata <- read_csv("https://data.london.gov.uk/download/msoa-atlas/20264159-36cb-4aa2-8371-ae884ae83e88/msoa-data.csv", col_names = TRUE, locale = locale(encoding = 'Latin1'))
#delete the last row, which is the average value for every column
MSOAdata <- MSOAdata[1:983,]
colnames(MSOAdata)
#select the column which we need
MSOAdata1 <- MSOAdata[c("Middle Super Output Area","MSOA Name",
                        "Economic Activity (2011 Census);Unemployment Rate;",
                        "Economic Activity (2011 Census);Economically active %;",
                        "Obesity;Percentage of the population aged 16+ with a BMI of 30+, modelled estimate, 2006-2008;",
                        "Mid-year Estimates 2012, by age;% 15-64;",
                        "Land Area;Hectares;")]
LondonMSOABNG <- st_transform(LondonMSOA, BNG)
#download night time economy data
NightEco <- read_xls("data/night-time-economy.xls",sheet="NTE businesses London MSOAs",col_names = TRUE)
#process the xls document so that it can easy to read
#extract the column of 2011
NightEco <- NightEco[c(1,3,14)]
colnames(NightEco) <- c("Area Code","category","workplaces")
#extract the 
NightEco <- NightEco[NightEco$category=="Any Night Time Economy category",]
#grep the area code begin with "E02"
NightEco <- NightEco[grep("^E02",NightEco$`Area Code`),]
NightEco <- NightEco[c(1,3)]
#merge the Night time economy workplaces document with the MSOAdata
MSOAdata1 <- merge(MSOAdata1,NightEco,by.x="Middle Super Output Area",by.y="Area Code")
names(MSOAdata1)
colnames(MSOAdata1) <- c("Code","MSOA Name","Unemployment Rate","Economically Active",
                         "Obesity","age 15-64","Land Area","Night Time")
#merge the house price
MSOAdata1 <- merge(MSOAdata1,MSOAHousePrice,by.x="Code",by.y="Code")
MSOAdata1 <- MSOAdata1[c(1:8,12)]
#merge with the London MSOA shapefile
MSOAHousePriceMap <- merge(MSOAdata1,LondonMSOABNG,by.x="Code",by.y="MSOA11CD")
#Then we can visualize the house price in London by MSOA
MSOAHousePriceMap <- st_as_sf(MSOAHousePriceMap)
#make invalid geometry valid
MSOAHousePriceMap <- st_make_valid(MSOAHousePriceMap)
MSOAHousePriceMap1 <- tm_shape(MSOAHousePriceMap) +
  tm_polygons("Price",palette = "Reds",title="Price(£)")+
  tm_layout(title="2011 London House Price Map")+
  tm_legend(position=c("left", "bottom"))
MSOAHousePriceMap1
#Now we can visualise the frequency distribution by histogram
colnames(MSOAHousePriceMap)[9] <- "House Price"
hist1 <- ggplot(MSOAHousePriceMap, aes(x=`House Price`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist1
hist2 <- ggplot(MSOAHousePriceMap, aes(x=`Unemployment Rate`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist2
hist3 <- ggplot(MSOAHousePriceMap, aes(x=`Economically Active`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist3
hist4 <- ggplot(MSOAHousePriceMap, aes(x=`Night Time`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist4
hist5 <- ggplot(MSOAHousePriceMap, aes(x=`Obesity`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist5
hist6 <- ggplot(MSOAHousePriceMap, aes(x=`age 15-64`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist6
hist7 <- ggplot(MSOAHousePriceMap, aes(x=`Land Area`)) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
hist7
#the hist1,hist4 and hist7 are right skewed, so they need to be log-transform
loghist1 <- ggplot(MSOAHousePriceMap, aes(x=log(`House Price`))) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
loghist1
loghist2 <- ggplot(MSOAHousePriceMap, aes(x=log(`Night Time`))) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
loghist2
loghist3 <- ggplot(MSOAHousePriceMap, aes(x=log(`Land Area`))) +
  geom_histogram(aes(y = ..density..),bins = 30) + 
  geom_density(colour="red", size=1, adjust=1)
loghist3
#Now we can general the Linear regression model
LRModel <- lm(log(`House Price`)~`Unemployment Rate`+`Economically Active`+log(`Night Time`)+
                `Obesity`+`age 15-64`+log(`Land Area`),data = MSOAHousePriceMap)
summary(LRModel)
MSOAHousePriceMap$LRModel_residual <- LRModel$residuals
#this step can observe the distribution of the residuals
DistError <- qplot(LRModel$residuals) + geom_histogram()
DistError
#at here we can test the multicolinearity
indvardf <- st_set_geometry(MSOAHousePriceMap,NULL)
indvardf <- indvardf[,c("Unemployment Rate","Economically Active","Night Time",
                        "Obesity","age 15-64","Land Area")]
indvardf[3] <- log(indvardf$`Night Time`)
indvardf[6] <- log(indvardf$`Land Area`)
cormat <- cor(indvardf[,1:6], use="complete.obs", method="pearson")
corrplot(cormat,type="lower")
VIF <- vif(LRModel)
VIF
#this step will test the residuals independent
#firstly we will observe the residual distribution in London
LRMResidualPlot <- tm_shape(MSOAHousePriceMap) +
  tm_polygons("LRModel_residual",
              palette = "RdYlBu",
              breaks=c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,1,1.5)) +
  tm_legend(show=FALSE)
LRMResidualPlot
legend1 <- tm_shape(MSOAHousePriceMap) +
  tm_polygons("LRModel_residual",
              palette = "RdYlBu",
              breaks=c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,1,1.5)) +
  tm_compass(north=0,position=c(0.4,0.5))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)
legend1 
ResPlot1 <- tmap_arrange(LRMResidualPlot,legend1)
ResPlot1
#test Moran's I for residual of linear regression model
MSOAHousePriceMapSP <- as(MSOAHousePriceMap,"Spatial")
coordsW <- coordinates(MSOAHousePriceMapSP)
plot(coordsW)
#here we choose Moran’s I statistic for 4-nearest neighbours
knn_MSOA <- knearneigh(coordsW, k=4)
LonMSOA_knn <- knn2nb(knn_MSOA)
plot(LonMSOA_knn, coordinates(coordsW), col="red")
plot(MSOAHousePriceMapSP,add=T)
LonMSOA.knn_4_weight <- nb2listw(LonMSOA_knn, style="C")
LRM_Moran <- moran.test(MSOAHousePriceMapSP@data$LRModel_residual, LonMSOA.knn_4_weight)
LRM_Moran
#using the Spatial Lagged Model
SLM_model <- lagsarlm(log(`House Price`)~`Unemployment Rate`+`Economically Active`+log(`Night Time`)+
                        `Obesity`+`age 15-64`+log(`Land Area`), data = MSOAHousePriceMap, nb2listw(LonMSOA_knn, style="C"), method = "eigen")
summary(SLM_model)
#observe the residual distribution for SLM
MSOAHousePriceMapSP@data$SLM_resids <- SLM_model$residuals
SLMResidualPlot <- tm_shape(MSOAHousePriceMapSP) +
  tm_polygons("SLM_resids",
              palette = "RdYlBu",
              breaks=c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,1,1.5)) +
  tm_legend(show=FALSE)
SLMResidualPlot
legend2 <- tm_shape(MSOAHousePriceMapSP) +
  tm_polygons("SLM_resids",
              palette = "RdYlBu",
              breaks=c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,1,1.5)) +
  tm_compass(north=0,position=c(0.4,0.5))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)
legend2
ResPlot2 <- tmap_arrange(SLMResidualPlot,legend2)
ResPlot2
#test the Moran's I for SLM
SLM_Moran <- moran.test(MSOAHousePriceMapSP@data$SLM_resids, LonMSOA.knn_4_weight)
SLM_Moran 
#using the Spatial Error Model
SEM_model <- errorsarlm(log(`House Price`)~`Unemployment Rate`+`Economically Active`+log(`Night Time`)+
                          `Obesity`+`age 15-64`+log(`Land Area`), data = MSOAHousePriceMap, nb2listw(LonMSOA_knn, style="C"), method = "eigen")
summary(SEM_model)
#observe the residual distribution for SEM
MSOAHousePriceMapSP@data$SEM_resids <- SEM_model$residuals
SEMResidualPlot <- tm_shape(MSOAHousePriceMapSP) +
  tm_polygons("SEM_resids",
              palette = "RdYlBu",
              breaks=c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,1,1.5)) +
  tm_legend(show=FALSE)
SEMResidualPlot
legend3 <- tm_shape(MSOAHousePriceMapSP) +
  tm_polygons("SEM_resids",
              palette = "RdYlBu",
              breaks=c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,1,1.5)) +
  tm_compass(north=0,position=c(0.4,0.5))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)
legend3 
ResPlot3 <- tmap_arrange(SEMResidualPlot,legend3)
ResPlot3
#test the Moran's I for SEM
SEM_Moran <- moran.test(MSOAHousePriceMapSP@data$SEM_resids, LonMSOA.knn_4_weight)
SEM_Moran 
