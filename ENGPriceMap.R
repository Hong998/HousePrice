#Mapping the house price in England
library(tidyverse)
library(tmap)
library(downloader)
library(ggplot2)
library(geojsonio)
library(reshape2)
library(plotly)
library(mapview)
library(sf)
library(sp)
#Mapping the house price in England
#download the geojson file to get the England Outline
EngOutline <- geojson_read("https://opendata.arcgis.com/datasets/8d3a9e6e7bd445e2bdcc26cdf007eac7_0.geojson", what = "sp")
qtm(EngOutline)
#download the house price csv file
HousePrice <- read_csv("https://data.london.gov.uk/download/average-house-prices/b1b0079e-698c-4c0b-b8c7-aa6189590ca4/land-registry-house-prices-borough.csv", col_names = TRUE, locale = locale(encoding = 'Latin1'))
colnames(HousePrice)[5] <- "Price"
#extract the England house price by regional and select the time and measure type
EngPrice <- HousePrice[HousePrice$Year=="Year ending Dec 2016"&HousePrice$Measure=="Mean",]
EngPrice <- EngPrice[grep("^E12",EngPrice$Code),]
BNG = "+init=epsg:27700"
EngOutlineBNG <- spTransform(EngOutline,BNG)
#merge the England Outline and England house price data by code
EngOutlineBNG@data <- merge(EngOutlineBNG@data,EngPrice,by.x="rgn15cd",by.y="Code")
EngPriceMap <- tm_shape(EngOutlineBNG) +
  tm_polygons("Price",
              palette = "Reds",title="2016 England House Price(£)")+
  tm_text("Area",size=0.75)+
  tm_credits("Reional House price data from London Datastore",size = 0.6,align = "right")+
  tm_layout(legend.width=0.8)+
  tm_compass(north=0,position=c(0.8,0.8))
EngPriceMap
