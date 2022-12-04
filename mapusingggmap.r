library('ggplot2')
library(ggspatial)
library(ggOceanMaps)
library('ggOceanMapsData')

## draw the map without bathmetry
basemap(limits= c(40, 160,-30,15), bathmetry =FALSE)

### Add data

p <-basemap(limits= c(40, 160, -30, 15), data=dt2, bathymetry=FALSE)+
  geom_point(data= po ,col='#457373',aes(x=lon,y=lat),size = 4)+
  geom_point(data= aceh ,col='#BC0303',aes(x=lon,y=lat),size = 4)+
  geom_point(data= sls ,col='#B95422',aes(x=lon,y=lat),size = 4)+
  geom_point(data= slw ,col='#CCB77B',aes(x=lon,y=lat),size = 4)+
  geom_point(data= ob ,col='#4B5916',aes(x=lon,y=lat),size = 4)+
   xlab("Longitude") +ylab("Latitude")
p
