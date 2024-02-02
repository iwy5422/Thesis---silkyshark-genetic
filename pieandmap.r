library('ggplot2')
library('scatterpie')
library("ggspatial")
library('ggOceanMaps')
library('devtools')
library('usethis')
devtools::install_github("MikkoVihtakari/ggOceanMapsData") # required by ggOceanMaps
devtools::install_github("MikkoVihtakari/ggOceanMaps",force = TRUE)
d<- read.csv('indosilkypie.csv', header = TRUE)

d$region <-factor(1:6)
p<- basemap(limits=c(90, 150, -10, 10), data=d, bathymetry=FALSE)

pdf(file = "indopie.pdf", width = 15, height = 10)
p+ geom_scatterpie(aes(x=lon, y=lat, group=region), data=d,pie_scale = 2,cols=LETTERS[1:2],alpha = 0.8) 
dev.off()

d1<- read.csv('indosilkypie_3.csv', header = TRUE)
p<- basemap(limits=c(90, 150, -10, 10), data=d1, bathymetry=FALSE)
pdf(file = "indosilkypie_3.pdf", width = 15, height = 10)
p+ geom_scatterpie(aes(x=lon, y=lat, group=region), data=d1,pie_scale = 2,cols=LETTERS[1:3],alpha = 0.8) 
dev.off()
