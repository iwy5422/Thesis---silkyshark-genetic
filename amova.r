library('adegenet')
library('ade4')
library('poppr')
silkyshark <- read.genalex('indosilky_0905_2.csv')
silkyshark

splitStrata(silkyshark) <- ~site/Region
silkyshark
silkystra<-addStrata(silkyshark,silkyshark$pop,name='Pop')
silkystra
table(strata(silkystra, ~Region))
table(strata(silkystra, ~Region/site)) #population

silkyamova<- poppr.amova(silkyshark, ~Region/site)
silkyamovacc <- poppr.amova(silkystra, ~Region/site, clonecorrect = TRUE)

silkyamova

###run significant
set.seed(1999)
silkysignif   <- randtest(silkyamova, nrepet = 999)

