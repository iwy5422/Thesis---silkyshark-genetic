library(ade4)
library(adegenet)
library(hierfstat)
library('poppr')

silkyshark_1 <- read.genalex('Indosilky_0905.csv')
is.genind(silkyshark_1)
bs.nc<-basic.stats(silkyshark_1)
bs.nc
wc(silkyshark_1)
genet.dist(silkyshark_1, method = "WC84")
#######use for allelic richness

allele.count(silkyshark_1,diploid = TRUE)
allelic.richness(silkyshark_1, min.n=8,  diploid = TRUE)

betas(silkyshark_1,nboot=0,lim=c(0.025,0.975),diploid=TRUE,betaijT=FALSE)
boot.ppfis(dat=silkyshark_1,nboot=100,quant=c(0.025,0.975),diploid=TRUE,dig=4)
boot.ppfst(dat=silkyshark_1,nboot=100,quant=c(0.025,0.975),diploid=TRUE)

genet.dist(silkyshark_1,diploid=TRUE,method="Nei87")
genet.dist(silkyshark_1,diploid=TRUE,method="Fst")

