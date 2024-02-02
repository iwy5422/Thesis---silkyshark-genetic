library('poppr')
library('magrittr')
data(Pinf)

MX<- popsub(Pinf, 'North America')
ia(MX, sample= 999)

MX

#clone correction 
MX %>% clonecorrect(strata= ~Continent/Country) %>% ia(sample = 999)

SA <- popsub(Pinf, "South America")
ia(SA, sample = 999)
SA %>% clonecorrect(strata= ~Continent/Country) %>% ia(sample = 999)

#Pairwise rd over all loci
mxpair <-MX %>% clonecorrect(strata= ~Continent/Country) %>% pair.ia
sapair <-SA %>% clonecorrect(strata= ~Continent/Country) %>% pair.ia

head(mxpair, 10)
head(sapair, 10)

#plotting the output of pair.ia
plotrange <- range(c(mxpair, sapair), na.rm = TRUE)
plot(mxpair, limits = plotrange)
plot(sapair, limits = plotrange)





