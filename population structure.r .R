library('mmod')
data('nancycats')
nancycats

#use Hendrick's standardized GST to assess population structure among these populations
Gst_Hedrick(nancycats)

#Genetic Distance 
library('poppr')
library('ape')
library('magrittr')
data("microbov")
set.seed(10)
ten_samples <- sample(nInd(microbov), 10)
mic10 <- microbov[ten_samples]
(micdist <- provesti.dist(mic10))

theTree <- micdist %>%
  nj() %>%      #calculate neighbor-joining tree
  ladderize()   # organize branches by clade
plot(theTree)
add.scale.bar(length = 0.05) # add a scale bar showing 5% difference

set.seed(999)
aboot(mic10, dist= provesti.dist, sample = 200, tree = 'nj', cutoff = 50, quiet = TRUE)

#Setting up the data
strata(microbov) <- data.frame(other(microbov))
microbov

nameStrata(microbov) <- ~Country/Breed/Species
#Analysis 
set.seed(999)
microbov %>% 
  genind2genpop(pop = ~Country/Breed) %>%
  aboot(cutoff = 50, quiet = TRUE, sample = 1000, distance = nei.dist)
#----------K means hierarchical clustering 
library('poppr')
data('Pinf')
Pinf

#cluster analysis
MX<- popsub(Pinf, 'North America')
MXclust <- find.clusters(MX)
##Choose the number PCs to retain (>= 1): 
# 50
##Choose the number of clusters (>=2): 
# 3
SA <- popsub(Pinf, 'South America')
SAclust <- find.clusters(SA)
##Choose the number PCs to retain (>= 1): 
# 30
##Choose the number of clusters (>=2): 
# 4
####--------------TREES
pinfreps <- c(2,2,6,2,2,2,2,2,3,3,2)
MXtree <- bruvo.boot(MX, replen = pinfreps, cutoff = 50, quiet = TRUE)
SAtree <- bruvo.boot(SA, replen = pinfreps, cutoff = 50, quiet = TRUE)

library('ape')
cols <- rainbow(4)
plot.phylo(MXtree, cex = 0.8, font = 2, adj = 0, tip.color = cols[MXclust$grp], 
           label.offset = 0.0125)
nodelabels(MXtree$node.label, adj = c(1.3, -0.5), frame ='n', cex= 0.8,
           font = 3, xpd = TRUE)
axisPhylo(3)

plot.phylo(SAtree, cex = 0.8, font = 2, adj = 0, tip.color = cols[SAclust$grp],
           label.offset = 0.0125)
nodelabels(SAtree$node.label, adj = c(1.3, -0.5), frame = "n", cex = 0.8,
           font = 3, xpd = TRUE)
axisPhylo(3)
