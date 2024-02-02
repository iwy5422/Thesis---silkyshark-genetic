library('poppr')
library('adegenet')
library('ade4')

#----------- Data preparation
monpop <- read.genalex('monpop.csv')
monpop
splitStrata(monpop) <- ~Tree/Year/Symptom
monpop

#-----------First Analysis
data('Pinf') # load the data
Pinf 
gac <- genotype_curve(Pinf, sample = 1000, quiet = T) 
# each boxplot, n loci were randomly sampled 1000 times in order to create the distribution
data('microbov')
microbov
gac <- genotype_curve(microbov, sample = 1000, quiet = T)
(pinflt <- locus_table(Pinf))
info_table(Pinf, type= 'missing', plot = TRUE)
tail(genind2df(Pinf, sep = '/'))
info_table(Pinf, type = 'ploidy', plot = TRUE)
Pinf.ploidy <- info_table(Pinf, type= 'ploidy', plot = TRUE, low = 'black', high = 'orange')
tail(Pinf.ploidy)
poppr(Pinf)

#Calculate the Number of multilocus genotypes (MLG) observed 
P.tab<- mlg.table(Pinf)

#--------- Population strata: subsetting and clone correction
library('poppr')
data(monpop) #import data
monpop
#------ assigning stratifications
splitStrata(monpop)<- ~Tree/Year/Symptom #only need to be run once after import data
monpop  
#----------using treemap() to visualize hierarchical stratifications
library('dplyr')
monstrata<- strata(monpop) %>%
  group_by(Tree, Year, Symptom) %>%
  summarize(Count = n())
monstrata # data frame 
library('treemap')
monstrata
nameStrata(monpop) # the order of variables
monstrata$Count # The variable used for the block size
# Adjusting the aesthetics for the labels
label_position <- list (c('center', 'top'), c('center', 'center'),c('center','bottom'))
label_size <- c(Tree = 0, Year = 15, Symptom = 15)

#Plotting, First three arguments are necessary
treemap(dtf = monstrata, index = nameStrata(monpop), vSize = 'Count',
        type = 'categorical', vColor = 'Tree', title = 'M. fructicola',
        align.labels = label_position, fontsize.labels = label_size)
# now analyze the data according to Tree and Year
setPop(monpop) <- ~Tree/Year
monpop
setPop(monpop) <- ~Symptom #analyze the data according to Symptom
monpop
setPop(monpop) <- ~Symptom/Tree
monpop

#--------- Locus stats, heterozygosity, HWE
library('poppr')
library('magrittr')
data('Pinf')
locus_table(Pinf)
#to check if alleles existy in certain population of the dataset
locus_table(Pinf, pop= 'North America')
locus_table(Pinf, pop= 'South America')

#--------- Phylogenetically uninformative loci
nLoc(Pinf) #the number of loci in this dataset
iPinf <- informloci(Pinf) 
#informloci remove loci that contain less than given percentage of divergent individuals
#the default is 2/N, where N equals the number of individuals in the data set
nLoc(iPinf)

#visualize missing data
data("nancycats")
info_table(nancycats, plot = TRUE)

#removing loci and genotypes 
nancycats %>% missingno('loci') %>% info_table(plot= TRUE, scaled = FALSE)












