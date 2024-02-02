library('poppr')
data(monpop)
splitStrata(monpop) <- ~Tree/Year/Symptom
setPop(monpop) <- ~Symptom
monpop

#calculate diversity 
(monpop_diversity <- poppr(monpop))

#genotypic richness
library('vegan')
mon.tab <- mlg.table(monpop, plot = FALSE)
min_sample <- min(rowSums(mon.tab))
rarecurve(mon.tab, sample = min_sample, xlab = 'Sample Size', ylab = 'Expected MLGs')
title('Rarefaction of Fruit Rot and Blossom Blight')

#genotypic diversity 
N <- monpop_diversity$N  #number of samples
lambda <- monpop_diversity$lambda #Simpson's index
(N/(N-1))* lambda #Corrected Simpson's index

# genotypic evenness
mon.tab <- mlg.table(monpop)




