library(adegenet)
library(poppr)
library(dplyr)
library(hierfstat)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(poppr)
library(ade4)
library(pegas)
library(genepop)

#######Read loci and location data

silky.loci <- read.loci('silkyloci0527.txt', header = TRUE, loci.sep = '\t', allele.sep = '/',col.pop = 2, col.loci = 3:10) 
print(silky.loci, details=TRUE)



#convert loci to genind 
silky.genind <- loci2genind(silky.loci)


#removes missing data
silky.genind <-missingno(pop = silky.genind, type = 'mean', cutoff=0.1)
#####DONE!

#####Convert to genepop!
silky.genpop <- genind2genpop(silky.genind, process.other = TRUE)
silky.genpop

basic_silky <- basic.stats(silky.genind, diploid = TRUE)
basic_silky
Ho_silky <-apply(basic_silky$Ho, MARGIN = 2, FUN = mean, na.rm = TRUE) %>%
  round(digits = 3)
Ho_silky
He_silky <-apply(basic_silky$Hs, MARGIN = 2, FUN = mean, na.rm = TRUE) %>%
  round(digits = 3)
He_silky


Fis_silky <-apply(basic_silky$Fis, MARGIN = 2, FUN = mean, na.rm = TRUE) %>%
  round(digits = 3)

Fis_silky






round(pegas::hw.test(silky.genind,B=1000), digits = 3)
HWE.test <- data.frame(sapply(seppop(silky.genind),function(ls)pegas::hw.test(ls,B=0)[,3]))
HWE.test.chisq <-t(data.matrix(HWE.test)) 
 {cat("Chi-square test(p-value):","\n") 
  round(HWE.test.chisq,3)}


HWE.test <- data.frame(sapply(seppop(silky.genind),function(ls)pegas::hw.test(ls,B=1000)[,4]))
HWE.test.MC <-t(data.matrix(HWE.test)) 
 {cat("Chi-square test(p-value):","\n") 
  round(HWE.test.MC,3)}

alpha = 0.00125
Prop.loci.out.of.HWE <- data.frame(Chisq=apply(HWE.test.chisq < alpha,1,mean),
                                   MC= apply(HWE.test.MC <alpha,1,mean))
Prop.loci.out.of.HWE



Chisq.fdr <-matrix(p.adjust(HWE.test.chisq, method='fdr'),
                   nrow=nrow(HWE.test.chisq))
MC.fdr <- matrix(p.adjust(HWE.test.MC, method = 'fdr'),
                 nrow = nrow(HWE.test.MC))
Prop.loci.out.of.HWE <-data.frame(Chisq=apply(HWE.test.chisq < alpha, 2, mean),
                                  MC=apply(HWE.test.MC<alpha,2,mean),
                                  Chisq.fdr = apply(Chisq.fdr<alpha,2,mean),
                                  MC.fdr=apply(MC.fdr<alpha,2,mean))
Prop.loci.out.of.HWE 


poppr::ia(silky.genind,sample=999)
LD.pair <- poppr::pair.ia(silky.genind)
LD.pair



Null.alleles <- PopGenReport::null.all(silky.genind)
Null.alleles$homozygotes$probability.obs
 {cat(" summary1 (Chakraborty et al. 1994):", "\n")
  round(Null.alleles$null.allele.freq$summary1,2)} 
Null.alleles$null.allele.freq$summary2





library('PopGenReport')
library('knitr')
######Allele information
basic_info('silky_genepop.txt')

Fst('silky_genpop.txt',
    sizes = FALSE,
    pairs = FALSE,
    outputFile = '',
    dataType = 'Diploid',
    verbose = interactive())


test_diff('indosilky_genpop.txt', genic = TRUE, pairs = FALSE, settingsFile = '', 
          dememorization = 10000,
          batches = 100,
          iterations = 5000,
          verbose = interactive())
genedivFis('indosilky_genpop.txt')


ibd(
  'indosilky_genpop.txt',
  outputFile = "",
  settingsFile = "",
  dataType = "Diploid",
  statistic = "F/(1-F)",
  geographicScale = "2D",
  CIcoverage = 0.95,
  testPoint = 0,
  minimalDistance = 1e-04,
  maximalDistance = 1e+09,
  mantelPermutations = 1000,
  mantelRankTest = FALSE,
  verbose = interactive()
)

test_LD(
  'silky_genpop.txt',
  outputFile = "",
  settingsFile = "",
  dememorization = 10000,
  batches = 100,
  iterations = 5000,
  verbose = interactive()
)
write_LD_tables('indosilky_genpop.txt', outputFile = "", verbose = interactive())


Nm_private(
  'indosilky_genpop.txt',
  outputFile = "",
  dataType = "Diploid",
  verbose = interactive()
)


nulls(
  'indosilky_genpop.txt',
  outputFile = "",
  settingsFile = "",
  nullAlleleMethod = "",
  CIcoverage = 0.95,
  verbose = interactive()
)

####summary 
silky.summary <- summary(silky.genind)
silky.summary
silky.hwe <-hw.test(x=silky.genind, permut = TRUE, nsim = 1000, res.type = 'matrix')
silky.hwe




sm.summary <- summary(sm.genend)
sm.summary
sm.hwe <-hw.test(x=sm.genend, permut = TRUE, nsim = 1000, res.type = 'matrix')
sm.hwe
nm.hwe <-hw.test(x=nm.genend, permut = TRUE, nsim = 1000, res.type = 'matrix')
nm.hwe



###NJ tree
indo.dist <- dist(x=indosilky.genind, method = 'euclidean', diag = T, upper = T)
indo.dist.pop <- dist.genpop(x=indosilky.genpop, method = 1, diag = T, upper = T)

indo.nj <- nj(indo.dist)
plot(as.vector(indo.dist), as.vector(as.dist(cophenetic(indo.nj))),
     xlab="Original distance", ylab='Reconstructured distance')
abline(lm(as.vector(indo.dist)~as.vector(as.dist(cophenetic(indo.nj)))), col='red')

plot.phylo(x=indo.nj, type='cladogram', edge.width = 2)



