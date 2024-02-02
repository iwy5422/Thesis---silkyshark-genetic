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



silky <- read.genalex('silky_test6_primer.csv')
silky_genind <- genclone2genind(silky)
silky.genpop <- genind2genpop(silky_genind, process.other = TRUE)
basic_silky <- basic.stats(silky_genind, diploid = TRUE)
basic_silky


round(pegas::hw.test(silky_genind,B=1000), digits = 3)
HWE.test <- data.frame(sapply(seppop(silky_genind),function(ls)pegas::hw.test(ls,B=0)[,3]))
HWE.test.chisq <-t(data.matrix(HWE.test)) 
{cat("Chi-square test(p-value):","\n") 
  round(HWE.test.chisq,3)}

library('knitr')
library('PopGenReport')

basic_info('silky_test6_primer.txt')

Fst('silky_test6_primer.txt',
    sizes = FALSE,
    pairs = FALSE,
    outputFile = '',
    dataType = 'Diploid',
    verbose = interactive())


test_diff('silky_test6_primer.txt', genic = TRUE, pairs = FALSE, settingsFile = '', 
          dememorization = 10000,
          batches = 100,
          iterations = 5000,
          verbose = interactive())
genedivFis('silky_test6.txt')


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
  'silky_test6_primer.txt',
  outputFile = "",
  settingsFile = "",
  dememorization = 10000,
  batches = 100,
  iterations = 5000,
  verbose = interactive()
)
write_LD_tables('silky_test6_primer.txt', outputFile = "", verbose = interactive())


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



