library('poppr')
silkyshark <- read.genalex('silky1.csv')
silkyshark
splitStrata(silkyshark) <- ~site/Region

gac <- genotype_curve(silkyshark, sample=1000, quiet = TRUE)
#sample=1000, each boxplox, n loci were randomly sampled 1000 times in order to create the distribution
(pinflt <- locus_table(silkyshark))
info_table(silkyshark, type = 'missing' , plot = TRUE)

