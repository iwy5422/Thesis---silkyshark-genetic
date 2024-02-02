library('poppr')
silkyshark <- read.genalex('silky1.csv')
silkyshark
splitStrata(silkyshark) <- ~subpop/pop
silkyshark
silkystra <- addStrata(silkyshark,silkyshark$pop, name='Pop_Subpop')
addStrata()
silkystra


library('dplyr')
silkystr <- strata(silkyshark) %>%
  group_by(site, Region) %>%
  summarize(Count=n())
silkystr

library('treemap')
silkystr
silkystr$Count
