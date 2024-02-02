library('poppr')
library('magrittr')
silkyshark <- read.genalex('silky1.csv')
silkyshark
splitStrata(silkyshark) <- ~site/Region
silkyshark

summary(silkyshark)

t26 <- silkyshark %>% setPop(~Region) %>% popsub('IN') %>% setPop(~site)
t26
reps <- c(Cafa3 = 4, Cafa11 = 4, Cafa16 = 5,
          Cafa28 = 4, Cafa41= 5, Cafa42 = 4,
          Cafa44 = 4, Cafa46 = 4)
sub9 <- c('Aceh', 'SLS', 'SLW', 'OB')
t26.9msn <- bruvo.msn(t26, replen = reps, sublist = sub9, showplot = FALSE)
set.seed(120)
plot_poppr_msn(t26, t26.9msn, inds = 'none', palette = cm.colors, nodebase = 1.25)
args(plot_poppr_msn)
imsn()
