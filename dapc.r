library(ade4)
library(adegenet)
library('poppr')
library("RColorBrewer")
library('wesanderson')


silkyshark <- read.genalex('silky1.csv')
silkyshark

silkyshark_genind <-genclone2genind(silkyshark)
silkyshark_genind 

#silkyshark_genind 跟silkyshark_genind@tab都要記得改
#########畫圖的code

splitStrata(silkyshark) <- ~subpop/pop
set.seed(999)
silkymx <- xvalDapc(tab(silkyshark, NA.method = "mean"), pop(silkyshark))
set.seed(999)
system.time(silkymx <- xvalDapc(tab(silkyshark, NA.method = "mean"), pop(silkyshark),
                              n.pca = 10:20, n.rep = 1000,
                              parallel = "multicore", ncpus = 4L))
names(silkymx)
silkymx[-1]

fourcol<-c('#457373', '#B95422','#CCB77B','#4B5916')#####4groups
newcol<-c('#457373', '#BC0303','#B95422','#CCB77B','#4B5916') #color 

png(file="/Users/chiayunli/Desktop/R file/DAPC_plot2.png",
    width=700, height=700)
scatter(silkymx$DAPC, col = newcol , cex = 2, legend = TRUE,
        clabel = FALSE, posi.leg = "topleft", scree.pca = TRUE, posi.pca = "topright", cleg = 0.75, xax = 1, yax = 2, inset.solid = 1)
dev.off()


############## 跑dapc的code

grp <-find.clusters(silkyshark_genind, clust = NULL, n.pca = NULL, n.clust = NULL,
                    method = c("kmeans", "ward"), stat = c("BIC","AIC", "WSS"),
                    choose.n.clust = TRUE, criterion = c("diffNgroup", "min","goesup",
                                                         "smoothNgoesup", "goodfit"), max.n.clust = round(nrow(silkyshark_genind@tab)/10),
                    n.iter = 1e5, n.start = 10, scale = FALSE, truenames = TRUE
)
dapc1 <- dapc(silkyshark_genind, grp$grp,n.pca= NULL, n.da= NULL, var.contrib = TRUE, scale = FALSE)
scatter(dapc1)

grp<-find.clusters(silkyshark_genind,max.n.clust=5)

write.table(dapc1$ind.coord,'CoorDAPC.txt', sep='\t')
write.table(dapc1$means,'GroupmeanDAPC.txt', sep='\t')
write.table(dapc1$grp.coord,'GroupCoorDAPC.txt', sep = '\t')
