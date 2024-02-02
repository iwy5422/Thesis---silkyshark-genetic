#Don't forget to set your working directory! I recommend setting it to the directory where you have your STRUCTURE results files and NOTHING ELSE.

#install.packages(c("devtools","ggplot2","gridExtra","gtable","label.switching","tidyr"),dependencies=T)

library(devtools)
install_github('royfrancis/pophelper')

library(pophelper)
library(gridExtra)

# STRUCTURE files (do not use this command to read local files)
### save all the _f file into a folder and put direction here
sfiles <- list.files(path="/Users/chiayunli/Desktop/R file/results", full.names=T) # <-- Look carefully here! You must put in the path where you have put the results files. No other files can be in that directory!
# basic usage
slist <- readQ(files=sfiles,indlabfromfile = TRUE)
readQ(files=sfiles,filetype="structure")

sr1 <- summariseQ(tabulateQ(slist))

write.csv(evannoMethodStructure(sr1), "evannoMethodStructure.csv", na = "") 
p <- evannoMethodStructure(data=sr1,exportplot=F,returnplot=T,returndata=F,basesize=12,linesize=0.7)
grid.arrange(p)


p1$data$grplab <- indosilky_loci$Pop


p1 <- plotQ(slist[1:10], imgoutput = 'join',returnplot=T,exportplot=F,basesize=11,showlegend = T, showyaxis = T, showindlab = T ,useindlab = T)
grid.arrange(p1$plot[[1]])

slist1 <- alignK(slist[1:10])
p1 <- plotQ(slist1, imgoutput = 'join',returnplot=T,exportplot=F,basesize=11, showlegend = T, showyaxis = T, showindlab = T)
grid.arrange(p1$plot[[1]])


clumppExport(readQ(sfiles), exportpath = getwd())

a <-readQ(files ="pop_K2-combined-merged.txt", filetype = "clumpp" )
pa<-plotQ(a,returnplot=T,exportplot=F,basesize=11,showlegend = T, showyaxis = T, showindlab = T)
grid.arrange(pa$plot[[1]])


b <-readQ(files ="pop_K2-combined-aligned.txt", filetype = "clumpp" )
pb <-plotQ(b,imgoutput = "join", returnplot=T,exportplot=F,basesize=11,showlegend = T, showyaxis = T, showindlab = T)

grid.arrange(pb$plot[[1]])




#######Use Shiny
install.packages(c("ggplot2","gridExtra","label.switching","tidyr","remotes",
                   "colourpicker","DT","highcharter","htmlwidgets","magrittr",
                   "markdown","RColorBrewer","shiny","shinyAce","shinyBS",
                   "shinythemes","shinyWidgets","viridisLite","writexl"),
                 repos = "http://cran.us.r-project.org")

# install pophelper package from GitHub
remotes::install_github('royfrancis/pophelper')

# install the package from GitHub
remotes::install_github('royfrancis/pophelperShiny')

# load library for use
library(pophelperShiny)

# launch app
runPophelper()



