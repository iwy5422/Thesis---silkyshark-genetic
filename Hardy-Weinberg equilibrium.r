library('pegas')
(nanhwe.full <- hw.test(nancycats, B= 1000)) #performs 1000 permutations
(nanhwe.pop <- seppop(nancycats) %>% lapply(hw.test, B =0))

#visualization of population-wise p-values
(nanhwe.mat <- sapply(nanhwe.pop, "[", i = TRUE, j= 3)) #Take the third column with all rows
#sapply and [ to loop to create a matrix that only contains populations in columns and loci in rows.
#Define an α value and set everything above that value to 1 so that we can visually detect candidate loci where we might reject the Ho of HWE.
alpha <- 0.05
newmat <- nanhwe.mat
newmat[newmat>alpha] <- 1

#using levelplot for heatmap
library('lattice')
levelplot(t(newmat))
