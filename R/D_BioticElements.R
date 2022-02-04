# reading libraries -------------------------------------------------------

library(spdep)
library(prabclus)
library(tidyverse)

###########################################################################
# Cerrado terrestrial vertebrates Biotic Element analysis -----------------
###########################################################################

# processing data: preparing raw objects for analysis ---------------------

head(centro) #checking overall structure
centro <- centro[, c(6,7)]

head(pa_table) #checking overall structure
pa_table <- pa_table[,c(4,14)]
names(pa_table) <- c("species", "sample")
head(pa_table)

dim(centro)[1] #checking the number of centroids; should be equal to the number of grid cells in $sample above
length(unique(pa_table$sample)) #checking the number of grid cells

length(unique(pa_table$species)) #checking the number of spp


# processing data: creating presence x absence matrix ---------------------

attach(pa_table)
mpa <-as.matrix(table(pa_table$species, pa_table$sample))
mpa <- 1*(mpa>0)

U <- mpa
colnames(U) <- NULL
rownames(U) <- NULL

# creating neighbourhood list ---------------------------------------------

plot(dd)
nblist <- poly2nb(dd)
nblist

# creating distance matrix ------------------------------------------------

ff<- coord2dist(file="centro", coordmatrix=centro[1:dim(centro)[1],], file.format="decimal2",output.dist=TRUE)
class(ff)

# prabinit ----------------------------------------------------------------

x <- prabinit(prabmatrix=U, rows.are.species=TRUE, neighborhood=nblist,
              geodist=ff, distance="geco", gtf=0.2)

class(x) #prab object used in prabtest and cdn

# prabtest ----------------------------------------------------------------

set.seed(42)
test <- prabtest(x, times=1000, pdfnb = TRUE) #prabtest started 30/09/2021 16:37/18:56

summary(test)
capture.output(summary(test), file = here("outputs", "tests", "prabtest.txt"))
saveRDS(object= test, file = here("outputs", "tests", "prabtest.rds"))

###########################################################################
# processing data: preparing to test vicariance premise number two -------
###########################################################################
#BEs is an object from D_clustering.R

BEs.no.noise <- BEs[BEs$BEs!=0,] # removing noise component
head(BEs.no.noise)
BEs.no.noise$binomial <- BEs.no.noise$species # duplicating species list

a <- separate(data=BEs.no.noise, col="binomial", into=c("genus", "epithet"), sep=" ") # breaking binomial
a <- merge(a, list, by="species")
a <- a[,c(1:5)]

# chi-square test for vicariance premisse number two ----------------------
set.seed(42)
chisq <- comp.test(a$genus.x, a$BEs)
capture.output(chisq, file = here("outputs", "tests", "chisq_tet_genus.txt"))

# chi-square test for the aggregation of terrestrial vetebrates cl --------

chisq <- comp.test(a$class, a$BEs)
capture.output(chisq, file = here("outputs", "tests", "chisq_tet_class.txt"))

# chi-square test for the assignment to BEs or to Noise Component ---------

classes <- c(124,	66,	63,	45,	42)
noise <- c(46,	32,	35,	18,	27)
wnBEs <- c(78,	34,	28,	27,	15)

chisq <- chisq.test(classes, noise, simulate.p.value = TRUE)
capture.output(chisq, file = here("outputs", "tests", "chisq_class_nc.txt"))

chisq <- chisq.test(classes, wnBEs)
capture.output(chisq, file = here("outputs", "tests", "chisq_class_wnBE.txt"))

# figuring out ------------------------------------------------------------
#only RR
plateau <- c(56, 16, 7, 6, 8) #Plateau spp
depression <- c(7, 11, 5, 1, 5) #Depression spp

#ALL units
plateau <- c(59, 17, 15, 12, 8) #Plateau spp
depression <- c(15, 13, 7, 4, 6) #Depression spp

chisq <- chisq.test(plateau, depression)
capture.output(chisq, file = here("outputs", "tests", "chisq_alt_sp.txt"))

# end ---------------------------------------------------------------------