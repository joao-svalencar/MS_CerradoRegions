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

dim(centro)[1] #checking the number of centroids; should be equal to the number of grid cells in $sample above
length(unique(pa_table$sample)) #checking the number of grid cells

length(unique(pa_table$species)) #checking the number of spp


# processing data: creating presence x absence matrix ---------------------

attach(pa_table)
mpa <-as.matrix(table(pa_table$species, pa_table$sample))
mpa <- 1*(mpa>0)

# write.csv(mpa, here("outputs", "tables", "SuppInfo_S4_mpa.csv"), row.names = TRUE)

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

class(x) #prab object used in 'prabtest' and 'cdn'

# prabtest ----------------------------------------------------------------

set.seed(42)
test <- prabtest(x, times=1000, pdfnb = TRUE) # prabtest started 30/09/2021 16:37/18:56

# test <- readRDS(here("outputs", "tests", "prabtest.rds")) # original result
summary(test)
capture.output(summary(test), file = here("outputs", "tests", "prabtest.txt"))

# saveRDS(object= test, file = here("outputs", "tests", "prabtest.rds"))

# Open D_clustering.R -----------------------------------------------------

###########################################################################
# processing data: preparing to test vicariance premise number two -------
###########################################################################
#BEs is an object from D_clustering.R

BEs.no.noise <- BEs[BEs$BEs!=0,] # removing noise component
head(BEs.no.noise)
BEs.no.noise$binomial <- BEs.no.noise$species # duplicating species list

a <- separate(data=BEs.no.noise, col="binomial", into=c("genus", "epithet"), sep=" ") # breaking binomial

# chi-square test for vicariance premisse number two ----------------------
set.seed(42)
chisq <- comp.test(a$genus, a$BEs)
capture.output(chisq, file = here("outputs", "tests", "chisq_tet_genus.txt"))

# chi-square test for the assignment to BEs or to Noise Component ---------
classes <- c(124,	66,	63,	45,	42) # number of species per vertebrate class
noise <- c(46,	32,	35,	18,	27) # number of species per vertebrate class assigned to noise
wnBEs <- c(78,	34,	28,	27,	15) # number of species per vertebrate class assigned to BEs

chisq <- chisq.test(classes, noise)
capture.output(chisq, file = here("outputs", "tests", "chisq_class_nc.txt"))

chisq <- chisq.test(classes, wnBEs)
capture.output(chisq, file = here("outputs", "tests", "chisq_class_wnBE.txt"))

# Test for the aggregation of vertebrate classes in distinct elevation categories --------
plateau <- c(56, 16, 7, 6, 8) #number of species per vertebrate class assigned to restricted plateau units
depression <- c(7, 11, 5, 1, 5) #number of species per vertebrate class assigned to restricted depression units

chisq <- chisq.test(plateau, depression)
capture.output(chisq, file = here("outputs", "tests", "chisq_alt_sp.txt"))

# Open C_tets.R and load the objects for the models of elevation
# end ---------------------------------------------------------------------