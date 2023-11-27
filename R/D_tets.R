library(tidyverse)
library(here)
library(tidyr)
library(prabclus)

sum(sort(unique(db_full$species))!=sort(unique(be$species))) #checking for typos
sum(sort(unique(db_full$species))!=sort(unique(list$species))) ##checking for typos
sum(sort(unique(db_unique$species))!=sort(unique(be$species))) #checking for typos
sum(sort(unique(db_unique$species))!=sort(unique(list$species))) ##checking for typos

#in the case of typos positive:
#unique(db_full$species)[which(sort(unique(db_full$species))!=sort(unique(be$species)))] #checking which are the typos

# Merging species list with BEs information -------------------------------

comp <- comp[, c(1, 15, 17)]
be <- merge(be, comp, by= "BEs")
list_be <- merge(be, list, by="species")
head(list_be) #unique records, with elevation, BEs and list information

# Exploring list ---------------------------------------------------

table(list$class) #number of species per class
#Amphibians       Aves    Lizards    Mammals     Snakes 
#124               45         66         42         63

# Merging and exploring db_full ------------------------------------

db_be_full <- merge(db_full, list_be, by="species") # add list info
head(db_be_full) #full database with list

table(db_be_full$class) #number of records per class
#Amphibians         Aves      Lizards      Mammals       Snakes
#     3074          6423         2738          840        10228

new_recs <- db_be_full[db_be_full$New == "s",] 
length(new_recs[,1]) #new records added to the analysis = 9,416

#increase in relation to Azevedo et al 2016 (216 species):
340/216

# Merging and exploring db_unique ----------------------------------

db_be <- merge(db_unique, list_be, by="species")
head(db_be) #unique records, with elevation, BEs and list information

db_be$BEs <- as.factor(db_be$BEs) #BEs into factors


summary(db_be$elevation) #summary with unique records
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   376.0   611.0   624.6   861.0  2067.0 

# Species elevation classification ----------------------------------------

list_be$elev_class <- NA
spp_krusk <- list_be$species #creating species list to loop

set.seed(42)

for(i in 1:length(spp_krusk))
{  
  spp <- spp_krusk[i] #creating object with species name
  spp_el <- db_be$elevation[db_be$species==spp] #selecting the elevation values for a given species 'i'
  el_sample <- sample(db_be$elevation, 1000, replace=FALSE) #sampling random elevation values from our database
  el_ktest <- c(spp_el, el_sample) #creating a vector combining a given species 'i' and the 1,000 random elevation values
  group <- rep(c("spp", "r.sample"), times=c(length(spp_el), 1000)) #creating a vector to group the species records and the random elevation values
  k.test <- data.frame(el_ktest, group) #creating a dataframe with elevation values and group categories
  kruskal <- kruskal.test(el_ktest~group, data=k.test) #comparing species elevation values and random elevation values
  
  class <- if(kruskal$p.value<0.05)
  {
    if(summary(spp_el)[3]>500)
      {"plateau"}
    else("depression")
  }else if(summary(spp_el)[2]>500)
    {"plateau"}
  else if(summary(spp_el)[5]<500)
    {"depression"}
  else("general")
  
  list_be$elev_class[list_be$species==spp] <- class
  #boxplot(k.test$el_ktest~k.test$group, xlab=class, ylab="Elevation", main=spp)
  #mtext(paste("p.value = ", kruskal$p.value), side=3)
}

# chi-square test for plateaus/depression class ---------------------------

set.seed(42)

# with all species assigned to BEs ----------------------------------------

chisq <- prabclus::comp.test(list_be$elev_class[list_be$BEs!=0], list_be$BEs[list_be$BEs!=0])
capture.output(chisq, file = here("outputs", "tests", "chisq_elev_noNoise.txt"))

# only with restricted BEs ------------------------------------------------

chisq <- prabclus::comp.test(list_be$elev_class[list_be$range_be=="restricted"], list_be$BEs[list_be$range_be=="restricted"])
capture.output(chisq, file = here("outputs", "tests", "chisq_elev_restricted.txt"))

# Biotic Elements elevation classification --------------------------------

list_be$BEs_elev_class <- NA
be_krusk <- unique(list_be$BEs) #creating species list to loop

set.seed(42)

for(i in 1:length(be_krusk))
{  
  be.k <- be_krusk[i] #creating object with species name
  be_el <- db_be$elevation[db_be$BEs==be.k] #selecting the elevation values for a given species 'i'
  el_sample <- sample(db_be$elevation, 1000, replace=FALSE) #sampling random elevation values from our database
  be_el_ktest <- c(be_el, el_sample) #creating a vector combining a given species 'i' and the 1,000 random elevation values
  group <- rep(c(be.k, "r.sample"), times=c(length(be_el), 1000)) #creating a vector to group the species records and the random elevation values
  k.test <- data.frame(be_el_ktest, group) #creating a dataframe with elevation values and group categories
  kruskal <- kruskal.test(be_el_ktest~group, data=k.test) #comparing species elevation values and random elevation values
  
  class <- if(kruskal$p.value<0.05)
  {
    if(summary(be_el)[3]>500)
    {"plateau"}
    else("depression")
  }else if(summary(be_el)[2]>500)
  {"plateau"}
  else if(summary(be_el)[5]<500)
  {"depression"}
  else("general")
  
  list_be$BEs_elev_class[list_be$BEs==be.k] <- class
  #boxplot(k.test$be_el_ktest~k.test$group, xlab=class, ylab="Elevation", main=paste("Biotic Element", be.k, sep=" "))
  #mtext(paste("p.value = ", kruskal$p.value), side=3)
}

# showing each BE elevation class -----------------------------------------

list_be[!duplicated(list_be$BEs), c(2, 3, 12)]
head(list_be)

write.csv(list_be, here("outputs", "tables", "full.list.csv"), row.names = FALSE)

# Open S_boxplot.R --------------------------------------------------------

# end ---------------------------------------------------------------------