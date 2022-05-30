library(tidyverse)
library(here)
library(tidyr)

sum(sort(unique(db_full$species))!=sort(unique(be$species))) #checking for typos
sum(sort(unique(db_full$species))!=sort(unique(list$species))) ##checking for typos

#in the case of typos positive:
#unique(db_full$species)[which(sort(unique(db_full$species))!=sort(unique(be$species)))] #checking which are the typos

db_be_full <- merge(db_full, list, by="species") # add list info
head(db_be_full) #full database com list

db_be_full <- merge(db_be_full, be, by="species") # add be info
head(db_be_full) #full database com list and be info

summary(db_be_full$elevation) #summary with full database (repeated elevation data for especimens from the same location)
#Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#0     375      610      609.9   823      2067

table(list$class) #number of species per class
#Amphibians       Aves    Lizards    Mammals     Snakes 
#124               45         66         42         63

#increase in relation to Azevedo et al 2016 (216 species):
340/216

table(db_be_full$class) #number of records per class
#Amphibians         Aves      Lizards      Mammals       Snakes
#     3074          6423         2738          840        10228

new_recs <- db_be_full[db_be_full$New == "s",] 
length(new_recs[,1]) #new records added to the analysis = 9,416

# On the unique records ---------------------------------------------------

summary(db_unique$elevation) #summary with unique records
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   376.0   611.0   624.6   861.0  2067.0 

# merging necessary fields to unique records dataframe --------------------

db_be <- merge(db_unique, list, by="species")
head(db_be) #unique records, with elevation, BEs and list information

db_be <- merge(db_unique, be, by="species") #add BEs info to dataframe
head(db_be)

comp <- comp[, c(1, 14:15)]
head(comp)

db_be <- merge(db_be, comp, by="BEs") #combine with info or BE range classification AND REMOVE NOISE COMPONENT SPECIES RECORDS
head(db_be)

str(db_be)

db_be$BEs <- as.factor(db_be$BEs) #BEs into factors
db_be$BEs <- relevel(db_be$BEs, "2") #BE 2 (widespread) as basal level

# linear models -----------------------------------------------------------

mod1 <- lm(elevation~BEs, data=db_be) #basal level is BE 2 the widespread BE
summary(mod1)
capture.output(summary(mod1), file = here("outputs", "tests", "elevation_all.txt"))

mod2 <- lm(elevation~BEs, data=db_be[db_be$range_be=="restricted",]) #basal level is BE 1 the highest BE
summary(mod2)
capture.output(summary(mod2), file = here("outputs", "tests", "elevation_restricted.txt"))

# Open S_boxplot.R --------------------------------------------------------