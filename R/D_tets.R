library(tidyverse)
library(here)
library(tidyr)

sum(sort(unique(db_full$species))!=sort(unique(be$species))) #checking for typos
sum(sort(unique(db_full$species))!=sort(unique(list$species))) ##checking for typos
sum(sort(unique(db_unique$species))!=sort(unique(be$species))) #checking for typos
sum(sort(unique(db_unique$species))!=sort(unique(list$species))) ##checking for typos

#in the case of typos positive:
#unique(db_full$species)[which(sort(unique(db_full$species))!=sort(unique(be$species)))] #checking which are the typos

# Merging and exploring elevation_full ------------------------------------

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

# Merging and exploring elevation_unique ----------------------------------

db_be <- merge(db_unique, list, by="species")
head(db_be) #unique records, with elevation, BEs and list information

db_be <- merge(db_be, be, by="species") #add BEs info to dataframe
head(db_be)

#comp <- comp[, c(1, 14:15)]
#head(comp)

#db_be <- merge(db_be, comp, by="BEs") #combine with info or BE range classification AND REMOVE NOISE COMPONENT SPECIES RECORDS
#head(db_be)

#str(db_be)

db_be$BEs <- as.factor(db_be$BEs) #BEs into factors
db_be$BEs <- relevel(db_be$BEs, "2") #BE 2 (widespread) as basal level

summary(db_be$elevation) #summary with unique records
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   376.0   611.0   624.6   861.0  2067.0 


# Species elevation classification ----------------------------------------

list$elev_class <- NA
spp_krusk <- names(which(table(db_be$species)>5)) #selecting species with more than 5 records

for(i in 1:length(spp_krusk))
{  
  spp <- spp_krusk[i] #creating object with species name
  spp_el <- db_be$elevation[db_be$species==spp] #selecting the elevation values for a given species 'i'
  #el_sample <- sample(db_be$elevation, 1000, replace=FALSE) #sampling random elevation values from our database
  el_sample <- sample(db_be$elevation, 1000, replace=TRUE) #sampling random elevation values from our database
  el_ktest <- c(spp_el, el_sample) #creating a vector combining a given species 'i' and the 1,000 random elevation values
  group <- rep(c("spp", "r.sample"), times=c(length(spp_el), 1000)) #creating a vector to group the species records and the random elevation values
  k.test <- data.frame(el_ktest, group) #creating a dataframe with elevation values and group categories
  kruskal <- kruskal.test(el_ktest~group, data=k.test) #comparing species elevation values and random elevation values
  
  class <- if(summary(db_be$elevation[db_be$species==spp])[2]>500)
              {
                "plateau"
              }else if(summary(db_be$elevation[db_be$species==spp])[5]<500)
                  {
                "depression"
                  }else if(kruskal$p.value<0.05)
                    {
                      if(tapply(k.test$el_ktest, k.test$group, median)[1]<tapply(k.test$el_ktest, k.test$group, median)[2])
                      {
                      "plateau"
                      }else("depression")
                  }else("general")
  
  list$elev_class[list$species==spp] <- class
  boxplot(k.test$el_ktest~k.test$group, xlab=class, ylab="Elevation", main=spp)
}

table(list$elev_class)
list

sd(db_be$elevation[db_be$species==spp])-summary(db_be$elevation[db_be$species==spp])[2]

sd(db_be$elevation[db_be$species==spp])-summary(db_be$elevation[db_be$species==spp])[5]

summary(db_be$elevation[db_be$species==spp])[1]-summary(db_be$elevation[db_be$species==spp])[3]
summary(db_be$elevation[db_be$species==spp])[5]-summary(db_be$elevation[db_be$species==spp])[3]

summary(db_be$elevation[db_be$species==spp])

# -------------------------------------------------------------------------



# linear models -----------------------------------------------------------
# mod1 - elevation_full ---------------------------------------------------

mod1 <- lm(elevation~BEs, data=db_be) #basal level is BE 2 the widespread BE
summary(mod1)
capture.output(summary(mod1), file = here("outputs", "tests", "elevation_all.txt"))

png(here("outputs", "tests","Diagnostics%02d.png"), width=6, height=6, units='in', res=300)
plot(mod1, ask = FALSE)
dev.off()

png(here("outputs", "tests","ResidualsHist.png"), width=6, height=6, units='in', res=300)
hist(mod1$residuals)
dev.off()

# mod2 - elevation_unique -------------------------------------------------

mod2 <- lm(elevation~BEs, data=db_be[db_be$range_be=="restricted",]) #basal level is BE 1 the highest BE
summary(mod2)
capture.output(summary(mod2), file = here("outputs", "tests", "elevation_restricted.txt"))

png(here("outputs", "tests","RestrictedDiagnostics%02d.png"), width=6, height=6, units='in', res=300)
plot(mod2, ask = FALSE)
dev.off()

png(here("outputs", "tests","RestrictedResidualsHist.png"), width=6, height=6, units='in', res=300)
hist(mod2$residuals)
dev.off()

# Open S_boxplot.R --------------------------------------------------------

# end ---------------------------------------------------------------------