library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lme4)
library(here)
library(tidyr)

list <- read.csv(here("data", "list.csv"))
#db <- read.csv(here("data", "baseunique_alt.csv"), stringsAsFactors=FALSE, fileEncoding="latin1")
db_full <- read.csv(here("data", "BD_endemics.csv"), stringsAsFactors=FALSE)
be <- read.csv(here("outputs", "tables", "n2_cd015.csv"))
comp <- read.csv(here("outputs", "tables", "summary.csv"))

head(list)
head(db_full)
head(be)
head(comp)

sum(sort(unique(db_full$species))!=sort(unique(be$species))) #checking for typos
sum(sort(unique(db_full$species))!=sort(unique(list$species))) ##checking for typos

#in the case of typos positive:
#unique(db_full$species)[which(sort(unique(db_full$species))!=sort(unique(be$species)))] #checking which are the typos

?merge
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

new_recs <- db_be_full[db_be_full$New == "s",] #numero de records para especies novas na análise 9,416

# Extracting unique records -----------------------------------------------

head(db_be_full)
db <- db_be_full[, c(1, 8:10)] #selecting required fields
head(db)

db$unique <- paste(db$species, db$latitude, db$longitude, db$elevation, sep=",") #combining spp name and coordinates

head(db) #checking the dataframe

uniquerec <- data.frame(unique(db$unique)) #selecting unique combinations of spp names and coordinates

head(uniquerec)

db_unique <- tidyr::separate(data=uniquerec, col="unique.db.unique.", into=c("species", "latitude", "longitude", "elevation"), sep=",") #separating spp name from coordinates into a unique records dataframe

head(db_unique) #checking the resulting dataframe
str(db_unique)

db_unique$elevation <- as.numeric(db_unique$elevation)

summary(db_unique$elevation) #summary with unique records
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   376.0   611.0   624.6   861.0  2067.0 

# merging necessary fields to unique records dataframe --------------------

db_be <- merge(db_unique, be, by="species") #add BEs info to dataframe
head(db_be)

db_be <- merge(db_be, list, by="species")
head(db_be) #unique records, with elevation, BEs and list information

comp <- comp[, c(1, 14)]
head(comp)

db_be <- merge(db_be, comp, by="BEs") #combine with info or BE range classification AND REMOVE NOISE COMPONENT SPECIES RECORDS
head(db_be)

str(db_be)

db_be$BEs <- as.factor(db_be$BEs) #transforma BEs em fatores
db_be$BEs <- relevel(db_be$BEs, "2") #transforma BE 2 (widespread) como nível basal dos fatores

str(db_be)

# linear models -----------------------------------------------------------

mod1 <- lm(elevation~BEs, data=db_be)
summary(mod1)
capture.output(summary(mod1), file = here("outputs", "tests", "elevation_all.txt"))

mod2 <- lm(elevation~BEs, data=db_be[db_be$range_be=="restricted",])
summary(mod2)
capture.output(summary(mod2), file = here("outputs", "tests", "elevation_restricted.txt"))

# modelos mistos ----------------------------------------------------------

lmm <- lmer(formula = alt ~ BEs + (1|family/genus/species), data = db_be)
summary(lmm)
print(lmm)
#Para o calculo de variação entre os efeitos aleatórios
vc <- c(60535, 29560) #Variance: c(Residual, Species:(genus:family), genus:family, family)
vc <- 100*c(60535, 29560)/sum(vc)
vc

#lmm2 <- lmer(formula = alt ~ BEs + (1|genus/species), data = db_be)
#summary(lmm2)

anova(lmm2, lmm, refit = FALSE) #significantivo, nao remover family

lmm3 <- lmer(formula = alt ~ BEs + (1|family) + (1|species), data = db_be)
anova(lmm3, lmm, refit = FALSE) #nao significativo, podemos remover genus
summary(lmm3) #eh o melhor

lmm4 <- lmer(formula = alt ~ BEs + (1|Species), data = db_be)
anova(lmm4, lmm3, refit = FALSE) #significativo, não remover family

lmm5 <- lmer(formula = alt ~ BEs + (1|family), data = db_be)
anova(lmm5, lmm3, refit = FALSE) #significativo, não remover species

#Melhor modelo lmm3
summary(lmm3)

vc <- c(60525, 26385, 3986) #Variance: c(Residual, Species:(genus:family), genus:family, family)
vc <- 100*c(60525, 26385, 3986)/sum(vc)
vc

# boxplot graph -----------------------------------------------------------

#Vetores com cores dos plots
col_be <- c("grey","orange","purple","purple","black","blue","red","green","orange","orange","orange",
            "orange", "purple", "black","blue","green","green", "green","green", "red","red","red") #organização 1 (wd, pt, rr)
#Graph one
#Ultimo grafico atualizado com col_be (wd, pt rr)
td <- ggplot(db_be, aes(x=BEs, y=elevation, fill=range_be)) +
  labs(x= "Biotic Elements", y= "Elevation (m)")+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 2000))+
  scale_x_discrete(limits=c("2","23","15","28","26","29","14","22","4","13","20","11","27","19", "10", "6","17","25","18","16","12","7","5","9","24","8","3","21","1"))+
  geom_hline(yintercept = 592)+
  geom_vline(xintercept = 1.5, linetype=2)+
  geom_vline(xintercept = 10.5, linetype=2)+
  theme_classic()
td

alt_graph <- td + geom_label(x=5, y=2000, label="Partial") + geom_label(x=19, y=2000, label="Restricted-Range")
alt_graph

ggsave("test2.png",
       plot = alt_graph,
       device = "png",
       path = here("outputs", "images"),
       units = "mm",
       width = 110,
       height = 75,
       dpi = 300)

?ggsave #salvar graficos!
