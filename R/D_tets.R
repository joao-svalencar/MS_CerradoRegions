library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggpattern)
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

comp <- comp[, c(1, 14:16)]
head(comp)

db_be <- merge(db_be, comp, by="BEs") #combine with info or BE range classification AND REMOVE NOISE COMPONENT SPECIES RECORDS
head(db_be)

str(db_be)

db_be$BEs <- as.factor(db_be$BEs) #transforma BEs em fatores
db_be$BEs <- relevel(db_be$BEs, "2") #transforma BE 2 (widespread) como nível basal dos fatores

# linear models -----------------------------------------------------------

mod1 <- lm(elevation~BEs, data=db_be)
summary(mod1)
capture.output(summary(mod1), file = here("outputs", "tests", "elevation_all.txt"))

mod2 <- lm(elevation~BEs, data=db_be[db_be$range_be=="restricted",])
summary(mod2)
capture.output(summary(mod2), file = here("outputs", "tests", "elevation_restricted.txt"))

# boxplot graph -----------------------------------------------------------

#Vetores com cores dos plots
col_be <- c("grey","orange","purple","purple","purple","grey","green","blue","blue","red",
            "purple","purple","orange", "orange", "orange", "orange","orange","orange", "black","black","black","brown","green","green","green","blue","blue","blue","red") #organização 1 (wd, pt, rr)


#Graph one
#Ultimo grafico atualizado com col_be (wd, pt rr)
td <- ggplot(db_be, aes(x=BEs, y=elevation, fill=alt_class)) +
  labs(x= "Biotic Elements", y= "Elevation (m)")+
  geom_boxplot(col=col_be)+
  scale_y_continuous(limits = c(0, 2000))+
  scale_fill_manual(values=c("white", "white", "lightgrey"), aesthetics = "fill")+
  scale_x_discrete(limits=c("2","23","15","28","26","29","22","14","4","13","20","17","11","27","19", "10", "6","25","18","16","12","7","5","9","3","24","8","21","1"))+
  geom_hline(yintercept = 592)+
  geom_vline(xintercept = 1.5, linetype=2)+
  geom_vline(xintercept = 10.5, linetype=2)+
  theme_classic()

td

alt_graph <- td + geom_label(x=5, y=2000, label="Partial") + geom_label(x=19, y=2000, label="Restricted-Range")
alt_graph

ggsave("test2.png",
       plot = td,
       device = "png",
       path = here("outputs", "images"),
       units = "mm",
       width = 110,
       height = 75,
       dpi = 300)

?ggsave #salvar graficos!

#com pattern:
td <- ggplot(db_be) +
  geom_boxplot_pattern(
    aes(x=BEs, y=elevation, pattern=alt_class), col=col_be,
    pattern_density=0.2, pattern_colour= 'black', pattern_spacing=0.01)+
  labs(x= "Biotic Elements", y= "Elevation (m)")+
  scale_y_continuous(limits = c(0, 2000))+
  scale_pattern_manual(values=c("none", "none", "circle"))+
  scale_x_discrete(limits=c("2","23","15","28","26","29","22","14","4","13","20","17","11","27","19", "10", "6","25","18","16","7","12","5","9","3","24","8","21","1"))+
  geom_hline(yintercept = 592)+
  geom_vline(xintercept = 1.5, linetype=2)+
  geom_vline(xintercept = 10.5, linetype=2)+
  theme_classic()+
  theme(legend.position='none', 
        axis.title.x = element_text(size=8), 
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8))
td

alt_graph <- td + geom_label(x=5, y=2000, label="Partial") + geom_label(x=19, y=2000, label="Restricted-Range")
alt_graph

ggsave("Fig 3.4.png",
       plot = alt_graph,
       device = "png",
       path = here("outputs", "images"),
       units = "mm",
       width = 110,
       height = 75,
       dpi = 300)
