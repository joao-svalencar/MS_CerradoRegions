install.packages("nlme")
install.packages("ape")
install.packages("MuMIn")
library(nlme)
library(ape)
library(MuMIn)
library(tidyr)


el_all <- read.csv(here("data", "cent_BEs_all.csv"), stringsAsFactors=FALSE)
head(el_all)
names(el_all) <- c("BEs", "longitude", "latitude", "elevation")

el_all<-separate(el_all, BEs, into=c("BE-letter", "BEs"), sep=" ")

comp[comp$range_be=="restricted",]
comp.int <- comp[,c(1, 14,15)]

el_all_class <- merge(el_all, comp.int, by="BEs")
head(el_all_class)

el_restricted <- el_all_class[el_all_class$range_be=="restricted",]
unique(el_restricted$BEs)
head(el_restricted)

el_restricted$BEs <- as.factor(el_restricted$BEs) #BEs into factors
el_all$BEs <- relevel(el_all$BEs, "BE 2") #BE 2 (widespread) as basal level
str(el_restricted)

mod2 <- nlme::gls(elevation~BEs, data=el_restricted) #basal level is BE 2 the widespread BE
summary(mod2)

plot(elevation~BEs, data=el_restricted)

semivario <- nlme::Variogram(mod2, form = ~longitude + latitude, resType = "normalized")
plot(semivario, smooth = TRUE, ylim=c(0,1.6))

#this means that positive spatial autocorrelation explains a lot about variation in our data.

# com o Moran's -----------------------------------------------------------

geo<-cbind(el_restricted$longitude, el_restricted$latitude)

# Then, let us produce a distance matrix (Euclidean) using the longitude and latitude values.

samples.dist <- as.matrix(dist(geo))
samples.dist.inv <- 1/samples.dist
diag(samples.dist.inv) <- 0

samples.dist[1:10,1:10]
samples.dist.inv[1:10,1:10]

Moran.I(log(el_restricted$elevation), samples.dist.inv, alternative="greater")
hist(log(el_restricted$elevation))
?Moran.I

summary(db_be$elevation)
hist(db_be$elevation)
hist(mod2$residuals)

