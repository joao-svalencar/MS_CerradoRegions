library(nlme)
library(ape)
library(MuMIn)
library(tidyr)


el_all <- read.csv(here::here("data", "BD_endemics.csv"), stringsAsFactors=FALSE)
head(el_all)

el_unique <- read.csv(here::here("data", "elevation_unique.csv"), stringsAsFactors=FALSE)
head(el_unique)
bd_unique <- read.csv(here::here("data", "BD_unique.csv"), stringsAsFactors=FALSE)
head(bd_unique)

el_unique <- cbind(el_unique, bd_unique)
el_unique <- el_unique[,c(1,2,4,5)]
head(el_unique)
comp.int <- comp[,c(1, 14,15)]
comp.int <- comp

head(list)
el_unique_be <- merge(el_unique, be, by="species")
el_unique_be <- merge(el_unique_be, comp.int, by="BEs")
head(el_unique_be)


el_unique_be$BEs <- as.factor(el_unique_be$BEs) #BEs into factors
el_unique_be$BEs <- relevel(el_unique_be$BEs, "2") #BE 2 (widespread) as basal level
str(el_unique_be)

mod <- nlme::gls(elevation~BEs, data=el_unique_be) #basal level is BE 2 the widespread BE
summary(mod)


semivario <- nlme::Variogram(mod2, form = ~longitude + latitude, resType = "normalized")
plot(semivario, smooth = TRUE, ylim=c(0,1.6))


mod.exp <- nlme::gls(elevation~BEs, data=el_unique_be, correlation = corExp(form = ~longitude + latitude, nugget=T))

gaussian.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corGaus(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )

spherical.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corSpher(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )

linear.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corLin(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )

ratio.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corRatio(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )

#this means that positive spatial autocorrelation explains a lot about variation in our data.

# com o Moran's -----------------------------------------------------------

geo<-cbind(el_restricted$longitude, el_restricted$latitude)
geo<-cbind(el_cores$longitude, el_cores$latitude)

# Then, let us produce a distance matrix (Euclidean) using the longitude and latitude values.

samples.dist <- as.matrix(dist(geo))
samples.dist.inv <- 1/samples.dist
diag(samples.dist.inv) <- 0

samples.dist[1:10,1:10]
samples.dist.inv[1:10,1:10]

Moran.I(log(el_cores$elevation), samples.dist.inv, alternative="greater")

?Moran.I

summary(db_be$elevation)
hist(db_be$elevation)
hist(mod2$residuals)

