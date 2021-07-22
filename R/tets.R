library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lme4)
library(here)

list <- read.csv(here("data", "list.csv"))
db <- read.csv(here("data", "baseunique_alt.csv"), stringsAsFactors=FALSE, fileEncoding="latin1")
db_full <- read.csv(here("data", "BD_endemics.csv"), stringsAsFactors=FALSE, fileEncoding="latin1")
be <- read.csv(here("outputs", "tables", "spp_hier_2_015.csv"))
comp <- read.csv(here("outputs", "tables", "comp.csv"))

head(list)
list <- list[,c(1:3, 5:7, 11:12)]
head(list)
head(db)
head(db_full) #já tem o alt
db_full <- db_full[,c(4:13)]
head(db_full)
head(be)
head(comp)
names(comp)[14] <- "range_be"


sum(sort(unique(db_full$Species))!=sort(unique(be$Species))) #Verificando se existem typos
sum(sort(unique(db_full$Species))!=sort(unique(list$Species))) #Verificando se existem typos
sum(sort(unique(db$Species))!=sort(unique(be$Species))) #Verificando se existem typos
sum(sort(unique(db$Species))!=sort(unique(list$Species))) #Verificando se existem typos

?merge
db_be_full <- merge(db_full, list, by="Species") # add list info
head(db_be_full) #full database com list

db_be_full <- merge(db_be_full, be, by="Species") # add be info
head(db_be_full) #full database com list and be info

summary(db_be_full$alt)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0     372     610     606     810    2067

table(db_be_full$class) #numero de registros por classe
#Amphibians         Aves      Lizards      Mammals       Snakes
#     3084          6458         2738          840        10834

new_recs <- db_be_full[db_be_full$New == "s",] #numero de records para especies novas na análise 9333

db_be <- merge(db, be, by="Species") #adiciona a info de BEs à base de dados
head(db_be)
sort(unique(db_be$BEs)) #verifica os BEs
unique(db_be$Species[db_be$BEs=="4"]) #verifica as spp que compoem o BE "x"

db_be <- merge(db_be, list, by="Species")
head(db_be) #unique values, com alt, BEs e list
str(db_be)

db_be <- merge(db_be, comp, by="BEs")
head(db_be) #unique values, com alt, BEs e list

?hist
hist(db_be$alt) #mostra a distribuicao das frequencias de alt
summary(db_be$alt) #verifica a classe de cada coluna no df
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   371.2   610.0   620.7   856.8  2067.0 

str(db_be)
db_be$BEs <- as.factor(db_be$BEs) #transforma BEs em fatores

#db_be <- db_be[db_be$BEs!=0,] #REMOVE OS NOISE COMPONENT! Cai de 14,318 para 9,340 registros unicos
#db_be$BEs <- relevel(db_be$BEs, "2") #transforma BE 2 (widespread) como nível basal dos fatores

##### MODELOS #####
mod1 <- lm(alt~BEs, data=db_be)
summary(mod1)
?par
par(mfrow=c(2,2))
plot(mod1)

mod2 <- lm(alt~BEs, data=db_be[db_be$range_be=="restricted",])
summary(mod2)
?par
par(mfrow=c(2,2))
plot(mod2)


#glm1 <- glm(formula = alt ~ BEs, family = "poisson", data = db_be)
#summary(glm1)
#as.data.frame(exp(coef(glm1)))

lmm <- lmer(formula = alt ~ BEs + (1|family/genus/Species), data = db_be)
summary(lmm)
print(lmm)
#Para o calculo de variação entre os efeitos aleatórios
vc <- c(60535, 29560) #Variance: c(Residual, Species:(genus:family), genus:family, family)
vc <- 100*c(60535, 29560)/sum(vc)
vc

#lmm2 <- lmer(formula = alt ~ BEs + (1|genus/Species), data = db_be)
#summary(lmm2)

anova(lmm2, lmm, refit = FALSE) #significantivo, nao remover family

lmm3 <- lmer(formula = alt ~ BEs + (1|family) + (1|Species), data = db_be)
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

#testando outro pacote:
library(nlme)
lmm8 <- lme(alt~BEs, random= ~1|family/Species, data=db_be)
summary(lmm8)

lmm9 <- lme(alt~BEs, random= ~1|Species, data=db_be)
summary(lmm9)


# boxplot graph -----------------------------------------------------------


#Vetores com cores dos plots
col_be <- c("grey","orange","purple","purple","black","blue","red","green","orange","orange","orange",
            "orange", "orange", "black","blue","green","green", "green","green", "red","red","red") #organização 1 (wd, pt, rr)

#col_be <- c("grey", "red", "green", "orange", "green", "red", "blue", "purple", "red", "orange","green",
"red", "red", "orange", "blue", "orange", "blue", "purple", "purple", "green", "purple") #cores de todos os BEs

col_res <- c("red", "green", "orange", "green", "red", "orange","green",
             "red", "red", "blue", "orange", "blue", "purple", "purple", "purple") #cores apenas dos restricted

col_wd <- c("orange", "purple", "darkgreen", "blue", "grey", "red") #cores apenas dos wide

col_rg <- c("orange", "orange", "orange", "purple", "purple", "purple", 
            "black", "darkgreen","darkgreen","darkgreen","blue","blue","red","red","red") #cores ordenadas por altitude

range <- c("N", "w", "r", "r", "r", "r", "w", "w", "w", "r", "r", "r",
           "r", "r", "w", "r", "r", "r", "r", "r", "w", "r") #range de todos os BEs + noise component

seq <- seq(0, 22) #criando objeto para combinar cores e range do BE na base de dados

col2 <- data.frame(seq, col_be, range) #combinando os vetores em um df

scale_x_discrete(limits=c("0","2","3","10","15","19","20","22","21","11","7","6","18","13","4","8","14","16","17","1","12","5","9"))

#Graph one
#Ultimo grafico atualizado com col_be (wd, pt rr)
td <- ggplot(db_be, aes(x=BEs, y=alt)) +
  labs(x= "Biotic Elements", y= "Elevation (m)")+
  geom_boxplot(color=col_be)+
  scale_y_continuous(limits = c(0, 2000))+
  scale_x_discrete(limits=c("2","20","19","22","21","3","10","15","7","11","6","18","13","4","17","8","14","16","12","9","1","5"))+
  geom_hline(yintercept = 570)+
  geom_vline(xintercept = 1.5, linetype=2)+
  geom_vline(xintercept = 8.5, linetype=2)+
  theme_classic()
td

alt_graph <- td + geom_label(x=5, y=2000, label="Partial") + geom_label(x=14.5, y=2000, label="Restricted-Range")

ggsave("test2.tiff",
       plot = alt_graph,
       device = "tiff",
       units = "mm",
       width = 110,
       height = 75,
       dpi = 300)

?ggsave #salvar graficos!
