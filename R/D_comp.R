install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

comp <- read.csv("composition.csv")
head(comp)
str(comp)
comp$alt_class <- as.factor(comp$alt_class)
unique(comp$alt_class)

#comp_nowd <- comp[comp$alt_class!="wide",]
#comp_nowd$codes <- c(1,1,0,1,1,0,1,1,1,1,0,1,0,0,0)
#comp_nowd$codes <- as.factor(comp_nowd$codes)
#str(comp_nowd)
#head(comp_nowd)

######
#lm <- lm(Area~Amphibians+Aves+Lizards+Mammals+Snakes, data=comp) #relação area do BE versus classe
#summary(lm)

#lm1 <- lm(Area~Aves+Lizards+Mammals+Snakes, data=comp)
#summary(lm1)

#anova(lm, lm1) #sem diferença, retenho lm1

#lm2 <- lm(Area~Aves+Lizards+Snakes, data=comp)
#summary(lm2)

#anova(lm1, lm2) #sem diferença, retenho lm2

#lm3 <- lm(Area~Aves+Mammals, data=comp)
#summary(lm3)

#anova(lm2, lm3) #diferentes! retemos o mais complexo Aves+Lizards+Mammals

#uma anarquia só num unico grafico
#ggplot(comp) + 
  #geom_jitter(aes(Aves,Area), colour="green") + geom_smooth(aes(Aves,Area), colour= "green", method=lm, se=FALSE) +
  #geom_jitter(aes(Amphibians,Area), colour="yellow") + geom_smooth(aes(Amphibians,Area),  colour= "yellow", method=lm, se=FALSE) +
  #geom_jitter(aes(Lizards, Area), colour="grey") + geom_smooth(aes(Lizards, Area),  colour= "grey", method=lm, se=FALSE) +
  #geom_jitter(aes(Mammals, Area), colour="blue") + geom_smooth(aes(Mammals, Area),  colour= "blue", method=lm, se=FALSE) +
  #geom_jitter(aes(Snakes, Area), colour="red") + geom_smooth(aes(Snakes, Area),  colour= "red", method=lm, se=FALSE) +
  #labs(x = "Number of individuals (N)", y = "Size of Biotic Elements")

#comp.tet <- data.frame(comp$Area, comp$Aves, comp$Amphibians, comp$Lizards, comp$Mammals, comp$Snakes)
#colnames(comp.tet) <- c("area", "aves", "amphibians", "lizards", "mammals", "snakes")

#gráficos separados e com legenda

#comp2 <- melt(comp.tet, id.vars='area')
#?melt
#ggplot(comp2) +
  #geom_jitter(aes(value, area, colour=variable),) + 
  #geom_smooth(aes(value, area, colour=variable), method=lm, se=FALSE) +
  #facet_wrap(~variable, scales="free_x") +
  #labs(x = "Number of individuals (N)", y = "Size of Biotic Elements")+ 
  #theme_classic()



#chi-square test for noise versus total:

be <- c(0.17424242,	0.40151515,	0.13636364,	0.10606061,	0.18181818) #class.be/total.be
noise <- c(0.10798122, 0.33333333, 0.22535211, 0.1314554, 0.20187793) #class.noise/total.noise 
total <- c(0.13333333, 0.35942029, 0.19130435, 0.12173913, 0.1942029) #class.total/species.total

#wd_sp <- c(0.22, 0.67, 0.04, 0.29, 0.61)
#rr_sp <- c(0.78, 0.33, 0.96, 0.71, 0.39)


a <- chisq.test(wd_sp, total)
a[[8]]
chisq.test(rr_sp, total)
chisq.test(be, total)
chisq.test(noise, total)

?chisq.test

#chi-square test for plateau versus depression:
plateau <- c(5, 51, 15, 10, 7) #para numero absoluto de espécies
depression <- c(5, 8, 12, 2, 8)

chisq.test(plateau, depression)

plateau <- c(2, 9, 6, 7, 5) #para numero de BEs
depression <- c(2, 2, 4, 2, 4)

chisq.test(plateau, depression)

# Quando da significativo significa que as spp de um mesmo genero estao todas num unico elemento biotico
# Portanto, para atender a premissa, o valor de p deve ser nao signifcativo (aka.: >0.05)

######################################## RELACAO ALTITUDITAL ENTRE BEs ##################################################

db <- read.csv("db.csv") #base de dados ja com valores de altitudes extraidos do QGIS
db <- db[,c(2,3,4,5)] #limpeza
colnames(db)[4] <- "alt" #atribuindo alt

head(db) #verificacao preliminar
unique(db$Species) #verificacao numero de spp

#list <- read.csv("list.csv") #lista spp por BE
#head(list) #verificacao preliminar
#which(sort(unique(list$Species))!=sort(unique(db$Species))) #comparacao entre os nomes das spp, verificacao de tipos
#table(list$Class)

db_be <- merge(db, list, by="Species") #combinacao dos objetos

head(db_be) #verificacao preliminar

db_be$BEs <- as.factor(db_be$BEs) #conversao de BEs para classe factor
str(db_be) #conferindo estrutura do objeto

#Vetores com cores dos plots

col_be <- c("grey", "red", "green", "orange", "green", "red", "blue", "purple", "red", "orange","green",
            "red", "red", "orange", "blue", "orange", "blue", "purple", "purple", "green", "purple") #cores de todos os BEs

col_res <- c("red", "green", "orange", "green", "red", "orange","green",
             "red", "red", "blue", "orange", "blue", "purple", "purple", "purple") #cores apenas dos restricted

col_wd <- c("orange", "purple", "darkgreen", "blue", "grey", "red") #cores apenas dos wide

col_rg <- c("orange", "orange", "orange", "purple", "purple", "purple", 
            "black", "darkgreen","darkgreen","darkgreen","blue","blue","red","red","red") #cores ordenadas por altitude

range <- c("N", "w", "r", "r", "r", "r", "w", "w", "w", "r", "r", "r",
           "r", "r", "w", "r", "r", "r", "r", "r", "w", "r") #range de todos os BEs + noise component

seq <- seq(0, 21) #criando objeto para combinar cores e range do BE na base de dados

####
col2 <- data.frame(seq, col_be, range) #combinando os vetores em um df
####
head(col2) #verificacao prelimilar
colnames(col2) <- c("BEs", "col", "range") #atribuicao de nomes as colunas
head(col2) #verificacao preliminar 2

db_be2 <- merge(db_be, col2, by="BEs") #combinacao dos objetos pela coluna BEs
head(db_be2) #verificacao preliminar
sort(unique(db_be2$BEs)) #verificacao do numero de bes

db_res <- db_be2[db_be2$range=="r",] #separando apenas BEs com restricted range
head(db_res) #verificacao preliminar
str(db_res) #verificacao da estrutura

db_wd <- db_be2[db_be2$range=="w",] #separando apenas BEs com restricted range
head(db_wd) #verificacao preliminar
str(db_wd) #verificacao da estrutura


sort(unique(db_res$BEs)) #verificacao dos bes presentes no objeto
sort(unique(db_wd$BEs)) #verificacao dos bes presentes no objeto

########## TESTES ESPECIES WD DOS BES ###########

unique(db_be2$Species)
length(db_be2$Species)
unique(db_be2$Species[db_be2$Species!="Polystictus superciliaris"])
length(db_be2$Species[db_be2$Species!="Polystictus superciliaris"])

####################################################
########## REMOVENDO ESPECIES WD DOS BES ###########
####################################################

#BE 3

db_nowd <- db_be2[db_be2$Species!="Polystictus superciliaris",] #remocao de 78 pontos
length(db_nowd$Species)

db_nowd <- db_nowd[db_nowd$Species!="Embernagra longicauda",] #remocao de 107 pontos
length(db_nowd$Species)

#BE 5

db_nowd <- db_nowd[db_nowd$Species!="Oligoryzomys rupestris",] #remocao de 3 pontos
length(db_nowd$Species)

#BE 10

db_nowd <- db_nowd[db_nowd$Species!="Ctenomys nattereri",] #remocao de 8 pontos
length(db_nowd$Species)

db_nowd <- db_nowd[db_nowd$Species!="Sporophila nigrorufa",] #remocao de 10 pontos
length(db_nowd$Species)

db_nowd <- db_nowd[db_nowd$Species!="Stenocercus albolineatus*",] #remocao de 14 pontos
length(db_nowd$Species)

#BE 11

db_nowd <- db_nowd[db_nowd$Species!="Proceratophrys goyana",] #remocao de 14 pontos
length(db_nowd$Species)

#BE 12

db_nowd <- db_nowd[db_nowd$Species!="Thrichomys apereoides",] #remocao de 11 pontos
length(db_nowd$Species)

#BE 15

db_nowd <- db_nowd[db_nowd$Species!="Akodon lindberghi",] #remocao de 5 pontos
length(db_nowd$Species)

#BE 17

db_nowd <- db_nowd[db_nowd$Species!="Scytalopus novacapitalis",] #remocao de 23 pontos
length(db_nowd$Species)

#BE 21

db_nowd <- db_nowd[db_nowd$Species!="Calomys tocantinsi",] #remocao de 6 pontos
length(db_nowd$Species)

db_nowd <- db_nowd[db_nowd$Species!="Cercomacra ferdinandi",] #remocao de 48 pontos
length(db_nowd$Species)

db_nowd <- db_nowd[db_nowd$Species!="Paroaria baeri",] #remocao de 22 pontos
length(db_nowd$Species)

#########################################################################
############### FIM REMOCAO de 13 WD SPECIES DOS RESTRICTED BEs #########
#########################################################################

length(unique(db_be2$Species))
unique(db_be2$Species)

length(unique(db_nowd$Species))
unique(db_nowd$Species)

length(unique(db_be2$Species))-length(unique(db_nowd$Species)) #total de 13 especies removidas

####################################################################################
#Analise do db reduzido
head(db_nowd)
str(db_nowd)
unique(db_nowd$BEs)

###################### RETIRANDO NOISE COMPONENT DA BASE ##########################
#da base completa
length(db_be2$BEs)
db_nonc <- db_be2[db_be2$BEs!=0,] #remocao de 2378 pontos
length(db_nonc$BEs)
str(db_nonc)
unique(db_nonc$BEs)

#da base reduzida
length(db_nowd$BEs)
db_nowdnc <- db_nowd[db_nowd$BEs!=0,] #remocao de 2378 pontos
length(db_nowdnc$BEs)
str(db_nonc)
unique(db_nonc$BEs)


# Agora temos quatro bases passiveis de analise e plot: db_be2, db_nowd, db_nonc e db_nowdnc
head(list) #aqui é pra adicionar as classes à base de dados
head(db_be2)

#verifica numero de spp
unique(list$Species)
unique(db_be2$Species)

list2 <- list[list$BEs!=0,]

#merging
db_be3 <- merge(db_be2, list, by="Species") #combinacao dos objetos pela coluna BEs
db_be3 <- merge(db_nowdnc, list2, by="Species") #combinacao dos objetos pela coluna BEs

#numero de spp
unique(db_be3$Species)
head(db_be3)

db_be3 <- db_be3[-8]
#################################################################################################################
lm0 <- lm(data=db_be3, formula=alt~Class) #altitude em função da classe ###########
summary(lm0)
#################################################################################################################
#modelo linear altitude/BEs
lm <- lm(alt~BEs, data=db_be2)
summary(lm)
#################################################################################################################

lm2 <- lm(alt~BEs, data=db_res) #só restricted BEs
summary(lm2)

lm3 <- lm(alt~BEs, data=db_nowdnc) #sem wd species e sem noise component species
summary(lm3)

############### boxplots altitudes versus BEs #####################
library(ggplot2)
library(gridExtra)
grid.arrange()




#plot experimental todos
td <- ggplot(db_nonc, aes(x=BEs, y=alt)) +
  geom_boxplot(color=col_be)+
  ggtitle("wide+restricted+NC")
td

td <- ggplot(db_be, aes(x=BEs, y=alt)) +
  geom_boxplot()+
  ggtitle("test boxplot 1")+
  scale_y_continuous(limits = c(0, 2000))+
  scale_x_discrete(limits=c("0","2","3","10","15","19","20","22","21","11","7","6","18","13","4","8","14","16","17","1","12","5","9"))+
  geom_hline(yintercept = 570.43)
td

#plot experimental todos e db reduzido
db_nowdnc$range_f <- factor(db_nowdnc$range, levels = c("w", "r"))
#geom_boxplot(color=c(col_wd, col_rg))+
geom_vline(xintercept = 6.5, linetype=2)+
td2 <- ggplot(db_be, aes(x=BEs, y=alt)) +
       labs(x= "Biotic Elements", y= "Elevation (m)")+
       geom_boxplot()+
       scale_y_continuous(limits = c(0, 2000))+
       scale_x_discrete(limits=c("0","2","3","10","15","19","20","22","21","11","7","6","18","13","4","8","14","16","17","1","12","5","9"))+
       geom_hline(yintercept = 570)+
       theme_classic()

td2 + geom_label(x=3, y=2000, label="Widespread") + geom_label(x=14, y=2000, label="Restricted-Range")

td2 + annotate(geom="text", x=3, y=1800, label="Widespread") + annotate(geom="text", x=14, y=1800, label="Restricted-Range")

# geom_boxplot(color=c(col_wd, col_rg))+
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

###plot experimental wide###
wd <- ggplot(db_wd, aes(x=BEs, y=alt))+
  geom_boxplot(color=c("orange", "purple", "green", "blue", "grey", "red"))+
  labs(x= "Widespread Biotic Elements", y= "Elevation (m)" )+
  scale_x_discrete(limits=c("14","8", "20","7", "1", "6"))+
  geom_hline(yintercept = 600)
wd

###### plot restricted ###### funciona, está adequando-se ao necessario
res <- ggplot(db_res, aes(x=BEs, y=alt)) +
       labs(x= "Restricted-Range Biotic Elements", y= "Elevation (m)" )+
       geom_boxplot(color=col_rg)+
       scale_fill_manual(values=col_res)+
       scale_x_discrete(limits=c("16","10", "4","21", "18", "19", "11", "5", "3", "17", "15", "9",   "2", "12", "13"))+
       geom_hline(yintercept = 600)
res

grid.arrange(wd, res, nrow=1)
?facet_wrap

############ alt versus class ############
db_be3$Class <- as.factor(db_be3$Class)

cs_alt <- ggplot(db_be3, aes(x=Class, y=alt)) +
  ggtitle("Altitude per class")+
  labs(x= "Class", y= "Elevation (m)" )+
  geom_boxplot(color=c("green", "blue", "orange", "yellow", "red"))+
  scale_fill_manual()+
  scale_x_discrete(limits=c("amphibians","aves", "lizards","mammals", "snakes"))+
  geom_hline(yintercept = 600)
cs_alt



###### regressoes logisticas #######

logit1 <- glm(data=comp_nowd, formula=codes~aves+snakes+amphibians+mammals+lizards, family=binomial(link="logit"))
summary(lm2)

lmliz <- glm(data=comp_nowd, formula=codes~lizards, family=binomial(link="logit"))
summary(lmliz)

lmbd <- glm(data=comp_nowd, formula=codes~aves, family=binomial(link="logit"))
summary(lmbd)

lmsnk <- glm(data=comp_nowd, formula=codes~snakes, family=binomial(link="logit"))
summary(lmsnk)

lmmms <- glm(data=comp_nowd, formula=codes~mammals, family=binomial(link="logit"))
summary(lmmms)

lmamp <- glm(data=comp_nowd, formula=codes~amphibians, family=binomial(link="logit"))
summary(lmamp)
######################################################################################