### TUTORIAL PARA ANALISE DE ELEMENTOS BIOTICOS EM R ###

## Passo 0. Instalar os pacotes que serao utilizados##

install.packages("maptools")
install.packages("MASS")
install.packages("Matrix")
install.packages("mclust")
install.packages("prabclus")#
install.packages("rgdal")
install.packages("sp")
install.packages("spData")#
install.packages("sf")
install.packages("spdep")#
install.packages("devtools")#
install.packages("gtools")#
install.packages("tidyverse")#


## Chamar os pacotes para funcionamento ##

library(prabclus)
library(sf)
library(maptools)
library(Matrix)
library(rgdal)
library(sp)
library(spData)
library(spdep)
library(gtools)
library(tidyverse)


## Passo 1. Selecione o working directory no seu computador ##
# O comando setwd() pode ser utilizado, mas voce precisa conhecer o caminho exato ate a pasta
# Se esta usando RStudio, na caixa inferior direita voce pode selecionar a pasta, clicar em "more" e selecionar "Set as Working Directory"
# Eh recomendado que o caminho de acesso a pasta seja curto, por exemplo, use uma pasta da area de trabalho.

setwd()
getwd()

## Passo 2. Devemos criar a matrix de presenca e ausencia:
# Para isso usaremos o intersect que fizemos entre a base de dados e o grid_final no QGIS (species_sample.csv)

# Atribuir a tabela "species_sample.csv" a Tabela_pXa ###

Tabela_pXa <- read.csv("sppsample.csv")
centro <- read.csv("coords.csv", header = FALSE)
### Abra a tabela e confira se as colunas estao devidamente separadas. 

head(Tabela_pXa)
head(centro)
length(unique(Tabela_pXa$sample))
length(unique(Tabela_pXa$species))
length(centro$V2)

centro <- centro[,-1]
sort(unique(Tabela_pXa$sample))
as.character(sort(centro$V1)) == colnames(T) 

### Caso as colunas nao estejam devidamente separadas selecione o separador em "sep="

### O comando attach transforma sua lista de especies em um vetor contendo quantos pontos por quadricula cada especie tem
?attach
attach(Tabela_pXa)

### A funcao table faz uma contagem resumindo as obs por celula do grid

T <- table(Tabela_pXa$species, Tabela_pXa$sample)

### Confira se T e uma tabela bonitinha de quadriculas por especie

head(T)

### Agora transforme a tabela T em uma matriz com o comando as.matrix e atribua a U

U <-as.matrix(T)

### Confira o U pra ver se nao zoou a sua tabela
U

### Agora vamos transformar os dados de "quantidade" de pontos por quadricula em uma matriz de zero e um

U <- 1*(U>0)
head(U)

### Confira se agora os valores absolutos foram realmente transformados e zero e um e se a tabela continua bonitinha

U

### O comando head() serve pra ler as primeira linhas de qualquer coisa que vc quiser

U[1:5,1:10]

### O comando write.csv vai transformar sua matriz U em um arquivo csv e salva-lo do diretorio de trabalho

### O primeiro item e o que voce quer salvaar, o segundo e como voce quer nomear o arquivo

write.csv(U, "mpa.csv")


### Abra o arquivo em excel copie a coluna com a lista de spp e cole em um novo arquivo. Salve esse novo arquivo como species_list.csv.

### Delete a coluna com o nome das spp e a coluna com numero dos ID; Salve as alteracoes e nomeie como U.csv

### Agora temos salvos os arquivos: a) O grid final; b) O arquivo com a coordenada dos centroides; c) O arquivo da intersecao entre spp e id da quadricula; d) A matriz de presenca e ausencia; e e) A lista de spp

### Na analise o R precisa reconhecer qual quadricula eh vizinha de qual

### Para a producao da lista de neighbors a partir do shp do grid:

dd <- readOGR(dsn=getwd(), layer="grid_tet")
# Escolha o shp do grid final (No exemplo grid_cerrado_ponto.shp)

# Plota dd e veja seu shape bonitinho ao lado direito >>> :D

plot(dd)

# O comando poly2nd(dd) constroi uma lista de neighbours a apartir de um shape file de poligonos
?poly2nb
nblist <- poly2nb(dd)

nblist

##### Para a producao do geodist a partir das coordenadas do centroide de cada quadricula

##### Chame o arquivo das coordenadas dos centroides e atribua a "centro". Se o arquivo estiver em .txt o comando seria read.table

centro <- read.csv("coords.csv", h=F)
dim(centro)

##### Roda o arquivo pra ver se ta bonitinho

centro
class(centro)

### O comando coord2dist calcula distancias geogr?ficas a partir das coordenadas dos centroides

### Em coordmatriz=centro [1:x], x eh o numero de quadriculas; dim(centro)[1], ? uma forma de n?o precisar ficar alterando esse valor na m?o
?coord2dist
ff<- coord2dist(file="centro", coordmatrix=centro[1:dim(centro)[1],], file.format="decimal2",output.dist=TRUE)

### O comando class identifica o tipo do seu arquivo R; Verifique se o arquivo e "dist"

class(ff)

### Para a leitura da matriz de prensenca e ausencia no prabinit

### Chame a matriz de presenca e ausencia e atribua a sma

sma <- read.csv("U.csv", h=F)

# Roda pra Conferir numero de spp e de celulas no grid

dim(sma)
dim(centro)

#### "Ai eh onde geralmente da erro" - Bolochio, 2019
#### Prabinit (usando geco) Josue usou gtf = 0.2 no artigo JB.
#### gtf Soh tem dois valores possiveis: 0.1 pra escalas menores e mais precisas, 0.2 para escalas mais amplas e menos precisas 

#### Ate agora a gente soh estava atribuindo nomes as paradas todas, aqui vamos por a porra toda pra rodar

#### Estamos usando o comando prabnit (do prabclus) pra juntar tudo. Estamos mandando ler a matriz sma sendo que as linhas sao spp; que a lista de vizinhanca eh a que a gente fez com nblist; que eh pra usar o coeficiente de distancia ff sendo que o parametro de distancia desse coeficiente eh pra escalas pequenas "gtf"
?prabinit
x <- prabinit(prabmatrix=sma, rows.are.species=TRUE, neighborhood=nblist,
              geodist=ff, distance="geco", gtf=0.2)
class(x)

y <- prabinit(prabmatrix=sma, rows.are.species=TRUE, neighborhood=nblist, 
              distance="kulczynski", gtf=0.2)

### Aqui eh onde a magica acontece!
###"Prabtest, se passar daqui com um resultado significativo pode comecar a escrever o paper..." - Bolochio, 2019

### !Descobrir o que o pdfnb!
?prabtest
test <- prabtest(x, times=1000, pdfnb = TRUE) #iniciado 22h01m 03/03/2021;
summary(test)
capture.output(summary(test), file = "prabtest_03-03-2021_2.txt")

##############################################################################################################################
##############################################################################################################################

############################################################
##### CERRADO #####
############################################################

#* Parametric bootstrap test for presence-absence data *
#Test statistics:  distratio , Tuning constant= 0.25 
#Distance:  geco 
#Simulation runs:  1000 
#Disjunction parameter for presence-absence pattern:  0.279638 
#Neighbor-based correction of region probabilities was used.
#Statistics value for original data:  0.3324413 
#Mean for null data:  0.3606486 , range:  0.3311097 0.3912978 
#p=  0.001998002 

#* Parametric bootstrap test for presence-absence data * 16/07/2020
#Test statistics:  distratio , Tuning constant= 0.25 
#Distance:  geco 
#Simulation runs:  1000 
#Disjunction parameter for presence-absence pattern:  0.2801043 
#Neighbor-based correction of region probabilities was used.
#Statistics value for original data:  0.320886 
#Mean for null data:  0.3572705 , range:  0.329104 0.3904964 
#p=  0.000999001 

#tetrapodas endemicos 22 de fevereiro de 2021#
#* Parametric bootstrap test for presence-absence data *
#Test statistics:  distratio , Tuning constant= 0.25 
#Distance:  geco 
#Simulation runs:  1000 
#Disjunction parameter for presence-absence pattern:  0.2614331 
#Neighbor-based correction of region probabilities was used.
#Statistics value for original data:  0.3214215 
#Mean for null data:  0.3552483 , range:  0.3298358 0.3865823 
#p=  0.000999001 

#tetrapodas endemicos 24 de fevereiro de 2021#
#* Parametric bootstrap test for presence-absence data *
#Test statistics:  distratio , Tuning constant= 0.25 
#Distance:  geco 
#Simulation runs:  1000 
#Disjunction parameter for presence-absence pattern:  0.2618194 
#Neighbor-based correction of region probabilities was used.
#Statistics value for original data:  0.3298416 
#Mean for null data:  0.3608719 , range:  0.3304291 0.3944636 
#p=  0.000999001 

#tetrapodas endemicos 03 de março de 2021 Final#
#* Parametric bootstrap test for presence-absence data *
#Test statistics:  distratio , Tuning constant= 0.25 
#Distance:  geco 
#Simulation runs:  1000 
#Disjunction parameter for presence-absence pattern:  0.2611203 
#Neighbor-based correction of region probabilities was used.
#Statistics value for original data:  0.3175467 
#Mean for null data:  0.3532767 , range:  0.3265534 0.3873543 
#p=  0.000999001
##############################################################################################################################
##############################################################################################################################

###### Busca a lista de especies dos elementos bioticos usando o MDS, como nos papers originais...
?prabclust #mixture method
?cmdscale

c<-(prabclust(x, nclus=0:10, mdsmethod = "classical", mdsdim = 4, nnk =ceiling(x$n.species/40), modelid = "all", permutations=0)) #calcula com classical
k<-(prabclust(x, nclus=0:10, mdsmethod = "kruskal", mdsdim = 4, nnk =ceiling(x$n.species/40), modelid = "all", permutations=0)) #calcula com kruskal

spp <- read.csv("spplist.csv", head = FALSE) #chama lista de spp
mix<- cbind(spp, c$clustering) #combina lista de spp com BEs
table(mix[2])

### NMDS mas com os cutdist. Quanto menor o cutdist, mais os pontos devem ser sobrepostos para formar BE (mais parecidas tem que ser as distribuicoes)

?hprabclust #hierarchical method
#mdsmethod kruskal dá um erro, mas é o método que reconhece clusters a partir de NMDS
#mdsmethod classical dá certo, mas nenhum outro trabalho usou PCoordA

k <- length(spp[,1])/40
cdn.table <- cdn(x, spp_list = spp)
capture.output(cdn.table, file = "cdn.csv")

#cdist 015, nnout 2

hier <- hprabclust(x, cutdist = 0.15, cutout=1, method="average", nnout=2, mdsplot=TRUE, mdsmethod="classical")
hier.l <- cbind(spp, hier$rclustering)
table(hier.l[2])
colnames(hier.l) <- c("Species", "BEs")
hier.l <- hier.l[order(hier.l$BEs),]

hier.l
########## Para exportar lista de spp por BEs para csv: ##########
write_csv(hier.l, "spp_hier_3_025.csv")

###################################### CHAMAR A FUNCAO MAPAR ######################################
install.packages("devtools") # if you have not installed "devtools" package
library(devtools)
devtools::install_github("joao-svalencar/mapar") #Não funciona...

devtools::source_url("https://github.com/joao-svalencar/mapar") #Não funciona...


#################### CHAMAR ARQUIVOS PARA FUNCAO ####################
library(rgdal)
g <- dd #grid
l <- hier.l #"atribua o objeto a(i) que quer mapear"
m <- read.table("mpa.csv", sep=",", head=TRUE, row.names=1) #matriz presenca/ausencia
s <- readOGR(dsn=getwd(), layer="Cerrado_ecor_2017") #shape area
str(l)
########## RETIRANDO NOISE COMPONENTS PARA RODAR A FUNCAO ##########

m <- m[l[,1][l[,2]!=0],] #TIRA NOISE COMPONENTS DA MATRIZ
l <- l[l[2]!=0,] #TIRA NOISE COMPONENTS DA LISTA DE SPP/BEs

########## OPCIONAL: RENOMEANDO AREAS PARA "BE i" ##########

for(i in 1:length(unique(l[,2]))) #mudando o nome das areas na minha lista de spp para BEi
{
  l[2][l[2]==i] <- paste('BE', i, sep=' ')
}

##################################### OPCOES DE MAPAR ###########################################
map.mix <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop=TRUE, shp=s, nsp=1) #mapar
cd010 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop=TRUE, shp=s, nsp=1) #mapar
cd015 <- mapar(grid=g, lsp=l, mpa=m, plot=FALSE, prop=FALSE, shp=s, nsp=1, grp=TRUE) #mapar
inscd020 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop= TRUE, shp=s, nsp=1) #mapar
cd025 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop=TRUE, shp=s, nsp=1) #mapar
cd030 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop=TRUE, shp=s, nsp=1) #mapar
cd035 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop=TRUE, propcut=0, grp = FALSE, shp=s, nsp=1) #mapar
cdist0.40 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop= TRUE, grp=FALSE, shp=s, nsp=1) #mapar
cdist0.45 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, prop=TRUE, shp=s, nsp=1) #mapar
cdist0.50 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, shp=s) #mapar
cdist0.55 <- mapar(grid=g, lsp=l, mpa=m, plot=TRUE, shp=s, prop=TRUE) #mapar

###################################### PARA PLOTAR BEs NO R #########################################

################################ PARA PLOTAR BEs INDIVIDUALMENTE ###############################
plot(s, main=paste("BE", 15)) #Plota shape e titulo do grafico
plot(cdist0.35[[2]][[8]], add=TRUE, col="black") #Plota o BE 15 do objeto cdist0.35

#################################### LOOPING PRA PLOTAR NO R ###################################
par(mar=c(1,1,1,1), mfrow=c(6,6)) #AJUSTAR ANTES DE PLOTAR EM LOOPINGS
?par

##### Para rodar o looping e salvar os BEs em PDF #####
pdf("hier_3_025-cerrado.pdf") #Abre arquivo em PDF para salvar os BEs. N?o rodar se quiser ver no console!!
par(mar=c(1,1,1,1), mfrow=c(2,1)) #Ajustar para o PDF

##### Looping de plot propriamente dito ##### Trocar o nome do arquivo contendo as listas da mapar
for(i in 1:length(cd025[[2]])) 
{
plot(s, main=paste("BE", i))
plot(cd025[[2]][[i]], col=rgb(1,0,0, alpha=cd025[[3]][[i]][match(cd025[[2]][[i]]@data[["id"]], names(cd025[[3]][[i]]))]), add=TRUE)
}

dev.off() #Desliga o device gr?fico. Obrigat?rio caso tenha escolhido salvar em PDF

cd010[[2]][[21]]@data[["id"]]
names(cd010[[3]][[21]])
################################### PARA VER QUEM COMPOEM OS BEs #####################################
########## Para visualizar no console: ##########

#Lembrar de selecionar o objeto com o cutdist desejado

for(i in 1:length(cd020[[1]]))
{
  cat("\nSpecies composition of BE", i, ":\n")
  cat(rownames(cd020[[1]][[i]]), sep="\n") #para ver quem compoe BEs  
}

#Comando para extrair output para arquivo texto
sink(file="spp_hier_clas_2_025", append=TRUE, type="output") #abre a conexao com o arquivo no wd
hier.l[order(hier.l$BEs),]
sink() #fecha a conexao com o arquivo no wd

####################################### PARA O TESTE DA PREMISSA #####################################
# Eh um teste de qui-quadrado comparando SP com BE (Real versus esperado) pra ver a probabilidade dos BEs terem sido formados por vicari?ncia
# Em SP, colocar um numeral para cada genero, repetir o numero em cada especie do genero
# Em BE, colar a sequencia que aparece na lista alfabetica de especies formada em aX acima

##### Para preparar os dados para o qui-quadrado no R #####
end <- read.csv("List_Endemics.csv") #carregando lista com info das spp
head(end)
end <- end[c(1:3, 5)] #selecionando infos de interesse
head(end)
unique(end$Species) #345 spp
head(hier.l)

list <- merge(hier.l, end, by="Species") #combinando dataframes

head(list)

list_nc <- list[list$BEs!=0,] #removendo noise component da lista
unique(list_nc$BEs) #conferindo se saiu
head(list_nc)
library(tidyverse) #pacote para manipulação de dados
?separate

b <- separate(data=list_nc, col="Species", into=c("genus", "species"), sep=" ") #considerando que a lista tenha "Species" como nome da coluna
head(b)
length(b$species)
b

#Em teoria, da pra fazer o teste mantendo o nome dos generos, sem substituir para numeral (ver help da funcao). Faca o teste:
?comp.test
comp.test(list_nc$genus, list_nc$BEs)

#Para salvar o resultado. PS.: Os valores mudam um pouco entre as rodadas. Aconselho usar os valores do doc salvo:

sink(file="chisquare_test_genus.txt", append=TRUE, type="output") #abre a conexao com o arquivo no wd
comp.test(list_nc$genus, list_nc$BEs)
sink()

sink(file="chisquare_test_class.txt", append=TRUE, type="output") #abre a conexao com o arquivo no wd
comp.test(list_nc$BEs, list_nc$class)
sink()

head(list_nc)
#Maaasss, se ainda estiver cetic@ use a funcao abaixo para transformar "Genus" em numerais
##### CONVERTENDO GENEROS PARA NUMEROS #####
for(i in 1:length(unique(list_nc[,5]))) #mudando o nome de generos para números
{
  list_nc[5][list_nc[5]==unique(list_nc[,5])[i]] <- as.numeric(i)
}

##### CONVERTENDO CLASSES PARA NUMEROS #####

for(i in 1:length(unique(list_nc[,3]))) #mudando o nome das classes para números
{
  list_nc[2][list_nc[2]==unique(list_nc[,3])[i]] <- as.numeric(i)
}

#Verificar se transformou certinho"
head(list_nc)

# Quando da significativo significa que as spp de um mesmo genero estao todas num unico elemento biotico
# Portanto, para atender a premissa, o valor de p deve ser nao signifcativo (aka.: >0.05)

m.comp <- matrix(data=comp$Aves)
m.comp <- cbind(m.comp, comp$Amphibians, comp$Lizards, comp$Mammals, comp$Snakes)
row.names(m.comp) <- comp$Biotic.Element
colnames(m.comp) <- c("aves", "amphibians", "lizards", "mammals", "snakes")
m.comp

m.comp <- t(m.comp)
comp.test(m.comp)

# Tudo lindo e maravilhoso. #SQN pq ainda falta plotar tudo em um mapa butinim

cd <- c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
noise <- c(260, 235, 202, 155, 115, 89, 59, 42)
be <- c(17, 21, 26, 30, 32, 31, 32, 27)
df <- data.frame(cd, noise, be)

?plot
plot(df$noise~df$cd, pch=16, 
     ylab="Noise Component", xlab= "Cutdist values", 
     cex.axis=1.3, cex.lab=1.3)

plot(df$be~df$cd, pch=16, 
     ylab="Number of Biotic Elements", xlab= "Cutdist values", 
     cex.axis=1.3, cex.lab=1.3)


q()


p <- c(5, 51, 15, 10, 7) #Plateau?
d <- c(5, 8, 12, 2, 8) #Depression?
pd <- data.frame(p, d)
