# Loading 'mapar' ---------------------------------------------------------

library(devtools)

# importing package from remote repository --------------------------------

devtools::install_github("joao-svalencar/mapar", ref="main", force=TRUE)
library(mapar)
# ?cdn

# processing data: creating species list ----------------------------------

spp <- as.data.frame(rownames(mpa)) # species list
names(spp) <- 'binomial'
head(spp)

# checking proportion -----------------------------------------------------

k <- length(spp[,1])/40
k

# cnd - cutdist/nnout table for hierarchical clustering method ------------

cdn.table <- cdn(x, spp_list = spp)
capture.output(cdn.table, file = "cdn.csv")

# cdn combinations, ordered from smallest to largest differences ----------
# Differences = k-(N Noise species/N BEs)

# nnout 4 cd 0.2
8.5-(142/19) #1.03

# nnout 2 cd 0.1
8.5-(233/21) #-2.6

# nnout 2 cd 0.15
8.5-(158/29) #3.05

# nnout 3 cd 0.15
8.5-(197/16) #-3.81

# nnout 3 cd 0.20
8.5-(114/26) #4.12

# Combination nnout 2, cutdist 0.15 choosen: 
# Smallest difference with the highest number of BEs; preserves spatial contiguity (see later)

# hierarchical method clustering ------------------------------------------
#OBS: To preview other regionalization outputs, parametres (nnout/cutdist) must be changed here:
hclust <- hprabclust(x, cutdist=0.15, cutout=1, method="average", nnout=2, mdsplot=TRUE, mdsmethod="classical")

# processing data: creating table output ----------------------------------

BEs <- cbind(spp, hclust$rclustering) #combina lista de spp com BEs

names(BEs) <- c("species", "BEs")
table(BEs[2]) #to check Noise component and N of BEs

# write.csv(BEs, here("outputs", "tables", "BEs.csv"), row.names = FALSE)

# Open S_maps.R -----------------------------------------------------------

# end ---------------------------------------------------------------------