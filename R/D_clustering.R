# reading libraries -------------------------------------------------------

library(prabclus)

# processing data: creating species list ----------------------------------

spp <- as.data.frame(rownames(mpa)) #indexes species list
names(spp) <- 'binomial'
head(spp)

# checking proportion -----------------------------------------------------

k <- length(spp[,1])/40
k

# Call cdn ----------------------------------------------------------------

cdn.table <- cdn(x, spp_list = spp)
capture.output(cdn.table, file = "cdn.csv")

# nnout 4 cd 0.2
8.5-7.47 #1.03

# nnout 2 cd 0.1
8.5-11.10 #-2.6

# nnout 2 cd 0.15
8.5-5.45 #3.05

# nnout 3 cd 0.15
8.5-12.31 #3.81

# nnout 3 cd 0.20
8.5-4.38 #4.12

# hierarchical method clustering ------------------------------------------

hclust <- hprabclust(x, cutdist=0.15, cutout=1, method="average", nnout=2, mdsplot=TRUE, mdsmethod="classical")

# processing data: creating table output ----------------------------------

BEs <- cbind(spp, hclust$rclustering) #combina lista de spp com BEs

names(BEs) <- c("species", "BEs")
table(BEs[2]) #to check Noise component and N of BEs

write.csv(BEs, here("outputs", "tables", "n2_cd015.csv"), row.names = FALSE)

# end ---------------------------------------------------------------------