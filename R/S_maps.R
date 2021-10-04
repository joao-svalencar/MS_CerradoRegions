###################################### CALL THE MAPAR FUNCTION ######################################
library(devtools)

# importing package from remote repository --------------------------------

devtools::install_github("joao-svalencar/mapar", ref="main") # calls package

# processing data: preparing objects to preview output --------------------

# Not to run #
# s # study-area shapefile
# dd # grid
# BEs # species list and respective BEs
# mpa # presence x absence matrix (with species names and gridcells id)

# processing data: removing noise component species -----------------------
mpa <-as.matrix(table(pa_table$species, pa_table$sample)) #only if repeating
mpa <- 1*(mpa>0) #only if repeating

mpa <- mpa[BEs[,1][BEs[,2]!=0],] #REMOVE NOISE COMPONENTS FROM MATRIX
BEs <- BEs[BEs[2]!=0,] #REMOVE NOISE COMPONENTS FROM SPP/BEs LIST

# processing data (optional): renaming areas as "BE i" --------------------

for(i in 1:length(unique(BEs[,2]))) # rename loop
{
  BEs[2][BEs[2]==i] <- paste('BE', i, sep=' ')
}

# creating summary figure output: output preview --------------------------

units <- mapar(grid = dd, lsp = BEs, mpa = mpa, plot = TRUE, prop = TRUE, shp = s, nsp = 1) #saving individual shapefiles
units <- mapar(grid = dd, lsp = BEs, mpa = mpa, plot = FALSE, prop = TRUE, shp = s, nsp = 1, grp = TRUE) #saving unique shapefile

# creating summary figure output: exporting -------------------------------

pdf(here("outputs", "figures", "n3_cd020.pdf")) # opens graphical device
par(mar=c(1,1,1,1), mfrow=c(2,1)) # exporting parameters

for(i in 1:length(units[[2]])) # exporting loop
{
  plot(s, main=paste("BE", i))
  plot(units[[2]][[i]], 
       col=rgb(1,0,0, alpha=units[[3]][[i]][match(units[[2]][[i]]@data[["id"]], 
       names(units[[3]][[i]]))]), add=TRUE)
}

dev.off() #turns off graphical device.

# end ---------------------------------------------------------------------