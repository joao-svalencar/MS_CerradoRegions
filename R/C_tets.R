# reading libraries -------------------------------------------------------

library(here)
library(sf)
library(rgdal)

# reading processed data: cerrado endemic terrestrial vetebrates ----------


# For Biotic Element Analysis ---------------------------------------------

pa_table <- read.csv(here("data", "sppsample.csv"))
centro <- read.csv(here("data", "coords.csv"), header=TRUE)
dd <- readOGR(dsn=here("data","shapes"), layer= ("grid_tet"))
s <- readOGR(dsn=here("data", "shapes"), layer="Cerrado") #study area shapefile

# For the linear model (elevation) ----------------------------------------
list <- read.csv(here("data", "list.csv"))
db <- read.csv(here("data", "baseunique_alt.csv"), stringsAsFactors=FALSE, fileEncoding="latin1")
db_full <- read.csv(here("data", "BD_endemics.csv"), stringsAsFactors=FALSE, fileEncoding="latin1")
be <- read.csv(here("outputs", "tables", "spp_hier_2_015.csv"))
comp <- read.csv(here("outputs", "tables", "comp.csv"))

# end ---------------------------------------------------------------------