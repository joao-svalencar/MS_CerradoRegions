# reading libraries -------------------------------------------------------

library(here)
library(rgdal)

# reading processed data: cerrado endemic terrestrial vetebrates ----------
# For Biotic Element Analysis ---------------------------------------------

pa_table <- read.csv(here("data", "sppsample.csv"))
centro <- read.csv(here("data", "coords.csv"), header=TRUE)
dd <- readOGR(dsn=here("data","shapes"), layer= ("grid_tet"))
s <- readOGR(dsn=here("data", "shapes"), layer="Cerrado") #study area shapefile

# For the linear model (elevation) ----------------------------------------
list <- read.csv(here("data", "list.csv"))
db_full <- read.csv(here("data", "BD_endemics.csv"), stringsAsFactors=FALSE)
be <- read.csv(here("outputs", "tables", "n2_cd015.csv")) #from D_clustering.R
comp <- read.csv(here("outputs", "tables", "summary.csv"))

# end ---------------------------------------------------------------------