# reading libraries -------------------------------------------------------

library(here)
library(rgdal)

# reading processed data: cerrado endemic terrestrial vetebrates ----------
# For Biotic Element Analysis ---------------------------------------------

pa_table <- read.csv(here("data", "sppsample.csv"))
centro <- read.csv(here("data", "coords.csv"), header=TRUE)
dd <- readOGR(dsn=here("data","shapes"), layer= ("grid_tet"))
s <- readOGR(dsn=here("data", "shapes"), layer="Cerrado") #study area shapefile

# Open D_BioticElements.R -------------------------------------------------

# For general results; species elevation classification and test  ---------

list <- read.csv(here("data", "list.csv"))
db_full <- read.csv(here("data", "elevation_full.csv"), stringsAsFactors=FALSE)
db_unique <- read.csv(here("data", "elevation_unique.csv"), stringsAsFactors=FALSE)
be <- read.csv(here("outputs", "tables", "BEs.csv")) #from D_clustering.R
comp <- read.csv(here("outputs", "tables", "summary.csv"))

# Open D_tets.R
# end ---------------------------------------------------------------------