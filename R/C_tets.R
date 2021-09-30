# reading libraries -------------------------------------------------------

library(here)
library(sf)
library(rgdal)

# reading processed data: cerrado endemic terrestrial vetebrates ----------

pa_table <- read.csv(here("data", "sppsample.csv"))
centro <- read.csv(here("data", "coords.csv"), header=TRUE)
dd <- readOGR(dsn=here("data","shapes"), layer= ("grid_tet"))
s <- readOGR(dsn=here("data", "shapes"), layer="Cerrado") #study area shapefile

# end ---------------------------------------------------------------------