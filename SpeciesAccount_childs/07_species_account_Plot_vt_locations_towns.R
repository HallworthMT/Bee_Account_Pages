library(rgbif)
library(sf)
library(raster)
library(DBI)
library(RPostgres)
library(rpostgis)
library(dbplyr)
library(dplyr)
library(rgdal)
library(dismo)
library(ENMeval)
library(doSNOW)
library(spatstat)
library(rgbif)
library(mapview)
library(jsonlite)
library(leaflet)

GBIFDb <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = 'vt-gbif-data',
                         host = 'localhost',
                         port = 5432, # or any other port specified by your DBA
                         user = "postgres",
                         password = "password")

DISTDb <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = 'vt-sdm-current',
                         host = 'localhost',
                         port = 5432, # or any other port specified by your DBA
                         user = "postgres",
                         password = "password")

VTshape <- sf::st_read(GBIFDb,
                       "vt_boundary_river_lake")
VTtowns <- sf::st_read(GBIFDb,
                       "towns")

spp_points <- sf::st_read(GBIFDb,
                          query = paste0('SELECT *
                                   FROM occurrence
                                   WHERE \"species\" = \'',spp,'\';'))

vttowns_in <- sf::st_within(spp_points, VTtowns)

pg_species <- tolower(params$species)
pg_species <- gsub(x = pg_species, pattern = " ", replacement = "_")

# Current distributions are found here C:\Users\mhall\State_Of_Biodiversity\SDM_NewEngland\SDM_tiffs_dists
sppDistsFiles <- list.files("C:/Users/mhall/State_Of_Biodiversity/SDM_NewEngland/SDM_tiffs_dists",
                            pattern = "*.tif",
                            full.names = TRUE)

sppDistValue <- grep(sppDistsFiles, pattern = pg_species, value = TRUE)


par(mar = c(0.5,0.5,0.5,0.5))
plot(VTshape$geometry)
plot(VTtowns$geometry, add = TRUE, border = "gray70")
plot(VTtowns$geometry[unlist(unique(vttowns_in))], col = "gray88", add = TRUE, border = "gray70")
points(spp_points$decimalLongitude,
       spp_points$decimalLatitude,
       pch = ".")
plot(VTshape$geometry, add = TRUE)