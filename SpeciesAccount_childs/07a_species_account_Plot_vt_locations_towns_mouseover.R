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

spp_points_NE <- sf::st_read(GBIFDb,
                             query = paste0('SELECT * 
                                            FROM occurrence_ne
                                            WHERE \"species\" = \'',spp,'\';'))

if(nrow(spp_points)>0){
# Figure out which town an observation is within  
vttowns_in <- sf::st_within(spp_points, VTtowns)

# determine how many observations are in each town
SppObsTowns <- table(unlist(vttowns_in))

# set the observations to 0 in each town 
VTtowns$sppObs <- 0

# replace the sppObs for each town with the observed number 
VTtowns$sppObs[as.numeric(names(SppObsTowns))] <- SppObsTowns

VTtownObs <- VTtowns[unlist(unique(vttowns_in)),]
}else{
VTtownObs <- VTtowns
VTtownObs$sppObs <- 0 
}

pg_species <- tolower(spp)
pg_species <- gsub(x = pg_species, pattern = " ", replacement = "_")

# Current distributions are found here C:\Users\mhall\State_Of_Biodiversity\SDM_NewEngland\SDM_tiffs_dists
sppDistsFiles <- list.files("C:/Users/mhall/State_Of_Biodiversity/SDM_NewEngland/SDM_tiffs_dists",
                            pattern = "*.tif",
                            full.names = TRUE)

sppDistValue <- grep(sppDistsFiles, pattern = pg_species, value = TRUE)

leafletPlot <- leaflet(VTshape, 
        height = 700, 
        #width = 1000,
        #these options below make the map 'STATIC' by removing zoom,
        # disabling drag and setting max zoom so mouse wheel doesnt change map
        options = leafletOptions(zoomControl = FALSE,
                                 minZoom = 8, maxZoom = 8,
                                 dragging = FALSE)) %>%
  setView(lng = -72.67, lat = 44.07, zoom = 8) %>%
  # Base groups
 # addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>% 
  # add the contour lines #
  addPolygons(stroke=TRUE, 
              weight = 1, 
              color = "gray", 
              fillColor = "transparent") %>% 
  addPolygons(data = VTtowns$geometry, 
              weight = 1,
              stroke = TRUE,
              color = "gray",
              fillColor = "transparent") %>%

  addPolygons(data = VTtownObs$geometry,
              weight = 1, 
              color = "gray",
              fillColor = ifelse(nrow(spp_points)>0,"gray88","white"),
              popup = paste0(VTtownObs$TOWNNAME,"<br> <i>",spp,"</i> <br> <i>n</i> = ",VTtownObs$sppObs,"<br>")) %>%

  # add darner observations #
  addCircleMarkers(lng = samp_spp$decimalLongitude[!is.na(samp_spp$decimalLatitude)],
                   lat = samp_spp$decimalLatitude[!is.na(samp_spp$decimalLatitude)],
                   label = samp_spp$species[!is.na(samp_spp$decimalLatitude)],
                   color = "black",
                   fillColor = "black",
                   weight = 1, 
                   radius = 1) %>%
# add inset map
addMiniMap(spp_points_NE,
  tiles = providers$Esri.OceanBasemap,
  position = 'bottomright', 
  width = 200, height = 200,
  toggleDisplay = FALSE) 


leafletPlot
