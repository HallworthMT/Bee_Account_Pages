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
library(fontawesome) 


htmltools::tagList(rmarkdown::html_dependency_font_awesome())

taxonList <- read.csv("C:/Users/mhall/State_Of_Biodiversity/Bee_Monograph_Hardy_et_al/Bees_species_keys_GBIF.csv")

spp <- params$species

taxonIDspp <- taxonList$speciesKey[which(taxonList$species==spp)]

# if(spp == "Andrena vernalis"){
#  spp <- "Andrena ziziae"
#}

sciName <- paste0(unlist(strsplit(spp, split = " ")),collapse = "%20")

GBIFDb <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = 'vt-gbif-data',
                         host = 'localhost',
                         port = 5432, # or any other port specified by your DBA
                         user = "postgres",
                         password = "password")

columnNames <- dbGetQuery(GBIFDb,
                          "SELECT column_name
                          FROM INFORMATION_SCHEMA.COLUMNS 
                          WHERE table_schema='public'
                          AND table_name='occurrence'
                          order by ordinal_position")

sppData <- data.frame(matrix(NA,
                             ncol=length(columnNames$column_name),
                             nrow=300, 
                             dimnames=list(NULL, columnNames$column_name)))

if(identical(taxonIDspp,integer(0))){
  
  initGBIF <- fromJSON(paste0("https://api.gbif.org/v1/occurrence/search?scientificName=",sciName,"&gadm_gid=USA.46_1&limit=300"))
}else{
  initGBIF <- fromJSON(paste0("https://api.gbif.org/v1/occurrence/search?taxon_key=",taxonIDspp,"&gadm_gid=USA.46_1&limit=300"))
}

numRecords <- initGBIF$count
numPages <- ceiling(initGBIF$count / initGBIF$limit)

if(numRecords>0){
  GBIFbees <- vector('list', numPages)
  
  for(page in 1:numPages){
    offset <- (page-1)*300
    
    if(identical(taxonIDspp,integer(0))){
      JSONresults <- fromJSON(paste0("https://api.gbif.org/v1/occurrence/search?scientificName=",sciName,"&gadm_gid=USA.46_1&limit=300&offset=",offset))$results
    }else{
      JSONresults <- fromJSON(paste0("https://api.gbif.org/v1/occurrence/search?taxon_key=",taxonIDspp,"&gadm_gid=USA.46_1&limit=300&offset=",offset))$results
    }
    
    these <- intersect(names(sppData),names(JSONresults))
    sppData[1:nrow(JSONresults),these] <- JSONresults[1:nrow(JSONresults),these]
    
    # pull out observations images if there are any # 
    jsonPhotos <- sapply(JSONresults$media,
                         FUN = function(x){
                           z <- x$identifier[1]
                           y <- ifelse(is.null(z),"NA",z)
                           # change default image size 
                           w <- gsub(y, pattern = "original", replacement = "small")
                           return(w)})
    
    GBIFbees[[page]] <- sppData
    GBIFbees[[page]]$photo <- NA
    GBIFbees[[page]]$photo[1:length(jsonPhotos)] <- jsonPhotos
    
    # return back to na
    sppData[]<-NA
  }
  
  samp_spp  <- do.call(rbind,GBIFbees)
  
  samp_spp$obs_url <- samp_spp$occurrenceID
  gbifThese <- grep(pattern = "inaturalist", x = samp_spp$obs_url, invert = TRUE)
  samp_spp$obs_url[gbifThese] <- paste0("https://www.gbif.org/occurrence/",samp_spp$gbifID[gbifThese])
  
  #### THIS GETS WEEK OF YEAR FOR GBIF SAMPLES # 
  samp_spp$wkyr <- as.numeric(format(
    as.POSIXct(samp_spp$eventDate, "%Y-%m-%dT%HH:%MM:%SS"),
    "%U"))
  
  gbifPheno <- tapply(samp_spp$gbifID,
                      list(samp_spp$wkyr),
                      FUN = function(x){
                        length(unique(x))
                      })
}