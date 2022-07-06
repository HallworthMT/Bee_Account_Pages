# Query the database to get habitat of observations # 

habitatOcc <- dbGetQuery(GBIFDb,
                         paste0('SELECT "landcover_discrete",
                                  COUNT(occurrence."gbifID") as lc_count
                          FROM occurrence LEFT OUTER JOIN obs_environmental ON (occurrence."species" = obs_environmental."species")
                          WHERE occurrence."species" = \'',params$species,'\'
                          AND occurrence."coordinateUncertaintyInMeters" <', params$locationUncertainty,
                                'GROUP BY "landcover_discrete"'))

# create percentages # 
habitatOcc$per_occs <- as.numeric(habitatOcc$lc_count)/sum(as.numeric(habitatOcc$lc_count))*100

# table with habitat values # 
lcClass <- data.frame(lc_code = c(20,30,40,50,60,70,80,90,111,114,115,116,121,124,125,126),
                      class = c("Shrubs", #20
                                "Herbaceous Vegetation", #30
                                "Cultivated Cropland", #40
                                "Urban/Builtup", #50
                                "Sparse Vegetation", #60
                                "Snow/Ice", #70
                                "Water", #80 
                                "herbaceous wetland",#90  
                                "closed evergreen needleleaf",#111  
                                "closed deciduous forest", #114  
                                "closed mixed forest", # 115  
                                "closed unknown forest",#  #116  
                                "Open evergreen needleleaf",  #121  
                                "Open deciduous broadleaf forest", #124  
                                "Open mixed forest", #125  
                                "Open unknown forest"), #126 
                      lc_cols = c("#FFBB22", #20
                                  "#FFFF4C", #30
                                  "#F096FF", #40
                                  "#FA0000", #50
                                  "#B4B4B4", #60
                                  "#F0F0F0", #70
                                  "#0032C8", #80
                                  "#0096A0", #90
                                  "#58481F", #111
                                  "#00CC00", #114
                                  "#4E751F", #115
                                  "#007800", #116
                                  "#666000", #121
                                  "#A0DC00", #124
                                  "#929900", #125
                                  "#648C00")) #126

# create new dataframe to fill with zeros but has all data
HabSpp <- lcClass

# set the values for all habitats to 0 
HabSpp$Obs <- 0

# fill data 
HabSpp$Obs[match(habitatOcc$landcover_discrete,HabSpp$lc_code)] <- habitatOcc$per_occs

# create a plot 
habYlab <- barplot(HabSpp$Obs, horiz = TRUE, plot = FALSE)
par(mar = c(3,12,1,1))
barplot(HabSpp$Obs, horiz = TRUE,
        xlim = c(0,100),
        xaxt = "n",
        col = HabSpp$lc_cols)
axis(2, at = habYlab, labels = HabSpp$class, las = 1, cex.axis = 0.75)
axis(1, line = -0.75, mgp = c(0,0.25,1), cex.axis = 0.9)
mtext(side = 1, line = 0.7, text = "% of observations")