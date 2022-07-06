
AssociatedTaxa <- dbGetQuery(GBIFDb,
                             paste0('SELECT * 
                             FROM "Bee_AssociatedTaxa"
                             WHERE species = \'',spp,'\' AND quality_grade = \'research\';'))


GBIFAssTax <- dbGetQuery(GBIFDb,
                         paste0('SELECT \"gbifID\",\"occurrenceID\",\"species\",\"vernacularName\",
                                       \"sex\", \"lifeStage\",\"degreeOfEstablishment\",
                                       \"associatedTaxa\",\"year\",\"month\", \"day\",
                                       \"eventDate\",\"recordedBy\",\"decimalLongitude\",
                                       \"decimalLatitude\"
                                FROM occurrence
                                WHERE species = \'',spp,'\'
                                      AND \"associatedTaxa\" !=\'\';'))

# Clean up the GBIF associated taxa # 
GBIFAssTax$associatedTaxa <- gsub(pattern = "host:", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "visiting flower of: ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "Visted flower of: ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "Visited flower of: ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "visited flower of", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "visited flower of ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "visting flower of: ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "visting flower: ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "Nectar Plant: ", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = "visited flower of", replacement = "", GBIFAssTax$associatedTaxa)
GBIFAssTax$associatedTaxa <- gsub(pattern = ": ", replacement = "", GBIFAssTax$associatedTaxa)


if(nrow(GBIFAssTax)>0){
  gbifAssTaxa <- data.frame("iNatID" = GBIFAssTax$gbifID,
                            "iNatUUID" = GBIFAssTax$occurrenceID,
                            "quality_grade" = "research",
                            "species" = GBIFAssTax$species,
                            "common_name" = GBIFAssTax$vernacularName,
                            "sex" = GBIFAssTax$sex,
                            "lifestate" = GBIFAssTax$lifeStage,
                            "endemic" = GBIFAssTax$degreeOfEstablishment,
                            "associatedTaxa" = GBIFAssTax$associatedTaxa,
                            "assRank" = NA,
                            "assTaxonID" = NA,
                            "assParentID" = NA,
                            "assPhoto" = NA,
                            "assEstablishment" = GBIFAssTax$degreeOfEstablishment,
                            "year" = GBIFAssTax$year,
                            "month"  = GBIFAssTax$month,
                            "day" = GBIFAssTax$day,
                            "eventDate" = GBIFAssTax$eventDate,
                            "recordedBy" = GBIFAssTax$recordedBy,
                            "decimalLongitude" = GBIFAssTax$decimalLongitude,
                            "decimalLatitude" = GBIFAssTax$decimalLatitude)
  # put the two data sets together # 
  
  AssociatedTaxa <- rbind(AssociatedTaxa, gbifAssTaxa)
}

if(nrow(AssociatedTaxa)!=0){
  
  # read in google sheet with corrections # 
  taxaSheet <- "https://docs.google.com/spreadsheets/d/1eaklGHxMAm3hNdMD2PqmoA40juiH1Z83tT5LwTErwrY/edit#gid=1289201043"
  
  # get the properties of the google sheet
  gs_props <- sheet_properties(taxaSheet)
  
  # read in the sheet of interest 3
  taxa_gs <- range_read(taxaSheet, sheet = which(gs_props$name == "Bee_AssTaxa"))
  
  AssociatedTaxa$taxaGenus <- taxa_gs$Genus[match(AssociatedTaxa$associatedTaxa,taxa_gs$associatedTaxa)]
  
  AssociatedTaxa$Flower <- taxa_gs$Flower[match(AssociatedTaxa$associatedTaxa,taxa_gs$associatedTaxa)]
  
  BeeAssTable <- table(AssociatedTaxa$Flower[AssociatedTaxa$associatedTaxa!="" | !is.na(AssociatedTaxa$Flower)])
  
  BeeAssTableGenus <- table(AssociatedTaxa$taxaGenus[AssociatedTaxa$associatedTaxa!="" |
                                                !is.na(AssociatedTaxa$taxaGenus)])
  
  BeeAssUrl <- AssociatedTaxa$assPhoto[match(names(BeeAssTable),AssociatedTaxa$associatedTaxa)]
  
  BeeAssUrl <- ifelse(is.na(BeeAssUrl),"C:/Users/mhall/State_Of_Biodiversity/SDM_NewEngland/No_picture_available.png",
                      BeeAssUrl)
  
  BeePropTable <- prop.table(BeeAssTable)
  
  if(!length(BeePropTable)==0){
  BeePropData <- data.frame(Vegetation = names(BeePropTable[order(BeePropTable,decreasing = TRUE)]),
                            Observations = as.numeric(BeeAssTable[order(BeePropTable,decreasing = TRUE)]),
                            Proportion = as.numeric(round(t(BeePropTable[order(BeePropTable,decreasing = TRUE)])*100,2)),
                            imgURL = paste0("<img src=",
                                            BeeAssUrl[order(BeePropTable,decreasing = TRUE)],
                                            ">"))
  }else{
  BeePropData <- data.frame(Vegetation = NA,
                              Observations = 0,
                              Proportion = 0,
                              imgURL = NA)
  }
  
 # kbl(mtcars[1:10, 1:6], caption = "Group Rows") %>%
 #   kable_paper("striped", full_width = F) %>%
 #   pack_rows("Group 1", 4, 7) %>%
 #   pack_rows("Group 2", 8, 10)
}