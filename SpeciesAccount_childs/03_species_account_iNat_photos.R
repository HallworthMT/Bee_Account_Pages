
# iNaturalist data observation to get the image for page # 
iNatPhotos <- fromJSON(paste0("https://api.inaturalist.org/v1/observations?taxon_name=",sciName,"&order=desc&order_by=created_at"))

photos <- iNatPhotos$results$taxon$default_photo
imgURL <- ifelse(any(!is.na(photos)),photos$medium_url[1],"C:/Users/mhall/State_Of_Biodiversity/SDM_NewEngland/No_picture_available.png")

imageCredit <- ifelse(any(!is.na(photos)),photos$attribution[1]," ")
imageCredit <- gsub(pattern = '^\\(c\\)$', replacement = "Copyright:", x = imageCredit)