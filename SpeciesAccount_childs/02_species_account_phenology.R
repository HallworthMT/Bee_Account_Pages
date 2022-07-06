
week_obs <- dbGetQuery(GBIFDb,
                       paste0('SELECT *
                       FROM obsdayinfo
                       JOIN occurrence ON obsdayinfo."gbifID" = occurrence."gbifID"
                       WHERE occurrence.species = \'',spp,'\'
                       ;'))

if(nrow(week_obs)>0){
  week_obs$sex[week_obs$sex==""]<-"UNKNOWN"
  
  sexes <- unique(week_obs$sex)
  wk_vals <- table(week_obs$week,week_obs$sex)
  wks <- row.names(wk_vals)
  # table out week 53
  wk_vals <- wk_vals[!(rownames(wk_vals) %in% "53"),]
  
  freqData <- data.frame(month = format(as.Date(paste(2014,1:52,1,sep="-"),"%Y-%U-%u"),"%b"),
                         week = 1:52,
                         obs = 0,
                         female = 0,
                         male = 0)
  
  if(length(sexes)==1){
    freqData$obs[as.numeric(names(wk_vals))]<- wk_vals
    sex.column <- grep(pattern=tolower(sexes),names(freqData))
    freqData[as.numeric(names(wk_vals)),sex.column] <- wk_vals
  }
  if(is.null(nrow(wk_vals))){
    freqData$obs[as.numeric(wks)] <- sum(wk_vals) 
    freqData$male[as.numeric(wks)] <- ifelse("MALE" %in% names(wk_vals),wk_vals[which(names(wk_vals)=="MALE")],0)
    freqData$female[as.numeric(wks)] <- ifelse("FEMALE" %in% names(wk_vals),wk_vals[which(names(wk_vals)=="FEMALE")],0)
  }else{
    freqData$obs[as.numeric(rownames(wk_vals))] <- apply(wk_vals,1,sum,na.rm = TRUE)
    freqData$female[as.numeric(rownames(wk_vals))]<- wk_vals[,"FEMALE"]
    freqData$male[as.numeric(rownames(wk_vals))]<- wk_vals[,"MALE"]
  }
  
  freqData$freqSpp = freqData$obs/sum(freqData$obs)
  freqData$freqFemale = freqData$female/sum(freqData$obs)
  freqData$freqMale = freqData$male/sum(freqData$obs)
}