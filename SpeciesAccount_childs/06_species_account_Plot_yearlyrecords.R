
if(numRecords >0){
  
sppObsYr <- table(samp_spp$year[samp_spp$year>1899])

current_year <- format(Sys.Date(),"%Y")
par(bty = "l", mar=c(5.1,4,1,0))
plot(NA, xlim = c(1900,as.numeric(current_year)),ylim = c(0,max(sppObsYr)),
     las = 1,
     xaxt = "n",
     ylab = "",
     xlab = "",
     yaxt = "n",
     main = "")
axis(1, at = seq(1900,current_year,10),mgp=c(0,0.2,0),tck=-0.01, cex.axis = 0.8)
axis(2, at = round(seq(0, max(sppObsYr),,10)), labels = prettyNum(round(seq(0,max(sppObsYr),,10)), big.mark = ","),las = 1, cex.axis = 0.8,
     mgp = c(0,0.21,0), tck=-0.01)
segments(x0=as.numeric(names(sppObsYr)),
         x1=as.numeric(names(sppObsYr)),
         y0 = 0,
         y1 = sppObsYr,
         lwd = 3,
         col = "gray50")
mtext(side = 2, line = 2, text = "Observations")
mtext(side = 1, line = 1.5, text = "Year")

}