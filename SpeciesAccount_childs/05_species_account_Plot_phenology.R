
if(sum(freqData$obs) >0){
  
  mons <- unique(freqData$month)
  
  yLabels <- c(max(freqData$freqSpp)+max(freqData$freqSpp)*0.2,
               max(freqData$freqSpp)+(max(freqData$freqSpp)*0.02))
  
  ylims <- c(-(max(freqData$freqSpp)+max(freqData$freqSpp)*0.2),
             max(freqData$freqSpp)+max(freqData$freqSpp)*0.2)
  
  layout(matrix(c(1,1,1,1,
                  2,2,2,2), nrow = 2, ncol = 4,byrow = TRUE))
  par(bty = "n")
  par(mar = c(0,0,0,0))
  plot(NA,ylim = ylims, 
       xlim = c(1,53),
       yaxt = "n", xaxt = "n",
       xlab = "",ylab = "")
  
  for(i in 1:52){
    polygon(x=c(i-1,i,i,i-1),
            y=c(freqData$freqSpp[i],freqData$freqSpp[i],
                -freqData$freqSpp[i],-freqData$freqSpp[i]), 
            border = "gray88",
            col = "gray88")
  }
  text(1,ylims[2]/2,"\\VE",vfont=c("sans serif symbol","plain"),cex=4)
  
  for(i in 1:52){
    polygon(x=c(i-1,i,i,i-1),
            y=c(freqData$freqFemale[i],freqData$freqFemale[i],
                -freqData$freqFemale[i],-freqData$freqFemale[i]), 
            border = "gray50",
            col = "#339933BF")
  }
  
  for(m in 1:12){
    xvals <- c(min(which(freqData$month==mons[m])-1),
               max(which(freqData$month==mons[m])))
    
    segments(x0 = xvals[1], x1=xvals[1],
             y0 = yLabels[1], y1 = -yLabels[1],
             col = "gray88")
    segments(x0 = xvals[2], x1=xvals[2],
             y0 = yLabels[2], y1 = -yLabels[2],
             col = "gray88")
    polygon(x = c(xvals[1],xvals[2],xvals[2],xvals[1]),
            y = c(yLabels[1],yLabels[1],yLabels[2],yLabels[2]),
            col = 'gray88',
            border = 'black')
    
    text(x = mean(xvals), 
         y = mean(yLabels), 
         mons[m], 
         cex = 1.2,
         col="black")
  }
  par(mar = c(0,0,0,0))
  par(bg = "#f7f7f7")
  plot(NA,ylim = ylims, 
       xlim = c(1,53),
       yaxt = "n", xaxt = "n",
       xlab = "",ylab = "")
  text(1,ylims[2]/2,"\\MA",vfont=c("sans serif symbol","plain"),cex=4)
  
  for(m in 1:12){
    xvals <- c(min(which(freqData$month==mons[m])-1),
               max(which(freqData$month==mons[m])))
    
    segments(x0 = xvals[1], x1=xvals[1],
             y0 = yLabels[1], y1 = -yLabels[1],
             col = "gray88")
    segments(x0 = xvals[2], x1=xvals[2],
             y0 = yLabels[2], y1 = -yLabels[2],
             col = "gray88")
  }
  for(i in 1:52){
    polygon(x=c(i-1,i,i,i-1),
            y=c(freqData$freqSpp[i],freqData$freqSpp[i],
                -freqData$freqSpp[i],-freqData$freqSpp[i]), 
            border = "gray88",
            col = "gray88")
  }
  for(i in 1:52){
    polygon(x=c(i-1,i,i,i-1),
            y=c(freqData$freqMale[i],freqData$freqMale[i],
                -freqData$freqMale[i],-freqData$freqMale[i]), 
            border = "gray50",
            col = "#339933BF")
  }
  
}else{cat("<p> not enough data for phenology</p>")}