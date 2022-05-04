#Plot2.R for complex migration model 2022

Plot2 = function(repEND){
  #set up to subset by replicate and by year born (-1 = migrants, 0 = init focal pop)
  rep = repEND[, (ncol(repEND))]
  yr = unique(repEND[, 1])
  mxLRS = ceiling(max(repEND[,3])) #rounds up the maximum LRS
  mxRRS = ceiling(max(repEND[,7])) #rounds up the maximum RRS
  mxindv= ceiling(max(repEND[,2])) #rounds up the maximum number of indv born
  mxSDL = ceiling(max(repEND[,4])) #rounds up the maximum SD in LRS
  mxSDR = ceiling(max(repEND[,8])) #rounds up the maximum SD in RRS
  
  
  if(plotit==1){
    
    #LRS over time
    plot(-100, -100 , xlab="generation born", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, (mxLRS+mxSDL))) 
    for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
      temp <- repEND[rep == i,] #create temp matrix for each unique replicate
      LRS <- temp[,3]
      lines(yr, LRS, lwd=2) #points(sub[,1], Ho, lwd=2)
      sd <- temp[,4]
      uppersd = LRS + sd
      lowersd = LRS - sd
      lines(yr, uppersd, lwd=1)
      lines(yr, lowersd, lwd=1)
      polygon(x = c(yr, rev(yr)), y = c(uppersd, rev(lowersd)), col = "lightblue", density = 40, angle = 90)
    }
    #lines(theEND[,1], Ho , xlab="time (years)", ylab="observed heterozygosity", cex = 2, lty = 1, col="black", lwd=5)
    dev.copy(png, "../Output/LRS_over_time.png")
    dev.off()
    #will want to add a buffer with SD sometime too
    
    #RRS over time
    plot(-100, -100 , xlab="generation born", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, (mxRRS+mxSDR))) 
    for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
      temp <- repEND[rep == i,] #create temp matrix for each unique replicate
      RRS <- temp[,7]
      lines(yr, RRS, lwd=2) #points(sub[,1], Ho, lwd=2)
      sd <- temp[,8]
      uppersd = RRS + sd
      lowersd = RRS - sd
      lines(yr, uppersd, lwd=1)
      lines(yr, lowersd, lwd=1)
      polygon(x = c(yr, rev(yr)), y = c(uppersd, rev(lowersd)), col = "lightpink", density = 40, angle = 45)
    }
    #lines(theEND[,1], Ho , xlab="time (years)", ylab="observed heterozygosity", cex = 2, lty = 1, col="black", lwd=5)
    dev.copy(png, "../Output/RRS_over_time.png")
    dev.off()
    #will want to add a buffer with SD sometime too
    
    
    #LRS for females vs males
    #observed vs expected hetero
    plot(-100, -100 , xlab="male LRS", ylab="female LRS", xlim=c(0, 1), ylim=c(0, 1)) 
    males     = repEND[,6]
    females   = repEND[,5]
    for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
      sub <- repEND[rep == i,] #unique replicate
      points(males, females, lwd=2)
    }
    abline(coef = c(0,1), col = "red")
    #points(He, Ho , xlab="expected heterozygosity", ylab="observed heterozygosity", cex = 1, lty = 1, col="black", lwd=5)
    dev.copy(png, "../Output/LRS_fem_vs_mal.png")
    dev.off()
    
    #LRS for males vs females over time
    par(mar = c(5,4,4,4)+0.3)
    plot(-100, -100 , xlab="generation born", ylab="female LRS", xlim=c(min(yr), max(yr)), ylim=c(0, mxLRS)) 
    for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
      sub <- repEND[rep == i,]  #unique replicate
      females = sub[,5]
      lines(yr, females, lwd=2, col = "pink")
    }
    par(new = TRUE)
    for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
      sub <- repEND[rep == i,] #unique replicate
      males = sub[,6]
      lines(yr, males, lwd=2, col = "blue")
    }
    axis(side = 4, at = NULL, labels = TRUE, col = "blue")
    mtext("male LRS", side=4, line =3, col = "blue")
    dev.copy(png, "../Output/LRS_male&female_overtime.png")
    dev.off()
    
    #number indv born in per generation
    plot(-100, -100 , xlab="generation born", ylab="number of babies", xlim=c(min(yr), max(yr)), ylim=c(0, mxindv)) 
    for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
      temp <- repEND[rep == i,] #create temp matrix for each unique replicate
      born <- temp[,2]
      lines(yr, born, lwd=2) #points(sub[,1], Ho, lwd=2)
    }
    #lines(theEND[,1], Ho , xlab="time (years)", ylab="observed heterozygosity", cex = 2, lty = 1, col="black", lwd=5)
    dev.copy(png, "../Output/nindv_born_per_generation.png")
    dev.off()
    
  }
}

#Current list of columns for repEND
  #as of 5/4/22
#gen born
#numb indv born that year
#mean LRS
#SD LRS
#LRS females
#LRS males
#mean relative fitness (relative to max)
#SD rel fit
#replicate number