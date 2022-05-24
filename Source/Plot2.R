#Plot2.R for complex migration model 2022

Plot2 = function(repEND){
  #set any NAs to zeros
  repEND[is.na(repEND)] = 0
  
  #set up to subset by replicate and by year born (-1 = migrants, 0 = init focal pop)
  rep = repEND[,9]                 #grabs replicate 
  par = repEND[,10]                #grabs parameter set
  yr = unique(repEND[,1])          #grabs unique year values
  mxLRS = ceiling(max(repEND[,3])) #rounds up the maximum LRS
  mxRRS = ceiling(max(repEND[,7])) #rounds up the maximum RRS
  mxindv= ceiling(max(repEND[,2])) #rounds up the maximum number of indv born
  mxSDL = ceiling(max(repEND[,4])) #rounds up the maximum SD in LRS
  mxSDR = ceiling(max(repEND[,8])) #rounds up the maximum SD in RRS
  
  
  if(plotit==1){
    
    ########################################################### 
    #LRS over time
    plot(-100, -100 , xlab="Generation born", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, (mxLRS+mxSDL))) 
    for(p in unique(par)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        temp <- repEND[rep == i,] #create temp matrix for each unique replicate
        LRS <- temp[,3]
        yyr <- temp[,1]
        lines(yyr, LRS, lwd=2) #points(sub[,1], Ho, lwd=2)
        sd <- temp[,4]
        uppersd = LRS + sd
        lowersd = LRS - sd
        lines(yyr, uppersd, lwd=1)
        lines(yyr, lowersd, lwd=1)
        polygon(x = c(yyr, rev(yyr)), y = c(uppersd, rev(lowersd)), col = "lightblue", density = 40, angle = 90)
      }
    }
    dev.copy(png, "../Output/LRS_over_time.png")
    dev.off()
    
    ########################################################### 
    #RRS over time
    plot(-100, -100 , xlab="Generation born", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, (mxRRS+mxSDR))) 
    for(p in unique(par)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        temp <- repEND[rep == i,] #create temp matrix for each unique replicate
        RRS <- temp[,7]
        yyr <- temp[,1]
        lines(yyr, RRS, lwd=2) #points(sub[,1], Ho, lwd=2)
        sd <- temp[,8]
        uppersd = RRS + sd
        lowersd = RRS - sd
        lines(yyr, uppersd, lwd=1)
        lines(yyr, lowersd, lwd=1)
        polygon(x = c(yyr, rev(yyr)), y = c(uppersd, rev(lowersd)), col = "lightpink", density = 40, angle = 45)
      }
    }
    dev.copy(png, "../Output/RRS_over_time.png")
    dev.off()
    
    ########################################################### 
    #LRS for females vs males
    #observed vs expected hetero
    plot(-100, -100 , xlab="Male LRS", ylab="Female LRS", xlim=c(0, mxLRS), ylim=c(0, mxLRS)) 
    for(p in unique(par)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- repEND[rep == i,] #unique replicate
        males <- sub[,6]
        females <- sub[,5]
        points(males, females, lwd=2)
      }
    }
    abline(coef = c(0,1), col = "red")
    #points(He, Ho , xlab="expected heterozygosity", ylab="observed heterozygosity", cex = 1, lty = 1, col="black", lwd=5)
    dev.copy(png, "../Output/LRS_fem_vs_mal.png")
    dev.off()
    
    ########################################################### 
    #LRS for males vs females over time
    par(mar = c(5,4,4,4)+0.3)
    plot(-100, -100 , xlab="Generation born", ylab="Female LRS", xlim=c(min(yr), max(yr)), ylim=c(0, mxLRS)) 
    for(p in unique(par)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- repEND[rep == i,]  #unique replicate
        females = sub[,5]
        yyr <- sub[,1]
        lines(yyr, females, lwd=2, col = "pink")
      }
      par(new = TRUE)
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- repEND[rep == i,] #unique replicate
        yyr <- temp[,1]
        males = sub[,6]
        lines(yyr, males, lwd=2, col = "blue")
      }
    }
    axis(side = 4, at = NULL, labels = TRUE, col = "blue")
    mtext("Male LRS", side=4, line =3, col = "blue")
    dev.copy(png, "../Output/LRS_male&female_overtime.png")
    dev.off()
    
    ########################################################### 
    #number indv born in per generation
    plot(-100, -100 , xlab="Generation born", ylab="Number of babies", xlim=c(min(yr), max(yr)), ylim=c(0, mxindv)) 
    for(p in unique(par)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        temp <- repEND[rep == i,] #create temp matrix for each unique replicate
        born <- temp[,2]
        yyr <- temp[,1]
        lines(yyr, born, lwd=2) #points(sub[,1], Ho, lwd=2)
      }
    }
    #lines(theEND[,1], Ho , xlab="time (years)", ylab="observed heterozygosity", cex = 2, lty = 1, col="black", lwd=5)
    dev.copy(png, "../Output/nindv_born_per_generation.png")
    dev.off()
    #NOTE: will want to remove years -1 and 0 probs
    
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