#Plot2.R for complex migration model 2022

#this will be used for Easley plotting of reproductive success!

setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM/CSVs")
directory = getwd()
outdir = paste(directory, "/figs/", sep = "")

repA = read.table("repEND_7.5_a.csv", header=T, sep=",")
repB = read.table("repEND_7.5_b.csv", header=T, sep=",")
repC = read.table("repEND_7.5_c.csv", header=T, sep=",")
repD = read.table("repEND_7.5_d.csv", header=T, sep=",")
table = read.table("tableforcertainyears_reprosuccess.csv", header=T, sep=",")

#set any NAs to zeros
#repEND[is.na(repEND)] = 0
repA[is.na(repA)] = 0
repB[is.na(repB)] = 0
repC[is.na(repC)] = 0
repD[is.na(repD)] = 0

repEND <- rbind(repA, repB, repC, repD)
mxLRS = ceiling(max(repEND[,3])) #rounds up the maximum LRS
mxRRS = ceiling(max(repEND[,7])) #rounds up the maximum RRS
mxindv= ceiling(max(repEND[,2])) #rounds up the maximum number of indv born
mxSDL = ceiling(max(repEND[,4])) #rounds up the maximum SD in LRS
mxSDR = ceiling(max(repEND[,8])) #rounds up the maximum SD in RRS
yr = repEND[,1]

#set up to subset by replicate and by year born (-1 = migrants, 0 = init focal pop)
{
  Arep = repA[,9]                 #grabs replicate 
  Apar = repA[,10]                #grabs parameter set
  Ayr = repA[,1]          #grabs unique year values
  Acol = repA[,11]
  Amig = repA[,12]
  ALRS = repA[,3]
}
{
  Brep = repB[,9]                 #grabs replicate 
  Bpar = repB[,10]                #grabs parameter set
  Byr = unique(repB[,1])          #grabs unique year values
  Bcol = repB[,11]
  Bmig = repB[,12]
  BLRS = repB[,3]
}
{
  Crep = repC[,9]                 #grabs replicate 
  Cpar = repC[,10]                #grabs parameter set
  Cyr = unique(repC[,1])          #grabs unique year values
  Ccol = repC[,11]
  Cmig = repC[,12]
  CLRS = repC[,3]
}
{
  Drep = repD[,9]                 #grabs replicate 
  Dpar = repD[,10]                #grabs parameter set
  Dyr = unique(repD[,1])          #grabs unique year values
  Dcol = repD[,11]
  Dmig = repD[,12]
  DLRS = repD[,3]
}
  
  
########################################################### 
#LRS over time
plot(-100, -100 , xlab="Generation born", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, (mxLRS+mxSDL)))  
for(i in 1:length(unique(Arep))){
  t <- repA[Arep == i,,drop=FALSE]
  LRS <- t[,3]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, LRS, lwd=2, col = color)
  
}
for(i in 1:length(unique(Brep))){
  t <- repB[Brep == i,,drop=FALSE]
  LRS <- t[,3]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, LRS, lwd=2, col = color)
  
}
for(i in 1:length(unique(Crep))){
  t <- repC[Crep == i,,drop=FALSE]
  LRS <- t[,3]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, LRS, lwd=2, col = color)
  
}
for(i in 1:length(unique(Drep))){
  t <- repD[Drep == i,,drop=FALSE]
  LRS <- t[,3]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, LRS, lwd=2, col = color)
  
}
legend("topleft", legend=c("m = 0", "m = 0.5", "m = 1", "m = 5"),
       col=c("blue", "red", "purple", "springgreen"), pch = 19, cex=1)
dev.copy(png, "../Output/LRS_over_time.png")
dev.off()
  

########################################################### 
#RRS over time
plot(-100, -100 , xlab="Generation born", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, (mxRRS))) 
for(i in 1:length(unique(Arep))){
  t <- repA[Arep == i,,drop=FALSE]
  RRS <- t[,7]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, RRS, lwd=2, col = color)
  
}
for(i in 1:length(unique(Brep))){
  t <- repB[Brep == i,,drop=FALSE]
  RRS <- t[,7]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, RRS, lwd=2, col = color)
  
}
for(i in 1:length(unique(Crep))){
  t <- repC[Crep == i,,drop=FALSE]
  RRS <- t[,7]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, RRS, lwd=2, col = color)
  
}
for(i in 1:length(unique(Drep))){
  t <- repD[Drep == i,,drop=FALSE]
  RRS <- t[,7]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, RRS, lwd=2, col = color)
  
}
legend("topleft", legend=c("m = 0", "m = 0.5", "m = 1", "m = 5"),
       col=c("blue", "red", "purple", "springgreen"), pch = 19, cex=1)
dev.copy(png, "../Output/RRS_over_time.png")
dev.off()
    

########################################################### 
#number indv born in per generation
plot(-100, -100 , xlab="Generation born", ylab="Number of babies", xlim=c(min(yr), max(yr)), ylim=c(0, 200)) #mxindv
for(i in 1:length(unique(Arep))){
  t <- repA[Arep == i,,drop=FALSE]
  born <- t[,2]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, born, lwd=2, col = color)
  
}
for(i in 1:length(unique(Brep))){
  t <- repB[Brep == i,,drop=FALSE]
  born <- t[,2]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, born, lwd=2, col = color)
  
}
for(i in 1:length(unique(Crep))){
  t <- repC[Crep == i,,drop=FALSE]
  born <- t[,2]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, born, lwd=2, col = color)
  
}
for(i in 1:length(unique(Drep))){
  t <- repD[Drep == i,,drop=FALSE]
  born <- t[,2]
  yr <- t[,1]
  color <- t[,11]
  lines(yr, born, lwd=2, col = color)
  
}
legend("topright", legend=c("m = 0", "m = 0.5", "m = 1", "m = 5"),
       col=c("blue", "red", "purple", "springgreen"), pch = 19, cex=1)
dev.copy(png, "../Output/nindv_born_per_generation.png")
dev.off()
#NOTE: will want to remove years -1 and 0 probs

##################################################################################
##################################################################################
##################################################################################

a <- repA[repA[,1] == 225, , drop=FALSE]
b <- repB[repB[,1] == 225, , drop=FALSE]
c <- repC[repC[,1] == 225, , drop=FALSE]
d <- repD[repD[,1] == 225, , drop=FALSE]



{
  table = matrix(nrow=4, ncol=18)            #each individual gets its own row.. matrix > dataframe -- "ncol = 7 + (nloci)*2
  colnames(table) <- c("param", "mig", "year", "nborn", "nbornLL", "nbornuL", "LRS", "LRSll", "LRSul", "LRSfemale", "LRSfemll", "LRSfemul", "LRSmale", "LRSmalll", "LRSmalul", "RRS", "RRSll", "RRSul")
  
  #param
  table[1,1] = "a"
  table[2,1] = "b"
  table[3,1] = "c"
  table[4,1] = "d"
  
  #mig
  table[1,2] = mean(a[,12])
  table[2,2] = mean(b[,12])
  table[3,2] = mean(c[,12])
  table[4,2] = mean(d[,12])
  
  #year
  table[1,3] = mean(a[,1])
  table[2,3] = mean(b[,1])
  table[3,3] = mean(c[,1])
  table[4,3] = mean(d[,1])
 
  
  #number born
  table[1,4] = mean(a[,2])
  table[2,4] = mean(b[,2])
  table[3,4] = mean(c[,2])
  table[4,4] = mean(d[,2])
  #nborn lower limit
  table[1,5] = quantile(a[,2], prob=0.025)
  table[2,5] = quantile(b[,2], prob=0.025)
  table[3,5] = quantile(c[,2], prob=0.025)
  table[4,5] = quantile(d[,2], prob=0.025)
  #nborn Upper Limit
  table[1,6] = quantile(a[,2], prob=0.975)
  table[2,6] = quantile(b[,2], prob=0.975)
  table[3,6] = quantile(c[,2], prob=0.975)
  table[4,6] = quantile(d[,2], prob=0.975)
 
  
  #LRS
  table[1,7] = mean(a[,3])
  table[2,7] = mean(b[,3])
  table[3,7] = mean(c[,3])
  table[4,7] = mean(d[,3])
  #H0 Lower limit
  table[1,8] = quantile(a[,3], prob=0.025)
  table[2,8] = quantile(b[,3], prob=0.025)
  table[3,8] = quantile(c[,3], prob=0.025)
  table[4,8] = quantile(d[,3], prob=0.025)
  #Ho Upper Limit
  table[1,9] = quantile(a[,3], prob=0.975)
  table[2,9] = quantile(b[,3], prob=0.975)
  table[3,9] = quantile(c[,3], prob=0.975)
  table[4,9] = quantile(d[,3], prob=0.975)
  
  #LRS females
  table[1,10] = mean(a[,5])
  table[2,10] = mean(b[,5])
  table[3,10] = mean(c[,5])
  table[4,10] = mean(d[,5])
  #LRS females Lower limit
  table[1,11] = quantile(a[,5], prob=0.025)
  table[2,11] = quantile(b[,5], prob=0.025)
  table[3,11] = quantile(c[,5], prob=0.025)
  table[4,11] = quantile(d[,5], prob=0.025)
  #LRS females Upper Limit
  table[1,12] = quantile(a[,5], prob=0.975)
  table[2,12] = quantile(b[,5], prob=0.975)
  table[3,12] = quantile(c[,5], prob=0.975)
  table[4,12] = quantile(d[,5], prob=0.975)
  
  #LRS males
  table[1,13] = mean(a[,6])
  table[2,13] = mean(b[,6])
  table[3,13] = mean(c[,6])
  table[4,13] = mean(d[,6])
  #LRS males Lower limit
  table[1,14] = quantile(a[,6], prob=0.025)
  table[2,14] = quantile(b[,6], prob=0.025)
  table[3,14] = quantile(c[,6], prob=0.025)
  table[4,14] = quantile(d[,6], prob=0.025)
  #LRS males Upper Limit
  table[1,15] = quantile(a[,6], prob=0.975)
  table[2,15] = quantile(b[,6], prob=0.975)
  table[3,15] = quantile(c[,6], prob=0.975)
  table[4,15] = quantile(d[,6], prob=0.975)
  
  
  #RRS
  table[1,16] = mean(a[,7])
  table[2,16] = mean(b[,7])
  table[3,16] = mean(c[,7])
  table[4,16] = mean(d[,7])
  #RRS Lower limit
  table[1,17] = quantile(a[,7], prob=0.025)
  table[2,17] = quantile(b[,7], prob=0.025)
  table[3,17] = quantile(c[,7], prob=0.025)
  table[4,17] = quantile(d[,7], prob=0.025)
  #RRS Upper Limit
  table[1,18] = quantile(a[,7], prob=0.975)
  table[2,18] = quantile(b[,7], prob=0.975)
  table[3,18] = quantile(c[,7], prob=0.975)
  table[4,18] = quantile(d[,7], prob=0.975)
  
}

tneg1 <- table
t0   <- table
t100 <- table
t150 <- table
t175 <- table
t200 <- table
t225 <- table


table <- rbind(tneg1, t0, t100, t150, t175, t200, t225)
write.table(table, paste(directory, "/tableforcertainyears_reprosuccess.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

png("nborn_at_certain_times.png")
plot(-100, -100 , xlab="year", ylab="nborn", xlim=c(-10, 230), ylim=c(0, 55)) 
for(i in 1:nrow(table)){
  points(table[,3], table[,4], col=table[,19], lwd = 3)
  for(d in 1:nrow(table)){
    xa = table[,3]
    xb = table[,3]
    ya = table[,6]
    yb = table[,5]
    arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col=table[,19], lwd=3)
  }
}
dev.off()

png("LRS_at_certain_times.png")
plot(-100, -100 , xlab="year", ylab="LRS", xlim=c(-10, 230), ylim=c(0, 3)) 
for(i in 1:nrow(table)){
  points(table[,3], table[,7])
  for(d in 1:nrow(table)){
    xa = table[,3]
    xb = table[,3]
    ya = table[,9]
    yb = table[,8]
    arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col=table[,19], lwd=3)
  }
}
dev.off()


png("RRS_at_certain_times.png")
plot(-100, -100 , xlab="year", ylab="RRS", xlim=c(-10, 230), ylim=c(0, .35)) 
for(i in 1:nrow(table)){
  points(table[,3], table[,16])
  for(d in 1:nrow(table)){
    xa = table[,3]
    xb = table[,3]
    ya = table[,18]
    yb = table[,17]
    arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col=table[,19], lwd=3)
  }
}
dev.off()


png("femaleLRS_at_certain_times.png")
plot(-100, -100 , xlab="year", ylab="female LRS", xlim=c(-10, 230), ylim=c(0, 3.5)) 
for(i in 1:nrow(table)){
  points(table[,3], table[,10])
  for(d in 1:nrow(table)){
    xa = table[,3]
    xb = table[,3]
    ya = table[,12]
    yb = table[,11]
    arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col=table[,19], lwd=3)
  }
}
dev.off()

png("maleLRS_at_certain_times.png")
plot(-100, -100 , xlab="year", ylab="male LRS", xlim=c(-10, 230), ylim=c(0, 3.5)) 
for(i in 1:nrow(table)){
  points(table[,3], table[,13])
  for(d in 1:nrow(table)){
    xa = table[,3]
    xb = table[,3]
    ya = table[,15]
    yb = table[,14]
    arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col=table[,19], lwd=3)
  }
}
dev.off()

png("maleVfemaleLRS.png")
plot(-100, -100 , xlab="female LRS", ylab="male LRS", xlim=c(0, 3.5), ylim=c(0, 3.5)) 
for(i in 1:nrow(table)){
  points(table[,10], table[,13])
}
abline(coef = c(0,1), col = "red")
dev.off()


for(k in 1:length(unique(para))){
  temp <- table[table[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,21], d1[,12], col = "red", lwd = 6)
    lines(d1[,21], d1[,12], col = "red", lwd = 4)
    for(d in 1:nrow(d1)){
      xa = d1[d,21]
      xb = d1[d,21]
      ya = d1[d,14]
      yb = d1[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="red", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,12], col = "darkgoldenrod1", lwd = 6)
    lines(d3[,21], d3[,12], col = "darkgoldenrod1", lwd = 4)
    for(c in 1:nrow(d3)){
      xa = d3[c,21]
      xb = d3[c,21]
      ya = d3[c,14]
      yb = d3[c,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="darkgoldenrod1", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,21], d4[,12], col = "gold", lwd = 6)
    lines(d4[,21], d4[,12], col = "gold", lwd = 4)
    for(e in 1:nrow(d4)){
      xa = d4[e,21]
      xb = d4[e,21]
      ya = d4[e,14]
      yb = d4[e,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="gold", lwd=3)
    }
  }else if(k == 8){
    d8 = temp[temp[,1] == 8, , drop=FALSE]
    points(d8[,21], d8[,12], col = "grey57", lwd = 6)
    lines(d8[,21], d8[,12], col = "grey57", lwd = 4)
    for(f in 1:nrow(d8)){
      xa = d8[f,21]
      xb = d8[f,21]
      ya = d8[f,14]
      yb = d8[f,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="grey57", lwd=3)
    }
  }
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("grey57", "red", "darkgoldenrod1", "gold"), pch = 19, cex=1)
#dev.copy(png, file = "/figs/fst_at_certain_times_c250.png")
dev.off()














for(p in unique(par)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        temp <- repEND[rep == i,] #create temp matrix for each unique replicate
        LRS <- temp[,3]
        yyr <- temp[,1]
        colo <- temp[,11]
        mig <- temp[,12]
        lines(yyr, LRS, lwd=2, col = colo)
        sd <- temp[,4]
        uppersd = LRS + sd
        lowersd = LRS - sd
        lines(yyr, uppersd, lwd=1)
        lines(yyr, lowersd, lwd=1)
        polygon(x = c(yyr, rev(yyr)), y = c(uppersd, rev(lowersd)), col = colo, density = 40, angle = 90)
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
    
    #if(par == "a"){
    #lines(yyr, LRS, lwd=2, col = col) #points(sub[,1], Ho, lwd=2)
    #}else if(par == "b"){
    #  lines(yyr, LRS, lwd=2, col = "blue") #points(sub[,1], Ho, lwd=2)
    #}else if(par == "c"){
    #  lines(yyr, LRS, lwd=2, col = "purple") #points(sub[,1], Ho, lwd=2)
    #}else if(par == "d"){
    #  lines(yyr, LRS, lwd=2, col = "red") #points(sub[,1], Ho, lwd=2)
    #}