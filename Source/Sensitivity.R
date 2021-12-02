#sensitivity analysis to check model parameters
#parameters of interest: K, pop growth rate, migration rate
#other potential parameters -- maxage, brood size, maturity, mortrate

#put all combos in one matrix
theEND[,18] = 1
theENDa[,18] = 2
theENDb[,18] = 3

k.Sens <- rbind(theEND, theENDa, theENDb)

k <- aov(k.Sens[,5]~as.factor(k.Sens[,18]))
summary(k)
TukeyHSD(k, conf.level=.95)
#all are signif, try another approach

boxplot(k.Sens[,5]~as.factor(k.Sens[,18]))

######################################################################
#theEND > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
#theEND1 > k=5000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
#theEND2 > k=10000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1), main="change in Ho with varying K") 

for(i in unique(theEND[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem <- theEND[theEND[,17] == i,]
  Ho <- tem[,5]
  lines(tem[,1], Ho, lwd=2, col="red") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theEND1[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  temp <- theEND1[theEND1[,17] == i,]
  Ho1 <- temp[,5]
  lines(temp[,1], Ho1, lwd=2, col="yellow") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theEND2[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem2 <- theEND2[theEND2[,17] == i,]
  Ho2 <- tem2[,5]
  lines(tem2[,1], Ho2, lwd=2, col="brown") #points(sub[,1], Ho, lwd=2)
}
legend("topright", legend=c("k = 1000", "k = 5000", "k = 10000"),
       col=c("red", "yellow", "brown"), lty=1:1, cex=0.8)

dev.copy(png, "../Output/Sensitivity/changingK.png")
dev.off()
#########################################################################3
#theEND > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
#theENDa > k=1000, growth =.2, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
#theENDb > k=1000, growth =.5, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1), main="change in Ho with varying growth rates") 

for(i in unique(theEND[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem <- theEND[theEND[,17] == i,]
  Ho <- tem[,5]
  lines(tem[,1], Ho, lwd=2, col="red") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theENDa[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  temp <- theENDa[theENDa[,17] == i,]
  Ho1 <- temp[,5]
  lines(temp[,1], Ho1, lwd=2, col="yellow") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theENDb[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem2 <- theENDb[theENDb[,17] == i,]
  Ho2 <- tem2[,5]
  lines(tem2[,1], Ho2, lwd=2, col="brown") #points(sub[,1], Ho, lwd=2)
}
legend("topright", legend=c("growth = 0.1", "growth = 0.2", "growth = 0.5"),
       col=c("red", "yellow", "brown"), lty=1:1, cex=0.8)

dev.copy(png, "../Output/Sensitivity/changinggrowthrates.png")
dev.off()

######################################################################
#theEND > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1, migration = 1-5 indv (up to .005 of k)
#theEND. > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1, migration = 5-10 indv (up to .01 of k)
#theEND_ > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1, migration = no indv

plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1), main="change in Ho with varying migration rates") 

for(i in unique(theEND[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem <- theEND[theEND[,17] == i,]
  Ho <- tem[,5]
  lines(tem[,1], Ho, lwd=2, col="red") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theEND.[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  temp <- theEND.[theEND.[,17] == i,]
  Ho1 <- temp[,5]
  lines(temp[,1], Ho1, lwd=2, col="yellow") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theEND_[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem2 <- theEND_[theEND_[,17] == i,]
  Ho2 <- tem2[,5]
  lines(tem2[,1], Ho2, lwd=2, col="brown") #points(sub[,1], Ho, lwd=2)
}
legend("topright", legend=c("migration 1-5 indv", "migration 5-10 indv", "no migration"),
       col=c("red", "yellow", "brown"), lty=1:1, cex=0.8)

dev.copy(png, "../Output/Sensitivity/changingmigrationrates.png")
dev.off()


######################################################################
#theEND > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1, migration = 1-5 indv (up to .005 of k)
#theEND. > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1, migration = 5-10 indv (up to .01 of k)
#theEND_ > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1, migration = no indv

plot(-100, -100 , xlab="time (years)", ylab="population size", xlim=c(0, max(theEND[,1])), ylim=c(0, k), main="change in pop size with varying migration rates") 

for(i in unique(theEND[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem <- theEND[theEND[,17] == i,]
  lines(tem[,1], tem[,2], lwd=2, col="red") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theEND.[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  temp <- theEND.[theEND.[,17] == i,]
  lines(temp[,1], temp[,2], lwd=2, col="yellow") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theEND_[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem2 <- theEND_[theEND_[,17] == i,]
  lines(tem2[,1], tem2[,2], lwd=2, col="brown") #points(sub[,1], Ho, lwd=2)
}
legend("topright", legend=c("migration 1-5 indv", "migration 5-10 indv", "no migration"),
       col=c("red", "yellow", "brown"), lty=1:1, cex=0.8)

dev.copy(png, "../Output/Sensitivity/popsizewithchangingmigrationrates.png")
dev.off()

#########################################################################3
#theEND > k=1000, growth =.1, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
#theENDa > k=1000, growth =.2, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
#theENDb > k=1000, growth =.5, mig variable 1-5, mort = 1/(maxage.V*2), max age=9, brood=2, maturity=1
plot(-100, -100 , xlab="time (years)", ylab="population size", xlim=c(0, max(theEND[,1])), ylim=c(0, k), main="change in pop size with varying growth rates") 

for(i in unique(theEND[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem <- theEND[theEND[,17] == i,]
  lines(tem[,1], tem[,2], lwd=2, col="red") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theENDa[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  temp <- theENDa[theENDa[,17] == i,]
  lines(temp[,1], temp[,2], lwd=2, col="yellow") #points(sub[,1], Ho, lwd=2)
}
for(i in unique(theENDb[,17])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
  tem2 <- theENDb[theENDb[,17] == i,]
  lines(tem2[,1], tem2[,2], lwd=2, col="brown") #points(sub[,1], Ho, lwd=2)
}
legend("topright", legend=c("growth = 0.1", "growth = 0.2", "growth = 0.5"),
       col=c("red", "yellow", "brown"), lty=1:1, cex=0.8)

dev.copy(png, "../Output/Sensitivity/changingpopsizewithgrowthrates.png")
dev.off()



~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
  
#these dont seem useful at all 

tip <- aov(k.Sens[,5]~k.Sens[,18])
summary(tip)

ex <- aov(theEND[,5]~theEND[,8])
summary(ex)

boxplot(theEND[,5]~theEND[,8])
boxplot(theEND[,2]~theEND[,8])

tell <- aov(theEND[,2]~theEND[,15])
summary(tell)

tend <- aov(theEND[,5]~theEND[,15])
summary(tend)
TukeyHSD(theEND[,5]~theEND[,15], conf.level=0.95)
t_test(theEND[,5]~theEND[,15])

boxplot(theEND[,5]~theEND[,15])
boxplot(theEND[,2]~theEND[,15])

boxplot(theEND[,3]~theEND[,15])



#theEND[,2] - popsize
#theEND[,8] - k
#theEND[,15] - growthrate
#theEND[,5] - Ho