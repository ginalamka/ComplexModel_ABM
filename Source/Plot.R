#this is for the complex model for aBM class
#this is taken somewhat from Janna's plot functions

#notes from Janna 11/19
#Do you remember the figures I showed in class where I plotted output varying only one parameter at a time? That's what I'd suggest. But, you def don't need ALL combinations. Probably you'll want like a control (no migrants?) and then you can use different colors to show output for levels of a particular variable.
#But, you'll still have to explore your output to know which variables are imporatn.
#(check Janna's figures from her paper to reference)

Plot = function(theEND){
  #PLOT IT
  
  ##CONSIDER WHETHER YOU WANT A LINE OR DOTS (for dots, instead of line(xxx), do plot(xxx))
  #all parameters and replicates are being plotted at once. will need to clean this up
    #to do that, will probably need to subser theEND -- ex try::   for(parameters$k[r] == r){N = theEND[,2]}
  
  if(plotit==1){
    #loop through each replicate
    #for(rep in 1:replicates){
      #for each parameter combo
      #for(pp in 1:nrow(parameters)){
        
        #number of alive, number of adults alive
        plot(-100, -100 , xlab="time (years)", ylab="population size", xlim=c(0, parameters$years[r]), ylim=c(0, parameters$k[r])) 
        #plot(theEND[,1],theEND[,2])
        
        N       = theEND[,2]               #c(Na, nrow(population[population[,9]==1, ,drop=FALSE]))
        adults  = theEND[,7]               #population[population[,9]==1, ,drop=FALSE]
        #Nadults = c(Nadults, nrow(alive[alive[,2] >= runvars$maturity[r],,drop=FALSE]))
        #lines(c(0:theEND[,1]), N , xlab="time (years)", ylab="population size", cex = 2, lty = 1, col="black", lwd=5)
        for(q in unique(theEND[,8])){       #unique K
          temp <- theEND[theEND[,8] ==q,]
          for(i in unique(theEND[,16])){  
            sub <- theEND[theEND[,16] == i,] #unique replicate
            lines(sub[,1], sub[,2], lwd=2)
            lines(sub[,1], sub[,7], col="blue", lwd=2)
          }
        }
        legend("bottomleft", legend=c("total population size", "total number of adults"),
               col=c("black", "blue"), lty=1:1, cex=0.8)
        #points(theEND[,1], N , xlab="time (years)", ylab="population size", cex = 1, lty = 1, col="black", lwd=2)
        #points(theEND[,1], adults , xlab="time (years)", ylab="population size", cex = 1, lty = 1, col="blue", lwd=2)
        dev.copy(png, "../Output/total_and_adult_population_size_over_time.png")
        dev.off()
        
        #proportion of migrants
        plot(-100, -100 , xlab="time (years)", ylab="proprtion of migrants in population", xlim=c(0, max(theEND[,1])), ylim=c(0, 1)) 
        mig     = theEND[,3]
        for(q in unique(theEND[,8])){    #unique K
          for(i in unique(theEND[,16])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
            sub <- theEND[theEND[,16] == i,]  #unique replicate
            lines(sub[,1], sub[,3], lwd=2)
          }
        }
        dev.copy(png, "../Output/proportion_of_migrants_in_the_population_over_time.png")
        dev.off()
        #REMOVED### lines(theEND[,1], mig , xlab="time (years)", ylab="proprtion of migrants in population", cex = 2, lty = 1, col="black", lwd=5)
        
        #observed vs expected hetero
        plot(-100, -100 , xlab="expected heterozygosity", ylab="observed heterozygosity", xlim=c(0, 1), ylim=c(0, 1)) 
        Ho     = theEND[,5]
        He     = theEND[,4]
        for(i in unique(theEND[,16])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
          sub <- theEND[theEND[,16] == i,] #unique replicate
          points(He, Ho, lwd=2)
        }
        abline(coef = c(0,1), col = "red")
        #points(He, Ho , xlab="expected heterozygosity", ylab="observed heterozygosity", cex = 1, lty = 1, col="black", lwd=5)
        dev.copy(png, "../Output/observed_vs_expected_heterozygosity.png")
        dev.off()
        
        #observed hetero over time
        plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1)) 
        #Ho     = theEND[,5]
        for(i in unique(theEND[,16])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
          sub <- theEND[theEND[,16] == i,] #unique replicate
          Ho <- sub[,5]
          lines(sub[,1], Ho, lwd=2) #points(sub[,1], Ho, lwd=2)
        }
        #lines(theEND[,1], Ho , xlab="time (years)", ylab="observed heterozygosity", cex = 2, lty = 1, col="black", lwd=5)
        dev.copy(png, "../Output/observed_heterozygosity_over_time.png")
        dev.off()
        
        #observed hetero over time with number of migrants
        #plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1)) 
        #lines(theEND[,1], Ho , xlab="time (years)", ylab="observed heterozygosity", cex = 2, lty = 1, col="black", lwd=5)
        #lines(theEND[,1], mig , xlab="time (years)", ylab="proprtion of migrants in population", cex = 2, lty = 1, col="blue", lwd=5)
        
        #observed heteroz and proportion of migrants in pop
        #par(mar = c(5,4,4,4)+0.3)
        #plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1)) 
        #lines(theEND[,1], Ho , cex = 2, lty = 1, col="black", lwd=2) #NOTICE points, not plot  #xlab="time (years)", ylab="observed heterozygosity",
        #par(new = TRUE)
        #lines(theEND[,1], mig , xlab="", ylab="", xlim=c(0, max(theEND[,1])), ylim=c(0,1), cex = 2, lty = 1, col="blue", lwd=2)
        #plot(theEND[,1], mig , xlab="", ylab="", cex = 2, lty = 1, col="blue", lwd=5)
        #axis(side = 4, at = NULL, labels = TRUE)
        #mtext("proportion of migrants in population", side=4, line =3)
        
        #observed heteroz and proportion of migrants in pop
        par(mar = c(5,4,4,4)+0.3)
        plot(-100, -100 , xlab="time (years)", ylab="observed heterozygosity", xlim=c(0, max(theEND[,1])), ylim=c(0, 1)) 
        for(i in unique(theEND[,16])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
          sub <- theEND[theEND[,16] == i,]  #unique replicate
          Ho <- sub[,5]
          lines(sub[,1], Ho, lwd=2)
        }
        par(new = TRUE)
        for(i in unique(theEND[,16])){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
          sub <- theEND[theEND[,16] == i,] #unique replicate
          mig <- sub[,3]  #prop migrants
          lines(sub[,1], mig, lwd=2, col = "blue")
        }
        axis(side = 4, at = NULL, labels = TRUE, col = "blue")
        mtext("proportion of migrants in population", side=4, line =3, col = "blue")
        dev.copy(png, "../Output/observed_het_and_proportion_migrants_over_time.png")
        dev.off()
        
        #proportion of migrant genomes in population (use migrant alleles)
      #}
    #}
  }else{print(paste("no plotting today!"))}
  
  
}

#notes 2/28/22 -- still need to add Fst calcuations. check in Analyze.R for more details

#columns in theEND:
  #1 > year
  #2 > popsize
  #3 > propmig
  #4 > He
  #5 > Ho
  #6 > meanRRS         #still need to add
  #7 > nadults
  #8 > k
  #9 > nSNP
  #10> maxage
  #11> broodsize
  #12>maturity
  #13>years
  #14>r0
  #15>ratemort
  #16>replicates (no column header)

#link for some good plotting tips: http://www.sthda.com/english/wiki/graphical-parameters
#https://bookdown.org/rdpeng/exdata/exploratory-graphs.html#scatterplot---using-color
#https://www.statology.org/abline-in-r/#:~:text=How%20to%20Use%20abline%20%28%29%20in%20R%20to,line.%20h%3A%20the%20y-value%20for%20the%20horizontal%20line.

#thoughts after class>>   
  #consider comparing N and Ne (effective breeders)
  #should migrants always be the breeders? (that would be effective migrants, not just n migrants)
