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
    
    #set up to subset by replicate and by year
    #remember that the replicate "r" will be the last column of theEND
    rep      = theEND[,11]                 #grabs replicate column
    para      = theEND[,12]                 #grabs parameter set
    yr       = unique(theEND[, 1])         #grabs unique years
    mxk      = ceiling(max(theEND[,2]))    #rounds up the maximum pop size. Note should = k
    N        = theEND[,2]                  #grabs population size
    adults   = theEND[,7]                  #grabs total number of adults
    He       = theEND[,4]                  #grabs expected heterozygosity
    Ho       = theEND[,5]                  #grabs observed heterozygosity
    sxratio  = theEND[,8]                  #grabs the sex ratio
    migprop  = theEND[,3]                  #grabs the proportion of migrants in the population
    nmig     = theEND[,9]                  #grabs the number of migrants in the population
    fst      = theEND[,10]                 #grabs Fst 
    fis      = theEND[,6]                  #grabs Fis
    
    #########################################################    
    #Plot the population size and number of adults over time
    plot(-100, -100 , xlab="Time (generation)", ylab="Population Size", xlim=c(0, max(yr)), ylim=c(0, mxk)) 
    for(p in unique(para)){
      temp <- theEND[para == p,]
      for(i in unique(rep)){  
        sub <- temp[rep == i,] #unique replicate
        lines(sub[,1], sub[,2], lwd=2)
        lines(sub[,1], sub[,7], col="blue", lwd=2)
      }
    }
    legend("bottomleft", legend=c("total population size", "total number of adults"),
            col=c("black", "blue"), lty=1:1, cex=0.8)
    dev.copy(png, "../Output/total_and_adult_population_size_over_time.png")
    #dev.off()
    
#CAHGNED HERE TO SEE IF THAT FIXES IT
    ##########################################################    
    #Plot the proportion of migrants in the population over time
    plot(-10, -10 , xlab="Time (generation)", ylab="Proprtion of migrants in population", xlim=c(0, max(yr)), ylim=c(0, 1)) 
    for(p in unique(para)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,]  #unique replicate
        lines(sub[,1], sub[,3], lwd=2)
      }
    }
    dev.copy(png, "/Output/proportion_of_migrants_in_the_population_over_time.png")
    dev.off()
 #CAHGNED HERE TOO TO TRY AND FIX
   
    ###########################################################
    #Plot the observed vs expected hetero
    plot(-100, -100 , xlab="Expected heterozygosity", ylab="Observed heterozygosity", xlim=c(0, 1), ylim=c(0, 1)) 
    for(p in unique(para)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,] #unique replicate
        HE <- sub[,4]
        HO <- sub[,5]
        points(HE, HO, lwd=2)
      }
    }
    abline(coef = c(0,1), col = "red")
    dev.copy(png, "../Output/observed_vs_expected_heterozygosity.png")
    dev.off()
        
    #########################################################
    #Plot the observed hetero over time
    plot(-100, -100 , xlab="Time (generation)", ylab="Observed heterozygosity", xlim=c(0, max(yr)), ylim=c(0, 1)) 
    for(p in unique(para)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,] #unique replicate
        HO <- sub[,5]
        lines(sub[,1], HO, lwd=2) #points(sub[,1], Ho, lwd=2)
      }
    }
    dev.copy(png, "../Output/observed_heterozygosity_over_time.png")
    dev.off()  
    
    
    
    
    
    
    
    
    #not sure why, but 5/5/22 when trying to fix the functions below, plotting stopped and I cant view them
    #will try again to complete this later. 
    #**may** be because I had "par" as the label for parameters but when I ran par(mar...) I broke it?
    #changed par to para --- will need to do above and finish fixing those changes below
    
    #########################################################
    #Plot the observed heteroz and proportion of migrants in pop over time
    par(mar = c(5,4,4,4)+0.3)
    plot(-100, -100 , xlab="Time (generation)", ylab="Observed heterozygosity", xlim=c(0, max(yr)), ylim=c(0, 1)) 
    for(p in unique(para)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,]  #unique replicate
        HO <- sub[,5]
        lines(sub[,1], HO, lwd=2)
      }
      par(new = TRUE)
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,] #unique replicate
        mig <- sub[,3]  #prop migrants
        lines(sub[,1], mig, lwd=2, col = "blue")
      }
    }
    axis(side = 4, at = NULL, labels = TRUE, col = "blue")
    mtext("Proportion of migrants in population", side=4, line =3, col = "blue")
    dev.copy(png, "../Output/observed_het_and_proportion_migrants_over_time.png")
    dev.off()
    
    ###########################################################        
    #Plot Fst over time
    plot(-100, -100 , xlab="Time (generation)", ylab="Fst", xlim=c(0, max(yr)), ylim=c(-1, 1)) 
    for(p in unique(para)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,] #unique replicate
        FST <- sub[,10]
        lines(sub[,1], FST, lwd=2) #points(sub[,1], Ho, lwd=2)
      }
      abline(h = 0, col = "steelblue", lty = 2)
      dev.copy(png, "../Output/FST_over_time.png")
      dev.off()
    }  

    ###########################################################        
    #Plot Fis over time
    plot(-100, -100 , xlab="Time (generation)", ylab="Fis", xlim=c(0, max(yr)), ylim=c(-1, 1)) 
    for(p in unique(para)){
      for(i in unique(rep)){  #this allows each rep to be a dif line rather than the lines through it. DO THIS FOR ALL PLOTS
        sub <- theEND[rep == i,] #unique replicate
        FIS <- sub[,6]
        lines(sub[,1], FIS, lwd=2) #points(sub[,1], Ho, lwd=2)
      }
      abline(h = 0, col = "steelblue", lty = 2)
      dev.copy(png, "../Output/FIS_over_time.png")
      dev.off()
    }  
        
        #additional things that I should make figs of that will need to be added to Analyze.R
          #prop migrant alleles in pop
          #number of males and females that bred in that year (may be able to replace Ne)
          #number of mates (use the function "table"; data$habitat.mate (or could try "apply"))
          #sex ratio
          #number of new migrants that generation (?)
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



#setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM/CSVs")
#theEND = read.table("theEND_param1.csv", header=T, sep=",")
