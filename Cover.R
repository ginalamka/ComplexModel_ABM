#Cover.R
#used for the complex migration model

setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM")
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#parameters
k.V           = 1000 #c(1000, 5000, 10000)                #carrying capacity
#REMOVED###allele.V      = c(0,1)              #alleles assigned to individuals, equal probability of each at all loci, A = 1, a = 0
nSNP.V        = 10  #scaleup                #number of SNPs simulated
nMicro.V      = 100 #tbd                 #number of microsats simulated
#REMOVED###sex.V         = c(0,1)              #males and females in population, 0 = Female, 1 = Male
maxage.V      = 9     #low ages dont work              #maximum age individuals can be; add one at first, so they will die at 4, start at -1
broodsize.V   = 2     #this is now the MAX brood size, aka max fecundity   #REMOVED##c(0:2)              #aka fecundity, remember that this is typically not constant in life; potentially Poisson distribution
#REMOVED###sexratio.V    = 0.5                 #equal ratio of females:males; consider a more real life value here, taken from pedigree data with Poisson dist
maturity.V    = 1                   #age indv becomes reproductively mature
years.V       = 100  #scaleup               #total run time; 200 year run following 50 year stabilization period
#REMOVED###reps.V        = 5 #scaleup                 #number of replications of the simulation
r0.V          = 0.1 #c(0.1, 0.2, 0.5)                 #per capita growth rate
ratemort.V    = 1/(maxage.V*2) ###0.2225 #??##(1/(maxage.V+2))      #proportion of adults that die each year --CHECK WITH JANNA WHERE THIS NUMBER CAME FROM; current value of .2225 is from Waser and Jones 1991
#nSNP.mig.V     = 10                   #number of special alleles for migrants -- these are ADDITIONAL alleles, migrants = 1, orig pop = 0, this will be easier to track than a random value
### when adding variables already marked out, don't forget to add 3 times below, on RunModel, and other functions that need the variable fed in

#potential migration rates: 1-5 indv, 5-10 indv, no migration

#generate list of parameter combinations
parameters = expand.grid(k.V, nSNP.V, nMicro.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, ratemort.V)
colnames(parameters) = c("k", "nSNP", "nMicro", "maxage", "broodsize", "maturity", "years", "r0", "ratemort")

#clean up, remember that these are still available in parameters
remove(k.V, nSNP.V, nMicro.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, ratemort.V)

replicates    = 5 #10
plotit        = 1    #1=yes, 0=no
r             = 1

#run model iterating over parameters 
theEND = NULL
for(r in 1:nrow(parameters)){
  FINAL = RunModel(parameters, r, directory, replicates)
  
  ####REMOVED## POP = RunModel(parameters, r, directory, replicates)
  ####REMOVED## write.table(POP, paste(directory, "/Output/CoverPop", r, ".csv", sep=""), sep=",", col.names=TRUE, row.names=F) 
  #####REMOVED##write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this for a .txt file, good for in a text editor. ; "/t" for macs
 
  theEND = rbind(theEND, FINAL)
}
#write.table(theEND, paste(directory, "/Output/summary_", r, ".csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
#summary table should have nrows = nparameters * nyears * nreplicates
  #^^ may have some fewer than above because some simulations may break before all years are able to be run

Plot(theEND)


##############################################################
##############################################################

#WAIT FOR REPRODUCTIVE SUCCESS UNTIL AFTER THE CLASS IS OVER!!!!! 
  #things that would need to be done:
    #year born and year died (would essentially need sliding window analysis per year)
    #total number of times an adult was a parent
    #amount of babies born to each parent that made it to maturity 
    #would probably need to hold pop because you need to run the whole simulation before knowing repro success
    #mayyyy need to be its own function within cover? or within replicates? or maybe just for each 100 years. think about this later


#consider adding in rbinom() ?? not sure why it would be relevant yet >>> binomials are for success/failures. would that be helpful?


#Use Janna's papers to look at inbreeding - check other models to see how to put an **inbreeding cost on indvs
  #things that her paper has: 
    #survival to maturity             .981 and .988
    #lifespan (years)                1.971 and 1.986
    #juvenile offspring              3.149 and 3.171
    #lifetime reproductive success   4.662 nd 4.707

#apply = give data, apply this function over all rows or all columns. can use apply then sample from 0-149