#Cover.R
#used for the complex migration model

setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM")
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#parameters
k.V           = 1000          #carrying capacity
allele.V      = c(0,1)        #alleles assigned to individuals, equal probability of each at all loci, A = 1, a = 0
nSNP.V        = 10          #number of SNPs simulated
nMicro.V      = 100           #number of microsats simulated
sex.V         = c(0,1)        #males and females in population, 0 = Female, 1 = Male
maxage.V      = 3             #maximum age individuals can be; add one at first, so they will die at 4, start at -1
broodsize.V   = c(0:2)        #aka fecundity, remember that this is typically not constant in life; potentially Poisson distribution
sexratio.V    = 0.5           #equal ratio of females:males; consider a more real life value here, taken from pedigree data with Poisson dist
maturity.V    = 1             #age indv becomes reproductively mature
years.V       = 250           #total run time; 200 year run following 50 year stabilization period
reps.V        = 100           #number of replications of the simulation
r0.V          = 0.1           #per capita growth rate
#nSNP.mig     =10             #number of special alleles for migrants -- these are ADDITIONAL alleles, migrants = 1, orig pop = 0, this will be easier to track than a random value

#generate list of parameter combinations
parameters = expand.grid(k.V, allele.V, nSNP.V, nMicro.V, sex.V, maxage.V, broodsize.V, sexratio.V, maturity.V, years.V, r0.V)
colnames(parameters) = c("k", "allele", "nSNP", "nMicro", "sex", "maxage", "broodsize", "sexratio", "maturity", "years", "r0")

#clean up, remember that these are still available in parameters
remove(k.V, allele.V, nSNP.V, nMicro.V, sex.V, maxage.V, broodsize.V, sexratio.V, maturity.V, years.V, r0.V)

replicates    = 10
r             = 1

#run model iterating over parameters 
for(r in 1:nrow(parameters)){
  POP = RunModel(parameters, r, directory, replicates)
  write.table(POP, paste(directory, "/Output/CoverPop.csv", sep=""), sep=",", col.names=TRUE, row.names=F) 
  #write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this for a .txt file, good for in a text editor. ; "/t" for macs
}
#notice as written, goes through 12 times because there are 12 sets of parameters

#Questions for class today 11/4/2021
#how do I get multiple replicates and years to run??
#How do I get my writeout table (csv) to be in each column?

#consider adding in rbinom() ?? not sure why it would be relevant yet


#Use Janna's papers to look at inbreeding - heck other mmodedls to see how to put an inbreeding cost on indvs

#"google coding standards" for ways to clean up the script