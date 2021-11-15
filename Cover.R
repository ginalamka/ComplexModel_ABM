#Cover.R
#used for the complex migration model

setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM")
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#parameters
k.V           = 1000                #carrying capacity
allele.V      = c(0,1)              #alleles assigned to individuals, equal probability of each at all loci, A = 1, a = 0
nSNP.V        = 10  #scaleup                #number of SNPs simulated
nMicro.V      = 100                 #number of microsats simulated
sex.V         = c(0,1)              #males and females in population, 0 = Female, 1 = Male
maxage.V      = 5     #low ages dont work              #maximum age individuals can be; add one at first, so they will die at 4, start at -1
broodsize.V   = c(0:2)              #aka fecundity, remember that this is typically not constant in life; potentially Poisson distribution
sexratio.V    = 0.5                 #equal ratio of females:males; consider a more real life value here, taken from pedigree data with Poisson dist
maturity.V    = 1                   #age indv becomes reproductively mature
years.V       = 100  #scaleup               #total run time; 200 year run following 50 year stabilization period
reps.V        = 5 #scaleup                 #number of replications of the simulation
r0.V          = 0.1                 #per capita growth rate
ratemort.V    = 1/(maxage.V*2) ###0.2225 #??##(1/(maxage.V+2))      #proportion of adults that die each year --CHECK WITH JANNA WHERE THIS NUMBER CAME FROM; current value of .2225 is from Waser and Jones 1991
#nSNP.mig     =10                   #number of special alleles for migrants -- these are ADDITIONAL alleles, migrants = 1, orig pop = 0, this will be easier to track than a random value

#generate list of parameter combinations
parameters = expand.grid(k.V, allele.V, nSNP.V, nMicro.V, sex.V, maxage.V, broodsize.V, sexratio.V, maturity.V, years.V, r0.V, ratemort.V)
colnames(parameters) = c("k", "allele", "nSNP", "nMicro", "sex", "maxage", "broodsize", "sexratio", "maturity", "years", "r0", "ratemort")

#clean up, remember that these are still available in parameters
remove(k.V, allele.V, nSNP.V, nMicro.V, sex.V, maxage.V, broodsize.V, sexratio.V, maturity.V, years.V, r0.V, ratemort.V)

replicates    = 3 #10
r             = 1

#run model iterating over parameters 
for(r in 1:nrow(parameters)){
  RunModel(parameters, r, directory, replicates)
  ####REMOVED## POP = RunModel(parameters, r, directory, replicates)
  ####REMOVED## write.table(POP, paste(directory, "/Output/CoverPop", r, ".csv", sep=""), sep=",", col.names=TRUE, row.names=F) 
  #####REMOVED##write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this for a .txt file, good for in a text editor. ; "/t" for macs
}
#notice as written, goes through 12 times because there are 12 sets of parameters

#Questions for class today 11/4/2021
#how do I get multiple replicates and years to run??
#How do I get my writeout table (csv) to be in each column?

#consider adding in rbinom() ?? not sure why it would be relevant yet


#Use Janna's papers to look at inbreeding - heck other mmodedls to see how to put an inbreeding cost on indvs
  #things that her paper has: 
    #survival to maturity             .981 and .988
    #lifespan (years)                1.971 and 1.986
    #juvenile offspring              3.149 and 3.171
    #lifetime reproductive success   4.662 nd 4.707

#notes 11/8
#problem with pop crashing after 6 years is because I am killing too much
#therefore, instead of random death, put in a age-related increase of chances of death
#maybe will want to make sure that only adults are mating, not babies -- important for age at maturity -- make temp pop to make sure that only adults are in pop
#age/lifespan for all individyuals to get this age/random mortality
    #could do a loop or could use the apply function
    #apply = give data, apply this function over all rows or all columns. can use apply then sample from 0-1
    #make sure that the values spit it out corrrectly, i.e. kills 30% not 70$ based on incorrect putting in probabilities

#for the column alive v dead, make sure that the functions taht need pop get a temp pop for only adults (mate choice, breeding) but hten make sure to add the indv (breed) to the main pop, not temp pop