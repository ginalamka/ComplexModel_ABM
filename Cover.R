#Cover.R
#used for the complex migration model

setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM") #setwd("C:/Users/Gina/Desktop/2022/ComplexModel_ABM") 
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#parameters
k.V           = c(1000, 500) #c(500, 1000, 5000, 10000)                #carrying capacity
nSNP.V        = 10  #scaleup                #number of SNPs simulated, used to track drift
maxage.V      = 9     #low ages dont work              #maximum age individuals can be; add one at first, so they will die at 4, start at -1
broodsize.V   = 2     #this is now the MAX brood size, aka max fecundity   #REMOVED##c(0:2)              #aka fecundity, remember that this is typically not constant in life; potentially Poisson distribution
maturity.V    = 1                   #age indv becomes reproductively mature
years.V       = 250  #scaleup               #total run time; 200 year run following 50 year stabilization period
r0.V          = 1 #0.1 #c(0.1, 0.2, 0.5)                 #per capita growth rate #1 is stable, <1 is decreasing, >1 is increasing
ratemort.V    = 1/(maxage.V*2) ###0.2225 #??##(1/(maxage.V+2))      #proportion of adults that die each year --CHECK WITH JANNA WHERE THIS NUMBER CAME FROM; current value of .2225 is from Waser and Jones 1991
nSNP.mig.V    = 10                   #number of special alleles for migrants -- these are ADDITIONAL alleles, migrants = 1, orig pop = 0, this will be easier to track than a random value
nSNP.cons.V   = 10        #number of conserved alleles within species -- used to track mutation
### when adding variables already marked out, don't forget to add 3 times below, on RunModel, and other functions that need the variable fed in


#potential migration rates: 1-5 indv, 5-10 indv, no migration

#generate list of parameter combinations
parameters = expand.grid(k.V, nSNP.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, ratemort.V, nSNP.mig.V, nSNP.cons.V)
colnames(parameters) = c("k", "nSNP", "maxage", "broodsize", "maturity", "years", "r0", "ratemort", "nSNP.mig", "nSNP.cons")

#clean up, remember that these are still available in parameters
remove(nSNP.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, ratemort.V, nSNP.mig.V, nSNP.cons.V) #k.V
#2/28/22 I am removing k.V from this so I can reference it in Stochastsic.R

replicates    = 5 #20 #5 #10
plotit        = 1    #1=yes, 0=no
plotit2       = 1    #1=yes, 0=no
mutate        = 1    #1=yes, 0=no   #average mammalian genome mutation rate is 2.2 x 10^-9 per base pair per year, https://doi.org/10.1073/pnas.022629899
#krats = 2844.77 MB = 2844770000 bp x 2.2*10-9  = 6.258494 === does this matter here??? 
#bannertailed 0.0081 mutants/generation/locus, in Busch, Waser, and DeWoody 2007 doi: 10.1111/j.1365-294X.2007.03283.x.
mu            = 0.001  #mutation rate

styr          = 100 #year to start pop decline
nwk           = 250 #pop size after decline -- probs makes sense to keep these even in vary decline years and decline rate. should end @ same pt for all pop sizes
dur           = 50  #duration of small pop size before pop growth 
edyr          = styr+dur #150 #year to end pop decline, first year at low pop size

s             = 5000 #(k.V*5) #size of source pop

r             = 1
#run model iterating over parameters 
theEND = NULL
repEND = NULL
for(r in 1:nrow(parameters)){
  ALL = RunModel(parameters, r, directory, replicates)
  FINAL = ALL[[1]]
  POP = ALL[[2]]
  REP = ALL[[3]]
  
  ####REMOVED## POP = RunModel(parameters, r, directory, replicates)
  ####REMOVED## write.table(POP, paste(directory, "/Output/CoverPop", r, ".csv", sep=""), sep=",", col.names=TRUE, row.names=F) 
  #####REMOVED##write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this for a .txt file, good for in a text editor. ; "/t" for macs
 
  theEND = rbind(theEND, FINAL)
  repEND = rbind(repEND, REP)
}
#write.table(theEND, paste(directory, "/Output/summary_", r, ".csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
#summary table should have nrows = nparameters * nyears * nreplicates
  #^^ may have some fewer than above because some simulations may break before all years are able to be run

Plot(theEND)
Plot2(repEND)
