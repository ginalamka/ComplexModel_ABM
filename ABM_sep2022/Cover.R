#Cover.R
#used for the complex migration model

setwd("/scratch/glamka/_proj_/_group_") # setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/ABM_sep2022") 
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")   #  outdir = paste("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM", "/Output_local/", sep = "")
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

prj = "_proj_"
grp = "_group_"

#parameters
k.V           = 100 #1000 #c(1000, 5000, 500) #c(500, 1000, 5000, 10000)                #carrying capacity
nSNP.V        = 100 #1000  #scaleup                #number of SNPs simulated, used to track drift
miggy.V       = "a" #c(0,"a","b","c")  #"a"=one mig per gen, "b"=1xof50@175, "c"=3xpf25@175|201|225  #migration parameter type
LBhet.V       = 0.45 #c(0.45, 0.07) #c(0.1, 0.45, 0.8) #lowerbound limit  #c(0.4, 0.6) #c(0.8, 0.9)   #THINK ABOUT THE BEST WAY TO SET THIS UP -- either give the values for lower bound and then in RunModel, +1 for upper bound. or change the lower and upper depending on the run
maxage.V      = 9 #c(15, 3) #9     #low ages dont work              #maximum age individuals can be; add one at first, so they will die at 4, start at -1
broodsize.V   = 2 #c(4, 6)  #2     #this is now the MAX brood size, aka max fecundity   #REMOVED##c(0:2)              #aka fecundity, remember that this is typically not constant in life; potentially Poisson distribution
maturity.V    = 1                   #age indv becomes reproductively mature
years.V       = 35 #350  #scaleup               #total run time; 200 year run following 50 year stabilization period
r0.V          = 1 #c(1, 0.5, 0.1)  #c(1, .8, 1.2) #0.1 #c(0.1, 0.2, 0.5)                 #per capita growth rate #0/1 is stable, <0/1 is decreasing, >0/1 is increasing - currently checking cuz r0+1 in log growth eq
nSNP.mig.V    = 10 #100                   #number of special alleles for migrants -- these are ADDITIONAL alleles, migrants = 1, orig pop = 0, this will be easier to track than a random value
nSNP.cons.V   = 0        #number of conserved alleles within species -- used to track mutation
#REMOVED##ratemort.V    = 1/(maxage.V*2) #note, I dont think this is used for anything anymore! ###0.2225 #??##(1/(maxage.V+2))      #proportion of adults that die each year --CHECK WITH JANNA WHERE THIS NUMBER CAME FROM; current value of .2225 is from Waser and Jones 1991
### when adding variables already marked out, don't forget to add 3 times below, on RunModel, and other functions that need the variable fed in


#potential migration rates: 1-5 indv, 5-10 indv, no migration

#generate list of parameter combinations
parameters = expand.grid(k.V, nSNP.V, miggy.V, LBhet.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, nSNP.mig.V, nSNP.cons.V)
colnames(parameters) = c("k", "nSNP", "miggy", "LBhet", "maxage", "broodsize", "maturity", "years", "r0", "nSNP.mig", "nSNP.cons")
write.table(parameters, paste(directory, "/Output/parameters__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

#clean up, remember that these are still available in parameters
remove(k.V, nSNP.V, miggy.V, LBhet.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, nSNP.mig.V, nSNP.cons.V) 
#2/28/22 I am removing k.V from this so I can reference it in Stochastsic.R

replicates    = 1 #20 #5 #10
allee         = 1    #1=yes, 0=no
matemigs      = 1    #1=yes, 0=no
plotit        = 0    #1=yes, 0=no
plotit2       = 0    #1=yes, 0=no
mutate        = 1    #1=yes, 0=no   #average mammalian genome mutation rate is 2.2 x 10^-9 per base pair per year, https://doi.org/10.1073/pnas.022629899
#krats = 2844.77 MB = 2844770000 bp x 2.2*10-9  = 6.258494 === does this matter here??? 
#bannertailed 0.0081 mutants/generation/locus, in Busch, Waser, and DeWoody 2007 doi: 10.1111/j.1365-294X.2007.03283.x.
mu            = 0.001  #mutation rate

styr          = 100 #year to start pop decline
nwk           = 300 #pop size after decline -- probs makes sense to keep these even in vary decline years and decline rate. should end @ same pt for all pop sizes
drp           = 10  #number of years for drop from k to nwk
dur           = 40  #duration of small pop size before pop growth 
edyr          = styr+drp #150 #year to end pop decline, first year at low pop size (nwk)

s             = 5000 #(k.V*5) #size of source pop

r             = 1
#run model iterating over parameters 
theEND = NULL
repEND = NULL
finalPOP = NULL
for(r in 1:nrow(parameters)){
  ALL = RunModel(parameters, r, directory, replicates, prj, grp)
  FINAL = ALL[[1]]
  REP = ALL[[2]]
  #POP = ALL[[2]]
  
  ####REMOVED## POP = RunModel(parameters, r, directory, replicates)
  ####REMOVED## write.table(POP, paste(directory, "/Output/CoverPop", r, ".csv", sep=""), sep=",", col.names=TRUE, row.names=F) 
  #####REMOVED##write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this for a .txt file, good for in a text editor. ; "/t" for macs
 
  theEND = rbind(theEND, FINAL)
  repEND = rbind(repEND, REP)
  #finalPOP = rbind(finalPOP, POP)
}
write.table(theEND, paste(directory, "/Output/summary__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(repEND, paste(directory, "/Output/rep_summary__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
#write.table(finalPOP, paste(directory, "/Output/POP__proj___group_.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
#summary table should have nrows = nparameters * nyears * nreplicates
  #^^ may have some fewer than above because some simulations may break before all years are able to be run

Plot(theEND)
Plot2(repEND)

print(paste("SUCCESSSS BITCHESSSS"))

#things that need changed from local > Easley
#makefile and run_model.sh needs to be changed each run
#Cover.R needed to change the working directory and FunctionSourcer.R path
#reload and call required R packages (and make sure it uses the correct R version)
#remove dead indv to make it faster to run (in RunModel.R)
#add write.table for the final datasets and parameters
#check all functions so that when checking/removing dead, that if nrow(dead)==0, pop is unchanged (fix this by using %NOTin% dead rather than -which%in%dead
#add an empty Output folder to put tables and figs in
#REMOVED reproductive success function since I wasnt holding pop object to speed up computational time before finalizing values for analysis

##>>Figure out plotting and how to make a makefile for downloading all of the output data.

#major things edited Dec 2022 [in progress]
#added miggy and LBhet as parameters
#add back in the different SNP types
#hold more data in Analyze -- n killed, n oldies, n heterozy killed
#increase reps and SNPs and years
#CRASH THE POP
