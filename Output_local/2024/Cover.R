#Cover.R used to run the simulation. All scripts are connected via this R script. See the README for more information.
#This model is written by Gina F. Lamka (https://ginalamka.weebly.com/)
  #adapted from Janna R. Willoughby's Captive Breeding IBM (https://github.com/jwillou/captivebreeding-IBM | doi:10.1111/cobi.13217)

#Set working directory and out directory
setwd("/scratch/glamka/p_2.20.24_LL3a_mi_2/20") # setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/ABM_sep2022") 
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")   #  outdir = paste("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM", "/Output_local/", sep = "")
#Source function scripts
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#define location of of project and groups for high performance computing cluster
prj = "p_2.20.24_LL3a_mi_2"
grp = "20"

#parameters
k.V           = 1000          #carrying capacity
nSNP.V        = 1000          #number of SNPs simulated, used to track drift
miggy.V       = "a" #c(0,          #migration parameter type -- set in Migrate.R; 0 = no migration
                 # "a",        #"a"=one mig per gen  
                 # "b",        #"b"=1xof100@151  
                 # "c",        #"c"=4xof25@151|165|181|195
                 # "d",        #"d"=one mig per gen @ >=edyr+dur+1 
                 # "e",        #"e"=1xof100@125
                 # "f")        #"f"=4xof25@125|140|155|170
LBhet.V       = 0.05 #c(0.4, 0.05)  #lowerbound limit for SOURCE POP -- called in RunModel.R
LBp.V         = 0.05 #c(0.4, 0.05)  #lowerbound limit for FOCAL POP -- called in RunModel.R
maxage.V      = 9             #maximum age individuals can be -- note, first step is ageing, so some indv start at -1
broodsize.V   = 2             #max brood size, aka max fecundity  
maturity.V    = 1             #age indv becomes reproductively mature
years.V       = 350           #total run time
r0.V          = 1             #per capita growth rate 
nSNP.mig.V    = 100           #number of migrant specific alleles -- these are ADDITIONAL alleles to nSNP above, migrants = 1, orig pop = 0 -- called in RunModel.R and Breed.R
nSNP.cons.V   = 100           #number of conserved alleles within species -- used to track mutation

### when adding additional variables, don't forget to add 3 times: in Cover.R below, in RunModel.R, and other functions that need the variable fed in

#generate list of parameter combinations
parameters = expand.grid(k.V, nSNP.V, miggy.V, LBhet.V, LBp.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, nSNP.mig.V, nSNP.cons.V)
colnames(parameters) = c("k", "nSNP", "miggy", "LBhet", "LBp", "maxage", "broodsize", "maturity", "years", "r0", "nSNP.mig", "nSNP.cons")
write.table(parameters, paste(directory, "/Output/parameters_p_2.20.24_LL3a_mi_2_20.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

#clean up, remember that these are still available in parameters
remove(k.V, nSNP.V, miggy.V, LBhet.V, LBp.V, maxage.V, broodsize.V, maturity.V, years.V, r0.V, nSNP.mig.V, nSNP.cons.V) 

#on/off switches for functions
replicates    = 1 
allee         = 1    #1=yes, 0=no
matemigs      = 1    #1=yes, 0=no
plotit        = 0    #1=yes, 0=no
plotit2       = 0    #1=yes, 0=no
mutate        = 1    #1=yes, 0=no  
mutate_init   = 1    #1=yes, 0=no
mu            = 0.00022 #0.0000000022  #mutation rate in noncoding SNPs #(2.2 x 10^-9) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC117386/
mu_co         = 0.000048 #0.00000000048  #deleterious mutation rate in coding SNPs #4.84 X10^-10 = 2.2x10^-9*.22 constraint #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC263826/
purge         = 1    #1=yes, 0=no
purge_mutants = 0    #1=yes, 0=no  #AS OF 1/24/24 THIS DOES NOT WORK CORRECTLY

#bottleneck parameters
styr          = 100 #year to start pop decline
nwk           = 300 #pop size after decline 
drp           = 10  #number of years to drop from k to nwk
dur           = 40  #duration of small pop size before pop growth 
edyr          = styr+drp  #year to end pop decline, first year at low pop size (nwk)
s             = 5000 #size of source pop

#run model iterating over parameters 
theEND = NULL
repEND = NULL
finalPOP = NULL
r=1  #use this when debugging, remove this when not skipping through the below line
for(r in 1:nrow(parameters)){
  ALL = RunModel(parameters, r, directory, replicates, prj, grp)
  FINAL = ALL[[1]]
  REP = ALL[[2]]
  #POP = ALL[[3]]  #keep if want to hold indv and genotype data, slows computation time considerably

  theEND = rbind(theEND, FINAL)  #final data calculated in Analyze.R
  repEND = rbind(repEND, REP)    #final data calculated in RepSucc.R
  #finalPOP = rbind(finalPOP, POP)  #keep if want to hold indv and genotype data, slows computation time considerably
}

#write out data tables
write.table(theEND, paste(directory, "/Output/summary_p_2.20.24_LL3a_mi_2_20.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(repEND, paste(directory, "/Output/rep_summary_p_2.20.24_LL3a_mi_2_20.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
#write.table(POP, paste(directory, "/Output/CoverPop.txt", sep=""), sep="\t", col.names=TRUE, row.names=F)  #use this code for a .txt file, good for in a text editor. ; "/t" for macs
#summary table should have nrows = nparameters * nyears * nreplicates
  #^^ may have some fewer than above because some simulations may break before all years are able to be run


#add celebratory note at the end, because it worked!
print(paste("SUCCESSSS BITCHESSSS"))


#Notes on things that will need changed when going from a local computer to a high performance computing cluster:
  #makefile and run_model.sh needs to be changed each run
  #Cover.R needed to change the working directory and FunctionSourcer.R path
  #reload and call required R packages (and make sure it uses the correct R version)
  #add write.table for the final datasets and parameters, typically done in makefile
  #check all functions so that when checking/removing dead, that if nrow(dead)==0, pop is unchanged (fix this by using %NOTin% dead rather than -which%in%dead)
  #add an empty Output folder to put tables and figs in
