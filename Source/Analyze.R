#Analyze.R
#used for complex model for ABM class fall 2021

#taken from Janna's captive breeding IBM, function WriteOut

Analyze = function(parameters, r, pop){  #should this be parameters or replicates?
  #get variables for run -- I think this can be copied from RunModel.R
  k             = parameters$k[r]
  #REMOVED###allele        = parameters$allele[r]
  nSNP          = parameters$nSNP[r]
  #REMOVED###nMicro        = parameters$nMicro[r]
  #REMOVED###sex           = parameters$sex[r]
  maxage        = parameters$maxage[r]
  broodsize     = parameters$broodsize[r]
  #REMOVED###sexratio      = parameters$sexratio[r]
  maturity      = parameters$maturity[r]
  years         = parameters$years[r]
  r0            = parameters$r0[r]
  ratemort      = parameters$ratemort[r]
  nSNP.mig      = parameters$nSNP.mig[r] 
  
  #writeout final POP == compare this to the final pop in Cover.R, should be the same
  #write.table(pop, paste(directory, "/Output/WriteOutPop.csv", sep=""), sep=",", col.names=T, row.names=F) #since in RunModel, might not need to feed it pop
  
  #check for indv ID numbers
  if(!length(pop[,1])==length(unique(pop[,1]))){ #notice that ! means NOT
    print(r)
    print("NON UNIQUE ID NUMBERS")
    return()
  }
  #if no indv, exit
  if(length(pop[,1])==0){return()}
  
  #SHOULD THESE BE ALL INDV OR ONLY ALIVE??
  #separate out alive indv
  alive = pop[pop[,8]==1, , drop=FALSE]
  ###could also use: pop = pop[pop[,8]!=0, , drop=FALSE]
  
  #calculate summary stats for final pop
  FIN = matrix(nrow=years, ncol=7)
  colnames(FIN) = c("year", "popsize", "propmig", "He", "Ho", "meanRRS", "nadults")
  
  #add year to summary matrix
  FIN[,1] = c(1:nrow(FIN))
  
  f = 1
  #for(f in 1:nrow(FIN)){
    year = FIN[f,1]
    
    #separate out alive in current year -- Janna did these from year born and year died column
    data = alive[alive[,8]>0, , drop = FALSE]
    
    x=NULL
    x = try(length(data[,1]), silent=TRUE)
    if(is.null(x)){break}
    if(x<1){break}
    if(!is.numeric(x)){break}
    
    #number of indv
    FIN[f,2] = nrow(data)
    
    #proportion migrants in population
    FIN[f,3] =  sum(data[,2]==-1)/length(data[,1])   #1 - sum(data[,2]==-1)/length(data[,1])
    
    #He and Ho - neutral (?)
    SNPS = (nSNP*2) + (nSNP.mig*2)
    genotype = data[, -c(ncol(data)-SNPS:ncol(data))]
    snps = rep(c(1,2),ncol(genotype)/2)
    
    HE = NULL
    HO = NULL
    
    loc.pos = seq(1, SNPS, 2)
    for(lp in loc.pos){
      #per locus heterozygostiy
      locus <- genotype[, c(lp, lp+1), drop=FALSE]
      geno  <- length(locus[,1])
      het   <- length(which(locus[,1] != locus[,2]))
      het.observed <- het/geno
      HO = c(HO, het.observed)
      
      freqs <- table(locus)
      homozygous = NULL
      for(v in 1:length(freqs)){
        homozygous = c(homozygous, (freqs[v]/sum(freqs)*freqs[v]/sum(freqs)))
      }
      het.expected <- 1 - sum(homozygous)
      HE = c(HE, het.expected)
    }
    FIN[f,4] <- mean(HE)
    FIN[f,5] <- mean(HO)
    
    #figure out how to find RRS, I think we need fecundity/indv LRS first
    FIN[f,6] = NA #mean(data[REPRODUCTIVE SUCCESS COLUMN])
    
    #find number of adults per year
    adults = data[data[,4]>= maturity, , drop = FALSE]
    FIN[f,7] = nrow(adults)
    
  #}
  
  params = parameters[rep(r, nrow(FIN)),]
  out = cbind(FIN,params)
  colnames(out) = c("year", "popsize", "propmig", "He", "Ho", "meanRRS", "nadults",
                    "k", "nSNP", "maxage", "broodsize", "maturity", "years", "r0", "ratemort") #add nSNP.mig if in data
  
  return(out)
}

#additional things that I should make figs of that will need to be added to Analyze.R
  #prop migrant alleles in pop
  #number of males and females that bred in that year (may be able to replace Ne)
  #number of mates (use the function "table"; data$habitat.mate (or could try "apply"))
  #sex ratio
  #number of new migrants that generation (?)
  #Fst


#notes 2/28/22
  #add in Fst. do this for every year, and then later will cherry pick the results for every generation
  #compare the Fst of the starting pop to the Fst of successive years to see how fst changes. 
  #use the Weir and Cockerheim's Fst-- this should be a function/package in R to calculate this to make it easier
    #consider any biases in independence, but that may not be an issue
      #POTENTIAL APPLICATION is trying to create a calculation for estimating migrants from Fst values. --could be linked to our data?

#consider adding 
  #number/proportion of migrants vs number/proportion of effective migrants
  #genome-wide Fst (compare across loci or in source vs pop)
  #a way to track inbreeding?? we know the pedigree so maybe if have a great grand parent in common? -- maybe can relate with real krat inbreeding data??
  #relationship between fitness and heterozygosity (hetero advantage)? is this a thing since most things are random, not selected for?
  #histogram for age of death

#to add: (noted 12/29/2021)
  #LRS, RRS >> must use info from ReproSuc.R, so maybe better to calculate there and then feed to Plot.R
  #number of alleles over time
  #number of migrant alleles in population over time
  #additional possible values
    #number of unique breeders unique(pop[,2])+unique(pop[,3])
    #mating success = total number of mates with whome an indv produced offspring
#-- also check the vortex abm paper to add any things they have that might be useful and informative here