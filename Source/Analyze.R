#Analyze.R
#used for complex model for ABM class fall 2021

#taken from Janna's captive breeding IBM, function WriteOut

Analyze = function(parameters, r, pop){  #should this be parameters or replicates?
  #get variables for run -- I think this can be copied from RunModel.R
  k             = parameters$k[r]
  allele        = parameters$allele[r]
  nSNP          = parameters$nSNP[r]
  nMicro        = parameters$nMicro[r]
  sex           = parameters$sex[r]
  maxage        = parameters$maxage[r]
  broodsize     = parameters$broodsize[r]
  sexratio      = parameters$sexratio[r]
  maturity      = parameters$maturity[r]
  years         = parameters$years[r]
  r0            = parameters$r0[r]
  
  #writeout final POP == compare this to the final pop in Cover.R, should be the same
  write.table(pop, paste(directory, "/Output/WriteOutPop.csv", sep=""), sep="/t", col.names=F, row.names=F) #since in RunModel, might not need to feed it pop
  
  #check for indv ID numbers
  if(!length(pop[,1])==length(unique(pop[,1]))){ #notice that ! means NOT
    print(r)
    print("NON UNIQUE ID NUMBERS")
    return()
  }
  #if no indv, exit
  if(length(pop[,1])==0){return()}
  
  #calculate summary stats for final pop
  FIN = matrix(nrow=years, ncol=5 + (maxage+1))
  AGES = NULL
  for(a in 0:maxage){AGES = c(AGES, paste("age", a, sep=""))}
  colnames(FIN) = c("year", "popsize", "He", "Ho", "meanRRS", AGES)
  
  #add year to summary matrix
  FIN[,1] = c(1:nrow(FIN))
  
  for(g in 1:nrow(FIN)){
    genotype = pop[, -c(ncol(pop)-(nSNP*2):ncol(pop))]
    SNPs = rep(c(1,2),ncol(genotype)/2)
    
    #He and Ho - neutral (?)
    HE = NULL
    HO = NULL
    
    loc.pos = seq(1, (nSNP*2), 2)
    for(lp in loc.pos){
      locus <- genotype[, c(1, lp+1), drop=FALSE]
      geno  <- length(locus[,1])
      het   <- length(which(locus[,1] != locus[,2]))
      het.observed <- het/geno
      HO = c(HO, het.observed)
      
      freqs <- table(locus)
      homozygous = NULL
      for(v in 1:length(freqs)){
        homozygous = c(homozygous, (freqs[1]/sum(freqs)*freqs[1]/sum(freqs)))
      }
      het.expected <- 1- sum(homozygous)
      HE = c(HE, het.expected)
    }
  }
  FIN[g,3] <- mean(HE)
  FIN[g,4] <- mean(HO)
  
  #figure out how to find RRS, I think we need fecundity/indv LRS first
  
  #ages = (?)
  
  
}
#would it be helpful to know proportion of population that is a migrant?