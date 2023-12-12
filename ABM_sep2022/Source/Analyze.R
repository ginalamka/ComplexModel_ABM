#Analyze data for Lamka and Willoughby 2023

Analyze = function(parameters, r, pop, mig, fstinit, fstsource, y, rr, nSNP, nSNP.mig, nSNP.cons, numboff, K, pos1, pos2, prj, grp){  #should this be parameters or replicates?
  #get variables from parameter table
  k             = parameters$k[r]
  nSNP          = parameters$nSNP[r]
  miggy         = parameters$miggy[r]
  LBhet         = parameters$LBhet[r]
  LBp           = parameters$LBp[r]
  maxage        = parameters$maxage[r]
  broodsize     = parameters$broodsize[r]
  maturity      = parameters$maturity[r]
  years         = parameters$years[r]
  r0            = parameters$r0[r]
  nSNP.mig      = parameters$nSNP.mig[r] 
  nSNP.cons     = parameters$nSNP.cons[r] 
  
  #check for indv ID numbers
  if(!length(pop[,1])==length(unique(pop[,1]))){    #notice that ! means NOT
    print(r)
    print("NON UNIQUE ID NUMBERS")
    return()
  }
  #if no indv, exit
  if(length(pop[,1])==0){return()}
  
  #separate out alive indv
  alive = pop[pop[,8]==1, , drop=FALSE]
  
  #calculate summary stats for final pop
  FIN = matrix(nrow=years+1, ncol=20)
  colnames(FIN) = c("year", "popsize", "propmig", "He", "Ho", "Fis", "nadults", "sxratio", "nmig", "Fst", "replicate", "parameterset", "numboff", "FstVSource", "FisVSource", "deltaK", "propMigSNPs", "Ho_allSNPs", "project", "group")
  
  f = 1
  FIN[f,1] <- y       #grab year
  
  FIN[f,16] <- K      #grab maximum population size (aka carrying capacity)
  
  #separate out alive in current year
  data = alive[alive[,8]>0, , drop = FALSE]
  
  x=NULL
  x = try(length(data[,1]), silent=TRUE)
  if(is.null(x)){break}
  if(x<1){break}
  if(!is.numeric(x)){break}
  
  #number of indv
  FIN[f,2] = nrow(data)
  
  #proportion migrants in population
  FIN[f,3] =  sum(data[,2]==-1)/length(data[,1])  
  
  #proportion of migrant genotypes in population
  FIN[f,17] <- mean(data[,12])
  
  #He and Ho - neutral 
  SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)       #add up all the SNP types
  genotype = data[, -c(ncol(data)-SNPS:ncol(data))]    #grab the genotypes
  
  HE = NULL
  HO = NULL
  
  loc.pos = seq(1, SNPS, 2)
  for(lp in loc.pos){
    #per locus heterozygostiy
    locus <- genotype[, c(lp, lp+1), drop=FALSE]
    #NOTE this includes MigSNPS while the other het value does not
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
  FIN[f,4] <- mean(HE)            #expected heterozygosity
  FIN[f,5] <- mean(data[,11])     #observed hererozygosity (only neutral SNPs)
  FIN[f,18] <- mean(HO)           #observed heterozygosity (all SNPs)
  
  #find number of adults per year
  adults = data[data[,4]>= maturity, , drop = FALSE]
  FIN[f,7] = nrow(adults)
  
  #find the sex ratio
  FIN[f,8] = mean(data[,'sex']) #<0.5 female, >.5 male
  #note this includes babies
  
  #note number of migrants that moved this year (not number of total migrants in the population)
  if(y != 0){
    FIN[f,9] = mig  #generated in Migrate.R, passed to RunModel.R, and passed here in Analyze.R
  }else{
    FIN[f,9] = 0  #this is for year 0, there are no migrants in this pop
  }
  
  #prepare the genotypes for hierfstat
  SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)                    #find number of SNPs
  fstdata <- data[, -c(ncol(data)-(SNPS):ncol(data))]               #grab SNPs
  
  #change 0s to 2s for hierfstat to read
  fstdata[fstdata[,]==0] <-2
  
  #merge pos1 and pos2 into pos1, then remove pos2
  fstdata[,pos1] <- as.numeric(paste(fstdata[,pos1], fstdata[,pos2], sep=""))
  fstdata <- fstdata[,-c(pos2)]
  
  #add pop identifier for calculations
  popident <- matrix(nrow=nrow(fstdata), ncol=1)
  popident[,1] = y
  fstdata <- cbind(popident,fstdata)
  
  if(y != 0){
    fstnow <- rbind(fstdata, fstinit)                        #bind the genotypes from this year with the initialized focal pop for comparison
    fstnow <- as.data.frame(fstnow)                          #turn into a dataframe
    calc <-wc(fstnow, diploid=TRUE, pol=0)                   #calc FST and FIS

    FIN[f,10] <- calc$FST
    FIN[f,6] <- calc$FIS
    
    fstdatavsource <- rbind(fstdata, fstsource)              #merge the genotypes from current year and initialized source pop to one matrix for calculations
    fstdatavsource <- as.data.frame(fstdatavsource)          #turn into a dataframe
    calcvsource <-wc(fstdatavsource, diploid=TRUE, pol=0)    #calc FST and FIS
    FIN[f,14] <- calcvsource$FST
  }
  if(y == 0){
    fstyo <- rbind(fstinit, fstsource)                       #merge current year and initialized year to one matrix for calculations
    fstyo <- as.data.frame(fstyo)                            #turn into a dataframe
    calcyo <-wc(fstyo, diploid=TRUE, pol=0)                  #calc FST and FIS
    
    FIN[f,10] <- 0      #no divergence at y=0
    FIN[f,6] <- 0       #no inbreeding at y=0
    FIN[f,14] <- calcyo$FST
    FIN[f,15] <- calcyo$FIS
    
    remove(fstyo)       #clean up
  }
  
  FIN[f,11] = rr        #add replicate number
  FIN[f,12] = r         #add parameter set number
  
  if(y == 0){
    numboff = 0
  }
  FIN[f,13] = numboff   #add number of offspring created
  
  FIN[f,19] = prj       #add project name
  
  FIN[f,20] = grp       #add group name
  
  params = parameters[rep(r, nrow(FIN)),]  #add parameter table
  out = cbind(FIN,params)
  colnames(out) = c("year", "popsize", "propmig", "He", "Ho", "Fis", "nadults", "sxratio", "nmig", "Fst", "replicate", "parameterset", "numboff", "FstVSource", "FisVSource", "deltaK", "propMigSNPs", "Ho_allSNPs", "project", "group",
                    "k", "nSNP", "miggy", "LBhet", "LBp", "maxage", "broodsize", "maturity", "years", "r0", "nSNP.mig", "nSNP.cons")
  
  remove(alive, adults, data, FIN, fstdata, genotype, locus, params, popident,
         freqs, geno, HE, het, het.expected, het.observed, HO, homozygous, loc.pos)  #clean up
  
  return(out)
}
