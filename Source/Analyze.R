#Analyze.R
#used for complex model for ABM class fall 2021

#taken from Janna's captive breeding IBM, function WriteOut

Analyze = function(parameters, r, pop, mig, focalpop, source1, y, init.het){  #should this be parameters or replicates?
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
  nSNP.cons     = parameters$nSNP.cons[r]                  #number of conserved alleles
  
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
  FIN = matrix(nrow=years+1, ncol=10)
  colnames(FIN) = c("year", "popsize", "propmig", "He", "Ho", "meanRRS", "nadults", "sxratio", "nmig", "Fst")
  #note that because this is for all years of the simulation, the initialized pop is not included in this (e.g., year 0)
  
  #add year to summary matrix
  #FIN[,1] = c(0:nrow(FIN))
  
  f = 1
  #for(f in 1:nrow(FIN)){
    #year = FIN[f,1] #-1 #doing this cuz also taking year 0 -- note that on 5/2/22 there were still errors with numbering on column 1
    FIN[f,1] <- y
  
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
    SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)
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
    
    ########################################NEW####################################3
    #find the sex ratio
    FIN[f,8] = mean(data[,'sex']) #<0.5 female, >.5 male
    #note this includes babies
    
    #note number of migrants
    if(y != 0){
      FIN[f,9] = mig  #generated in Migrate.R, passed to RunModel.R, and passed here in Analyze.R
    }else{
      FIN[f,9] = 0  #this is for year 0, there are no migrants in this pop
    }
    
    
    #calc the proportion of migrant alleles in the population
    #do I want heterozygosity of mig alleles? or freq of mig alleles? or something different?
    #?
    #?
    #?
    
    #table function https://www.datasciencemadesimple.com/table-function-in-r/#:~:text=Table%20function%20in%20R%20-table%20%28%29%2C%20performs%20categorical,creating%20Frequency%20tables%20with%20condition%20and%20cross%20tabulations.
    
    #calc Fst
    #Fst for this population compared to the initialized pop (aka focalpop)
    #https://bios1140.github.io/understanding-fst-the-fixation-index.html
    #mark: https://www.molecularecologist.com/2012/05/14/calculating-pair-wise-unbiased-fst-with-r/   AND     https://www.molecularecologist.com/wp-content/uploads/2012/05/Pairwise-WeirCockerhams-FST.r1.txt
    
    #Fst for this pop compared to the source pop (aka source1)
    
    #DOUBLE CHECK THESE EQUATIONS
    #https://www.uwyo.edu/dbmcd/popecol/maylects/fst.html
    #I *think* I should be averaging between subpops for Hs but I'm not 100% sure
    
    #Fst = (expected heterozy of total pop - expected heterozygosity of subpop) / expected heterozy of total pop
    #if heterozy of focal pop is hetero in year 0 
    #and subpop is the hetero for year y
    #can't I use the previous calculations for Fst?
    
    #Fis = (expected heterozygosity of subpop - observed heterozygosity of indv) / expected hetero of subpop
    
    #LINKS FOR CALC FST USING HIERFSTAT
    #https://rdrr.io/cran/hierfstat/
    #https://rdrr.io/cran/hierfstat/#vignettes
    #https://rdrr.io/cran/hierfstat/man/biall2dos.html
    #https://rdrr.io/cran/hierfstat/man/fs.dosage.html
    #https://rdrr.io/cran/hierfstat/man/fstat2dos.html
    #https://rdrr.io/cran/hierfstat/man/nb.alleles.html
    #https://rdrr.io/cran/hierfstat/man/pairwise.neifst.html
    #https://rdrr.io/cran/hierfstat/man/pairwise.WCfst.html
    #https://rdrr.io/cran/hierfstat/man/ppfst.html
    
    library(hierfstat)
    
    
    if(y == 0){
      
      #first, get hetero values for the **source pop** to compare to the **initialized focal pop**
      sSNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)
      sgenotype = source1[, -c(ncol(source1)-sSNPS:ncol(source1))]
      ssnps = rep(c(1,2),ncol(sgenotype)/2)
      
      sHE = NULL
      sHO = NULL
      
      sloc.pos = seq(1, sSNPS, 2)
      for(slp in sloc.pos){
        #per locus heterozygostiy
        slocus <- sgenotype[, c(slp, slp+1), drop=FALSE]
        sgeno  <- length(slocus[,1])
        shet   <- length(which(slocus[,1] != slocus[,2]))
        shet.observed <- shet/sgeno
        sHO = c(sHO, shet.observed)
        
        sfreqs <- table(slocus)
        shomozygous = NULL
        for(sv in 1:length(sfreqs)){
          shomozygous = c(shomozygous, (sfreqs[sv]/sum(sfreqs)*sfreqs[sv]/sum(sfreqs)))
        }
        shet.expected <- 1 - sum(shomozygous)
        sHE = c(sHE, shet.expected)
      }
      
      obs.source <- mean(sHO)
      obs.foc <- mean(HO)
      mean.OH <- (obs.source + obs.foc)/2
      
      fst <- (mean.OH - obs.foc) / mean.OH
      
      FIN[f,10] = fst  
    }else{
      
      #calculate the Fst between **pop in year y** and the **initialized focal pop**
      obs.init <- init.het #from year 0
      obs.pop <- FIN[f,5]  #from year y
      
      mean.obs <- (obs.init + obs.pop)/2
      FST <- (mean.obs - obs.pop) / mean.obs
      
      FIN[f,10] = FST
      
    }
    
    
    #Fis for this pop
    
    #NOTES 4/18/22 -- consider another pairwise measure tha tmay work rather than FST. potentially the number of unique alleles??
    #this could be because FST is not sensitive to minor changes because the scale is in evolutionary time
    #may also want to add the number of variable SNPS (across indvs) since heterozy is within an indv
    
    #ALSO add from meeting 4/18/22
    #year 0 should be in this table >> look at the photo I took to see how to do this. in short, will want to ski[ to analyze in year 0 and then go through all timesteps]
    #that way, year 0 will be in the same tbale and iwll make it easier to analyze for following years
    #ALSO will want to add SNPs that are the SAME between pop and source so that we can seperate mutation, migration, and drift forces. think about this more.

    
  #}
  
  params = parameters[rep(r, nrow(FIN)),]
  out = cbind(FIN,params)
  colnames(out) = c("year", "popsize", "propmig", "He", "Ho", "meanRRS", "nadults", "sxratio", "nmig", "Fst",
                    "k", "nSNP", "maxage", "broodsize", "maturity", "years", "r0", "ratemort", "nSNP.mig", "nSNP.cons")
  
  return(out)
}

#additional things that I should make figs of that will need to be added to Analyze.R
  #prop migrant alleles in pop
  #number of males and females that bred in that year (may be able to replace Ne)
  #number of mates (use the function "table"; data$habitat.mate (or could try "apply")) >> TABLE THIS FOR NOW, PROB WONT NEED OR WANT THIS
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


#attempted and removed 5/3/2022
#library(GeneClusterNet)

#gtypes1 <- focalpop[,11:ncol(focalpop)] #focalpop is initialized pop
#gtypes2 <- source1[,11:ncol(source1)]   #source1 is the initialized source
#gtypes3 <- data[,11:ncol(data)]         #data are all current alive indv

#if(!(ncol(gtypes1) == ncol(gtypes2) && ncol(gtypes1) == ncol(gtypes3))){
#  print("GENOTYPE ERROR")
#  return()
#}
#u <- ncol(gtypes1) + 1
#gtypes1 <- cbind(c(1),gtypes1)
#gtypes2 <- cbind(c(2),gtypes2)
#gtypes3 <- cbind(c(3),gtypes3)

#all.gtypes <- rbind(gtypes1, gtypes2, gtypes3)
#fst <- fst(all.gtypes, all.gtypes[,1], 3)