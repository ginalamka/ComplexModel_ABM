#Generate new offspring in Lamka and Willoughby 2023

Breed = function(pop, pairs, numboff, k, sz, nSNP, nSNP.mig, broodsize, y, mu, mutate, nSNP.cons, pos1, pos2, rr, r, prj, grp, matemigs){
 
  #randomly narrow down pairings from pairs if n_pairs > numboff
  if(is.null(nrow(pairs))==TRUE){
    print(paste("no pairs of parents available"))
    break
    
  }else if(nrow(pairs)>= numboff){
    #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
    if(matemigs == 1){
      migparents = pairs[pairs[,3]==2,,drop=FALSE]           #grab parents with migrant ancestry
      nonmigrents = pairs[pairs[,3]!=2,,drop=FALSE]          #grab parents without migrants
      
      if(nrow(migparents) < numboff){                        #if there are more offspring needed, grab some nonmig parents
        pairings = sample(1:nrow(nonmigrents), numboff-nrow(migparents), replace = F, prob = NULL) #grab the additional parent pairs needed
        parents <- nonmigrents[pairings,,drop=FALSE]
        parents <- rbind(migparents,parents)                 #make parent matrix
        
      }else{
        parents <- migparents
        pairings=NULL
      }
      remove(migparents, nonmigrents)  #clean up
      
    }else{
      #if not preferentially mating migrants, randomly select pairs
      pairings = sample(1:nrow(pairs), numboff, replace = F, prob = NULL)
      parents <- pairs[pairings,,drop=FALSE]
    }
    
  }else{
    pairings = NULL
    parents <- pairs
  }
  
  #generate fecundity for each set of parents
  fecundity = sample(seq(1,broodsize,1),nrow(parents),replace=T, prob = NULL) 
  parents[,3] = fecundity       
  nbabes = sum(parents[,3])
  TEMP = NULL
  for(n in 1:nrow(parents)){
    t = parents[n, ,drop=FALSE] #need drop = false or else will lose 
    f = t[1,3]       #use this to store the number of offspring per parent set
    t[1,3] = 1       #use this as a check for later on to make sure this loop works
    while(f > 0){
      TEMP = rbind(TEMP, t)
      f = f - 1
      if(f==0){       #another check so it doesnt get stuck in this loop
        break
      }
    }
  }
  parents = TEMP
  remove(TEMP)
  
  #generate unique IDS
  SZ = seq(from = sz+1, to = sz + nrow(parents), by =1)
  
  babies = matrix(nrow=nrow(parents), ncol=12)         #make new matrix for offspring     
  colnames(babies) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness", "prop migrant SNPs")
  babies[,1] = SZ                                      #each individual has unique ID name; sequence starting at 1, through k, with each 1 iteration
  babies[,2] = parents[,1]                             #grab mom
  babies[,3] = parents[,2]                             #grab dad
  babies[,4] = 0                                       #give age  
  babies[,5] = sample(c(0,1),nrow(babies),replace=T)   #assign male (1) or female (0) 
  babies[,6] = NA                                      #number of times as a parent - calculated in RepSucc.R
  babies[,7] = NA                                      #number of offspring survive to maturity - calculated in RepSucc.R
  babies[,8] = 1                                       #make every baby alive
  babies[,9] = y                                       #generation born
  babies[,10] = 0                                      #generation died
  babies[,11] = NA                                     #relative fitness - calculated below
  babies[,12] = NA                                     #proportion of migrant SNPs - calculated below
  
  #create a check to make sure the correct number of babies are being added to pop
  #use bb to track number of added indvs for unique IDs
  if(nrow(babies) <= numboff){
    bb = nrow(babies)
    
  }else if(nrow(babies) > numboff){
    #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
    if(matemigs == 1){
      migbbys = babies[babies[,2]<=0|babies[,3]<=0,,drop=FALSE]    #grab babies with migrant parents
      
      #if only migrant offspring, select from them the numboff needed
      if(nrow(migbbys) > numboff){
        kept = sample(migbbys[,1], numboff, replace = FALSE, prob = NULL) #remove babies so that you generate only the number needed
        babies = babies[which(babies[,1]%in%kept), , drop=FALSE] 
        remove(kept, migbbys)
        rm=NULL
        
      }else{
        keep = migbbys[,1]
        if(length(keep)>=1){
          BABIES = babies[-which(babies[,1]%in%keep),,drop=FALSE]
        }else{
          BABIES = babies
        }
        rm = sample(BABIES[,1], nrow(babies)-numboff, replace = FALSE, prob = NULL) #remove babies so that you generate only the number needed
        BABIES = BABIES[-which(BABIES[,1]%in%rm), , drop=FALSE] 
        babies <- rbind(migbbys, BABIES)
        remove(keep, migbbys, BABIES)  #clean up
      }
      
    }else{
      #if not preferentially choosing migrant offspring, randomly select to have numboff needed
      rm = sample(babies[,1], nrow(babies)-numboff, replace = FALSE, prob = NULL) 
      babies = babies[-which(babies[,1]%in%rm), , drop=FALSE] 
    }
    bb = nrow(babies)
    remove(rm)
  }else if(is.null(nrow(babies))==TRUE){
      bb = 1
      print(paste("only one new baby"))
  }
  
  #rename babies so count doesnt get messed up
  babies =  as.matrix(babies)
  babies[,1] = seq(from = (sz+1), to = (sz+bb), by = 1)
  
  #prep parent genotypes
  f = babies[,2]
  m = babies[,3]
  
  fem = pop[-which(pop[,1]%NOTin%f), , drop = FALSE]
  mal = pop[-which(pop[,1]%NOTin%m), , drop = FALSE]
  
  if(nrow(mal) == 0){
    print(paste("can't generate father genotypes"))
    break
  }
  if(nrow(fem)==0){
    print(paste("can't generate mother genotypes"))
    break
  }
  
  SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)       #calculate total number of SNP loci
  
  babygeno = matrix(nrow=bb, ncol=SNPS)                #create genotype matrix
  #loop over each row in babies
  for(i in 1:nrow(babies)){
    mom = babies[i,2]
    dad = babies[i,3]
    
    mm = pop[pop[,1] == mom, , drop=FALSE]
    dd = pop[pop[,1] == dad, , drop=FALSE]
    
    momgeno = mm[, -c(ncol(mm)-(SNPS):ncol(mm))]       #grab mom's SNPs
    dadgeno = dd[, -c(ncol(dd)-(SNPS):ncol(dd))]       #grab dad's SNPs
    
    #from each snp (2 columns), grab 1 of mom's alleles
    momgeno.s = pos1 + (sample(0:1, length(pos1), replace=T)) #list of values to pull, exactly 1 allele (here it is index number) from each set of two columns
    momgeno.s = momgeno[momgeno.s] #these are now the actual alleles
    
    #from each snp (2 columns), grab 1 of dad's alleles
    dadgeno.s = pos1 + (sample(0:1, length(pos1), replace=T)) #list of values to pull, exactly 1 allele (here it is index number) from each set of two columns
    dadgeno.s = dadgeno[dadgeno.s] #these are now the actual alleles
    
    #interweave mom and dad's genotypes so that the loci are jumbled
    babygeno[i,pos1] = momgeno.s
    babygeno[i,pos2] = dadgeno.s
  }
  
  if(mutate == 1){  #if mutate is turned "on"
    drift <- babygeno[,1:(nSNP*2),drop=FALSE]                #grab drift SNPs
    miggeno <- babygeno[,(nSNP*2+1):SNPS,drop=FALSE]         #grab pop-specific SNPs
    
    #only allow mutation to happen on drift SNPs, not SNPs that are population-specific
    for(x in 1:nrow(drift)){    
      mut <- sample(c("Y","N"), nSNP*2, replace = TRUE, prob = c(mu,1-mu))   
      init <- drift[x,]                                      #keep track of the 'ancestral' state within this individual
      drift[x, which(mut=='Y' & drift[x,]==1)] <- 0
      #if a SNP is supposed to mutate, but its ancestral state was '1' (i.e., it's already been mutated in the previous line),
      #then set its index in mut to 'N', indicating that no further mutations should happen in this round.
      mut[which(mut=='Y' & init==1)] <- 'N'
      drift[x, which(mut=='Y' & drift[x,]==0)] <- 1
    }
    babygeno <- cbind(drift, miggeno)                        #combine drift and pop-specific SNPs
  }else{
    print(paste("no mutation"))
  }
  
  #calculate relative fitness (heterozygosity)
  het <- matrix(nrow=nrow(babygeno), ncol=1)
  for(g in 1:nrow(babygeno)){
    w <- sum(babygeno[g ,seq(1,ncol(babygeno),2)]!=babygeno[g,seq(2,ncol(babygeno),2)])/(ncol(babygeno)/2)
    het[g,1] <- w
  } 
  babies[,11] <- het
  #note that all SNPs are being considered here
  
  #calculate proportion of migrant SNPs
  migrantgen <- babygeno[, -c(ncol(babygeno)-(nSNP.mig*2):ncol(babygeno))]
  migrantgen <- matrix(unlist(migrantgen), nrow = bb, ncol = nSNP.mig*2)
  mSNP <- matrix(nrow = bb, ncol = 1)
  for(q in 1:nrow(migrantgen)){
    ww <- sum(migrantgen[q,])/ncol(migrantgen)
    mSNP[q,1] <- ww
  }
  babies[,12] <- mSNP
  
  #create matrix for the number of effective parents
  NE = matrix(nrow=1, ncol=12)
  colnames(NE) <- c("year", "eff_mom", "eff_dad", "nbabies", "naliveadults", "possible_mom", "possible_dad", "eff_mig", "parameterset", "replicate", "project", "group") 
  
  NE[1,1] = y                             #grab year
  NE[1,2] = length(unique(babies[,2]))    #grab n unique effective moms
  NE[1,3] = length(unique(babies[,3]))    #grab n unique effective dads
  NE[1,4] = nrow(babies)                  #grab n babies
  
  alive = pop[pop[,8]==1,,drop=FALSE]
  adult = alive[alive[,4]!=0,,drop=FALSE]
  adult_f = adult[adult[,5]==0,,drop=FALSE]
  adult_m = adult[adult[,5]==1,,drop=FALSE]
  NE[1,5] = nrow(adult)                   #grab n alive adults
  NE[1,6] = nrow(adult_f)                 #grab n possible moms
  NE[1,7] = nrow(adult_m)                 #grab n possible dads
  
  mig_f = babies[babies[,2]<=-2,,drop=FALSE]
  mig_m = babies[babies[,3]<=-2,,drop=FALSE]
  NE[1,8] = length(unique(mig_f[,2])) + length(unique(mig_m[,3]))    #grab number of migrant parents
  
  NE[1,9] = r
  NE[1,10] = rr
  NE[1,11] = prj
  NE[1,12] = grp
  
  if(isTRUE(y == 1 && r == 1 && rr == 1)){   #create new table for first run
    write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
  }else{                                     #append to the previously made table if not the first run
    write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=FALSE, append=TRUE, quote=FALSE, row.names=FALSE)
  }
  
  print(paste("there are", nrow(babies), "babies added to the pop"))
  
  babies = cbind(babies, babygeno)
  pop = rbind(pop, babies)
  
  remove(babies, babygeno, dd, fem, het, mal, migrantgen, mm, mSNP, pairs, parents, pairings, 
         t, dadgeno, dadgeno.s, f, fecundity, momgeno, momgeno.s, mut, nbabes, SZ, 
         NE, alive, adult, adult_f, adult_m, mig_f, mig_m)
  
  return(list(pop,bb))
}