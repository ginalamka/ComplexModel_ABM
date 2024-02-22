#Generate new offspring in Lamka and Willoughby 2023

Breed = function(pop, pairs, numboff, k, sz, nSNP, nSNP.mig, broodsize, y, mu, mutate, nSNP.cons, pos1, pos2, rr, r, prj, grp, matemigs, mu_co){
 
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
  
  babies = matrix(nrow=nrow(parents), ncol=19)         #make new matrix for offspring     
  colnames(babies) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness", "prop migrant SNPs", "mu_drift", "mu_pop", "mu_cons", "tot_mu_cons", "del_mu", "how dead", "longestROH")
  babies[,1] = SZ                                      #each individual has unique ID name; sequence starting at 1, through k, with each 1 iteration
  babies[,2] = parents[,1]                             #grab mom
  babies[,3] = parents[,2]                             #grab dad
  babies[,4] = 0                                       #give age  
  babies[,5] = sample(c(0,1),nrow(babies),replace=T)   #assign male (1) or female (0) 
  babies[,6] = NA                                      #number of times as a parent - calculated in RepSucc.R
  babies[,7] = NA                                      #number of offspring survive to maturity - calculated in RepSucc.R
  babies[,8] = 1                                       #make every baby alive
  babies[,9] = y                                       #generation born
  babies[,10] = NA                                     #generation died
  babies[,11] = NA                                     #relative fitness - calculated below
  babies[,12] = NA                                     #proportion of migrant SNPs - calculated below
  babies[,13] = 0                                      #number of added mutations in drift SNPs
  babies[,14] = 0                                      #number of added mutations in pop-specific SNPs
  babies[,15] = 0                                      #number of added mutations in conserved SNPs
  babies[,16] = NA                                     #total number of mutations in conserved SNPs
  babies[,17] = NA                                     #number of deleterious recessive mutations in conserved SNPs
  babies[,18] = NA                                     #died how? - 1 = age death, 2 = het death, 3 = total mut (age death), 4 = del mut (fit death)
  babies[,19] = NA                                     #length of longest ROH in nSNPs - calculated below
  
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
    drift <- babygeno[,1:(nSNP*2),drop=FALSE]                            #grab drift SNPs
    miggeno <- babygeno[,(nSNP*2+1):(SNPS-nSNP.cons*2),drop=FALSE]       #grab pop-specific SNPs
    congeno <- babygeno[,(nSNP*2+nSNP.mig*2+1):SNPS,drop=FALSE]          #grab conserved SNPs
    
    #only allow mutation to happen on drift SNPs, not SNPs that are population-specific
    for(x in 1:nrow(drift)){    
      mut <- sample(c("Y","N"), nSNP*2, replace = TRUE, prob = c(mu,1-mu)) 
      babies[x,13] = length(which(mut=="Y"))
      init <- drift[x,]                                      #keep track of the 'ancestral' state within this individual
      drift[x, which(mut=='Y' & drift[x,]==1)] <- 0
      #if a SNP is supposed to mutate, but its ancestral state was '1' (i.e., it's already been mutated in the previous line),
      #then set its index in mut to 'N', indicating that no further mutations should happen in this round.
      mut[which(mut=='Y' & init==1)] <- 'N'
      drift[x, which(mut=='Y' & drift[x,]==0)] <- 1
    }
    remove(x, mut, init)
    
    #for(x in 1:nrow(miggeno)){    
    #  mut <- sample(c("Y","N"), nSNP.mig*2, replace = TRUE, prob = c(mu_co,1-mu_co)) #note CODING mutation rate (U)
    #  babies[x,14] = length(which(mut=="Y"))
    #  init <- miggeno[x,]                                      #keep track of the 'ancestral' state within this individual
    #  miggeno[x, which(mut=='Y' & miggeno[x,]==1)] <- 0
    #  #if a SNP is supposed to mutate, but its ancestral state was '1' (i.e., it's already been mutated in the previous line),
    #  #then set its index in mut to 'N', indicating that no further mutations should happen in this round.
    #  mut[which(mut=='Y' & init==1)] <- 'N'
    #  miggeno[x, which(mut=='Y' & miggeno[x,]==0)] <- 1
    #}
    #remove(x, mut, init)
    
    for(x in 1:nrow(congeno)){    
      mut <- sample(c("Y","N"), nSNP.cons*2, replace = TRUE, prob = c(mu_co,1-mu_co)) #note CODING mutation rate (U)
      babies[x,15] = length(which(mut=="Y"))
      init <- congeno[x,]                                      #keep track of the 'ancestral' state within this individual
      congeno[x, which(mut=='Y' & congeno[x,]==1)] <- 0
      #if a SNP is supposed to mutate, but its ancestral state was '1' (i.e., it's already been mutated in the previous line),
      #then set its index in mut to 'N', indicating that no further mutations should happen in this round.
      mut[which(mut=='Y' & init==1)] <- 'N'
      congeno[x, which(mut=='Y' & congeno[x,]==0)] <- 1
    }
    remove(x, mut, init)
    
    babygeno <- cbind(drift, miggeno, congeno)                        #combine drift and pop-specific SNPs
  }else{
    print(paste("no mutation"))
  }
  
  #calculate total number of mutations at conserved SNPs
  totmut <- matrix(nrow=nrow(congeno), ncol=1)
  for(j in 1:nrow(congeno)){
    jj = sum(congeno[j,])
    totmut[j,1] <- jj
  }
  babies[,16] <- totmut
  
  #calculate the number of deleterious recessive mutations at conserved SNPs using 
  #a function to count the number of consecutive 1s in a sequence of two columns
  count_consecutive_ones <- function(row) {
    count <- 0
    for (i in seq(1, length(row), by = 2)) {
      if (i + 1 <= length(row) && row[i] == 1 & row[i + 1] == 1) {
        count <- count + 1
      }
    }
    return(count)
  }
  # Loop through rows of the congeno matrix
  for (row_index in 1:nrow(congeno)) {
    row <- congeno[row_index, ]
    consecutive_ones_count <- count_consecutive_ones(row)
    babies[row_index, 17] <- consecutive_ones_count
  }
  
  #calculate relative fitness (heterozygosity)
  het <- matrix(nrow=nrow(babygeno), ncol=1)
  for(g in 1:nrow(drift)){
    w <- sum(drift[g ,seq(1,ncol(drift),2)]!=drift[g,seq(2,ncol(drift),2)])/(ncol(drift)/2)
    het[g,1] <- w
  } 
  babies[,11] <- het
  #note that only nSNPs (drift SNPs) are being considered here
  
  #calculate ROHs for generated genotypes across nSNPs only
  bbyROH <- matrix(nrow=nrow(babygeno), ncol=1)
  for (row in 1:nrow(drift)) {
    current_run_length <- 0
    longest_run <- 0
    
    for (col in 1:(ncol(drift) - 1)) {
      if (drift[row, col] == drift[row, col + 1]) {
        # Columns are the same (homozygous)
        current_run_length <- current_run_length + 1
      } else {
        # Columns are different (heterozygous)
        if (current_run_length > longest_run) {
          longest_run <- current_run_length
        }
        current_run_length <- 0
      }
    }
    
    # Check for the last run
    if (current_run_length > longest_run) {
      longest_run <- current_run_length
    }
    
    # Store the result in the matrix
    bbyROH[row, ] <- longest_run
  }
  babies[,19] = bbyROH
  
  #calculate proportion of migrant SNPs
  #migrantgen <- babygeno[, -c(ncol(babygeno)-(nSNP.mig*2)-(nSNP.cons*2)+1:ncol(babygeno)-(nSNP.cons*2))]
  #migrantgen <- matrix(unlist(migrantgen), nrow = bb, ncol = nSNP.mig*2)
  mSNP <- matrix(nrow = bb, ncol = 1)
  for(q in 1:nrow(miggeno)){
    ww <- sum(miggeno[q,])/ncol(miggeno)
    mSNP[q,1] <- ww
  }
  babies[,12] <- mSNP
  
  # #create matrix for the number of effective parents
  # NE = matrix(nrow=1, ncol=12)
  # colnames(NE) <- c("year", "eff_mom", "eff_dad", "nbabies", "naliveadults", "possible_mom", "possible_dad", "eff_mig", "parameterset", "replicate", "project", "group") 
  # 
  # NE[1,1] = y                             #grab year
  # NE[1,2] = length(unique(babies[,2]))    #grab n unique effective moms
  # NE[1,3] = length(unique(babies[,3]))    #grab n unique effective dads
  # NE[1,4] = nrow(babies)                  #grab n babies
  # 
  # alive = pop[pop[,8]==1,,drop=FALSE]
  # adult = alive[alive[,4]!=0,,drop=FALSE]
  # adult_f = adult[adult[,5]==0,,drop=FALSE]
  # adult_m = adult[adult[,5]==1,,drop=FALSE]
  # NE[1,5] = nrow(adult)                   #grab n alive adults
  # NE[1,6] = nrow(adult_f)                 #grab n possible moms
  # NE[1,7] = nrow(adult_m)                 #grab n possible dads
  # 
  # mig_f = babies[babies[,2]<=-2,,drop=FALSE]
  # mig_m = babies[babies[,3]<=-2,,drop=FALSE]
  # NE[1,8] = length(unique(mig_f[,2])) + length(unique(mig_m[,3]))    #grab number of migrant parents
  # 
  # NE[1,9] = r
  # NE[1,10] = rr
  # NE[1,11] = prj
  # NE[1,12] = grp
  # 
  # if(isTRUE(y == 1 && r == 1 && rr == 1)){   #create new table for first run
  #   write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
  # }else{                                     #append to the previously made table if not the first run
  #   write.table(NE, paste(directory, "/Output/Ne_counts.csv", sep=""), sep=",", col.names=FALSE, append=TRUE, quote=FALSE, row.names=FALSE)
  # }
  
  print(paste("there are", nrow(babies), "babies added to the pop"))
  
  babies = cbind(babies, babygeno)
  pop = rbind(pop, babies)
  
  remove(babies, babygeno, dd, fem, het, mal, mm, mSNP, pairs, parents, pairings, 
         t, dadgeno, dadgeno.s, f, fecundity, momgeno, momgeno.s, nbabes, SZ)
         #NE, alive, adult, adult_f, adult_m, mig_f, mig_m, migrantgen)
  
  return(list(pop,bb))
}

#Notes 1/16/24 after talking with Janna
#mutate only in drift and cons SNPs. pop-spec SNPs will be used just to track migrant ancestry
#give two dif mutation rates- one for drift SNPs and one for conding SNPs (less freq)
#FIND CITATIONS FOR THESE RATES FIRST
#consider the fitness response to mutation
  #does number of mutation matter? or only if homozygous?
  #makes sense to affect at maturity but also as they age - FIND CITATIONS (if there are some)
  #death chances should be a RATE, not a cutoff value

#notes 1/15/24
#adding in recessive deleterious mutation in conserved SNPs -- consider LOF mut
#add mutation possible in pop-specific SNPs -- can this be considered "missense" mut
#if mutation in drift SNPs, consider this a silent mutation
#will need to purge indv with deleterious mutation in kill function
#consider if there is a reason to mutate SNPs seperately or altogether and then just find the 

#NOTE pop[,13:15] ONLY tells if THAT INDV was mutated, not how many mutations they hold
  #so should this be the number of 1s in conserved regions?
  #probs need to see how this runs before figuring out this question