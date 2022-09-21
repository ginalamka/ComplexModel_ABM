#RunModel.R for complex migration model for ABM class fall 2021

RunModel = function(parameters, r, directory, replicates){
  FINAL = NULL
  REP   = NULL 
  rr=1  #remove this when not skipping through the below line
  for(rr in 1:replicates){
    k             = parameters$k[r]
    #REMOVED###allele        = parameters$allele[r]
    nSNP          = parameters$nSNP[r]
    nMicro        = parameters$nMicro[r]
    #REMOVED###sex           = parameters$sex[r]
    maxage        = parameters$maxage[r]
    broodsize     = parameters$broodsize[r]
    #REMOVED###sexratio      = parameters$sexratio[r]
    maturity      = parameters$maturity[r]
    years         = parameters$years[r]
    r0            = parameters$r0[r]
    ratemort      = parameters$ratemort[r]
    nSNP.mig      = parameters$nSNP.mig[r]                   #number of special alleles for migrants -- these are ADDITIONAL alleles, migrants = 1, orig pop = 0, this will be easier to track than a random value
    nSNP.cons     = parameters$nSNP.cons[r]                  #number of conserved alleles
    
    #initialize population
    pop = matrix(nrow=k, ncol=11)            #each individual gets its own row.. matrix > dataframe -- "ncol = 7 + (nloci)*2
    colnames(pop) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness") #just to give a better understanding of what these variables are, set names
    pop[,1] = seq(1,k,1)                    #each individual has unique ID name; sequence starting at 1, through k, with each 1 interation
    pop[,2:3] = 0                            #at this point, we are putting all equal to zero because this is the initial generation and we dont know parents
    #pop[,2] = rep(0,k)                      #mom id - later will not be 0, this is useful for debugging #saying replicate 0 100 times
    #pop[,3] = rep(0,k)                      #dad id - later will not be 0, this is useful for debugging
    pop[,4] = rpois(k,maturity)-1  ##sample(seq(0,maxage,1),k,replace=T)-1   #set age between 0 and 4 and subtract 1 because we add one at the first generation #FOR UNIFORM DIST: dunif(k, min =0, max = maturity, log = FALSE)-1 
    pop[,5] = sample(c(0,1),k,replace=T)    #each individual assigned male (1) or female (0) #sample from zero k times, with replacements. aka set sex
    pop[,6] = NA #this will be for number of times as a parent  #REMOVED###sample(c(0,1),k,replace=T)    #set allele 1 as either A=1 or a=0
    pop[,7] = NA #this will be for number of times offspring survive to maturity   #REMOVED###sample(c(0,1),k,replace=T)    #set allele 2 as either A=1 or a=0
    pop[,8] = 1                             #alive or dead? alive = 1, dead = 0
    pop[,9] = 0                             #generation born
    pop[,10] = 0                            #generation died
    pop[,11] = NA                            #relative fitness #at this point, we are putting all equal to zero because this is the initial generation
    sz = k #to keep track of the number of indv for ID'ing later
    
    #generate SNPs for the starting pop -- taken from Janna's Captive breeding IBM
    popgen = matrix(nrow=k, ncol=nSNP*2)
    columns = seq(1,(nSNP*2),2)
    for(l in 1:nSNP){
      p = sample(seq(from=0.4, to=0.6, by=0.01),1) ##0.5 #sample(seq(from=0, to=1, by=0.01), 1)
      #create pool of genotypes in HWE
      pool = c(rep(0, round(k*p*p, 0)),                                      #homozygous p*p
               rep(1, round(k*(1-p)*(1-p), 0)),                              #homozygous (1-p)*(1-p)  
               rep(2, k-(round(k*p*p, 0)+(round(k*(1-p)*(1-p), 0))))         #heterozygous
               )
      gtype = sample(pool, k, replace = FALSE)
      for(kk in 1:k){
        if(gtype[kk]==0){                 #homo (0,0)
          popgen[kk,columns[l]]   = 0
          popgen[kk,columns[l]+1] = 0
          next
        }else if(gtype[kk]==1){           #hetero (0,1)
          popgen[kk,columns[l]]   = 0
          popgen[kk,columns[l]+1] = 1
        }else{                            #homo (1,1)
          popgen[kk,columns[l]]   = 1
          popgen[kk,columns[l]+1] = 1
        }
      }
      #colnames(popgen) <- c('SNP', l)
      pool = NULL
      
      #add genotypes to pop matrix
    }
    
    het <- matrix(nrow=nrow(popgen), ncol=1)
    for(g in 1:nrow(popgen)){
      w <- sum(popgen[g ,seq(1,ncol(popgen),2)]!=popgen[g,seq(2,ncol(popgen),2)])/(ncol(popgen)/2)
      het[g,1] <- w
    } #note to add the other SNPs in here if wanted
    pop[,11] <- het
    
    #REMOVE4EVOLUTION###create migrant and nonmigrant unique SNPs 
    #REMOVE4EVOLUTION##popSNPs = matrix(nrow=k, ncol=nSNP.mig*2)
    #REMOVE4EVOLUTION##columnsb = seq(1,(nSNP.mig*2),2)
    #REMOVE4EVOLUTION##for(b in 1:nrow(popSNPs)){    #set up similar to above in case change the sequence or format later
    #REMOVE4EVOLUTION##  popSNPs[b,] = 0
    #REMOVE4EVOLUTION##}
    
    #REMOVE4EVOLUTION###create conserved SNPs    
    #REMOVE4EVOLUTION##conSNPs = matrix(nrow=k, ncol=nSNP.cons*2)
    #REMOVE4EVOLUTION##columnsc = seq(1,(nSNP.cons*2),2)
    #REMOVE4EVOLUTION##for(c in 1:nrow(conSNPs)){    #set up similar to above in case change the sequence or format later
    #REMOVE4EVOLUTION##  conSNPs[c,] = 0
    #REMOVE4EVOLUTION##}
    
    #REMOVE4EVOLUTION##focalpop <- cbind(pop, popgen, popSNPs, conSNPs)   ##??not sure why, but not binding correctly???
    focalpop <- cbind(pop, popgen)
    pop <- focalpop
    
    #calculate heterozygosity for each indv, put it in pop table
    
    
    #write starting pop to table
    ####REMOVED### write.table(pop, paste(directory, "/Output/focal_population", r, ".csv", sep=""), sep=",", col.names=T, row.names=F)
    
    #clean up
    #REMOVE4EVOLUTION##remove(popgen, popSNPs, conSNPs) 
    
    #notes from talking with Janna 10/21 -- doesnt quite work yet
    #plan is to add in additional SNPs to track genotypes. this will help set up Breed.R
    #f = 0.2 #allele freq, means 20% of the time, allele 0, 80% of the time, allele 1
    #x = sample(c(0,1), 2(nrow(pop)), freq = c(f,1-f))
    #pop[,i] = x[1:nrow(pop)]
    #pop[,i+1] = x[nrow(pop)+1:length(x)]
    #for(i in 1:10){
    #  seq(1,nSNP*2,2)
    #  }
    

    #make sure to add in additional SNPs for Source pop also!!
    
    #options for SNPs = can do 0-2 values with 1 column per SNP OR 2 columns per SNP with 0-1 
    #implications of each decision is based on calculating heterozygosity vs generating offspring
    
    #initialize source population 
    source = matrix(nrow=s, ncol=11)            #each individual gets its own row.. matrix > dataframe
    colnames(source) <- c("id", "mom", "dad", "age", "sex", "n offspring", "n adult offspring", "alive", "gen born", "gen died", "relative fitness") #just to give a better understanding of what these variables are, set names
    source[,1] = seq(-(s),-1,1)                     #each individual has unique ID name; sequence starting at -1, through -k, with each 1 interation, negative flag for source pop
    source[,2:3] = -1                           #at this point, we are putting all equal to negative 1 to flag from source pop, and we dont know parents/parents arent in focal pop
    source[,4] = sample(seq(0,maxage,1),s,replace=T)   #set age between 0 and 4 (source isnt aged, so dont subtract 1); consider if age 0 should be able to migrate
    source[,5] = sample(c(0,1),s,replace=T)    #each individual assigned male (1) or female (0) #sample from zero k times, with replacements. aka set sex
    source[,6] = NA #this will be for number of times as a parent   #REMOVED##sample(c(0,1),k,replace=T)    #set allele 1 as either A=1 or a=0
    source[,7] = NA #for number of offspring that reach maturity ##REMOVED#### sample(c(0,1),k,replace=T)    #set allele 2 as either A=1 or a=0
    source[,8] = 1                             #alive or dead? alive = 1, dead = 0
    source[,9] = -1                            #generation born
    source[,10] = 0                            #generation died
    source[,11] = NA                            #relative fitness
    
    #generate source gentoypes
    sourcegen = matrix(nrow=s, ncol=nSNP*2)
    columns = seq(1,(nSNP*2),2)
    for(l in 1:nSNP){
      p = sample(seq(from=0.4, to=0.6, by=0.01), 1)
      #create pool of genotypes in HWE
      pool = c(rep(0, round(s*p*p, 0)),                                      #homozygous p*p
               rep(1, round(s*(1-p)*(1-p), 0)),                              #homozygous (1-p)*(1-p)  
               rep(2, s-(round(s*p*p, 0)+(round(s*(1-p)*(1-p), 0))))         #heterozygous
      )
      gtype = sample(pool, s, replace = FALSE)
      for(ss in 1:s){
        if(gtype[ss]==0){                 #homo (0,0)
          sourcegen[ss,columns[l]]   = 0
          sourcegen[ss,columns[l]+1] = 0
          next
        }else if(gtype[ss]==1){           #hetero (0,1)
          sourcegen[ss,columns[l]]   = 0
          sourcegen[ss,columns[l]+1] = 1
        }else{                            #homo (1,1)
          sourcegen[ss,columns[l]]   = 1
          sourcegen[ss,columns[l]+1] = 1
        }
      }
      #colnames(sourcegen) <- c('SNP', l)
      #pool = NULL
      
      #add genotypes to source matrix
      
    }
    
    sourcehet <- matrix(nrow=nrow(sourcegen), ncol=1)
    for(j in 1:nrow(sourcegen)){
      z <- sum(sourcegen[j ,seq(1,ncol(sourcegen),2)]!=sourcegen[j,seq(2,ncol(sourcegen),2)])/(ncol(sourcegen)/2)
      sourcehet[j,1] <- z
    } #note to add the other SNPs in here if wanted
    source[,11] <- sourcehet
    
    #REMOVE4EVOLUTION###create migrant and nonmigrant unique SNPs
    #REMOVE4EVOLUTION##migSNPs = matrix(nrow=s, ncol=nSNP.mig*2)
    #REMOVE4EVOLUTION##columnsd= seq(1,(nSNP.mig*2),2)
    #REMOVE4EVOLUTION##for(d in 1:nrow(migSNPs)){    #set up similar to above in case change the sequence or format later
    #REMOVE4EVOLUTION##  migSNPs[d,] = 1
    #REMOVE4EVOLUTION##}
    
    #REMOVE4EVOLUTION###create conserved SNPs    
    #REMOVE4EVOLUTION##conSNPs = matrix(nrow=s, ncol=nSNP.cons*2)
    #REMOVE4EVOLUTION##columnse = seq(1,(nSNP.cons*2),2)
    #REMOVE4EVOLUTION##for(e in 1:nrow(conSNPs)){    #set up similar to above in case change the sequence or format later
    #REMOVE4EVOLUTION##  conSNPs[e,] = 0
    #REMOVE4EVOLUTION##}
    
    #REMOVE4EVOLUTION##source1 <- cbind(source, sourcegen, migSNPs, conSNPs)        #also doesnt work????
    source1 <- cbind(source, sourcegen)
    source <- source1
    
    #write starting source to table
    #### REMOVED### write.table(source, paste(directory, "/Output/source", r, ".csv", sep=""), sep=",", col.names=T, row.names=F)
    
    #REMOVE4EVOLUTION###clean up
    #REMOVE4EVOLUTION##remove(sourcegen, pool, migSNPs, l, c, d, kk, ss)
    #REMOVE4EVOLUTION##remove(gtype, columns, columnsb, columnsc, columnsd, columnse)
    
    #create for loop for each time step
    for(y in 0:years){
      if(y != 0){
        pop = AgeUp(pop)                        #age pop + 1 year
        pop = FitnessDeath(pop, maturity, ratemort, y)                #kill indv
        #pop = DeathByAge(pop, maxage)           #age-dependent mortality
        if(nrow(pop) <= 10){
          print(paste("Population low, less than 10 indv"))
          break
        }
        #REMOVE##tttt = Stochastic(pop, stoch, k, numboff, styr, endyr, nwk, dur, y, years, r0, parameters, r)
        #REMOVE##pop = tttt[[1]]
        #REMOVE##k = tttt[[2]]
        
        #REMEMBER THE NEED TO TRACK NEW K !!!
        
        if(nrow(pop) <= 10){
          print(paste("Population low, less than 10 indv"))
          break
        }
        #pop = RandomDeath(pop)                  #random mortality
        tt = Migrate(pop, source)             #subpop migration
        pop = tt[[1]]
        mig = tt[[2]]  #0
        sz = sz + mig #may need to edit since dead are not being removed from pop
        source = tt[[3]]
        if(nrow(pop) <= 4){
          print(paste("Population crash @ MateChoice, less than 4 indv"))
          break
        }
        pairs = MateChoice(pop, sex, maturity)  
        if(is.null(pairs)==TRUE){
          print(paste("skipping pop size next, breed due to no parents"))
          break  #consider whether this should be next or break
        }
        #REMOVED## if(sum(pairs[,1]) < 0 | sum(pairs[,2]) < 0){
        #REMOVED##   print(paste("Only migrants available as parents"))
        #REMOVED##   break
        #REMOVED## }
        numboff = PopSizeNext(pop, k, r0, maturity) #ADD NEW K MODIFIER
        if(numboff <= 1){
          print(paste("No new babies, skip breed"))
          next
        }
        ttt = Breed(pop, pairs, numboff, k, sz, nSNP, nSNP.mig, broodsize, y, mu, mutate, nSNP.cons) #still needs work 
        pop = ttt[[1]]
        bb = ttt[[2]]
        sz = sz + bb
        
        pop = AgeDeath(pop, maxage, ratemort, y)                #kill indv based on age
        
        
        print(paste("DONE!", y, "rep", rr))
        
        #REMOVED###pop <- pop[pop[,8]==1,, drop=FALSE] #remove dead indv -- put in place for Evolution on 6/7/22
        
        ###REMOVED### write.table(pop, paste(directory, "/Output/testRunModel" , y, ".csv", sep=""), sep=",", col.names=T, row.names=F)
        ###REMOVED### return (pop)
        
        #y <- y+1
      }
      
      #analyze each replicate
      out = Analyze(parameters, r, pop, mig, focalpop, source1, y, init.het, rr, nSNP, nSNP.mig, nSNP.cons, numboff)
      #out[1,1] = y
      #out[1,ncol(out)+1] = rr
      FINAL = rbind(FINAL, out[1,])
      init.het <- FINAL[1,5]
      
      #consider if something needs to be changed in Analyze for the different death types or if that needs tracked at all.
      
    }
    
    #THIS IS WHERE I CALC RRS using pop data
    aa = RepSucc(pop, maturity, years, rr, r)
    POP = aa[[1]]  #this is the final pop with all indv and all indv data
    rep = aa[[2]]
    REP = rbind(REP, rep)
    
    #still need to figure out how to analyze this. probs will want per year in FINAL, but unsure how to do that yet.
    #otherwise may need to move this up to calc per year, but that would greatly increase computational time
    
    print(paste("REPLICATE", rr, "DONE!"))
    
  } 
  return(list(FINAL, POP, REP))
}

#next will go to the next loop AKA the next year
#break will completely stop the loop AKA the next replicate

#add in checks with breaks -- this is especially important going through replicates
#for example, check that we have 1 male and 1 female before pairing
