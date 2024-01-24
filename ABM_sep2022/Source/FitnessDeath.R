#Fitness-Dependent death for Lamka and Willoughby 2023
#this will impose an increase in probability of death with decreasing heterozygosity

FitnessDeath = function(pop, maturity, y, purge, nSNP.cons){
  print(paste("nrow(pop)", nrow(pop), "sum(pop[,8]", sum(pop[,8])))
  dead = pop[pop[,8] == 0, , drop=FALSE]            #remove dead indvs
  print(paste("nrow(dead)", nrow(dead)))
  pop = pop[pop[,8] == 1, , drop=FALSE]             #isolate alive
  print(paste("nrow(pop)", nrow(pop)))
  #adults = pop[pop[,4] >= maturity, , drop=FALSE]   #grab adults
  #n_adults = nrow(adults)
  table(pop[,4])
  print(paste(table(pop[,4])))
  mature = pop[pop[,4] == maturity, , drop=FALSE]   #isolate the indv that just reached maturity
  print(paste("nrow(mature)", nrow(mature)))
  #influencing fitness-induced death once at the year of maturity helps control for fitness influences better than if compounded every year indv ages
  pop = pop[pop[,4] != maturity, , drop=FALSE]
  print(paste("nrow(pop)", nrow(pop)))
  if(nrow(mature)>1){
    for(ee in 1:nrow(mature)){
      het = mature[ee,11] +0.01                     #this controls for if het = 0
      mature[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/het/100,(1-(1/het/100))))
    }
    nkilled = NULL
    nkilled = mature[mature[,8]==0,,drop=FALSE]    #grab dead mature indvs
    mature = mature[mature[,8]!=0,,drop=FALSE]     #grab alive mature indvs
    nkilled[,18] = 2
    nkilled[,10] = y
    print(paste("killed", nrow(nkilled), "individuals"))
    
    }else{
      print(paste("not enough mature for fit-induced death"))
      nkilled = NULL
    }
  
  if(purge == 1){
    if(purge_mutants == 1){   #if want to grab only mutants
      mature = mature[mature[,16]!=0,,drop=FALSE]
      hold = mature[mature[,16]==0,,drop=FALSE]
      print(paste("there are", nrow(mature), "mature indv with mutations to purge thru"))
    }
    #note, this purges indv AT THE AGE OF MATURITY based on the number of deleterious recessive mutations in conserved SNPs
    #AgeDeath uses all mutations every year to evaluate chance of death
    if(nrow(mature)>1){
      for(aa in 1:nrow(mature)){
        del = mature[aa,17] + 1
        mature[aa,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(del/100,(1-(del/100))))
      }
      npurged = mature[mature[,8]==0,,drop=FALSE]   #grab purged indv
      mature = mature[mature[,8]==1,,drop=FALSE]    #grab mature alive indv
      npurged[,18] = 4
      npurged[,10] = y
    
      if(purge_mutants == 1){  #recombine subset datasets
        mature = rbind(mature, hold)
        remove(hold)
      }
    }else{print(paste("not enough mature for fit-induced purging"))
      if(purge_mutants == 1){  #recombine subset datasets
        mature = rbind(mature, hold)
        remove(hold)
      }
      npurged = NULL}
    
  }else{npurged = NULL}
  
  print(paste(nrow(pop), nrow(mature), nrow(npurged), nrow(nkilled), nrow(dead)))
  
  pop = rbind(pop, mature, npurged, nkilled, dead)  #combine pop, mature, and previously removed dead indv
  
  remove(dead, mature, npurged, nkilled)   #clean up
  
  return(pop)
}
