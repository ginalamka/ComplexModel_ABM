#Age-Dependent death for Lamka and Willoughby 2023
#this will impose an increased in probability of death with increasing age AND kill indv over the maxage

AgeDeath = function(pop, maxage, y){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  pop = pop[pop[,8] == 1, , drop=FALSE]           #isolate alive
  
  babes = pop[pop[,4] == 0, , drop=FALSE]         #remove babies from chance of dying
  pop = pop[pop[,4] != 0, , drop=FALSE]           #isolate those that are not newly generated babies
  

  if(nrow(pop) > 1){
    for(ee in 1:nrow(pop)){
        age = pop[ee,4] 
        pop[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(age/maxage,(1-(age/maxage))))

        if(pop[ee,8]==0){
          pop[ee,10] = y                          #change the year died to current year
        }
      }
      nkilled = NULL
      nkilled = pop[pop[,8]==0,,drop=FALSE]
      pop = pop[pop[,8]==1,,drop=FALSE]
      nkilled[,18] = 1
      totalkilled = nrow(nkilled)  
      print(paste("killed", totalkilled, "individuals"))
      
    }else{
      print(paste("no killing in AgeDeath"))
    }
  
  if(purge == 1){
    if(purge_mutants == 1){    #if only want to grab mutants
      pop = pop[pop[,16]!=0,,drop=FALSE]
      hold = pop[pop[,16]==0,,drop=FALSE]
    }
    if(nrow(pop)>1){
      
      for(aa in 1:nrow(pop)){
        nmut = pop[aa,16] + 1           #controls for if nmut = 0
        pop[aa,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(nmut/100,(1-(nmut/100))))
        #NOTE, this kills indv based on TOTAL number of Mutations in CONSERVED SNPs 
        #this is NOT number of homozygous deleterious mutations -- that occurs in FitnessDeath
      }
      npurged = pop[pop[,8]==0,,drop=FALSE]   #grab purged indv
      pop = pop[pop[,8]==1,,drop=FALSE]       #grab mature alive indv
      npurged[,18] = 3
      npurged[,10] = y
      
    }else{print(paste("not enough mutants for age-induced purging"))
      npurged = NULL}
    
    if(purge_mutants == 1){    #recombine subset datasets
      pop = rbind(pop, hold)
      remove(hold)
    }
  }
  
  #combine pop and previously removed dead indv
  pop = rbind(pop, babes, nkilled, npurged, dead)
  
  remove(babes, dead, nkilled, npurged, totalkilled, age)  #clean up
  
  return(pop)
}
