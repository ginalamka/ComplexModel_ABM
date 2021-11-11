#Age-Dependent death
#taking the place of DeathByAge and RandomDeath
#used for complex model for ABM class 2021

#this will impose an increase in probability of death wiht increasing age
Death = function(pop, maxage, ratemort){
  dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
  pop = pop[pop[,8] == 1, , drop=FALSE]                  #isolate alive
  
  mort.rate = pop[,4]/maxage                        #calculate mortality rate (age/maxage)
  
  #find old individuals and mark as dead
  oldies = NULL
  oldies = try(pop[pop[,4]>=maxage, 1])

  if(nrow(pop)>1){
    pop[pop[,1] %in% oldies,8]  = 0     #oldies become dead
    #pop[pop[,1] %in% oldies,10] = y    #this is to put year died if I create that column
    
    #kill some more individuals
    nkill = round((nrow(pop) * ratemort), 0) - length(oldies)
    if(any(nkill>0)){
      kill  = sample(1:length(pop[,1]), nkill, replace=FALSE, mort.rate)
      pop[kill,8]  = 0
      #pop[kill,10] = g   #this is if I have a generation died column
    }
  }else{
    print(paste("no death cuz all dead"))
  }
  nowalive = nrow(pop) - sum(pop[,8])
  print(paste("killed", nowalive, "individuals"))
  
  #combine pop and previously removed dead indv
  pop = rbind(pop, dead)
  
  return(pop)
}
  
#notes 11/8
#problem with pop crashing after 6 years is because I am killing too much
#therefore, instead of random death, put in a age-related increase of chances of death
#maybe will want to make sure that only adults are mating, not babies -- important for age at maturity -- make temp pop to make sure that only adults are in pop
#age/lifespan for all individyuals to get this age/random mortality
#could do a loop or could use the apply function
#apply = give data, apply this function over all rows or all columns. can use apply then sample from 0-1
#make sure that the values spit it out corrrectly, i.e. kills 30% not 70$ based on incorrect putting in probabilities

#for the column alive v dead, make sure that the functions taht need pop get a temp pop for only adults (mate choice, breeding) but hten make sure to add the indv (breed) to the main pop, not temp pop