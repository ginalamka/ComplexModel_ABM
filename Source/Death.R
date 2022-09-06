#Age-Dependent death
#taking the place of DeathByAge and RandomDeath
#used for complex model for ABM class 2021

#this will impose an increase in probability of death wiht increasing age
Death = function(pop, maxage, ratemort, y){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  #alreadydead = pop[-which(pop[,1]%NOTin%dead),]
  #pop = pop[-which(pop[,1]%in%dead),]
  
  pop = pop[pop[,8] == 1, , drop=FALSE]                  #isolate alive
  
  #mort.rate = (maxage - pop[,4])*.25  #this makes chance of death at age 0, 0==== ##pop[,4]/maxage                        #calculate mortality rate (age/maxage)
  
  #find old individuals and mark as dead
  oldies = NULL
  oldies = (pop[pop[,4]>=maxage, , drop=FALSE])
  
  print(paste("there are", nrow(oldies), "oldies killed"))

  if(nrow(pop)>1){
    pop[pop[,1] %in% oldies,8]  = 0     #oldies become dead
    pop[pop[,1] %in% oldies,10] = y    #this is to put year died if I create that column
    oldies = pop[(pop[,8] == 0), , drop = FALSE]
    pop = pop[(pop[,8] == 1), , drop = FALSE]
    
    #kill some more individuals
    nkill = round((nrow(pop) * ratemort), 0) - nrow(oldies)
    if((nkill>0)){
      kill  = sample(1:length(pop[,1]), nkill, replace=FALSE) #this is for RANDOM death
      
      tpop  = sort(pop[,11], decreasing = TRUE)
      kill  = sample(1:length(pop[,1]), nkill, replace=FALSE, prob=pop[,11]) #this is for fitness-influenced death
      
      pop[kill,8]  = 0
      pop[kill,10] = y   #this is if I have a generation died column
      
      totalkilled = nkill + nrow(oldies)
      print(paste("killed", totalkilled, "individuals"))
      
    }else{
      print(paste("enough dead from age"))
    }
  }
  #combine pop and previously removed dead indv
  pop = rbind(pop, oldies, dead)
  
  #if(nrow(alreadydead) >= 1){
    #pop = rbind(pop,alreadydead)
  #}
  
  return(pop)
}


##DOUBLE CHECK THAT NOT LOSING INDVS
#RARELY, but still a thing, a few individuals die that shouldnt (i.e. they are not at max age and are killed with oldies but the number to kill is smaller than oldies, so that shouldnt happen). keep and eye on this and figure out why this occurs
