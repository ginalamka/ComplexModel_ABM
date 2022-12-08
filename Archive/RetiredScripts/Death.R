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
      #kill  = sample(1:length(pop[,1]), nkill, replace=FALSE) #this is for RANDOM death
      
      #USE_OLD##kill  = sample(1:length(pop[,1]), nkill, replace=FALSE, prob=(1-pop[,11])) #this is for fitness-influenced death
      #pop[kill,8]  = 0
      #pop[kill,10] = y   #this is if I have a generation died column
      
      for(ee in 1:nrow(pop)){
        het = pop[ee,11] +0.000001 #this controls for if het =0
        pop[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/het/100,(1-(1/het/100))))
        
        #notes 9/16/22 - do this only at the year of maturity-- ONCE because the fitness effect that happens every year would be stronger if it compounded
        #if you have the maxhet change over time, the probability of dying would change each year so it would be hard to measure.
        # look for the realtionship of heterozyogisty to probability of death to see if there is stuff on there (might not be)
        #do oldies > fitness at maturity age > age-based death (age/lifespan) === DO NOT PUT A LIMIT ON THE NUMBER TO KILL
        #
      }
      nkilled = pop[pop[,8]==0,,drop=FALSE]
      totalkilled = nrow(nkilled) + nrow(oldies)
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
