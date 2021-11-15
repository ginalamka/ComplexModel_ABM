#Age-Dependent death
#taking the place of DeathByAge and RandomDeath
#used for complex model for ABM class 2021

#this will impose an increase in probability of death wiht increasing age
Death = function(pop, maxage, ratemort){
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
    #pop[pop[,1] %in% oldies,10] = y    #this is to put year died if I create that column
    oldies = pop[(pop[,8] == 0), , drop = FALSE]
    pop = pop[(pop[,8] == 1), , drop = FALSE]
    
    #kill some more individuals
    nkill = round((nrow(pop) * ratemort), 0) - nrow(oldies)
    if((nkill>0)){
      kill  = sample(1:length(pop[,1]), nkill, replace=FALSE)
      pop[kill,8]  = 0
      #pop[kill,10] = g   #this is if I have a generation died column
      
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
  
#notes 11/8
#problem with pop crashing after 6 years is because I am killing too much
#therefore, instead of random death, put in a age-related increase of chances of death
#maybe will want to make sure that only adults are mating, not babies -- important for age at maturity -- make temp pop to make sure that only adults are in pop
#age/lifespan for all individyuals to get this age/random mortality
#could do a loop or could use the apply function
#apply = give data, apply this function over all rows or all columns. can use apply then sample from 0-1
#make sure that the values spit it out corrrectly, i.e. kills 30% not 70$ based on incorrect putting in probabilities

#for the column alive v dead, make sure that the functions taht need pop get a temp pop for only adults (mate choice, breeding) but hten make sure to add the indv (breed) to the main pop, not temp pop

#fake = matrix(nrow=5, ncol = 5)
#fake[,1] = c(1:5)
#fake[,2] = c(-2:2)
#m = fake[fake[,2] <=0, , drop=FALSE]
#m
#if(fake[,1]%in%m){
#  fake[,3] = 1
#}else{
#  fake[,3] = 0
#}


##DOUBLE CHECK THAT NOT LOSING INDVS
#RARELY, but still a thing, a few individuals die that shouldnt (i.e. they are not at max age and are killed with oldies but the number to kill is smaller than oldies, so that shouldnt happen). keep and eye on this and figure out why this occurs
