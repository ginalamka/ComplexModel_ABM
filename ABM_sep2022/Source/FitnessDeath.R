#Fitness-Dependent death
#taking the place of Death
#used for complex model for ABM class 2021

#this will impose an increase in probability of death wiht decreasing heterozygosity
FitnessDeath = function(pop, maturity, y){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  pop = pop[pop[,8] == 1, , drop=FALSE]           #isolate alive
  
  mature = pop[pop[,4] == maturity, , drop=FALSE] #isolate the indv that just reached maturity
  #influcing fitness-induced death at maturity helps control for fitness influences better than if compounded every year
  pop = pop[pop[,4] != maturity, , drop=FALSE]
  
  if(nrow(mature)>1){
    for(ee in 1:nrow(mature)){
      
      het = mature[ee,11] +0.01 #this controls for if het =0
      mature[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/het/100,(1-(1/het/100))))
        
        #notes 9/16/22 - do this only at the year of maturity-- ONCE because the fitness effect that happens every year would be stronger if it compounded
        #if you have the maxhet change over time, the probability of dying would change each year so it would be hard to measure.
        # look for the realtionship of heterozyogisty to probability of death to see if there is stuff on there (might not be)
        #do oldies > fitness at maturity age > age-based death (age/lifespan) === DO NOT PUT A LIMIT ON THE NUMBER TO KILL
        #
    }
     nkilled = NULL
     nkilled = mature[mature[,8]==0,,drop=FALSE]
     print(paste("killed", nrow(nkilled), "individuals"))
      
    }else{
      print(paste("not enough mature for fit-induced death"))
      nkilled = NULL
    }


  #combine pop and previously removed dead indv
  pop = rbind(pop, mature, dead)
  
  #if(nrow(alreadydead) >= 1){
    #pop = rbind(pop,alreadydead)
  #}
  
  remove(dead, mature, nkilled)
  
  return(pop)
}


##DOUBLE CHECK THAT NOT LOSING INDVS
#RARELY, but still a thing, a few individuals die that shouldnt (i.e. they are not at max age and are killed with oldies but the number to kill is smaller than oldies, so that shouldnt happen). keep and eye on this and figure out why this occurs
