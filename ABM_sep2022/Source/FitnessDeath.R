#Fitness-Dependent death for Lamka and Willoughby 2023
#this will impose an increase in probability of death with decreasing heterozygosity

FitnessDeath = function(pop, maturity, y){
  dead = pop[pop[,8] == 0, , drop=FALSE]            #remove dead indvs
  
  pop = pop[pop[,8] == 1, , drop=FALSE]             #isolate alive
  
  adults = pop[pop[,4] >= maturity, , drop=FALSE]   #grab adults
  n_adults = nrow(adults)
  
  mature = pop[pop[,4] == maturity, , drop=FALSE]   #isolate the indv that just reached maturity
  #influencing fitness-induced death once at the year of maturity helps control for fitness influences better than if compounded every year indv ages
  pop = pop[pop[,4] != maturity, , drop=FALSE]
  
  if(nrow(mature)>1){
    for(ee in 1:nrow(mature)){
      het = mature[ee,11] +0.01                     #this controls for if het = 0
      mature[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/het/100,(1-(1/het/100))))
    }
     nkilled = NULL
     nkilled = mature[mature[,8]==0,,drop=FALSE]    #grab dead mature indvs
     print(paste("killed", nrow(nkilled), "individuals"))
      
    }else{
      print(paste("not enough mature for fit-induced death"))
      nkilled = NULL
    }
  
  pop = rbind(pop, mature, dead)  #combine pop, mature, and previously removed dead indv
  
  remove(dead, mature, nkilled)   #clean up
  
  return(pop)
}
