#used for complex model for ABM class
#this will examine the population size and trend for the future generation. 
#this will also help in giving the number of offspring to generate from mating pairs

PopSizeNext = function(pop, k, r0, maturity){
  dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
  alive = pop[-which(pop[,1]%in%dead), , drop = FALSE]
  #since only returning numboff, dont need to rbind dead and alive into pop
  #pop = pop[pop[,4] >= maturity, , drop=FALSE]          #isolate adults -- if only want effective pop size to count here
  #if only using adults, remove immature
  
  #calculate the current population size following aging and death
  ###REMOVED# Nt = nrow(pop) #use this if removing dead from pop
  Nt = nrow(alive)
  
  #calculate the new pop size with the logistic growth equation
  Ntt = Nt*(1+r0*(1-(Nt/k))) #logistic
  #r0 is the per capita growth rate, set as a parameter in Cover.R
  
  #add Density Independent variance in growth
  Nt1 = round(rnorm(1, Ntt, 1), 0)
  Nt1 = as.integer(Nt1)
  print(paste("The next generation's population size will be", Nt1))
  
  #determine the number of additional offspring to produce
  numboff = Nt1 - Nt
  
  #for some reason numboff is a double and causing problems
  #turn numboff into an integer
  numboff = as.integer(numboff)
  print(paste("the number of offspring needed is", numboff))
  
  return(numboff)
}

