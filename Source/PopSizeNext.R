#used for complex model for ABM class
#this will examine the population size and trend for the future generation. 
#this will also help in giving the number of offspring to generate from mating pairs

PopSizeNext = function(pop, k, r0){
  #calculate the current population size following aging and death
  Nt = nrow(pop)
  
  #calculate the new pop size with the logistic growth equation
  Ntt = Nt*(1+r0*(1-(Nt/k))) #logistic
  #r0 is the per capita growth rate, set as a parameter in Cover.R
  
  #add Density Independent variance in growth
  Nt1 = round(rnorm(1, Ntt, 1), 0)
  
  #determine the number of additional offspring to produce
  numboff = Nt1 - Nt
  
  return(numboff)
}
