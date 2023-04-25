#Age-related death for Complex Migration Model for ABM class Fall 2021

DeathByAge = function(pop, maxage){
  #create pop1 for dataset without indv over maxage (3)
  pop1 = pop[-which(pop[,4] >= maxage),]
  
  #rename to pop
  pop = pop1
  return(pop)
}