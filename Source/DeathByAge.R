#Age-related death for Complex Migration Model for ABM class Fall 2021

DeathByAge = function(pop, maxage){
  pop1 = pop[-which(pop[,4] >= maxage),]
  pop = pop1
  return(pop)
}
