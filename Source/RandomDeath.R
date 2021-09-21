#Random Death
#random death for Complex Migration Model for ABM class Fall 2021 
## NOTE - this is currently RANDOM, but condider imposing a fitness/heterozygosity cost to imply natural selection
## consider implications of a no selection model and a "natural selection" model. 

RandomDeath = function(pop){
  #randomly select indv to die
  #syntax: sample(group, X, replace) with group being the column in the matrix, X being the number to kill per run
  #the number of indv to die will vary, so calibrate this later
  
  dead = sample(pop[,1], 10, replace = FALSE)
  pop1 = pop[-which(pop[,1]%in%dead),] 
  pop <- pop1
  
  return(pop)
}

#to do the opposite and randomly select those that live (if 98 out of 100 indv), do this:
#alive = sample(pop[,1], 98, replace = FALSE)

#note that instead of %in%, if only killing ONE indv, can use == ; but since we are killing a list of indv, must use %in% or %NOTin%