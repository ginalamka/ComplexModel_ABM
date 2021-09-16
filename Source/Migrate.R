#Migrate
#used for complex migration model for ABM class

Migrate = function(pop, source){
  #select number of migrants, from 1-5
  mig = sample(c(1:5), 1, replace=T)
  
  for(m in 1:mig){
    #select migrant without replacement
    migrant = sample(1:nrow(source), mig, replace = F)
    
    #take migrant from source and put into pop
    pop = rbind(pop, source[migrant,])
  } 
  return(pop)
}
