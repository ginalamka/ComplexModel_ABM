#Migrate
#used for complex migration model for ABM class

Migrate = function(pop, source){
  #select number of migrants, from 1-5
  mig = sample(c(1:5), 1, replace=T)
  
  print(paste("there are", mig, "migrants this year"))
  
  for(m in 1:mig){
    #select migrant without replacement
    migrant = sample(1:nrow(source), 1, replace = F)
    
    #take migrant from source and put into pop
    pop = rbind(pop, source[migrant,])
    #remove migrant from source
    source = source[-migrant,]
    } 
  return(list(pop, mig, source))
}

#NOTES 12/16/2021
#consider if migrants should have a generation born of 0, the year they migrated, or the year they migrated minus their age.
#think about the implications of each choice.

#see https://doi.org/10.1023/A:1025563107092 for info about how they decided how many migrants to add - constant, attraction, and avoidance

#source1 = source[-which(source[migrant,]),] #attempt to take out migrants from source pop >> currently not working


#in function
#return(list(n, pop)) #where n = number of indv added

#in runmodel.R
#temp = function(n, pop)

#n = unlist(temp)[1] #unlist the first object
#pop = unlist(temp)[[2]]
