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

#source1 = source[-which(source[migrant,]),] #attempt to take out migrants from source pop >> currently not working


#in function
#return(list(n, pop)) #where n = number of indv added

#in runmodel.R
#temp = function(n, pop)

#n = unlist(temp)[1] #unlist the first object
#pop = unlist(temp)[[2]]
