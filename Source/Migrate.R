#Migrate
#used for complex migration model for ABM class

Migrate = function(pop, source){
  #select number of migrants, from 1-5
#  N = sum(pop[,8])  #gets census size
#  Nmig = round(N*.05)  #5% of N will be new migrants == vary this later == perhaps as a parameter???
#  Vmig = round(N*.01)  #adds 1% variance +/- Nmig 
#  mig = sample(c((Nmig-Vmig):(Nmig+Vmig)), 1, replace=T) #randomly sample within the variance of Nmig the number of migrants that year
  #NOTE-- TO IMPLEMENT THIS, will need to change the number of indv in the source pop, probs around 10k. this will take time tho so I will do this later
  #remember that H DROPS significantly when there is only 1-5 migrants per year when K stays around 1000. this is an argument AGAINST the 1 mig per generation rule
  
  mig = sample(c(1:15), 1, replace=T) #OLD
  
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

#things to remember from Allendorf's book, pg 209: the larger the demes, the slower they are diverging through drift; 
  #therefore proportionally fewer migrants needed to counteract drift

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
