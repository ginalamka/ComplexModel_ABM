#Age-Dependent death
#taking the place of DeathByAge and RandomDeath
#used for complex model for ABM class 2021

#this will impose an increase in probability of death wiht increasing age
Death = function(pop, maxage){
  if(pop[,4] >= maxage){
    pop[,8] = 0
  }else{
    print(paste("no death"))
  }
  dead = pop[pop[,8] == 0, , drop=FALSE]
}


#notes 11/8
#problem with pop crashing after 6 years is because I am killing too much
#therefore, instead of random death, put in a age-related increase of chances of death
#maybe will want to make sure that only adults are mating, not babies -- important for age at maturity -- make temp pop to make sure that only adults are in pop
#age/lifespan for all individyuals to get this age/random mortality
#could do a loop or could use the apply function
#apply = give data, apply this function over all rows or all columns. can use apply then sample from 0-1
#make sure that the values spit it out corrrectly, i.e. kills 30% not 70$ based on incorrect putting in probabilities

#for the column alive v dead, make sure that the functions taht need pop get a temp pop for only adults (mate choice, breeding) but hten make sure to add the indv (breed) to the main pop, not temp pop