#Breed
#for complex model for ABM class

Breed = function(pop, pairs, years){
  #use for loop to iterate over pairs (?)
  
  #use sample function to randomly select fecundity of each pair
  breedingpairs = matrix(nrow=nrow(pairs), ncol=7)            #each individual gets its own row.. matrix > dataframe
  colnames(breedingpairs) <- c("id", "mom", "dad", "age", "sex", "allele1", "allele2") #just to give a better understanding of what these variables are, set names
  breedingpairs[,1] = seq(years*1000,years*1000+nrow,1)            
  
  pairs <- pairs + pairs[,3]
  pop = rbind(pop, source[migrant,])
  pairs[,3] <- sample(c(0:2), pairs, replace=T)
  
  #add the number of offspring decided by fecundity
  
  #add column for parents for reproductive success
  
  #put mom and dad id in matrix
  
  #give added offspring an ID number, sex, and allele freq
  
  
  
}
#think about adding in a "generation born" and "generation died" column in pop