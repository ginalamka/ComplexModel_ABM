#Breed
#for complex model for ABM class

##{THIS STILL DOES NOT RUN}
#rethink the steps to make this happen

Breed = function(pop, pairs, years, numboff, k){
  #randomize pairs so that all have an opportunity to make babies
  
  
  
  
  
  #use for loop to iterate over pairs (?)
  
  #use sample function to randomly select fecundity of each pair
  breedingpairs = matrix(nrow=nrow(pairs), ncol=7)            #make new matrix for breeding pairs
  colnames(breedingpairs) <- c("id", "mom", "dad", "age", "sex", "allele1", "allele2") #name columns
  breedingpairs[,1] = seq(years*1000,years*1000+nrow,1)         #get new numerical value for individuals in this generation ?? - maybe add generation column? 
  
  pairs <- pairs + pairs[,3]
  pop = rbind(pop, source[migrant,])
  pairs[,3] <- sample(c(0:2), pairs, replace=T)
  
  #add the number of offspring decided by fecundity
  
  #add column for parents for reproductive success
  
  #put mom and dad id in matrix
  
  #give added offspring an ID number, sex, and allele freq
  
  
  
}
#think about adding in a "generation born" and "generation died" columns in pop

"
BREEDING NOTES FROM JANNA 10/19/2021  
next steps: make sure to randomize the order of the pairs table before breeding
then generate fecundity for each pair down th list for a certain number of times to get past the number of offspring needed to reach K
make sure to buffer by overshooting the number
then before rturning offspring, drop some until the total number needed
total offspring/2*fecundity
check at end of function if below or above K
make sure to break with an error message if too small
return offspring and then rbind them to pop
""