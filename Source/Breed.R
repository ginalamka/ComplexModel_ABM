#Breed
#for complex model for ABM class

##{THIS STILL DOES NOT RUN}
#rethink the steps to make this happen

#seems to be breaking because of returning numboff

Breed = function(pop, pairs, numboff, k){
  #consider if fecundity should be generated here or added as a column in pairs in MateChoice.R
  print(paste("the number of offspring STILL needed is", numboff))
  print(paste("numboff is STILL an", typeof(numboff)))
  
  #randomly select pairings from pairs so that there are double the number of pairs than offspring needed to be generated (since broodsize can be 0)
  pairings = sample(1:nrow(pairs), numboff*2, replace = F)
  parents <- pairs[pairings,]
  
  #generate fecundity for each set of parents
  fecundity = sample(seq(0,2,1),nrow(parents),replace=T)
  parents <- cbind(parents, fecundity)
  
  parents = parents[-which(parents[,3] == 0),] #consider if it makes sense having fecundity 0-2 or 1-2. remove this line if 0 is not possible
  
  nbabes = sum(parents[,3])
  #consider changing the matrix so that nrow = nbabes, not number of parents
  
  #currently this works so that each pair only makes 1 baby
  #figure this out so that we have varying fecundity and only some of the offspring survive
  
  newid = seq(from = (max(pop[,1])*10) +1, to = (max(pop[,1])*10) + nrow(parents), by = 1)
  
  babies = matrix(nrow=nrow(parents), ncol=7) #make new matrix for offspring     
  colnames(babies) <- c("id", "mom", "dad", "age", "sex", "allele1", "allele2")
  babies[,1] = newid                   #each individual has unique ID name; sequence starting at 1, through k, with each 1 iteration
  babies[,2] = parents[,1]
  babies[,3] = parents[,2]
  babies[,4] = 0    #first of the year - consider if these should be 0 or -1
  babies[,5] = sample(c(0,1),nrow(babies),replace=T)    #each individual assigned male (1) or female (0) #sample from zero nrow times, with replacements. aka set sex
  babies[,6] = sample(c(0,1),nrow(babies),replace=T)    #set allele 1 as either A=1 or a=0
  babies[,7] = sample(c(0,1),nrow(babies),replace=T)    #set allele 2 as either A=1 or a=0
  
  #create a check to make sure the correct number of babies are being added to pop
  if(nrow(babies) > numboff){
    rm = sample(babies[,1], nrow(babies)-numboff, replace = FALSE) #remove babies so that you generate only the number needed
    babies = babies[-which(babies[,1]%in%rm),] 
  } else if(numboff > nrow(babies)){
    print(paste("need more offspring generated"))
    next
  } else{
    #add babies to pop
    pop = rbind(pop, babies)
  }
  
  #think about adding in a "generation born" and "generation died" columns in pop
  #consider adding a column for calculating parents' lifetime reproductive success
  return(pop)
}



#BREEDING NOTES FROM JANNA 10/19/2021  
#next steps: make sure to randomize the order of the pairs table before breeding
#then generate fecundity for each pair down th list for a certain number of times to get past the number of offspring needed to reach K
#make sure to buffer by overshooting the number
#then before rturning offspring, drop some until the total number needed
#total offspring/2*fecundity
#check at end of function if below or above K
#make sure to break with an error message if too small
#return offspring and then rbind them to pop
