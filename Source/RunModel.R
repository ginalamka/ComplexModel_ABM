#RunModel.R for complex migration model for ABM class fall 2021

RunModel = function(parameters, r, directory){
  k             = parameters$k[r]
  allele        = parameters$allele[r]
  nSNP          = parameters$nSNP[r]
  nMicro        = parameters$nMicro[r]
  sex           = parameters$sex[r]
  maxage        = parameters$maxage[r]
  broodsize     = parameters$broodsize[r]
  sexratio      = parameters$sexratio[r]
  maturity      = parameters$maturity[r]
  years         = parameters$years[r]
  
  #initialize population
  pop = matrix(nrow=k, ncol=7)            #each individual gets its own row.. matrix > dataframe
  colnames(pop) <- c("id", "mom", "dad", "age", "sex", "allele1", "allele2") #just to give a better understanding of what these variables are, set names
  pop[,1] = seq(1,k,1)                    #each individual has unique ID name; sequence starting at 1, through k, with each 1 interation
  pop[,2:3] = 0                            #at this point, we are putting all equal to zero because this is the initial generation and we dont know parents
  #pop[,2] = rep(0,k)                      #mom id - later will not be 0, this is useful for debugging #saying replicate 0 100 times
  #pop[,3] = rep(0,k)                      #dad id - later will not be 0, this is useful for debugging
  pop[,4] = sample(seq(0,maxage,1),k,replace=T)-1   #set age between 0 and 4 and subtract 1 because we add one at the first generation
  pop[,5] = sample(c(0,1),k,replace=T)    #each individual assigned male (1) or female (0) #sample from zero k times, with replacements. aka set sex
  pop[,6] = sample(c(0,1),k,replace=T)    #set allele 1 as either A=1 or a=0
  pop[,7] = sample(c(0,1),k,replace=T)    #set allele 2 as either A=1 or a=0

  #create for loop for each time step
  for(y in 1:years){
    pop = AgeUp(pop)
    pop = DeathByAge(pop, maxage)
    #dead = sample(seq(1,nrow(pop),1),1,replace=F) #take a sample of the sequence 1-all the rows in matrix "pop", take out 1 individual and do not replace it
    #pop = pop[-dead,] #now re-create "pop" with this change
    return (pop)
  }
  write.table(pop, paste(directory, "/Output/testRunModel.txt", sep=""), sep="/t", col.names=F, row.names=F)
}  