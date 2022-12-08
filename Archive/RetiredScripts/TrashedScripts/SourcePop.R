#SourcePop.R for complex migration model for ABM class fall 2021

#for now, this will be here as a function, so that I have the script but may change it later
#potentially, this may be better placed within "RunModel.R" before/after initializing the focal pop

SourcePop = function(k, sex, maxage, allele){
 
  #initialize Source population
  source = matrix(nrow=k, ncol=7)            #each individual gets its own row.. matrix > dataframe
  colnames(source) <- c("id", "mom", "dad", "age", "sex", "allele1", "allele2") #just to give a better understanding of what these variables are, set names
  source[,1] = seq(-1000,-1,1)                     #each individual has unique ID name; sequence starting at -1, through -k, with each 1 interation, negative flag for source pop
  source[,2:3] = -1                           #at this point, we are putting all equal to negative 1 to flag from source pop, and we dont know parents/parents arent in focal pop
  source[,4] = sample(seq(0,maxage,1),k,replace=T)   #set age between 0 and 4 (source isnt aged, so dont subtract 1); consider if age 0 should be able to migrate
  source[,5] = sample(c(0,1),k,replace=T)    #each individual assigned male (1) or female (0) #sample from zero k times, with replacements. aka set sex
  source[,6] = sample(c(0,1),k,replace=T)    #set allele 1 as either A=1 or a=0
  source[,7] = sample(c(0,1),k,replace=T)    #set allele 2 as either A=1 or a=0
  
  return(source)
}

#consider if new alleles should be given here, and what the implications would be
#consider altering the allele frequencies at the source and focal populations 
#consider altering the population size of source population, think about why indv disperse 
  #i.e. if they are looking for resources vs more mates vs want less competition
#think about age and sex biases