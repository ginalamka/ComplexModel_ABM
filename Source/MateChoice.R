#Mate
#used for complex model for ABM class

MateChoice = function(pop, sex){
  #find which sex has more, male or female.
  mean(pop[,'sex']) #<0.5 female, >.5 male
  
  #turn "sex" into the value of sex with the fewest indv, 0=female, 1=male
  sex <- c(0,1)[which.min(tabulate(match(pop[,'sex'], c(0,1))))]
  
  #randomly select indv of the most sex with replacement*
  mates <- sample(pop[pop[,'sex'] != sex, 'id'], tabulate(match(pop[,'sex'], sex)), replace=FALSE) #this means indv can only mate once
  
  #pair individuals
  pairs <- cbind(pop[pop[,'sex'] == sex, 'id'], mates)
  
  #randomize the pairs so all indv have chance of mating
  rand = sample(1:nrow(pairs),nrow(pairs))
  pairs[rand, ]
  
  pairs <- rand
  colnames(pairs) <- c('mate1','mate2')
  return(pairs)
}
#NOTES FROM JANNA 10/19/2021
#try this flow: get all females that are mature, randomize them, then match them with males
#that will rearrange this whole function, but can be done -- SIMPLIFY
#the main thing is to randomize the pairs so that offspring generated are from a random pair

#*note, because krats have a promiscuous system, allow replacement so multiple indv can mate more than once (monogamous would be set up different)
#as set up currently, only the sex with the most indv will be used more than once (i.e. not both sexes with multiple mating events)
#consider requiring that migrants mate, or dont and then can compare N and Ne(?)


##MAKE SURE TO KNOW IF MALE/FEMALE IS MATE 1or2**