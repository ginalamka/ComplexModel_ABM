#Mate
#used for complex model for ABM class

MateChoice = function(pop, sex){
  #find which sex has more, male or female.
  ck = mean(pop[,'sex']) #<0.5 female, >.5 male
  return(ck)
  if(ck == 1){
    print(paste("Only males left"))
    next
  }else if(ck == 0){
    print(paste("Only females left"))
    next
  }else{
    #REMOVED###turn "sex" into the value of sex with the fewest indv, 0=female, 1=male
    #REMOVED###sex <- c(0,1)[which.min(tabulate(match(pop[,'sex'], c(0,1))))]
    
    fem = 0 #this matches males to females by putting the first column always as females
    
    #match those of opposite sex with replacement*
    mates <- sample(pop[pop[,'sex'] != fem, 'id'], tabulate(match(pop[,'sex'], fem)), replace=TRUE) #this means indv can mate more than once
    #for now, replace = true since sometimes more males than females
    #note, != means NOT 
    
    #pair individuals
    pairs <- cbind(pop[pop[,'sex'] == fem, 'id'], mates)
    
    #randomize the pairs so all indv have chance of mating
    rand = sample(1:nrow(pairs),nrow(pairs))
    pairs <- pairs[rand, ]
    
    #pairs <- rand
    colnames(pairs) <- c('mom','dad')
    return(pairs)
  }
}
#NOTES FROM JANNA 10/19/2021
#try this flow: get all females that are mature, randomize them, then match them with males
#that will rearrange this whole function, but can be done -- SIMPLIFY
#the main thing is to randomize the pairs so that offspring generated are from a random pair
# https://stackoverflow.com/questions/9081498/randomly-re-order-shuffle-rows-of-a-matrix

#*note, because krats have a promiscuous system, allow replacement so multiple indv can mate more than once (monogamous would be set up different)
#as set up currently, only the sex with the most indv will be used more than once (i.e. not both sexes with multiple mating events)
#consider requiring that migrants mate, or dont and then can compare N and Ne(?)


##MAKE SURE TO KNOW IF MALE/FEMALE IS MATE 1or2**