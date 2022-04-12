#Mate
#used for complex model for ABM class

MateChoice = function(pop, sex, maturity){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  pop = pop[-which(pop[,1]%in%dead), , drop=FALSE]
  
  immature  = pop[pop[,4] < maturity, ,drop=FALSE]          #remove immature indvs
  pop =       pop[pop[,4] >= maturity, ,drop=FALSE]         #pop without immature
  #since not returning pop, don't need to re-add pop and immature at end of function
  
  #find which sex has more, male or female.
  ck = mean(pop[,'sex']) #<0.5 female, >.5 male
  print(paste("the sex ratio is", ck))
  if(ck == 1){
    print(paste("Only males left"))
    return()   ##ERROR HERE BECAUSE BREAK/NEXT ARENT IN A LOOP (if is not a loop)
  }else if(ck == 0){
    print(paste("Only females left"))
    return() #break #next
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
    
    if(nrow(pairs) >= 3){
      #randomize the pairs so all indv have chance of mating
      rand = sample(1:nrow(pairs),nrow(pairs))
      pairs <- pairs[rand, ]
      
      #pairs <- rand
      colnames(pairs) <- c('mom','dad')
    }
    return(pairs)
  }
}
#Reminder: migrants are not preferentially chosen, so migrants =/= effective migrants