#Assigning mates for Lamka and Willoughby 2023
#Reminder: if migrants are not preferentially chosen, migrants =/= effective migrants
#includes on/off switches for Allee effects and preferentially choosing migrant parents to mate

MateChoice = function(pop, sex, maturity, allee, matemigs){
  dead = pop[pop[,8] == 0, , drop=FALSE]                    #remove dead indvs
  pop = pop[which(pop[,1]%NOTin%dead), , drop=FALSE]        #grab alive indvs
  
  immature  = pop[pop[,4] < maturity, ,drop=FALSE]          #remove immature indvs
  pop =       pop[pop[,4] >= maturity, ,drop=FALSE]         #pop without immature
  #note, since not returning pop, don't need to re-add pop and immature at end of function
  
  #find which sex has more, male or female. if only one sex avail to breed, leave function
  ck = mean(pop[,'sex']) #<0.5 female, >0.5 male
  print(paste("the sex ratio is", ck))
  if(ck == 1){
    print(paste("Only males left"))
    return()  
  }else if(ck == 0){
    print(paste("Only females left"))
    return() 
  }else{
    #REMOVED###turn "sex" into the value of sex with the fewest indv, 0=female, 1=male
    #REMOVED###sex <- c(0,1)[which.min(tabulate(match(pop[,'sex'], c(0,1))))]
    
    fem = 0 #this matches males to females by putting the first column always as females
    
    #match those of opposite sex with replacement*
    #first, grab males so that there are the same number of mates as there are females 
    mates <- sample(pop[pop[,'sex'] != fem, 'id'], tabulate(match(pop[,'sex'], fem)), replace=TRUE) #this means indv can mate more than once
    
    #pair individuals - females with males so n_pairs = n_females
    pairs <- cbind(pop[pop[,'sex'] == fem, 'id'], mates)
    
    print(paste("there are", nrow(pairs), "pairs"))
    
    #if allee effect switch is turned on (1 = on, 0 = off)
    if(allee == 1){
      al = nrow(pairs)
      matedpairs <- cbind(pairs, rep(1, al))
      for(lee in 1:nrow(matedpairs)){
        
        #if preferentially mating migrants switch is turned on (1 = on, 0 = off)
        if(matemigs == 1){
          #grab pairs with migrant parents
          mom = matedpairs[lee,1]
          dad = matedpairs[lee,2]
          
          if(mom >= 1 & dad >= 1){
            #find random chance of mates finding each other so that as Nc ^, chance of interacting ^
            matedpairs[lee,3] = base::sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/al,(1-(1/al)))) 
          }else{
            #give migrant parents unique identifier
            matedpairs[lee,3] = 2
          }
          remove(mom, dad)     #clean up
        
          }else{
          #find random chance of mates finding each other so that as Nc ^, chance of interacting ^ -- happens for all indv if matemigs switch is turned off
          matedpairs[lee,3] = base::sample(x=c(0,1), size = 1, replace = TRUE, prob = c(1/al,(1-(1/al))))
        }
      }
      
      keptpairs = matedpairs[matedpairs[,3]>=1,,drop=FALSE]   #grab pairs not removed by allee effect
      pairs = keptpairs  #keptpairs[,c(1:2)]  <- use this if don't want migrant identifier
      
      remove(lee, matedpairs, keptpairs)     #clean up
    }else{
      al = nrow(pairs)
      pairs <- cbind(pairs, rep(1, al))
    }
    
    if(nrow(pairs) >= 3){
      #randomize the pairs
      rand = sample(1:nrow(pairs),nrow(pairs))
      pairs <- pairs[rand, ]
      
      remove(rand)      #clean up
    }
    colnames(pairs) <- c('mom','dad','migident')
    
    remove(dead, immature, mates)       #clean up
    
    return(pairs)
  }
}