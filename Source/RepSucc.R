#THIS IS THE SCECOND CODE FOR THIS FUNCTION, ReproSuc.R IS MY VERSION, THIS IS THE ONE SUGGESTED BY AVRIL

#to calculate the reproductive success in my pop extinction model going into spring 2022
#need: year born, year died, # times being a parent, amount of babies that survived to maturity
#flow: calc total number of times being a parent > calc how many offspring survive greater than 1 year > then in writeout figure out the rest??

#designated location for this will be in RunModel.R, after runing a replicate
#data object to use will be pop, which is the focal population after y years. contains indv-level data for all

RepSucc = function(pop, maturity){
  #calc the total number of offspring
  for(i in unique(pop[,1])){                          #iterate over id
    if(!is.null(nrow(pop[which(pop[,2] == i | pop[,3] == i),, drop = FALSE]))){
      temp = pop[which(pop[,2] == i | pop[,3] == i),, drop = FALSE]           #find if id is in mom OR dad column
      n = nrow(temp)
      pop[pop[,1] == i, 6] <- n                      #put the value in the noffspring column
    }else{
      next
    }

  }
  
  #calc the total number of adult offspring
  for(j in unique(pop[,1])){                          #iterate over id
    if(!is.null(nrow(pop[which(pop[,2] == j | pop[,3] == j & pop[,4] >= maturity),, drop=FALSE]))){
      tem = pop[which(pop[,2] == j | pop[,3] == j & pop[,4] >= maturity),, drop = FALSE]           #find if id is in mom OR dad column
      m = nrow(tem)
      pop[pop[,1] == j, 7] <- m                     #put the value in the noffspring column
    }else{
      next
    }
  }
  
  
  ########################################################################################################
  #set up for calculations
  
  
  #NOTES 3/29/22
  #first, calc the lifetime reproductive success, then subset by year (probs birth year or year reached maturity?) for yearly comparison
  #then will probably select years within each of the stages (+,-,stable pop size)
  
  REP = matrix(nrow=years, ncol=6)
  colnames(REP) = c("year", "popsize", "meanLRS", "SD", "LRSfemale", "LRSmale")
  
  #add year to summary matrix
  REP[,1] = c(1:nrow(REP))
  
  for(f in 1:nrow(REP)){
    year = REP[f,1]
  }
  
  for(q in unique(pop[,9])){       #unique generation born
    temp <- pop[pop[,9] ==q,]
    
    REP[f,2] = nrow(temp[,9]==q)
    
    
  }
  
  return(pop)

}




#main question: Does the reproductive success change over time?
  #by birth year? -- may be influenced by age classes
  #by if alive in given year? -- interaction with longevity (esp for long lived sp)


#NOTE: because I 1) breed, then 2)immediately age, then 3)death... that makes it so that all indv that are created, stay alive *at least until 1 year* 
##EXCEPT for babies created in the last year (becuse they havent been able to age yet)
##THEREFORE, columns 6 and 7 should be *nearly* identical - numboff in y ...**IF MATURITY = 1 **
###** this would be more different (probably) if maturity is a different value
###*MIGHT need to remove those that havent died anyways because those wouldnt be complete measures of repro success****


#Next steps: 
#should I calculate the reproductive success for migrants vs nonmigrants
#OR
#look at relative reproductive success of the pops before migration occurs?
#OR
#look at reproductive success as a function of number of migrants

#this might be hard because the number of migrants are set currently between 1-5 indv, so there is always migration happening
