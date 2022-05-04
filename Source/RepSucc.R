#THIS IS THE SCECOND CODE FOR THIS FUNCTION, ReproSuc.R IS MY VERSION, THIS IS THE ONE SUGGESTED BY AVRIL

#to calculate the reproductive success in my pop extinction model going into spring 2022
#need: year born, year died, # times being a parent, amount of babies that survived to maturity
#flow: calc total number of times being a parent > calc how many offspring survive greater than 1 year > then in writeout figure out the rest??

#designated location for this will be in RunModel.R, after runing a replicate
#data object to use will be pop, which is the focal population after y years. contains indv-level data for all

RepSucc = function(pop, maturity, years){
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
    if(!is.null(nrow(pop[which((pop[,2] == j | pop[,3] == j) & pop[,4] >= maturity),, drop=FALSE]))){
      tem = pop[which((pop[,2] == j | pop[,3] == j) & pop[,4] >= maturity),, drop = FALSE]           #find if id is in mom OR dad column
      m = nrow(tem)
      pop[pop[,1] == j, 7] <- m                     #put the value in the noffspring column
    }else{
      next
    }
  }
  
  #calc the relative fitness - defined as repro succ of the indv/max repro succ
  max <- max(pop[,7])
  for(m in unique(pop[,1])){
    rel <- pop[pop[,1] == m, 7]
    rel.fit <- rel/max
    
    pop[pop[,1] == m, 11] <- rel.fit
  }
  
  ########################################################################################################
  #set up for calculations
  
  
  #NOTES 3/29/22
  #first, calc the lifetime reproductive success, then subset by year (probs birth year or year reached maturity?) for yearly comparison
  #then will probably select years within each of the stages (+,-,stable pop size)
  
  REP = matrix(nrow=years+2, ncol=8) #to add source (-1) and init (0) pops
  colnames(REP) = c("YearBorn", "nBornThisYear", "meanLRS", "SD", "LRSfemale", "LRSmale", "meanRRS", "SDRRS")
  
  #add year to summary matrix
  REP[,1] = c(-1:(nrow(REP)-2))  #-1 to years cuz the initial pop has a generation born of 0 and initial source has a gen born of -1
  
  #don't think I need this cuz of the previous line
  #for(f in 1:nrow(REP)){
  #  year = REP[f,1]
  #}
  
  #calc number of indv born in this generation
  for(q in unique(pop[,9])){       #unique generation born
    temp <- pop[pop[,9] ==q,,drop=FALSE]
    
    REP[(q+2),2] = nrow(temp)   #nrow(temp[,9]==q)
  }
  
  #calc the mean LRS for this generation
  for(e in unique(pop[,9])){       #unique generation born
    te <- pop[pop[,9] ==e,,drop=FALSE]
    
    REP[(e+2),3] = mean(te[,7])   #find mean number of adult offspring
    #e+1 because year 0 is included
    
    REP[(e+2),4] = sd(te[,7])     #find standard deviation in number of adult offspring
    
    #calc mean LRS for males and females
    females <- te[te[,5]==0,,drop=FALSE] #0=female
    frs = mean(females[,7])
    REP[(e+2),5] = frs
    
    males <- te[te[,5] ==1,,drop=FALSE] #1=male
    mrs = mean(males[,7])
    REP[(e+2),6] = mrs
    
    #calc mean relative fitness
    REP[(e+2),7] = mean(te[,11])  #find mean relative fitness
    
    REP[(e+2),8] = sd(te[,7])     #find standard deviation in relative fitness
  }
  
  #note that for year 0, it includes pop founders and -1 includes source pop (i.e., migrants)
  #remember that the last few years will have odd values because they havent lived a lifetime yet
  
  #note that it is possible that some of the columns will be NA if there were no offspring produced that year

  
  return(list(pop, REP)) #need to return pop cuz calc repro success. need to return REP cuz want those for Plot.R

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
