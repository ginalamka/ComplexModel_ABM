#used to calculate the reproductive success in Lamka and Willoughby 2023

RepSucc = function(pop_indv, maturity, years, rr, r, prj, grp){
  #calc the total number of offspring
  for(i in unique(pop_indv[,1])){                          #iterate over id
    if(!is.null(nrow(pop_indv[which(pop_indv[,2] == i | pop_indv[,3] == i),, drop = FALSE]))){
      temp = pop_indv[which(pop_indv[,2] == i | pop_indv[,3] == i),, drop = FALSE]           #find if id is in mom OR dad column
      n = nrow(temp)
      pop_indv[pop_indv[,1] == i, 6] <- n                  #put the value in the noffspring column
    }else{
      next
    }
  }
  
  #calc the total number of adult offspring
  for(j in unique(pop_indv[,1])){                          #iterate over id
    if(!is.null(nrow(pop_indv[which((pop_indv[,2] == j | pop_indv[,3] == j) & pop_indv[,4] >= maturity),, drop=FALSE]))){
      tem = pop_indv[which((pop_indv[,2] == j | pop_indv[,3] == j) & pop_indv[,4] >= maturity),, drop = FALSE]           #find if id is in mom OR dad column
      m = nrow(tem)
      pop_indv[pop_indv[,1] == j, 7] <- m                  #put the value in the noffspring column
    }else{
      next
    }
  }
  
  #calc the relative fitness - defined as repro succ of the indv/max repro succ /birth generation
  for(e in unique(pop_indv[,9])){
    b_yr = pop_indv[pop_indv[,9]==e,,drop=FALSE]
    b_yr[,7] <- as.numeric(b_yr[,7])
    max <- max(b_yr[,7])
    for(m in unique(b_yr[,1])){
      rel <- b_yr[b_yr[,1] == m, 7]
      rel.fit <- rel/max
      
      pop_indv[pop_indv[,1] == m, 10] <- rel.fit             #put value in gen died column
    }
  }
  
  #set up for calculations
  
  REP = matrix(nrow=years+1, ncol=14) 
  colnames(REP) = c("YearBorn", "nBornThisYear", "meanLRS", "SD", "LRSfemale", "LRSmale", "meanRRS", "SDRRS", "replicate", "parameterset", "LRSmigs", "LRSnative", "project", "group")
  
  #add year to summary matrix
  REP[,1] = c(0:(nrow(REP)-1))    #-1 to years cuz the initial pop has a generation born of 0 and migrants have a gen born of y[entered pop]
  
  #calc number of indv born in this generation
  for(q in unique(pop_indv[,9])){                         #unique generation born
    temp <- pop_indv[pop_indv[,9] ==q,,drop=FALSE]
    REP[(q+1),2] = nrow(temp)   
  } #note that it is possible that some of the columns will be NA if there were no offspring produced that year
  
  #calc the mean LRS for this generation
  for(e in unique(pop_indv[,9])){                         #unique generation born
    te <- pop_indv[pop_indv[,9] ==e,,drop=FALSE]
    
    REP[(e+1),3] = mean(te[,7])                           #find mean number of adult offspring
    #e+1 because year 0 is included
    
    REP[(e+1),4] = sd(te[,7])                             #find standard deviation in number of adult offspring
    
    #calc mean LRS for males and females
    females <- te[te[,5]==0,,drop=FALSE]                  #0 = female
    frs = mean(females[,7])
    REP[(e+1),5] = frs
    
    males <- te[te[,5] ==1,,drop=FALSE]                   #1 = male
    mrs = mean(males[,7])
    REP[(e+1),6] = mrs
    
    migr <- te[te[,2] ==-1,,drop=FALSE]                   #-1 is identifier for indv in Source pop
    miglrs <- mean(migr[,7])
    REP[(e+1),11] = miglrs
    
    nativ <- te[te[,2] !=-1,,drop=FALSE]                  #-1 is identifier for indv in Source pop
    natlrs <- mean(nativ[,7])
    REP[(e+1),12] = natlrs
    
    #calc mean relative fitness
    REP[(e+1),7] = mean(te[,10])                          #find mean relative fitness
    
    REP[(e+1),8] = sd(te[,10])                            #find standard deviation in relative fitness
    
  }
  REP[,9] = rr                                            #note replicate number
  
  REP[,10] = r                                            #note the parameter set number
  
  REP[,13] = prj                                          #note project name
  
  REP[,14] = grp                                          #note group
  
  return(list(pop_indv, REP)) 

}
