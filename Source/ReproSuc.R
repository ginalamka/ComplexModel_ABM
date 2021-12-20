#to calculate the reproductive success in my pop extinction model going into spring 2022
#flow: calc total number of times being a parent > calc how many survive greater than 1 year > then in writeout figure out the rest??

#designated location for this will be in RunModel.R, after runing a replicate
#data object to use will be pop, which is the focal population after y years. contains indv-level data for all

ReproSuc = function(pop){
  moms = pop[pop[,1] == moms, , drop = FALSE]
  dads = pop[pop[,3] == TRUE, , drop = FALSE]
  
  #pop1 = pop[-which(pop[,4] >= maxage),]
  
  moms = pop[,2]
  
  for(q in unique(pop[,2])){
    sub <- pop[pop[,2] == q,]
    for(i in unique(pop[,1])){
      temp <- pop[pop[,1]==i,]
      print()
    }
  }

}


for(q in unique(theEND[,8])){
  temp <- theEND[theEND[,8] ==q,]
  for(i in unique(theEND[,17])){  
    sub <- theEND[theEND[,17] == i,]
    lines(sub[,1], sub[,2], lwd=2)
    lines(sub[,1], sub[,7], col="blue", lwd=2)
  }