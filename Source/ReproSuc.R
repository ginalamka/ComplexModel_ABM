#to calculate the reproductive success in my pop extinction model going into spring 2022
#flow: calc total number of times being a parent > calc how many survive greater than 1 year > then in writeout figure out the rest??

#designated location for this will be in RunModel.R, after runing a replicate
#data object to use will be pop, which is the focal population after y years. contains indv-level data for all

ReproSuc = function(pop){
 
  moms = pop[,2]
  mo = subset(moms, moms >0, drop =FALSE)
  ms = subset(moms, moms <(-1), drop = FALSE)
  rbind(mo,ms)
  #moms = moms[-which(moms==0&-1), , drop=FALSE] #subset(moms, moms !is.true == 0|-1) #select moms from focal pop, if 0, founder, if -1, source pop migrant
  dads = pop[,3]
  da = subset(dads, dads>0) #select dads from focal pop, if 0, founder, if -1, source pop migrant
  ds = subset(dads, dads<-1)
  rbind(da,ds)
  
  for(m in 1:length(moms)){   #spits out number of objects in the list
    for(i in moms[m]){        #selects value of m for ID
      if(pop[pop[,1]==i,]){         #finds when ID = value of m, aka i
        a <- pop[pop[,1]==i,, drop=FALSE]
        
        add <- function(a){a[,6]+1}
        
        sapply(a, add)
        
        #pop[,6] = pop[,6]+1   #adds offspring
        
      } #else{print(paste("error in finding mom"))}
    }
  }
  
  fem = pop[-which(pop[,1]%NOTin%f), ,drop=FALSE]
  mal = pop[-which(pop[,1]%NOTin%m), ,drop=FALSE]
  
  
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

#links for some of the errors I've been getting
#https://www.programmingr.com/r-error-messages/error-the-condition-has-length-1-and-only-the-first-element-will-be-used/
#https://data-flair.training/blogs/r-list-tutorial/#:~:text=%20How%20to%20Access%20R%20List%20Elements%20,an%20example.%20Give%20names%20to%20the...%20More%20


for(q in unique(theEND[,8])){
  temp <- theEND[theEND[,8] ==q,]
  for(i in unique(theEND[,17])){  
    sub <- theEND[theEND[,17] == i,]
    lines(sub[,1], sub[,2], lwd=2)
    lines(sub[,1], sub[,7], col="blue", lwd=2)
  }