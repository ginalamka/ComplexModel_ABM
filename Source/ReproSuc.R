#to calculate the reproductive success in my pop extinction model going into spring 2022
#need: year born, year died, # times being a parent, amount of babies that survived to maturity
#flow: calc total number of times being a parent > calc how many offspring survive greater than 1 year > then in writeout figure out the rest??

#designated location for this will be in RunModel.R, after runing a replicate
#data object to use will be pop, which is the focal population after y years. contains indv-level data for all

ReproSuc = function(pop){
 
  moms = pop[,2]
  mo = subset(moms, moms >0) #, drop =FALSE)
  ms = subset(moms, moms <(-1)) #, drop = FALSE)
  #moms = moms[-which(moms==0&-1), , drop=FALSE] #subset(moms, moms !is.true == 0|-1) #select moms from focal pop, if 0, founder, if -1, source pop migrant
  
  mo = matrix(unlist(mo), nrow = length(mo), byrow = TRUE)
  ms = matrix(unlist(ms), nrow = length(ms), byrow = TRUE)
  moms = rbind(mo,ms)
  remove(mo, ms)
  
  dads = pop[,3]
  da = subset(dads, dads>0) #select dads from focal pop, if 0, founder, if -1, source pop migrant
  ds = subset(dads, dads<(-1))
  
  da = matrix(unlist(da), nrow = length(da), byrow = TRUE)
  ds = matrix(unlist(ds), nrow = length(ds), byrow = TRUE)
  dads = rbind(da,ds)
  remove(da, ds)
  
  for(i in 1:nrow(moms)){                           #i is the row number in moms
    x<- moms[i,]                                    #x is the value of the mom's id
    n <- colCounts(moms, value = x)                 #n is the number of times x is a mom
    
    #pop[pop[,1]==x,]   #to select the row of pop where id = x
    
    
    
    while(pop[pop[,1]==x,]){         #finds when ID = value of m, aka i
      #REMOBVED#pop[,6] = n
      #takes a LONGGG time and may or may not work..
      
      a <- pop[pop[,1]==x,, drop=FALSE]
      a[,6] =  n      ##a[,6]+1
      
      #now I need to figure out how ot move ot the next one
      #i = i+1               #dont think this is right. not sure how to step though?
      #next                  #so far this seems to do nothing but get stuck in the loop?
      #warnings suggest using ifelse() but not sure how to make that work yet
    } 
  }
  #colCounts(dads, value = -107)
  
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
  
  for(u in unique(sub$id)){
    d = subset(sub, sub$id == u)
    ####d = sort(d, d$year, decreasing = FALSE,)
    
    r <- 1
    while(r < nrow(d)){
      x.1 <- d$adjlat[r]
      x.2 <- d$adjlat[r+1]
      y.1 <- d$adjlong[r]
      y.2 <- d$adjlong[r+1]
      
      
      c <- sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2) 
      m = matrix(nrow=1, ncol=5)
      m[,1] = u
      m[,2] = c
      m[,3] = d$year[r+1]
      m[,4] = d$sex[r]
      
      r <- r+1
      fm = rbind(fm,m)
    } 
    #dst = rbind(dst, fm)
    colnames(fm) <- c("id", "distance", "year", "sex")
  }
  
  
  
  
  for(q in unique(pop[,2])){
    sub <- pop[pop[,2] == q,]
    for(i in unique(pop[,1])){
      temp <- pop[pop[,1]==i,]
      print()
    }
  }

}

#still not sure if the number of times being a parent should be added when in Breed.R or after all years. 

#links for some of the errors I've been getting
#https://www.programmingr.com/r-error-messages/error-the-condition-has-length-1-and-only-the-first-element-will-be-used/
#https://data-flair.training/blogs/r-list-tutorial/#:~:text=%20How%20to%20Access%20R%20List%20Elements%20,an%20example.%20Give%20names%20to%20the...%20More%20
#https://r-coder.com/list-r/#Extract_elements_from_list_in_R


for(q in unique(theEND[,8])){
  temp <- theEND[theEND[,8] ==q,]
  for(i in unique(theEND[,17])){  
    sub <- theEND[theEND[,17] == i,]
    lines(sub[,1], sub[,2], lwd=2)
    lines(sub[,1], sub[,7], col="blue", lwd=2)
  }