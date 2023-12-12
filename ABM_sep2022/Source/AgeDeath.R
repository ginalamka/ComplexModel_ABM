#Age-Dependent death for Lamka and Willoughby 2023
#this will impose an increased in probability of death with increasing age AND kill indv over the maxage

AgeDeath = function(pop, maxage, y){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  pop = pop[pop[,8] == 1, , drop=FALSE]           #isolate alive
  
  babes = pop[pop[,4] == 0, , drop=FALSE]         #remove babies from chance of dying
  pop = pop[pop[,4] != 0, , drop=FALSE]           #isolate those that are not newly generated babies
  

  if(nrow(pop) > 1){
    for(ee in 1:nrow(pop)){
        age = pop[ee,4] 
        pop[ee,8] = sample(x=c(0,1), size = 1, replace = TRUE, prob = c(age/maxage,(1-(age/maxage))))

        if(pop[ee,8]==0){
          pop[ee,10] = y                          #change the year died to current year
        }
      }
      nkilled = NULL
      nkilled = pop[pop[,8]==0,,drop=FALSE]
      totalkilled = nrow(nkilled)  
      print(paste("killed", totalkilled, "individuals"))
      
    }else{
      print(paste("no killing in AgeDeath"))
    }
  
  #combine pop and previously removed dead indv
  pop = rbind(pop, babes, dead)
  
  remove(babes, dead, nkilled, totalkilled, age)  #clean up
  
  return(pop)
}
