#AgeUp

AgeUp = function(pop){
  dead = pop[pop[,8] == 0, , drop=FALSE]          #remove dead indvs
  
  if(nrow(dead)>= 1){
    pop = pop[-which(pop[,1]%in%dead), , drop=FALSE]
  }
  
  if(!is.null(nrow(pop))){
    pop[,4] = pop[,4] + 1
    
  }
  pop<- rbind(pop,dead)
  remove(dead)
  return(pop)
}

#testing
## AgeUp(pop)

#must remove dead before aging -- taken from MateChoice.R

#12/28/2021
#note that sometimes the age is 10. check how that can be and what might need to be done to fix that. 