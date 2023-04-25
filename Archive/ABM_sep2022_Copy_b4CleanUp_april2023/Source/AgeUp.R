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