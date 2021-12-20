#to calculate the reproductive success in my pop extinction model going into spring 2022
#flow: calc total number of times being a parent > calc how many survive greater than 1 year > then in writeout figure out the rest??

#designated location for this will be in RunModel.R, after runing a replicate
#data object to use will be FINAL, which is the focal population after y years. contains indv-level data for all

ReproSuc = function(pop){
  moms = pop[pop[,1] == moms, , drop = FALSE]
  dads = pop[pop[,3] == TRUE, , drop = FALSE]
  
  #pop1 = pop[-which(pop[,4] >= maxage),]
  
  moms = pop[,2]
  moms = pop[-which(pop[,1] == moms),]
}