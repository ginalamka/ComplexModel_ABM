#to give stochastic change in K in my pop extinction model going into spring 2022

#idea will be to have an on/off switch (in Cover.R) for stochastic events to occur every "z" year

#the Stochastic check will be within Death.R so that it will change the numb

#types of drops in K
  #stochasticity for bottlenecks
    #change in K
    #if modeling habitat destruction, may be a dramatic drop
    #OR ^ rec(?) over time with a stairstep decrease in K over time (~5% per year?)

#location of this function would be in RunModel.R (feed into it the on/off switch) right before PopSizeNext.R
  #or consider putting it in death.. ?

#stochastic death is always RANDOM.. right??? >> double check this and make sure to note a citation for the first draft of these data

#set up is similar to & modeled after RandomDeath.R

Stochastic = function(pop, stoch, k){
  dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
  alive = pop[-which(pop[,1]%in%dead), , drop = FALSE]
  #since only returning numboff, dont need to rbind dead and alive into pop
  
  #pop = pop[pop[,4] >= maturity, , drop=FALSE]          #isolate adults -- if only want effective pop size to count here
  
  #calculate the current population size
  Nt = nrow(alive)
  
  #find new K
  #will k need to be changed for every year or can it get back to the original K? think about these implications
  if(stoch == 2){
    new_k = Nt * .95    #drop in K by 5% of pop size
  }else if(stoch == 1){
    new_k = Nt * .7    #drop in K by 30%
  }
  #randomly select indv to die
  #syntax: sample(group, X, replace) with group being the column in the matrix, X being the number to kill per run
  kill = sample(alive[,1], (Nt - new_k), replace = FALSE)
  alive = alive[-which(alive[,1]%in%kill),]
  
  k <- new_k
  
  print(paste("K is now", k, ". ", nrow(kill), "have been killed"))
  
  return(alive)
}
#need to figure out how to regulate what years this occurs. that will depend on the stochastic type and where this is placed 

#notes from talking with Janna 2/14
#dont forget to add a check to make sure that k > 20 (or whatever makes sense/is my popsize cutoff)
#need to add new parameters: decline start year, decline end year, end year + duration at low k
  #will also need to know what the drop percent is, but will do that as we go
  #then after year+duration, will want logistic growth rate to take over for the population to recover

#also note modulus 
  #for example, if want to select every other year, will do "year % 2" -- will give true/false 
    #if want a value divisible by 5, do value % 5