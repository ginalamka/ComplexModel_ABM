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

"new parameters:
styr          = 100 #year to start pop decline
edyr          = 150 #year to end pop decline, first year at low pop size
nwk           = 250 #pop size after decline -- probs makes sense to keep these even in vary decline years and decline rate. should end @ same pt for all pop sizes
dur           = 50  #duration of small pop size before pop growth "

#put this function after PopSizeNext, before Breed. 
  #this allows me to kill those still alive but not kill the new babies. 
  #I *think* that means I should make sure not to kill parents from MateChoice
    #to make it easier -- just put this after Breed, at the very end. 
Stochastic = function(pop, stoch, k, numboff, styr, endyr, nwk, dur, y, years){   #may not need stoch (if will always have stoch change) or numboff (if kill fresh babies too)
  ##check out Analyze.R -- I think I can just do multiple returns for each if statement?
  stkilled = NULL
  
  if(y < styr){   
    #if it is before the stochastic decline period, skip this function
    #k= k.V #probs not necessary -- only necessary for the increase -- rememebr to remove this again later
    stkilled = NULL
    dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
    pop = pop[-which(pop[,1]%in%dead), , drop = FALSE]
    #just doing this to set the data up like the others
    
  }else if(y > edyr+dur){   #think about what to do with K after the decline - when do I set K back?
    #if it is before the stochastic decline period, skip this function
    stkilled = NULL
    dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
    pop = pop[-which(pop[,1]%in%dead), , drop = FALSE]
    #just doing this to set the data up like the others
    
    incrate = round((k.V - nrow(pop))/(years-y))   #another way to do this would be to use teh intrinsic growth rate from PopSizeNext.
    k <- k + incrate
    
  }else if(styr <= y & y < edyr){
    dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
    pop = pop[-which(pop[,1]%in%dead), , drop = FALSE]
    
    killrate = round((nrow(pop)-nwk)/(edyr-y))   #the number to decrease in k per year 
    #previously ##killrate = round((nrow(pop)-nwk)/(edyr-styr))
    
    k <- k-killrate                          #new k
    
    numbkill = nrow(pop) - k #- numboff    #add this if I am protecting the new babies and not killing them. consider implications
    
    if(numbkill > 0){
      stkill = sample(1:nrow(pop), numbkill, replace = FALSE)     #randomly select those to kill
      pop[stkill,8] = 0
    }

  }else{                  #if during the duration period of bottleneck, maintain small pop size ##REMOVED#(y > edyr & y <= edyr+dur)
    dead = pop[pop[,8] == 0, , drop=FALSE]                 #remove dead indv
    pop = pop[-which(pop[,1]%in%dead), , drop = FALSE]
    
    numbkill = nrow(pop) - nwk          #find if too many have been created and kill some
    if(numbkill > 0){
      stkill = sample(1:nrow(pop), numbkill, replace = FALSE)     #randomly select those to kill
      pop[stkill,8] = 0
    }
    
  }
  
  print(paste("K is now", k, ".", length(stkilled), "have been killed"))
  
  pop = rbind(dead, pop)
  return(list(pop,k)) #dont forget to split these when going back into RunModel
}
  
  
#also note modulus 
  #for example, if want to select every other year, will do "year % 2" -- will give true/false 
    #if want a value divisible by 5, do value % 5
