#to give stochastic change in K in my pop extinction model going into spring 2022

#idea will be to have an on/off switch (in Cover.R) for stochastic events to occur every "z" year

#types of drops in K
  #stochasticity for bottlenecks
    #change in K
    #if modeling habitat destruction, may be a dramatic drop
    #OR ^ rec(?) over time with a stairstep decrease in K over time (~5% per year?)

#location of this function would be in RunModel.R (feed into it the on/off switch) right before PopSizeNext.R

#stochastic death is always RANDOM.. right??? >> double check this and make sure to note a citation for the first draft of these data