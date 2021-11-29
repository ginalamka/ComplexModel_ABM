tip <- aov(theEND[,2]~theEND[,8])
summary(tip)

ex <- aov(theEND[,5]~theEND[,8])
summary(ex)

boxplot(theEND[,5]~theEND[,8])
boxplot(theEND[,2]~theEND[,8])

tell <- aov(theEND[,2]~theEND[,15])
summary(tell)

tend <- aov(theEND[,5]~theEND[,15])
summary(tend)
TukeyHSD(theEND[,5]~theEND[,15], conf.level=0.95)
t_test(theEND[,5]~theEND[,15])

boxplot(theEND[,5]~theEND[,15])
boxplot(theEND[,2]~theEND[,15])

boxplot(theEND[,3]~theEND[,15])



#theEND[,2] - popsize
#theEND[,8] - k
#theEND[,15] - growthrate
#theEND[,5] - Ho