#FunctionSourcer.R
#this is for the complex migration ABM for class Fall 2021

#set working directory, import packages, source functions,
setwd(paste(directory,"/source/", sep = '')) #set temp working directory

#import packages
#library()

#source functions
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/AgeUp.R", sep = ''))
source(paste(getwd(), "/DeathByAge.R", sep = ''))
source(paste(getwd(), "/SourcePop.R", sep = ''))

#source(paste(getwd(), "/Migrate.R", sep = ''))
#source(paste(getwd(), "/Mate.R", sep = ''))
#source(paste(getwd(), "/Reproduce.R", sep = ''))
#source(paste(getwd(), "/DeathByFitness.R", sep = ''))