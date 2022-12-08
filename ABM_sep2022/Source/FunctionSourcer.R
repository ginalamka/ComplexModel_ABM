#FunctionSourcer.R
#this is for the complex migration ABM for class Fall 2021

#set working directory, import packages, source functions,
setwd(paste(directory,"/Source/", sep = '')) #set temp working directory

#import packages
#install.packages("reshape2", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("ape", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("adegenet", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("hierfstat", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("plyr", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#install.packages("gtools", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
.libPaths("/home/gfl0003") 

#library()
library(reshape2)   #commented out 10/4/22 cuz I was getting pissed. I think this is just for plotting
library(adegenet)   #commented out 10/4/22 cuz I was getting pissed. I think this is just for plotting
library(scales) #may need this for plotting, this allows transparency in colors for overlapping lines
library(matrixStats) #need this for colCount and rowCount in ReproSuc.R
library(hierfstat)   #need this for FST analysis in Analyze.R
library(tidyr)       #need this for FST analysis in Analyze.R
library(gtools)      #commented out 10/4/22 cuz I was getting pissed. I think this is just for plotting

'%NOTin%' <- Negate(`%in%`) #this defines the not in function so that I can select identities that are not defined by something (see RandomDeaths.R for an example)
#note that the ' ' is not included in the %NOTin% operator -- see https://r-lang.com/not-in-r/#:~:text=The%20not%20in%20operator%20is%20a%20logical%20vector%2C,a%20vector.%20The%20%21%20indicates%20logical%20negation%20%28NOT%29.

#source functions
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/AgeUp.R", sep = ''))
source(paste(getwd(), "/AgeDeath.R", sep = ''))
source(paste(getwd(), "/FitnessDeath.R", sep = ''))
source(paste(getwd(), "/Migrate.R", sep = ''))
source(paste(getwd(), "/MateChoice.R", sep = ''))
source(paste(getwd(), "/PopSizeNext.R", sep = ''))
source(paste(getwd(), "/Breed.R", sep = ''))
source(paste(getwd(), "/Analyze.R", sep = ''))
source(paste(getwd(), "/Plot.R", sep = ''))       #on/off switch
source(paste(getwd(), "/Plot2.R", sep = ''))       #on/off switch
source(paste(getwd(), "/RepSucc.R", sep = ''))

####old, removed functions
#source(paste(getwd(), "/DeathByAge.R", sep = ''))
#source(paste(getwd(), "/RandomDeath.R", sep = ''))
#source(paste(getwd(), "/ReproSuc.R", sep = ''))   #new function is RepSucc.R
#source(paste(getwd(), "/Death.R", sep = ''))      #two new functions: AgeDeath.R and FitnessDeath.R
#source(paste(getwd(), "/Stochastic.R", sep = '')) #on/off switch
#source(paste(getwd(), "/SourcePop.R", sep = ''))
#source(paste(getwd(), "/Sensitivity.R", sep = '')) #turn on as needed