#FunctionSourcer.R
#this is for the complex migration ABM for class Fall 2021

#set working directory, import packages, source functions,
setwd(paste(directory,"/source/", sep = '')) #set temp working directory

#import packages
#library()
library(scales) #may need this for plotting, this allows transparency in colors for overlapping lines

'%NOTin%' <- Negate(`%in%`) #this defines the not in function so that I can select identities that are not defined by something (see RandomDeaths.R for an example)
#note that the ' ' is not included in the %NOTin% operator -- see https://r-lang.com/not-in-r/#:~:text=The%20not%20in%20operator%20is%20a%20logical%20vector%2C,a%20vector.%20The%20%21%20indicates%20logical%20negation%20%28NOT%29.

#source functions
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/AgeUp.R", sep = ''))
source(paste(getwd(), "/Death.R", sep = ''))
source(paste(getwd(), "/SourcePop.R", sep = ''))
source(paste(getwd(), "/Migrate.R", sep = ''))
source(paste(getwd(), "/MateChoice.R", sep = ''))
source(paste(getwd(), "/PopSizeNext.R", sep = ''))
source(paste(getwd(), "/Breed.R", sep = ''))
source(paste(getwd(), "/Analyze.R", sep = ''))
source(paste(getwd(), "/Plot.R", sep = ''))

####old, removed functions
#source(paste(getwd(), "/DeathByAge.R", sep = ''))
#source(paste(getwd(), "/RandomDeath.R", sep = ''))