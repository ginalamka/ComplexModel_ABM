#Final pop FST analyses

#set working directory
setwd("/home/gfl0003/datahold/fst") # setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/ABM_sep2022") 
directory = getwd()
outdir = paste(directory, "/Output/", sep = "")   #  outdir = paste("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM", "/Output_local/", sep = "")

#like function sourcer, can I source the datafiles??

#read in data -- will need to figure out naming scheme - perhaps just need to change per run like Cover.R
data = read.table("fst.csv", header=T, sep=",")

rep = data[,1]                 #replicate number
ident = data[,2]               #pop identifier
geno = data[,3:ncol(data)]     #12 SNP matrix for genotypes

#note we already know FST vs initialized source & vs initialized focal pop
#now, we want FST vs each other -- do we want FST across replicates?
#or do we select one replicate and only compare that one to one replicate of another parameter set?

#do we want to know the FST across initialized pops?

#so is this more like relative changes? or actual differences in final genotypes?

#OR do we compare all of these to one initiailized pop?