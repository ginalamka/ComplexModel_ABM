#WRITTEN BY ANDREA, ABM CLASS - DAY 2 NOTES
#use this for complex model migration (if possible)

#simulate movement between two pops that have 100 individuals each 
#exactly one individual moves


#set up pop A
popA=matrix(nrow=100, ncol=2) #where born and where now
popA[,1]= rep("A", 100)
popA[,2]= rep("A",100)
colnames(popA) = c("born", "now")

popB=matrix(nrow=100, ncol=2) #where born and where now
popB[,1]= rep("B", 100)
popB[,2]= rep("B",100)
colnames(popB) = c("born", "now")

migrate = 1
gens = 50

for(g in 1:50){
  toA = sample(1:nrow(popA), migrate, replace = F)
  
  migA = popA[toA, , drop= F]
  migA[1,2] = "B"
  popA = popA[-toA]
  
  toB = sample(1:nrow(popB), migrate, replace = F)
  migB = popB[toB, , drop= F]
  mig[1,2] = "A"
  popB = popB[-toB]
#sometimes when you take a row out it gets it out so we add the drop = F thing so R can keep it in the matrix
  
  
  
} 
  
  
  
  