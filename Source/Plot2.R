#Plot2.R for complex migration model 2022

Plot2 = function(repEND){
  #set up to subset by replicate and by year born (-1 = migrants, 0 = init focal pop)
  rep = repEND[, (ncol(repEND))]
  yr = repEND[, 1]
  
  if(plotit==1){
    
  }
}

#Current list of columns for repEND
  #as of 5/4/22
#gen born
#numb indv born that year
#mean LRS
#SD LRS
#LRS females
#LRS males
#mean relative fitness (relative to max)
#SD rel fit
#replicate number