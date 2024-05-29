#Migrate individuals from source to pop for Lamka and Willoughby 2023

{
  #  a = 1 migrant per generation
  #  b= 100 @ y=151
  #  c= 25 @ y=151,165,181,195
  #  d= 1 migrant when y >= edyr + dur+1 (i.e., after habitat restoration) - not included in final publication
  #  e= 100 @ y=125
  #  f= 25 @ y=125, 140, 155, 170
}

Migrate = function(pop, source, y, miggy, styr, edyr, dur){
  if(miggy == "a"){
    mig = 1
    print(paste("there are", mig, "migrants this year"))
    
    for(m in 1:mig){
      #select migrant without replacement
      migrant = sample(1:nrow(source), 1, replace = F)
      
      #change gen born to the generation the migrant entered the pop
      source[migrant,9] <- y   
      
      #take migrant from source and put into pop
      pop = rbind(pop, source[migrant,])
      #remove migrant from source
      source = source[-migrant,]
    }
  }else if(miggy == "b"){
    if(y == 151){   
      mig = 100
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        #change gen born to the generation the migrant entered the pop
        source[migrant,9] <- y   
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
    }else{mig=0}
  }else if(miggy == "c"){
    if(y == 151|y == 165|y == 181|y ==195){  
      mig = 25 
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        #change gen born to the generation the migrant entered the pop
        source[migrant,9] <- y   
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      } 
    }else{mig=0}
  }else if(miggy == "d"){
    if(y >= edyr + dur+1){      
      mig = 1
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        #change gen born to the generation the migrant entered the pop
        source[migrant,9] <- y   
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
    }else{mig=0}
  }else if(miggy == "e"){
    if(y == 125){
      mig = 100
      
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        #change gen born to the generation the migrant entered the pop
        source[migrant,9] <- y   
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
    }else{mig=0}
  }else if(miggy == "f"){
    if(y == 125|y == 140|y == 155|y == 170){
      mig = 25
      print(paste("there are", mig, "migrants this year"))
      
      for(m in 1:mig){
        #select migrant without replacement
        migrant = sample(1:nrow(source), 1, replace = F)
        
        #change gen born to the generation the migrant entered the pop
        source[migrant,9] <- y   
        
        #take migrant from source and put into pop
        pop = rbind(pop, source[migrant,])
        #remove migrant from source
        source = source[-migrant,]
      }
    }else{mig=0}
  }else{
    mig = 0
  }
  
  return(list(pop, mig, source))
}
