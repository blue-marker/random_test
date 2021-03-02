#global variables and vectors

data       <- read.csv("./BTCUSD/Phase 2.1 VTLs/Phase2_1.csv",sep=",") # Input File Name is output.csv

Cycle_10    <- data[,3]    # 10 days high and low
Cycle_20    <- data[,4]   # 20 days high and low
Cycle_80    <- data[,5]   # 80 days high and low

CP     <- data[,2]                     # closing prices
No     <- data[,1]                     # stores sr no

Low10  <- which(Cycle_10[1:length(Cycle_10)] == "LOW")        # index location of all Low's for 10 days Turn
High10 <- which(Cycle_10[1:length(Cycle_10)] == "HIGH")

Low20  <- which(Cycle_20[1:length(Cycle_20)] == "LOW")   # index location of all Low's for 20 days Turn
High20 <- which(Cycle_20[1:length(Cycle_20)] == "HIGH")

Low80  <- which(Cycle_80[1:length(Cycle_80)] == "LOW")   # index location of all Low's for 80 days Turn
High80 <- which(Cycle_80[1:length(Cycle_80)] == "HIGH")

case2 = FALSE

LowVTL      <- c()
VTLlow      <- c()
valid_VTL   <- c()
comp_vtl    <- c()

final_VTL       <<- c()
VTL_startpoints <<- c()
start_end       <<- c()

a1_         = 0
a2_         = 0

drawVTL <- function(a1,a2){
   
    slope = (CP[a2]-CP[a1])/(No[a2]-No[a1])
    diff = a2 - a1
    
    if (!is.na(a2)){  
      
      i=0
      while(i <= diff)
      {
        #print(i)
        LowVTL[a1+i] = CP[a1] + slope*(i)
        i=i+1
        
      }
    }
  
  return(LowVTL)
}



innerLoop <- function(a1,a2,b1,b2,a3){
  
  while(a2 <= b2 & !is.na(a2) & !is.na(b2) )
  { 
    comp_vtl <- c()

    VTLlow = drawVTL(a1,a2)

    for(k in a1:a2){
      
      
         
        if(VTLlow[k] <= CP[k]){
          
          comp_vtl <- c(comp_vtl, TRUE)  #print a2 to a3
        }
        else if(VTLlow[k] > CP[k]) {
          
          comp_vtl <- c(comp_vtl, FALSE)
          VTLlow   <- c()
          
          break
        }
       
    }
    
    if(all(comp_vtl==TRUE)){
      
      slope = (CP[a2]-CP[a1])/(No[a2]-No[a1])
      
      for(k in a2:b2){
        
        VTLlow[k+1] = VTLlow[k]  + slope
        
        if(VTLlow[k] <= CP[k]){
          
          comp_vtl <- c(comp_vtl, TRUE) 
        }
        else if(VTLlow[k] > CP[k]) {
          
          comp_vtl <- c(comp_vtl, FALSE)
          VTLlow <- c()
          
          break
        }
        
      }
    }
    
    
    
    
    if(all(comp_vtl==TRUE)){
      
      valid_VTL = VTLlow 
      a1_ = a1
      a2_ = a2
      slope = (CP[a2_]-CP[a1_])/(No[a2_]-No[a1_])
      tempa2_ = a2_
      
      start_end[a2_-1]  <<- "END"
      start_end[a2_]    <<- "START"
      VTL_startpoints  <<- c(VTL_startpoints,a2_)
      
      
      while(a2_ <= a3-1 & !is.na(a3) & !is.na(a2_)){

        if(!is.na(valid_VTL[a2_])) {
          
          valid_VTL[a2_+1] = slope + valid_VTL[a2_]
          
        }

        a2_ = a2_ + 1
      }
      
      valid_VTL[1:tempa2_-1] = 0
      
      
      final_VTL[tempa2_:a3] <<- valid_VTL[tempa2_:a3]
      
      break
    }
    
    a2 = a2 +1
  }
  return(comp_vtl)
}


lastLoop <- function(a1,a2){
  
  while(a1 <= a2 & !is.na(a1) & !is.na(a2) ){ 
    
    comp_vtl <- c()
    VTLlow = drawVTL(a1,a2)
    
    for(k in a1:a2){
      
      if(VTLlow[k] <= CP[k] ){
        
        comp_vtl <- c(comp_vtl, TRUE)  
      }
      else if(VTLlow[k] > CP[k]) {
        
        comp_vtl <- c(comp_vtl, FALSE)
        VTLlow <- c()
        
        break
      }
      
    }
    
    if(all(comp_vtl==TRUE)){
      
      valid_VTL = VTLlow 
      
      a1_ = a1
      a2_ = a2
      
      valid_VTL[1:a1_-1] = 0
      final_VTL[a1_:a2_] <<-  valid_VTL[a1_:a2_] #
      
      break
    }
    
    a1 = a1 +1
  }
  
  return(comp_vtl)
  
}






FindVTL <- function(High_Cycle,Low_Cycle){
  
  for(q in 1:length(Low_Cycle)) {
    
    b1 = High_Cycle[q]         #first high
    b2 = High_Cycle[q+1]       #second high
    a1 = Low_Cycle[q]          #first Low
    a2 = Low_Cycle[q+1]        #second Low
    a3 = Low_Cycle[q+2]

    
    if(is.na(a3) & !is.na(a2) & !is.na(a1)){
      a3 = length(CP)       
    }
    
    if(is.na(a3) & is.na(a2) & !is.na(a1)){
      
      a2 = length(CP)       
      checkVTL <- c(lastLoop(a1,a2))
      
      if(all(checkVTL==TRUE)) {
        next
      }
      
    } 
    
    #case1: a1 is fixed and a2 is incremented in the loop
    tempA2 = a2
    checkVTL = innerLoop(a1,a2,b1,b2,a3)
    
    if(all(checkVTL==TRUE)) {
      next
    }
    case2 = TRUE
    
    #case2: a1 is incremented 
    if(case2==TRUE){
      a2 = tempA2
      
      for(i in a1+1:a2-1) {
        a1 = i
        checkVTL = innerLoop(a1,a2,b1,b2,a3)
        
        if(all(checkVTL==TRUE)) {
          break
        }
      }
      
    }
    
  }
  
  for(p in Low_Cycle[2]:length(CP))
  {
    if(is.na(final_VTL[p]))
    {
      slope = final_VTL[p-1] - final_VTL[p-2]
      final_VTL[p] <<- slope + final_VTL[p-1]
    }
    
  }
  
  return(final_VTL)
}


Find_Downcross <- function(VTL_startpoints){

  Downcross <- c()
  qr = length(VTL_startpoints)
  
  for(q in 1:qr){
    

    r1 = VTL_startpoints[q]        #a2_
    r2 = VTL_startpoints[q+1]-1    #a2_-1  
    
    if(is.na(r2)){
      r2 = length(CP)
    }
    
    if(!is.na(r1) & !is.na(r2)) {
      for(i in r1:r2){
        
        if(!is.na(CP[i]) & !is.na(final_VTL[i])){
          if(CP[i] < final_VTL[i]){
            Downcross[i] <- "Downcross"
            break
          }
        }
      }
    }
  }
  Downcross[length(CP)] = NA
  return(Downcross)
}

VTL20days   = FindVTL(High10,Low10)
Downcross20 = Find_Downcross(VTL_startpoints)
start_end_20 = start_end

final_VTL  <<- c()
start_end <<- c()
VTL_startpoints <<- c()

VTL80days=FindVTL(High20,Low20)
Downcross80 = Find_Downcross(VTL_startpoints)
start_end_80 = start_end

final_VTL  <<- c()
start_end <<- c()
VTL_startpoints <<- c()


VTL360days=FindVTL(High80,Low80)
Downcross360 = Find_Downcross(VTL_startpoints)
start_end_360 = start_end
VTL_startpoints  <<- c()

#bind <- cbind(CP,VTL20days,start_end_20,Downcross20,VTL80days,start_end_80,Downcross80,VTL360days,start_end_360,Downcross360)

bind <- cbind(CP,VTL20days,VTL80days,VTL360days)
write.csv(bind,"finalLowOutput.csv")



