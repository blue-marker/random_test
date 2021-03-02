#global variables and vectors

data       <- read.csv("./BTCUSD/Phase 2.1 VTLs/Phase2_1.csv",sep=",")             # Input File Name 

Cycle_10   <- data[,3]   # 10 days high and low
Cycle_20   <- data[,4]   # 20 days high and low
Cycle_80   <- data[,5]   # 80 days high and low

CP         <- data[,2]                                      # closing prices
No         <- data[,1]                                      # stores sr no

Low10      <- which(Cycle_10[1:length(Cycle_10)] == "LOW")  # index location of all Low's for 20 days Turn
High10     <- which(Cycle_10[1:length(Cycle_10)] == "HIGH") # index location of all High's for 20 days Turn

Low20      <- which(Cycle_20[1:length(Cycle_20)] == "LOW")  # index location of all Low's for 80 days Turn
High20     <- which(Cycle_20[1:length(Cycle_20)] == "HIGH") # index location of all High's for 80 days Turn

Low80      <- which(Cycle_80[1:length(Cycle_80)] == "LOW")  # index location of all Low's for 320 days Turn
High80     <- which(Cycle_80[1:length(Cycle_80)] == "HIGH") # index location of all High's for 320 days Turn

case2      =  FALSE

HighVTL         <- c()
VTLhigh         <- c()
valid_VTL       <- c()
comp_vtl        <- c()
final_VTL       <<- c()
VTL_startpoints <<- c()
start_end       <<- c()


drawVTL <- function(b1,b2){
  
  slope = (CP[b2]-CP[b1])/(No[b2]-No[b1])
  diff  = b2 - b1
  
  if (!is.na(b2)){  
    
    i=0
    while(i <= diff){
      
      HighVTL[b1+i] = CP[b1] + slope*(i)
      i=i+1
      
    }
  }
  
  return(HighVTL)
}



innerLoop <- function(b1,b2,a2,a3,b3,q){
  
  while(b2 <= a3 & !is.na(a3) & !is.na(b2) ){ 
    
    comp_vtl <- c()
    
    VTLhigh = drawVTL(b1,b2)
    
    for(k in b1:b2){ 
      
      if(!is.na(VTLhigh[k]) & !is.na(CP[k])){
        
        if(VTLhigh[k] >= CP[k]){
          
          comp_vtl <- c(comp_vtl, TRUE)  
        }
        else if(VTLhigh[k] < CP[k]) {
          
          comp_vtl <- c(comp_vtl, FALSE)
          VTLhigh <- c()
          
          break
        }
        
      }
    }
    
    if(all(comp_vtl==TRUE)){
      
      slope = (CP[b2]-CP[b1])/(No[b2]-No[b1])
      
      for(k in b2:a3){
        
        VTLhigh[k+1] = VTLhigh[k]  + slope
        
        if(VTLhigh[k] >= CP[k]){
  
          comp_vtl <- c(comp_vtl, TRUE) 
        }
        else if(VTLhigh[k] < CP[k]) {
          
          comp_vtl <- c(comp_vtl, FALSE)
          VTLhigh  <- c()
          
          break
        }
        
      }
    }
    
    
    if(all(comp_vtl==TRUE)){
      
      
      valid_VTL = VTLhigh                              #index of valid_VTL is from b1_ to b2_
      b1_       = b1
      b2_       = b2
      slope     = (CP[b2_]-CP[b1_])/(No[b2_]-No[b1_])
      tempb2_   = b2_
      
      start_end[b2_-1]  <<- "END"
      start_end[b2_]    <<- "START"
      VTL_startpoints  <<- c(VTL_startpoints,b2_)
        
      while(b2_ < b3-1 & !is.na(b3) & !is.na(b2_)){
        
        if(!is.na(valid_VTL[b2_])) {
          
          valid_VTL[b2_+1] = slope + valid_VTL[b2_]
          
        }
        
        b2_ = b2_ + 1
      }
      
      
      
      valid_VTL[1:tempb2_-1] = 0
      final_VTL[tempb2_:b3] <<-  valid_VTL[tempb2_:b3] #first point a1 is left out in the final_VTL
      
      break
    }
    
    b2 = b2 +1
  }
  
  return(comp_vtl)
  
}



lastLoop <- function(b1,b2){
  
  while(b1 <= b2 & !is.na(b1) & !is.na(b2) ){ 
    
    comp_vtl <- c()
    VTLhigh  =  drawVTL(b1,b2)
    
    for(k in b1:b2){
      
      if(VTLhigh[k] >= CP[k] ){
        
        comp_vtl <- c(comp_vtl, TRUE)  
      }
      else if(VTLhigh[k] < CP[k]) {
        
        comp_vtl <- c(comp_vtl, FALSE)
        VTLhigh <- c()
        
        break
      }
      
    }
    
    if(all(comp_vtl==TRUE)){
      
      valid_VTL = VTLhigh 
      b1_       = b1
      b2_       = b2
      
      
      start_end[b1_-1]  <<- "END"
      start_end[b1_]    <<- "START"
      
      VTL_startpoints <<- c(VTL_startpoints,b1_)
      start_end[b2]  <<- "END"
      
      valid_VTL[1:b1_-1] = 0
      final_VTL[b1_:b2_] <<-  valid_VTL[b1_:b2_] #
      
      break
    }
    
    b1 = b1 +1
  }
  
  return(comp_vtl)
  
}





FindVTL <- function(High_Cycle,Low_Cycle,Higher_Cycle){
  
  for(q in 1:length(High_Cycle)) {
    
    b1 = High_Cycle[q]         #first high
    b2 = High_Cycle[q+1]       #second high
    b3 = High_Cycle[q+2]       #third high
    
    a2 = Low_Cycle[q+1]          #second Low
    a3 = Low_Cycle[q+2]
    
    if(is.na(b3) & !is.na(b2) & !is.na(b1)){
      b3 = length(CP)       
    }
    
    if(is.na(b3) & is.na(b2) & !is.na(b1)){
      
      b2 = length(CP)       
      checkVTL <- c(lastLoop(b1,b2))
      
      if(all(checkVTL==TRUE)) {
        next
      }
      
    }
    
    if(Higher_Cycle != 0){
      checkFor_HigherCycle = "LOW" %in% Higher_Cycle[b1:b2]

      if(checkFor_HigherCycle == TRUE){
        next
      }
    }

    
    for(i in b1:b2-1) {
      b1 = i
      checkVTL <- c(innerLoop(b1,b2,a2,a3,b3,q))

      if(all(checkVTL==TRUE)) {
        break
      }
    }
    
  }
  
  for(p in  High_Cycle[2]:length(CP))
  {
    if(is.na(final_VTL[p]))
    {
      slope = final_VTL[p-1] - final_VTL[p-2]
      final_VTL[p] <<- slope + final_VTL[p-1]
    }
    
  }
  
  return(final_VTL)
  
}



Find_Upcross <- function(VTL_startpoints){
  
  Upcross <- c()
  qr = length(VTL_startpoints)
  
  for(q in 1:qr){
    # print(q)
    r1 = VTL_startpoints[q]        #b2
    r2 = VTL_startpoints[q+1]-1    #b2-1  
    
    if(is.na(r2)){
      r2 = length(CP)
    }
    
    if(!is.na(r1) & !is.na(r2)) {
      for(i in r1:r2){
        cp_temp       = CP[i] 
        final_vtl_temp = final_VTL[i]
        
        if(!is.na(CP[i]) & !is.na(final_VTL[i])){
          if(CP[i] > final_VTL[i]){
            Upcross[i] <- "Upcross"
            # print(Upcross)
            break
          }
        }
      }
    }
  }
  Upcross[length(CP)] = NA
  return(Upcross)
  
}


VTL20days= FindVTL(High10,Low10,Cycle_20)
Upcross20 = Find_Upcross(VTL_startpoints)
start_end_20 = start_end
final_VTL  <<- c()
start_end <<- c()
VTL_startpoints <<- c()


VTL80days  = FindVTL(High20,Low20,Cycle_80)
Upcross80  = Find_Upcross(VTL_startpoints)
start_end_80 = start_end
final_VTL <<- c()
start_end <<- c()
VTL_startpoints <<- c()


VTL360days = FindVTL(High80,Low80,0)
Upcross360 = Find_Upcross(VTL_startpoints)
start_end_360 = start_end
VTL_startpoints  <<- c()


bind <- cbind(CP,VTL20days,start_end_20,Upcross20,VTL80days,start_end_80,Upcross80,VTL360days,start_end_360,Upcross360)
write.csv(bind,"finalHighOutput.csv")

# VTL80days,start_end_80,Upcross80,VTL360days,start_end_360,Upcross360

