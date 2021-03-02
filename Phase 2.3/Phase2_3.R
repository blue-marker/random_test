#global variables and vectors

data       <- read.csv("BTCUSD/Phase 2.3/Phase2_3.csv",sep=",")             # Input File Name 

No         <- data[,1]   # stores index no
CP         <- data[,2]   # closing prices

Cycle_20    <- data[,4]    # 20 days high and low
Cycle_80    <- data[,7]    # 80 days high and low
Cycle_320   <- data[,10]   # 320 days high and low

Low_VTL_20   <- data[,5] 
High_VTL_20  <- data[,3] 
Low_VTL_80   <- data[,8] 
High_VTL_80  <- data[,6] 
Low_VTL_320  <- data[,11] 
High_VTL_320 <- data[,9] 

FLD          <- data[,12]       


Low20_values    <- CP[which(Cycle_20 [1:length(Cycle_20)] == "LOW")]   # values of all Lows for 20 days Turn in index order
Low20_index     <- which(Cycle_20 [1:length(Cycle_20)] == "LOW")       # index location of all Low's for 20 days Turn

High20_values   <- CP[which(Cycle_20 [1:length(Cycle_20)] == "HIGH")]  # values of all Highs for 20 days Turn in index order
High20_index    <- which(Cycle_20 [1:length(Cycle_20)] == "HIGH")      # index location of all High's for 20 days Turn

Low80_values    <- CP[which(Cycle_80[1:length(Cycle_80)] == "LOW")]    # values of all Lows for 80 days Turn in index order
Low80_index     <- which(Cycle_80[1:length(Cycle_80)] == "LOW")        # index location of all Low's for 80 days Turn


High80_values   <- CP[which(Cycle_80[1:length(Cycle_80)] == "HIGH")]   # values of all High's for 80 days Turn in index order
High80_index    <- which(Cycle_80[1:length(Cycle_80)] == "HIGH")       # index location of all Highs for 80 days Turn


Low320_values   <- CP[which(Cycle_320[1:length(Cycle_320)] == "LOW")]  # values of all Lows for 320 days Turn in index order
Low320_index    <- which(Cycle_320[1:length(Cycle_320)] == "LOW")      # index location of all Lows for 320 days Turn


High320_values  <- CP[which(Cycle_320[1:length(Cycle_320)] == "HIGH")] # values of all Lows for 320 days Turn in index order
High320_index   <- which(Cycle_320[1:length(Cycle_320)] == "HIGH")     # index location of all Highs for 320 days Turn


through <- function(List_index,List_values,Cycle){

  flag  = 1
  count = 0
  List_ <- c()
  k = 1
  temp_k = NA
  start  = FALSE

  for(i in 1:length(CP)){

    if(!is.na(List_index[k])){
      # print(List_values[k])
      if(i == List_index[k]){
        start <-c(start,CP[i])
        count <-c(count,(i+flag))
        k = k + 1
      }

      if(i %in% count){
        index = match(i,count)
        count = count[-index]
        temp_k = start[index]
        start = start[-index]

      }


    }

    if(is.na(temp_k)){
      List_[i] = NA
    } else{
      List_[i] = temp_k
    }


  }
  return(List_)
}



# through <- function(List_index,List_values,Cycle){
#   
#   flag  = as.integer(Cycle)/4
#   count = 0
#   List_ <- c()
#   k = 1
#   temp_k = NA
#   start  = FALSE
#   
#   for(i in 1:length(CP)){
#    
#     if(!is.na(List_index[k])){
#       # print(List_values[k])
#       if(i == List_index[k]){
#         start <-c(start,CP[i])
#         count <-c(count,(i+flag))
#         k = k + 1
#       }
#       
#       if(i %in% count){
#         index = match(i,count)
#         count = count[-index] 
#         temp_k = start[index]
#         start = start[-index]
#         
#       }
#     
#       
#     }
#     
#     if(is.na(temp_k)){
#       List_[i] = NA
#     } else{
#       List_[i] = temp_k
#     }
#     
#     
#   }
#   return(List_)
# }

#check out the input to know how this pans out
Cycle = 20
Low20  = through(Low20_index,Low20_values,Cycle)
High20 = through(High20_index,High20_values,Cycle)

Cycle = 80
Low80  = through(Low80_index,Low80_values,Cycle)
High80 = through(High80_index,High80_values,Cycle)

Cycle = 320
Low320 = through(Low320_index,Low320_values,Cycle)
High320 = through(High320_index,High320_values,Cycle)



count         = 0
vector        <- c()
vector[1]     <- 0
prev_high     <- c()
prev_high[1]  <- NA
i = 1

Cycle_320_v <- Cycle_320 



while(i <= length(Cycle_320_v)){

  if(Cycle_320_v[i] == "LOW"){

    if(count>0){
      count = 0
      vector[1] <- 0
    }

    count         = 0
    vector[1]     <- 0
  }


  if(Cycle_80[i]=="HIGH"){

    prev_high[1] <- i
    prev_high[2] <- CP[i]

  }

  if(Cycle_80[i]=="LOW" & Cycle_320_v[i]!="LOW" & !is.na(Low320[i]) & !is.na(prev_high[1])){

    count = count + 1
    temp = sqrt(( CP[i]- prev_high[2])**2 + (i-prev_high[1])**2)

    if(vector[1] < temp){
      vector[1] <- temp
      vector[2] <- i
    }

  }


  if(count == 4){

     i           = vector[2]
    Cycle_320_v[i] = "LOW"
    count = 0
    vector[1] <- 0
    vector[2] <- 0
    next
  }

  i = i + 1

}

Cycle  = 320
Low320_values   <- CP[which(Cycle_320_v[1:length(Cycle_320_v)] == "LOW")]  # values of all Lows for 320 days Turn in index order
Low320_index    <- which(Cycle_320_v[1:length(Cycle_320_v)] == "LOW")      # index location of all Lows for 320 days Turn

Low320_v = through(Low320_index,Low320_values,Cycle)






findPhases <- function(CP,Low_VTL,High_VTL,Low_Cycle,High_Cycle,position){
  
  i = 1
  
  Phase   <- c()
  consec  <- c()
  count   = 0
  consec2 <- c()
  count2  = 0
  consec3 <- c()
  count3  = 0
  consec4 <- c()
  count4  = 0
  consec5 <- c()
  count5  = 0
  consec6 <- c()
  count6  = 0
  consec7 <- c()
  count7  = 0
  consec8 <- c()
  count8  = 0
  consec9 <- c()
  count9  = 0
  consec10 <- c()
  count10  = 0
  consec11 <- c()
  count11  = 0
  factor  = 2.67
  factor2 = 3
  
  was_bearish = FALSE
  is_neutral    = FALSE
  is_bullish    = TRUE
  is_para       = FALSE
  is_bearish    = FALSE
  factor        = 2.67
  VTL_check     = TRUE
  checkForcross = ""
  para_check    = ""
  neutral_ends  = FALSE
  bearish_check = FALSE
  para_limit    = NA
  next_low      = NA
  presentinlow  = FALSE
  para2_check   = TRUE
  is_para2      = FALSE
  was_para2     = FALSE
  was_para      = FALSE
  range2        =  NA
  nextHigh      =  NA
  FLD_cross     = FALSE
  flag          = 0
  virtual_low   <- c()
  prev_high     <- c()
  prev_high[1]  <- NA
  vector        <- c()
  vector[1]     <- 0
  saved_low     = NA
  is_flag       = FALSE
  
  while(i <= length(CP)){
    print(i)
    
    if(!is.na(next_low)){
      
      if(next_low==i){
        
        presentinlow = FALSE
        next_low = NA
      }
    }
 
    if(is.na(Low_VTL[i]))   { Low_VTL[i] = CP[i] - 1 }
    
    if(is.na(Low_Cycle[i])) { Low_Cycle[i] = CP[i] - 1}
    
    if((CP[i] < Low_VTL[i] | CP[i] < Low_Cycle[i]) & neutral_ends==FALSE & is_bearish==FALSE & was_para==FALSE & was_para2==FALSE & is_para==FALSE & is_para2==FALSE){
      
      count     =  count + 1
      consec[i] <- i
      
      if(count > 1 & is.na(consec[i-1])) {
        
        count     = 0
        consec    <- c()
        count     = count + 1
        consec[i] <- i
        
      }
      
      if(count == position){
        
        Phase[i]     <- "NEUTRAL"
        is_neutral   = TRUE
        is_bullish   = FALSE
        neutral_ends = TRUE
        count        = 0
        consec       <- c()
        
        i = i + 1
      }
      
    }
    
    if(!is.na(Low80[i])){
      
      if(is_neutral == TRUE & (CP[i] < Low80[i]) & was_bearish==TRUE){
        
        count4     =  count4 + 1
        consec4[i] <- i
        
        if(count4 > 1 & is.na(consec4[i-1])) {
          
          count4     = 0
          consec4    <- c()
          count4     = count4 + 1
          consec4[i] <- i
          
        }
        
        if(count4 == position){
          
          if(was_para2==TRUE){
            was_para2 = FALSE
          }
          
          was_bearish = FALSE
          is_bearish     = TRUE
          bearish_check = TRUE
          
          is_neutral   = FALSE
          neutral_ends = FALSE
          
          count4        = 0
          consec4      <- c()
          
          count        = 0
          consec       <- c()
          
        }
        
        
        
      }
      
    }
    
    
    if(is_neutral==TRUE & was_para==TRUE ){
      
      if(CP[i]<Low20[i]){
        
        count9     =  count9 + 1
        consec9[i] <- i
        
        if(count9 > 1 & is.na(consec9[i-1])) {
          
          count9     = 0
          consec9    <- c()
          count9     = count9 + 1
          consec9[i] <- i
          
        }
        
        if(count9 == position){
          
          is_bearish    = TRUE
          bearish_check = TRUE
          is_neutral    = FALSE
          neutral_ends  = FALSE
          was_para      = FALSE
          
          
          count9     =   0
          consec9    <- c()
          
          count        =   0
          consec       <- c()
        }

        
        
      }
      
    }
    
    if(is_neutral==TRUE & was_para==TRUE ){
      
      if(CP[i]>FLD[i] & FLD_cross==FALSE){
        
        count10     =  count10 + 1
        consec10[i] <- i
        
        if(count10 > 1 & is.na(consec10[i-1])) {
          
          count10    = 0
          consec10    <- c()
          count10     = count10 + 1
          consec10[i] <- i
          
        }
        
        if(count10 == position){
          
          FLD_cross= TRUE
          
          count10     =   0
          consec10    <- c()
          
        }
      
      }
      
      if(CP[i]<FLD[i] & FLD_cross==TRUE){
        
        count10     =  count10 + 1
        consec10[i] <- i
        
        if(count10 > 1 & is.na(consec10[i-1])) {
          
          count10    = 0
          consec10    <- c()
          count10     = count10 + 1
          consec10[i] <- i
          
        }
        
        if(count10 == position){
          
          is_bearish    = TRUE
          bearish_check = TRUE
          
          is_neutral    = FALSE
          neutral_ends  = FALSE
          
          FLD_cross    = FALSE
          was_para     = FALSE
          
          count10     =   0
          consec10    <- c()
          
          count9     =   0
          consec9    <- c()
          
          count        =   0
          consec       <- c()
        }
        
        
      }
      
    }
    
    if(is_neutral==TRUE & was_para==TRUE){
      
      if(CP[i]>max_para1){
        
        count11     =  count11 + 1
        consec11[i] <- i
        
        if(count11 > 1 & is.na(consec11[i-1])) {
          
          count11    = 0
          consec11   <- c()
          count11     = count11 + 1
          consec11[i] <- i
          
        }
        
        if(count11 == position){
          
          is_para      = TRUE
          is_neutral   = FALSE
          neutral_ends = FALSE
          was_para     = FALSE
          
          
          count11        =   0
          consec11      <- c()
          
          count        =   0
          consec       <- c()
        }
        
        
      }
      
    }
    
    if(is_neutral==TRUE & was_para2==TRUE){
      
      if(CP[i]>max_para){
        
        count11     =  count11 + 1
        consec11[i] <- i
        
        if(count11 > 1 & is.na(consec11[i-1])) {
          
          count11    = 0
          consec11   <- c()
          count11     = count11 + 1
          consec11[i] <- i
          
        }
        
        if(count11 == position){
          
          para2_check  = FALSE
          is_para2     = TRUE
          is_neutral   = FALSE
          neutral_ends = FALSE
          was_para2    = FALSE
          
          max_para     =  CP[i]
          
          is_bullish   = FALSE
          count11        =   0
          consec11      <- c()
          
          count        =   0
          consec       <- c()
        }
        
      }
      
    }
    
  
    if(is.na(High_VTL[i])) { High_VTL[i] = CP[i] + 1 }
    
    if(is.na(High_Cycle[i])) { High_Cycle[i] = CP[i] + 1 }
    
    if((CP[i] > High_VTL[i] | CP[i] > High_Cycle[i]) & neutral_ends==TRUE & is_bearish==FALSE & was_bearish==FALSE & was_para2 !=TRUE){
      
      count     = count + 1
      consec[i] <- i
      
      if(count > 1 & is.na(consec[i-1])) {
        
        count = 0
        consec <- c()
        count = count + 1
        consec[i] <- i
      }
      
      if(count == position){
        
        Phase[i]     <- "NEUTRAL"
        is_neutral   = FALSE
        neutral_ends = FALSE
        is_bullish   = TRUE
        count        = 0
        consec       <- c()
        
        FLD_cross = FALSE
        was_para = FALSE
        was_bearish = FALSE
        
        count9     =   0
        consec9    <- c()
        count10     =   0
        consec10    <- c()
        count11     =   0
        consec11    <- c()
      }
      
    } else if((CP[i] > High_Cycle[i]) & neutral_ends==TRUE & is_bearish==FALSE & was_bearish==FALSE & was_para2==TRUE){
      
      
      count     = count + 1
      consec[i] <- i
      
      if(count > 1 & is.na(consec[i-1])) {
        
        count = 0
        consec <- c()
        count = count + 1
        consec[i] <- i
      }
      
      if(count == position){
        
        Phase[i]     <- "NEUTRAL"
        is_neutral   = FALSE
        neutral_ends = FALSE
        is_bullish   = TRUE
        count        = 0
        consec       <- c()
        
        was_para2   = FALSE
        was_bearish = FALSE
        
        count11        =   0
        consec11      <- c()
      }
      
      
      
    }
    
    if(was_bearish==TRUE & (CP[i] > High_Cycle[i]) & neutral_ends==TRUE & is_bearish==FALSE){
      
      count     = count + 1
      consec[i] <- i
      
      if(count > 1 & is.na(consec[i-1])) {
        
        count = 0
        consec <- c()
        count = count + 1
        consec[i] <- i
      }
      
      if(count == position){
        
        if(was_para2==TRUE){
          was_para2 = FALSE
        }
        Phase[i]     <- "NEUTRAL"
        is_neutral   = FALSE
        neutral_ends = FALSE
        is_bullish   = TRUE
        count        = 0
        consec       <- c()
        
        was_bearish = FALSE
      }
      
    }
    
    
    
    
    
    if(is_neutral==TRUE){
      
      Phase[i] <- "NEUTRAL"
    }
    else if(is_bullish==TRUE){
      
      Phase[i] <- "BULLISH"
    }
    
    
    #code for parabolic 2
    
    
    if(Cycle_320[i]=="LOW" & para2_check==TRUE & !is.na(High320[i])){
      
      High_list    = which(Cycle_80[1:length(CP)]=="HIGH")
      
      j=1
      for(high_index in High_list){
        
        if(high_index>i){
          nextHigh = High_list[j+1]
          print(nextHigh)
          break
        }
        j = j + 1
      }
      
      range2      = ((High320[i]-CP[i])*factor2) +High320[i]
      para2_check = FALSE
      
    }
    
    if(!is.na(range2) & !is.na(nextHigh) & para2_check==FALSE & is_para2==FALSE & CP[i]>range2 & i<nextHigh ){
      
      count7     = count7 + 1
      consec7[i] <- i
      
      if(count7 > 1 & is.na(consec7[i-1])) {
        
        count7 = 0
        consec7 <- c()
        count7 = count7 + 1
        consec7[i] <- i
      }
      
      if(count7 == position){
        
        Phase[i] = "PARABOLIC2"
        max_para = CP[i]
        is_para2  = TRUE
        
        count7      = 0
        consec7     <- c()
 
      }
      
      
    }else if(!is.na(nextHigh) & i==nextHigh){
      
      para2_check   = TRUE
      count7        =  0
      consec7       <- c()
      
    }
    
    if(is_para2==TRUE){
      
      Phase[i] = "PARABOLIC2"
      
      if(max_para < CP[i]){
        
        max_para = CP[i]
      }
      
      if(CP[i]< FLD[i]){
        
        count8     = count8 + 1
        consec8[i] <- i
        
        if(count8 > 1 & is.na(consec8[i-1])) {
          
          count8     = 0
          consec8    <- c()
          count8     = count8 + 1
          consec8[i] <- i
        }
        
        if(count8 == position){
          
          is_neutral   = TRUE
          neutral_ends = TRUE
          is_bullish   = TRUE
          was_para2   = TRUE
          is_para2  = FALSE
          para2_check = TRUE
          
          count8        = 0
          consec8       <- c()
          
        }
       
        
      }
      
    }
    
    
    
    
    
    #code for parabolic phase
    
    if(!is.na(High_VTL_80[i])){  
      
      slope = (High_VTL_80[i]-High_VTL_80[i-1])
      
      if((slope>0) & (CP[i] > High_VTL_80[i]) & is_bullish==TRUE & VTL_check==TRUE & is_bearish==FALSE & presentinlow==FALSE){
        
        count2     = count2 + 1 
        consec2[i] <- i
        
        if(count2 > 1 & is.na(consec2[i-1])) {
          
          count2     =  0
          consec2    <- c()
          count2     =  count2 + 1
          consec2[i] <- i
          
        }
        
        if(count2==position){
          
          if(is.na(Low320_v[i])){
            
            Low320_v[i] = min(CP[1:i])
          }
          
          range         = CP[i] - Low320_v[i]
          para_limit    = CP[i] + (range*factor)
          para_range_1  = i   #High VTL crossing index
          para_check    = TRUE
          VTL_check     = FALSE
          count2         = 0
          consec2        <- c()
        }
        
      }
      
    }
    
    
    if(!is.na(para_limit) & para_check==TRUE){
      
      if(Cycle_80[i] != "HIGH"){
        
        if((CP[i] >= para_limit) & i !=para_range_1){
          
          count6     = count6 + 1
          consec6[i] <- i
          
          if(count6 > 1 & is.na(consec6[i-1])) {
            
            count6 = 0
            consec6 <- c()
            count6 = count6 + 1
            consec6[i] <- i
          }
          
          if(count6 == position){
            
            para_range_2   = i
            is_para        = TRUE
            para_check     = FALSE
            max_para1      = CP[i]
            count6 = 0
            consec6 <- c()
          }
          
          
        }
        
        
      } else if(Cycle_80[i] == "HIGH"){
        
        
        high_index  =  i
        initial     =  para_range_1 - (position-2) #609-
        i           =  initial
        para_check  =  FALSE
        VTL_check   =  TRUE
        
        limit_diff  =  high_index - i 
        limit_check =  position - 1 
        
        count6 = 0
        consec6 <- c()
        
        if(limit_check == limit_diff){
        
          p = high_index
          
          for(n in Cycle_320_v[high_index:length(CP)]){
            
            if(n=="LOW"){
              next_low = p
              break
            }
            p = p + 1
          }
          
          presentinlow = TRUE
        
  
        }
        
        next
        
      }
      
    }
    
    
    if(is_para==TRUE){
      
      Phase[i] = "PARABOLIC"
      
      if((CP[i] < FLD[i])){
        
        count2     = count2 + 1 
        consec2[i] <- i
        
        if(count2 > 1 & is.na(consec2[i-1])) {
          
          count2     =  0
          consec2    <- c()
          count2     =  count2 + 1
          consec2[i] <- i
          
        }
        
        if(count2 == position){
          
          Phase[i]      = "NEUTRAL"
          is_para       = FALSE
          VTL_check     = TRUE
          is_neutral    = TRUE
          neutral_ends  = TRUE
          was_para      = TRUE
          # is_bearish    = TRUE
          # bearish_check = TRUE
          
          presentinlow = TRUE
          
          q = i
          
          for(n in Cycle_320_v[i:length(CP)]){
            
            if(n=="LOW"){
              next_low = q
              break
            }
            q = q + 1
          }
          
          
          
          count2         = 0
          consec2        <- c()
          
          i = i + 1
          next
        }
        
      }else if(max_para1 < CP[i]){
        
        max_para1 = CP[i]
      }
      
    }
    
    if(!is.na(Low320[i])){
      
      if(CP[i] < Low320[i] & is_bearish==FALSE & is_para==FALSE){
        
        count3     = count3 + 1 
        consec3[i] <- i
        
        if(count3 > 1 & is.na(consec3[i-1])) {
          
          count3     =  0
          consec3    <- c()
          count3     =  count3 + 1
          consec3[i] <- i
          
        }
        
        if(count3 == position){
          
          is_bearish    = TRUE
          bearish_check = TRUE
        }
      }
    }
    
    
    if(bearish_check==TRUE){
      
      Phase[i]        <- "BEARISH"
      bearish_check   =  FALSE
      i = i + 1 
      
    }
    
    
    if(is_bearish == TRUE){
      
      Phase[i] <- "BEARISH"
      
      if(CP[i] > High_VTL_80[i]){
        
        count     = count + 1 
        consec[i] <- i
        
        if(count > 1 & is.na(consec[i-1])) {
          
          count     =  0
          consec    <- c()
          count     =  count + 1
          consec[i] <- i
          
        }
        
        if(count==position){
          
          is_bearish    =  FALSE
          is_neutral    =  TRUE
          neutral_ends  =  TRUE
          Phase[i]      <- "NEUTRAL"
          was_bearish   = TRUE
          
          count5     =  0
          consec5    <- c()
          
          count        = 0
          consec       <- c()
        }
        
      }
      
      if(CP[i] > High80[i]){
        
        count5     = count5 + 1 
        consec5[i] <- i
        
        if(count5 > 1 & is.na(consec5[i-1])) {
          
          count5     =  0
          consec5    <- c()
          count5     =  count5 + 1
          consec5[i] <- i
          
        }
        
        if(count5==position){
          
          is_bearish   = FALSE
          is_bullish   = TRUE
          Phase[i]     <- "BULLISH"
          is_bearish   = FALSE
          was_bearish = FALSE
          
          is_neutral    =  FALSE
          neutral_ends  =  FALSE
          
          count5     =  0
          consec5    <- c()
          
          count        = 0
          consec       <- c()
        }

      }
      
    }
    
    i = i + 1
    
  }
  return(Phase)
  
}



Phase_outputs  = findPhases(CP,Low_VTL_80,High_VTL_80,Low80,High80,position)

bind <- cbind(Phase_outputs)
#bind <- cbind(High80,High20,Cycle_320_v,High320,Low20,Low80,Low320,Low320_v)
write.csv(bind,"phase2.3.csv")






