#global variables and vectors

data       <- read.csv("BTCUSD/Phase 3/Phase_3.csv",sep=",")             # Input File Name 

No         <- data[,1]   # stores index no
CP         <- data[,3]   # closing prices

Cycle_20    <- data[,4]    # 20 days high and low
Cycle_80    <- data[,5]    # 80 days high and low
Cycle_320   <- data[,6]   # 320 days high and low

# Low_VTL_20   <- data[,5] 
# High_VTL_20  <- data[,3] 
# Low_VTL_80   <- data[,8] 
# High_VTL_80  <- data[,6] 
# Low_VTL_320  <- data[,11] 
# High_VTL_320 <- data[,9] 
# 
# FLD          <- data[,12]       


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

#check out the input to know how this pans out
Cycle = 20
Low20  = through(Low20_index,Low20_values,Cycle)
High20 = through(High20_index,High20_values,Cycle)
Cycle = 80
Low80  = through(Low80_index,Low80_values,Cycle)
High80 = through(High80_index,High80_values,Cycle)

BuyEntry  <- c()
SellEntry <- c()


Entry_buyprice = NA
Entry_sellprice = NA
profit_buyexit_value = NA
profit_sellexit_value = NA

data2       <- read.csv("phase2.3.csv",sep=",")   
Phases      <- data2[,2] 



Buy  = FALSE
Sell = FALSE
high20 = NA
high80 = NA
low20 = NA
low80 = NA

i = 1

while(i <= length(CP)){
  
  # 20 Day Breakout Cycle
  print(i)
  
  if((!is.na(High20[i]) & (CP[i] > High20[i]) & Buy==FALSE & is.na(high20)) | (!is.na(High20[i]) & (CP[i] > High20[i]) & Buy==FALSE & high20 != High20[i])){
    
    BuyEntry[i] <- "Long-Entry"
    high20 = High20[i]
    
    Entry_buyprice = CP[i]
    profit_buyexit_value = Entry_buyprice + (Entry_buyprice * (0.50))
    Buy = TRUE
    
    i = i + 1
    next
  }
  
  if(!is.na(FLD[i])){
    # print(CP[i])
    
    if(Buy==TRUE & (CP[i] > profit_buyexit_value) & (CP[i] < FLD[i]) ){
      BuyEntry[i]          <- "Long-ProfitExit"
      Entry_buyprice    = NA
      Buy = FALSE
    }
    
  }else {
    if(Buy==TRUE & (CP[i] > profit_buyexit_value)){
      BuyEntry[i]          <- "Long-ProfitExit"
      Entry_buyprice    = NA
      Buy = FALSE
    }
  }


  if(!is.na(Low80[i]) & (CP[i] < Low80[i]) & Buy==TRUE ){
    BuyEntry[i]  <- "Long-SwingExit"
    Buy = FALSE
  }
  
  
  if(!is.na(Low20[i]) & (CP[i] < Low20[i]) & Buy==TRUE ){
    BuyEntry[i]  <- "Long-TurnExit"
    Buy = FALSE
  
  }
  
  if(i!=1 & Buy ==TRUE){
    
    if((Phases[i]=="NEUTRAL" & Phases[i-1] !="NEUTRAL")|(Phases[i]=="BEARISH" & Phases[i-1] !="BEARISH")){
      
      BuyEntry[i] <- "Long-TrendExit"
      Buy = FALSE
      
    }
  }
  
 i = i + 1

}


i = 1

while(i <= length(CP)){
 
  
  # 20 Day Breakout Cycle
  
  if((!is.na(Low20[i]) & (CP[i] < Low20[i]) & Sell==FALSE & is.na(low20)) | (!is.na(Low20[i]) & (CP[i] < Low20[i]) & Sell==FALSE & low20 != Low20[i])){
    
    SellEntry[i] <- "Short-Entry"
    low20 = Low20[i]
    Entry_sellprice = CP[i]
    profit_sellexit_value = Entry_sellprice - (Entry_sellprice * (0.50))
    Sell = TRUE
    
    i = i + 1
    next
  }
  
  
  if(!is.na(FLD[i])){
    
    if(Sell==TRUE & (CP[i] < profit_sellexit_value) & (CP[i] > FLD[i])){
      
      SellEntry[i]  <- "Short-ProfitExit"
      Entry_sellprice = NA
      Sell = FALSE
    }
    
  }else {
    
    if(Sell==TRUE & (CP[i] < profit_sellexit_value)){
      
      SellEntry[i]  <- "Short-ProfitExit"
      Entry_sellprice = NA
      Sell = FALSE
    }
  }
  
  
  if(!is.na(High80[i]) & (CP[i] > High80[i]) & Sell==TRUE){
    SellEntry[i]  <- "Short-SwingExit"
    Sell = FALSE
  }
  
  
  if(!is.na(High20[i]) & (CP[i] > High20[i]) & Sell==TRUE){
    SellEntry[i]  <- "Short-TurnExit"
    Sell = FALSE
    
  }
  
  
  if(i != 1 & Sell==TRUE){
    
    if((Phases[i]=="NEUTRAL" & Phases[i-1] !="NEUTRAL")|(Phases[i]=="BULLISH" & Phases[i-1] !="BULLISH")){
      
      SellEntry[i] <- "Short-TrendExit"
      Sell = FALSE
    }
  }
  
  i = i + 1
  
}

# 80 Day Cycle

BuyEntry1  <- c()
SellEntry1 <- c()


Entry_buyprice        = NA
Entry_sellprice       = NA
profit_buyexit_value  = NA
profit_sellexit_value = NA


Buy  = FALSE
Sell = FALSE

i = 1

while(i <= length(CP)){

  # 80 Day Breakout Cycle

  if( (!is.na(High80[i]) & (CP[i] > High80[i]) & Buy==FALSE & is.na(high80)) | (!is.na(High80[i]) & (CP[i] > High80[i]) & Buy==FALSE & high80 != High80[i])){

    BuyEntry1[i] <- "Long-Entry"
    high80 = High80[i]
    Entry_buyprice = CP[i]
    profit_buyexit_value = Entry_buyprice + (Entry_buyprice * (0.5))
    Buy = TRUE

    i = i + 1
    next
  }


  if(!is.na(FLD[i])){
    
    if(Buy==TRUE & (CP[i] > profit_buyexit_value) & (CP[i] < FLD[i])){
      BuyEntry1[i]          <- "Long-ProfitExit"
      Entry_buyprice    = NA
      Buy = FALSE
    }
    
  }else {
    if(Buy==TRUE & (CP[i] > profit_buyexit_value)){
      BuyEntry1[i]          <- "Long-ProfitExit"
      Entry_buyprice    = NA
      Buy = FALSE
    }
  }


  if(!is.na(Low80[i]) & (CP[i] < Low80[i]) & Buy==TRUE ){
    BuyEntry1[i]  <- "Long-SwingExit"
    Buy = FALSE
  }


  if(!is.na(Low20[i]) & (CP[i] < Low20[i]) & Buy==TRUE ){
    BuyEntry1[i]  <- "Long-TurnExit"
    Buy = FALSE

  }

  if(i!=1 & Buy ==TRUE){
    
    if((Phases[i]=="NEUTRAL" & Phases[i-1] !="NEUTRAL")|(Phases[i]=="BEARISH" & Phases[i-1] !="BEARISH")){
      
      BuyEntry1[i] <- "Long-TrendExit"
      Buy = FALSE
      
    }
  }

  i = i + 1

}


i = 1

while(i <= length(CP)){


  # 80 Day Breakout Cycle

  if((!is.na(Low80[i]) & (CP[i] < Low80[i]) & Sell==FALSE & is.na(low80)) | (!is.na(Low80[i]) & (CP[i] < Low80[i]) & Sell==FALSE & low80 != Low80[i])){

    SellEntry1[i] <- "Short-Entry"
    low80 = Low80[i]
    Entry_sellprice = CP[i]
    profit_sellexit_value = Entry_sellprice -(Entry_sellprice * (0.5))
    Sell = TRUE

    i = i + 1
    next
  }


  if(!is.na(FLD[i])){
    
    if(Sell==TRUE & (CP[i] < profit_sellexit_value) & (CP[i] > FLD[i])){
      
      SellEntry1[i]  <- "Short-ProfitExit"
      Entry_sellprice = NA
      Sell = FALSE
    }
    
  }else {
    
    if(Sell==TRUE & (CP[i] < profit_sellexit_value)){
      
      SellEntry1[i]  <- "Short-ProfitExit"
      Entry_sellprice = NA
      Sell = FALSE
    }
  }


  if(!is.na(High80[i]) & (CP[i] > High80[i]) & Sell==TRUE){
    SellEntry1[i]  <- "Short-SwingExit"
    Sell = FALSE
  }


  if(!is.na(High20[i]) & (CP[i] > High20[i]) & Sell==TRUE){
    SellEntry1[i]  <- "Short-TurnExit"
    Sell = FALSE

  }


  
  if(i != 1 & Sell==TRUE){
    
    if((Phases[i]=="NEUTRAL" & Phases[i-1] !="NEUTRAL")|(Phases[i]=="BULLISH" & Phases[i-1] !="BULLISH")){
      
      SellEntry1[i] <- "Short-TrendExit"
      Sell = FALSE
    }
  }

  i = i + 1

}

# Trend Trading 


BuyEntry2  <- c()
SellEntry2 <- c()


Entry_buyprice        = NA
Entry_sellprice       = NA
profit_buyexit_value  = NA
profit_sellexit_value = NA


Buy  = FALSE
Sell = FALSE

i = 1
state = FALSE

while(i <= length(CP)){
  
  print(i)
  
  # 80 Day Breakout Cycle
    if(i!=1){
      if(Phases[i]=="BULLISH" & Phases[i-1] !="BULLISH"){
        state = TRUE
      }
    }
  
    
    if((Phases[i]=="BULLISH" & i==1) | (Buy==FALSE & state==TRUE)){

      BuyEntry2[i] <- "Long-Entry"
      Entry_buyprice = CP[i]
      profit_buyexit_value = Entry_buyprice + Entry_buyprice * (0.5)
      Buy=TRUE
      
      state = FALSE
      i = i + 1
      next
    }
  
  if(!is.na(FLD[i])){
    
    if(Buy==TRUE & (CP[i] > profit_buyexit_value) & (CP[i] < FLD[i])){
      BuyEntry2[i]          <- "Long-ProfitExit"
      Entry_buyprice    = NA
      Buy = FALSE
    }
    
  }else {
    if(Buy==TRUE & (CP[i] > profit_buyexit_value)){
      BuyEntry2[i]          <- "Long-ProfitExit"
      Entry_buyprice    = NA
      Buy = FALSE
    }
  }
  
  

  
  
  if(!is.na(Low80[i]) & (CP[i] < Low80[i]) & Buy==TRUE ){
    BuyEntry2[i]  <- "Long-SwingExit"
    Buy = FALSE
  }
  
  
  if(!is.na(Low20[i]) & (CP[i] < Low20[i]) & Buy==TRUE ){
    BuyEntry2[i]  <- "Long-TurnExit"
    Buy = FALSE
    
  }
  
  if(i!=1 & Buy ==TRUE){
    
    if((Phases[i]=="NEUTRAL" & Phases[i-1] !="NEUTRAL")|(Phases[i]=="BEARISH" & Phases[i-1] !="BEARISH")){
      
      BuyEntry2[i] <- "Long-TrendExit"
      Buy = FALSE
      
    }
  }
  
  i = i + 1
  
}


i = 1
state = FALSE

while(i <= length(CP)){
  
  
  # Trend Trading
    if(i != 1){
      if(Phases[i]=="BEARISH" & Phases[i-1] !="BEARISH"){
        state = TRUE
      } 
    }
   
  
    if((Phases[i]=="BEARISH" & i==1) | (Sell==FALSE & state==TRUE)){

      SellEntry2[i] <- "Short-Entry"
      Entry_sellprice = CP[i]
      profit_sellexit_value = Entry_sellprice - Entry_sellprice * (0.50)
      Sell = TRUE
      
      state = FALSE
      i = i + 1
      next
    }
  
  if(!is.na(FLD[i])){
    
    if(Sell==TRUE & (CP[i] < profit_sellexit_value) & (CP[i] > FLD[i])){
      
      SellEntry2[i]  <- "Short-ProfitExit"
      Entry_sellprice = NA
      Sell = FALSE
    }
    
  }else {
    
    if(Sell==TRUE & (CP[i] < profit_sellexit_value)){
      
      SellEntry2[i]  <- "Short-ProfitExit"
      Entry_sellprice = NA
      Sell = FALSE
    }
  }
  

  
  
  if(!is.na(High80[i]) & (CP[i] > High80[i]) & Sell==TRUE){
    SellEntry2[i]  <- "Short-SwingExit"
    Sell = FALSE
  }
  
  
  if(!is.na(High20[i]) & (CP[i] > High20[i]) & Sell==TRUE){
    SellEntry2[i]  <- "Short-TurnExit"
    Sell = FALSE
    
  }
  
  
  if(i != 1 & Sell==TRUE){
    
    if((Phases[i]=="NEUTRAL" & Phases[i-1] !="NEUTRAL")|(Phases[i]=="BULLISH" & Phases[i-1] !="BULLISH")){
      
      SellEntry2[i] <- "Short-TrendExit"
      Sell = FALSE
    }
  }
  
  i = i + 1
  
}




bind <- cbind(BuyEntry)
write.csv(bind,"BuyEntry.csv")
bind <- cbind(SellEntry)
write.csv(bind,"SellEntry.csv")

bind <- cbind(BuyEntry1)
write.csv(bind,"BuyEntry1.csv")
bind <- cbind(SellEntry1)
write.csv(bind,"SellEntry1.csv")

bind <- cbind(BuyEntry2)
write.csv(bind,"BuyEntry2.csv")
bind <- cbind(SellEntry2)
write.csv(bind,"SellEntry2.csv")