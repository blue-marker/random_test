data       <- read.csv("./BTCUSD/Phase 2.2/Phase2_2.csv",sep=",")             # Input File Name 

No         <- data[,1]   # stores index no
Date       <- data[,2]   # closing prices
CP         <- data[,3]   # closing prices

Cycle_20    <- data[,4]
Cycle_80    <- data[,5]    # 80 days high and low
Cycle_320   <- data[,6]    # 320 days high and low]    # 20 days high and low


library(gsubfn)

# x <- as.Date(x,format='%m/%d/%Y')
# y <- as.Date(y,format='%m-%d-%Y')
# 
# t = julian(y,x)
# t = difftime(y,x, units = "days") 
# print(t)


findSummary <- function(Cycle){
  Cycle_Trend         <- c()
  SumfirstLowDate     <- c()
  SumhighDate         <- c()
  SumsecondLowDate    <- c()
  SumfirstLowPrice    <- c()
  SumhighPrice        <- c()
  SumsecondLowPrice   <- c()
  Date_            <- c()
  Price_           <- c()
  Rally_Duration   <- c()
  Decline_Duration <- c()
  Rally_amount     <- c()
  Decline_amount   <- c()
  Nature           <- c()
  cycle_start      <- TRUE
  First_LOW        = NA

  i = 1
  
  while(i <= length(CP)){

    if((Cycle[i]=="LOW" | Cycle[i]=="LOW*") & cycle_start == TRUE){
      First_LOW = Cycle[i]
      cycle_start = FALSE
      SumfirstLowPrice[i] <- CP[i]
      SumfirstLowDate[i]  <- Date[i]
      
      date_format1 = as.Date(Date[i],format='%m/%d/%Y')
      date_format2 = as.Date(Date[i],format='%m-%d-%Y')
      
      if(is.na(date_format1)){
        firstLowDate = date_format2 
      } else{
        firstLowDate = date_format1 
      }
      
      firstLowPrice <- CP[i]
      
      i = i + 1
      next
    }

    if(Cycle[i]=="HIGH" | Cycle[i]=="HIGH*"){
      SumhighDate[i]  <- Date[i]
      SumhighPrice[i]  <- CP[i]
      
      date_format1 = as.Date(Date[i],format='%m/%d/%Y')
      date_format2 = as.Date(Date[i],format='%m-%d-%Y')
      
      if(is.na(date_format1)){
        HighDate     = date_format2 
      } else{
        HighDate     = date_format1 
      }
      
  
      HighPrice    <- CP[i]
      
      Rally_Duration[i] <- julian(HighDate,firstLowDate)
      Rally_amount[i]      <- (CP[i]-firstLowPrice)/firstLowPrice



    }

    if((Cycle[i]=="LOW" | Cycle[i]=="LOW*") & cycle_start==FALSE){
      
      cycle_start = TRUE
      SumsecondLowDate[i]    <- Date[i]
      
      date_format1 = as.Date(Date[i],format='%m/%d/%Y')
      date_format2 = as.Date(Date[i],format='%m-%d-%Y')
      
      if(is.na(date_format1)){
        secondLowDate     = date_format2 
      } else{
        secondLowDate     = date_format1 
      }
      
      
      Decline_Duration[i] <- julian(secondLowDate,HighDate)
      SumsecondLowPrice[i]    <- CP[i]

      Decline_amount[i]   <- (HighPrice -CP[i])/CP[i]
      
      
      x = abs(HighPrice - firstLowPrice)
      y = abs(HighPrice - CP[i])
        
      if(CP[i] > firstLowPrice){

        Cycle_Trend[i] <- "BULLISH"

      } else if(CP[i] < firstLowPrice){

        Cycle_Trend[i] <- "BEARISH"

      } else if( (x*0.9) <= y & y <= (x*1.1) ){

        Cycle_Trend[i] <- "NEUTRAL"

      }
      
      if(First_LOW=="LOW*" | Cycle[i]=="LOW*" ){
        Nature[i]    <- "BACKFILLED"
      } else {
        Nature[i]    <- "NATURAL"
      }
      
      next
    }
    
    i = i + 1

  }
  
  
  SumfirstLowPrice[length(CP)]              <- NA
  SumfirstLowDate[length(CP)]               <- NA
  SumhighPrice[length(CP)]                  <- NA
  SumhighDate[length(CP)]                   <- NA
  SumsecondLowPrice[length(CP)]             <- NA
  SumsecondLowDate[length(CP)]              <- NA
  Cycle_Trend[length(CP)]       <- NA
  Date_[length(CP)]             <- NA
  Price_[length(CP)]            <- NA
  Rally_Duration[length(CP)]    <- NA
  Decline_Duration[length(CP)]  <- NA
  Rally_amount[length(CP)]      <- NA
  Decline_amount[length(CP)]    <- NA
  Nature[length(CP)]            <- NA
  Cycle_Trend[length(CP)]       <- NA
  
  #bind <- cbind(Date,CP,Cycle,Cycle_Summary,Price_Summary,Rally_Duration,Decline_Duration,Rally_amount,Decline_amount,Cycle_Trend,Nature)
  
  
  
  SumfirstLowPrice              <- SumfirstLowPrice [!is.na(SumfirstLowPrice)]
  SumfirstLowDate               <- SumfirstLowDate[!is.na(SumfirstLowDate)]
  SumhighPrice                  <- SumhighPrice[!is.na(SumhighPrice)]
  SumhighDate                   <- SumhighDate[!is.na(SumhighDate)]
  SumsecondLowPrice             <- SumsecondLowPrice[!is.na(SumsecondLowPrice)]
  SumsecondLowDate              <- SumsecondLowDate[!is.na(SumsecondLowDate)]
  Cycle_Trend       <- Cycle_Trend[!is.na(Cycle_Trend)]
  Date_             <- Date_[!is.na(Date_)]
  Price_            <- Price_[!is.na(Price_ )]
  Rally_Duration    <- Rally_Duration[!is.na(Rally_Duration)]
  Decline_Duration  <- Decline_Duration[!is.na(Decline_Duration)]
  Rally_amount      <- Rally_amount[!is.na(Rally_amount)]
  Decline_amount    <- Decline_amount[!is.na(Decline_amount)]
  Nature            <- Nature[!is.na(Nature)]
  
  
  
  print(length(Nature))
  print(length(SumfirstLowPrice))
  print(length(SumfirstLowDate))
  print(length(SumhighPrice))
  print(length(SumhighDate))
  print(length(SumsecondLowPrice))
  print(length(SumsecondLowDate))
  print(length(Rally_Duration ))
  print(length(Decline_Duration))
  print(length(Rally_amount))
  print(length(Decline_amount))
  print(length(Cycle_Trend))
  
  bind <- cbind(SumfirstLowPrice,SumfirstLowDate,SumhighPrice,SumhighDate,SumsecondLowPrice,SumsecondLowDate,Rally_Duration,Decline_Duration,Rally_amount,Decline_amount,Cycle_Trend,Nature)
  
  write.csv(bind,"Phase2.2_output.csv")

}

# 
findSummary(Cycle_20)
# findSummary(Cycle_80)
# findSummary(Cycle_320)

# bind <- cbind(Cycle_Summary,Price_Summary,Rally_Duration,Decline_Duration,Rally_amount,Decline_amount,Cycle_Trend)
# write.csv(bind,"Phase2.2_output1.csv")

# phase_summary = findSummary(Cycle_80)
# bind <- cbind(phase_summary)
# write.csv(bind,"Phase2.2_output2.csv")
# 
# phase_summary = findSummary(Cycle_320)
# bind <- cbind(phase_summary)
# write.csv(bind,"Phase2.2_output3.csv")
