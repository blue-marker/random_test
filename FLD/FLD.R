#File containing closing prices and dates is stored in a variable
data<-read.csv("./BTCUSD/FLD/FLD.csv",sep=",") # File Name is Test2.csv
numbers = data[,2] # numbers used to store Closing Price
library("xlsx")
# "window_size" variable contains the required Moving Average days.
MovingAvgCal<-function(window_size)
{
i=1
MovingAvgR<-c()
while(i<length(numbers))
	{
	if(!is.na(mean(numbers[seq(i,(window_size/2)+i-1)])))
	MovingAvgR[floor((window_size/4)+(window_size/2)+i)-1]<-mean(numbers[seq(i,(window_size/2)+i-1)])
	i=i+1
	
	}
return(MovingAvgR)
}
ma10=MovingAvgCal(10)
ma20=MovingAvgCal(20)
ma40=MovingAvgCal(40)
ma80=MovingAvgCal(80)
ma320=MovingAvgCal(320)


fillRows<-function(ColumnToFill)
{
i5=1
	while(i5<=length(ma320)& length(ColumnToFill)>0)
	{
	     if(is.na(ColumnToFill[i5]))
 	    	ColumnToFill[i5]=NA
		if(!is.na(ColumnToFill[i5]))
		ColumnToFill[i5]=ColumnToFill[i5]
		else
		ColumnToFill[i5]=""
		i5=i5+1
	}
return(ColumnToFill)
}
ma10=fillRows(ma10)
ma20=fillRows(ma20)
ma40=fillRows(ma40)
ma80=fillRows(ma80)
ma320=fillRows(ma320)

lowFind<-function(ma,window_size)# Calculating low function taking moving average and  window size
{
ii=0
ma20=ma
ws=window_size/2
ad<-c()
ad1<-c()
i5=1
	while(i5<=length(ma20))
	{   
	 	if(is.na(ma20[i5]))
     		ma20[i5]=100000+i5
		if(!is.na(ma20[i5]))# If Displaced Moving Average is NA then fill with a large number like 100000
		ma20[i5]=ma20[i5]
		else
		ma20[i5]=""
		i5=i5+1
	}


	myco<-c()
	while(ii<length(ma20))
	{
	cyc=-ceiling(ws)  #cyc contain value window_size/2
	cyct=-cyc		 # cyct used for storing negative value of cyc
		i=1
		while(cyc<=cyct)
		{
			
		# If window size is 20 then it compares before and after value by 10 (window_size/2)
		if(cyct+ii+ws<length(ma20)&& cyct+cyc+ii+ws<length(ma20) && cyct+cyc+ii!=0)
		{
          # If first value is less than the second value, then 't' refers to 'True' else 'f' refers to 'False' will be stored in array(ad)
		if(ma20[cyct+ii+ws]<=ma20[cyct+cyc+ii+ws] && !is.na(ma20[cyct+ii+ws]) && ma20[cyct+ii+ws]!="")
			ad[i]<-"t"
		else
			ad[i]<-"f"
		}
		cyc=cyc+1
		i=i+1

		}
	if('f' %in% ad) {}
	else
	myco[cyct+ii+ws]="LOW" #if value is minimum between -window_cycle/2 to  +window_cycle/2 then we will mark that index as low	
ii=ii+1
}
   myco= fillRows(myco)
   return(myco)  # All lows are returned

}

high<-function(jack,ma)
{
	dtsync10<-c()
	dtsync10<-which(jack[1:length(jack)]=="LOW")
	st=1
	jack[which.max(ma[seq(1,dtsync10[st]-1,1)])]="HIGH"
	
	while(st<nchar(dtsync10))
	{
	jack[which.max(ma[seq(dtsync10[st]+1,dtsync10[st+1]-1,1)])+dtsync10[st]+1-1]="HIGH"
	st=st+1
	}
	jack = fillRows(jack)
	return(jack)
}

low10=high(fillRows(lowFind(ma10,ceiling(10/8))),ma10)
low20=high(fillRows(lowFind(ma20,ceiling(20/8))),ma20)
low40=high(fillRows(lowFind(ma40,ceiling(40/8))),ma40)
low80= high(fillRows(lowFind(ma80,ceiling(80/8))),ma80)
low320=high(fillRows(lowFind(ma320,ceiling(320/8))),ma320)
numbers=fillRows(numbers)
i=1
UpDownCross<-function(ma10,low){
result<-c()

while(i<length(ma10))
{
result[i]=as.numeric(numbers[i])-as.numeric(ma10[i])
i=i+1
}
i1=1
while(i1<length(ma10))
{
if(sign(result[i1])!=sign(result[i1+1])& !is.na(result[i1]) &!is.na(result[i1+1]))
{
if(sign(result[i1+1])==1 )
{
low[i1+1]="UPCROSS"}

if(sign(result[i1+1])==-1 )
low[i1+1]="DOWNCROSS"
}
i1=i1+1
}


return(low)
}

low10Cross=fillRows(UpDownCross(ma10,low10))
low20Cross=fillRows(UpDownCross(ma20,low20))
low40Cross=fillRows(UpDownCross(ma40,low40))
low80Cross=fillRows(UpDownCross(ma80,low80))
MovingAvgWincycle10=ma10
MovingAvgWincycle20=ma20
MovingAvgWincycle40=ma40
MovingAvgWincycle80=ma80
MovingAvgWincycle320=ma320

namechange<-function(low,days)
{
result<-c()
lowcount<-which(low %in% c("LOW"))
highcount<-which(low %in% c("HIGH"))
low[lowcount]=paste(days,"day FLD HIGH forecast")
low[highcount]=paste(days,"day FLD LOW forecast")
return(low)
}
low10=namechange(low10,"10")
low20=namechange(low20,"20")
low40=namechange(low40,"40")
low80=namechange(low80,"80")


bind<-cbind(numbers,MovingAvgWincycle10,low10,low10Cross,MovingAvgWincycle20,low20,low20Cross,MovingAvgWincycle40,low40,low40Cross,MovingAvgWincycle80,low80,low80Cross,MovingAvgWincycle320,low320)
write.csv(bind,"sam.csv")
write.xlsx(bind,file = "ko.xlsx",sheetName="FLDs", append=FALSE)


