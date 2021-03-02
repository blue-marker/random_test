#File containing closing prices and dates is stored in a variable

data<-read.csv("./BTCUSD/Phase 1/Phase_1.csv",sep=",")[,1:2] # File Name is test.csv
numbers = data[,2] # numbers used to store Closing Price

# "window_size" variable contains the required Moving Average days.

window_size=10 
ma10<-filter(data[,2],rep(1/window_size,window_size)) # Displaced Moving Average for 10 days calculated
window_size=20
ma20<-filter(data[,2],rep(1/window_size,window_size)) # Displaced Moving Average for 20 days calculated
window_size=80
ma80<-filter(data[,2],rep(1/window_size,window_size)) # Displaced Moving Average for 80 days calculated


# Function fillRows fills the empty/blank rows of the input column.

fillRows<-function(ColumnToFill)
{
i5=1
	while(i5<=length(numbers)& length(ColumnToFill)>0)
	{
	     if(is.na(ColumnToFill[i5]))
 	    	ColumnToFill[i5]="  "
		if(!is.na(ColumnToFill[i5]))
		ColumnToFill[i5]=ColumnToFill[i5]
		else
		ColumnToFill[i5]=" "
		i5=i5+1
	}
return(ColumnToFill)
}


# lowFind function find all lows of Moving Average Columns for all Moving Average Columns.
# Arguments "window_cycle" and "ma"(Displaced Moving Average) is passed to the function



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
		if(!is.na(ma20[i5]))# If Displaced Moving Average is NA then fill with a large number
		ma20[i5]=ma20[i5]
		else
		ma20[i5]=" "
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
			if(ma20[cyct+ii+ws]<=ma20[cyct+cyc+ii+ws] && !is.na(ma20[cyct+ii+ws]) )
			{
				ad[i]<-"t"
			}
			else
			{
			ad[i]<-"f"
	
			}

		}
		cyc=cyc+1
		i=i+1

		}
	if('f' %in% ad) 
	{}
	else
	{
	myco[cyct+ii+ws]="LOW" #if value is minimum between -window_cycle/2 to  +window_cycle/2 then we will mark that index as low
	}
ii=ii+1
}
   myco= fillRows(myco)
   return(myco)  # All lows are returned

}
# Variables defined to store repsective lowFind for moving average.

lowcp=lowFind(numbers,6)   # lowcp is variable stores the low of Closing Price
low10=lowFind(ma10,6)      # low10 is variable stores the low value of moving avg10
low20=lowFind(ma20,20)     # low20 is variable stores the low value of moving avg20
low80=lowFind(ma80,80)     # low80 is variable stores the low value of moving avg80

# DeleteLow function delete lows if distance between two lows is less than 7 days than highest closing price low.
# "highLow" argument is passed to the function, to find highest low amongst two consecutive lows in a distance of 7 days.

DeleteLow<-function(highLow,window_cycle){
i4=0
dt<-c()
dth<-c()
diff<-c()
ic=1
dt<-which(highLow[i4+1:length(numbers)]=="LOW")
len=length(dt)
while(len>0)
{
if(!is.na(dt[ic]) && !is.na(dt[ic+1]))
{ 
	if(highLow[dt[ic]]=="") # If one low value is deleted then previous low value will be compared with the next low value.
       { 
      dt[ic]=dt[ic-1]
     if(-(dt[ic]-dt[ic+1])<7 )
      {

      		
         if(data[dt[ic],2]<data[dt[ic+1],2])
      	{
		 highLow[dt[ic+1]]=""
	      }	
	    if(data[dt[ic+1],2]<data[dt[ic],2])
      	{
		 highLow[dt[ic]]=""
	      }
	   }
     }
	else
	{
	if(-(dt[ic]-dt[ic+1])<=7 )
       {		
         if(data[dt[ic],2]<data[dt[ic+1],2])
      	{
		 highLow[dt[ic+1]]=""
	      }	
	    if(data[dt[ic+1],2]<data[dt[ic],2])
      	{
		 highLow[dt[ic]]=""
	      }
	  }
	}
}
ic=ic+1
len=len-1
}

return(highLow)  # Final output is returned of highLow.

}

# newCol variable stores result of "DeleteLow()" 

newCol=DeleteLow(lowcp,20)

# The difference between two consecutive lows is less than (0.5*window_cycle(wc)) then the higher low will be deleted from column sync.

lowdel<-function(sync,wc)
{
dtsync10<-which(sync %in% c("LOW","LOW*"))
stt=1
while(stt<length(dtsync10)+1)
  {
   start=stt
   while(start<stt+1)
   {
       #if difference between two low is less than or eql to 0.5* window_cycle then we will delete higher low
	if(sync[dtsync10[start]]=="" )
	{
	dtsync10[start]=dtsync10[start-1]

	if(dtsync10[start+1]-dtsync10[start]<0.5*wc && !is.na(dtsync10[start+1]) && !is.na(dtsync10[start]))
	   {
	      if(numbers[dtsync10[start]]<numbers[dtsync10[start+1]])
	        {
	         sync[dtsync10[start+1]]=""
	        sync[dtsync10[start]]="LOW"
			print(dtsync10[start+1])
	        }
           }
	 else
	   {
		dtsync10[start]=dtsync10[start-1]
		if(dtsync10[start+1]-dtsync10[start]<0.5*wc && !is.na(dtsync10[start+1]) && !is.na(dtsync10[start]))
		{
		  if(numbers[dtsync10[start]]<numbers[dtsync10[start+1]])
		  {
			  sync[dtsync10[start+1]]=""
	  
		  }

	 	 if(numbers[dtsync10[start]]>numbers[dtsync10[start+1]] && !is.na(numbers[dtsync10[start+1]]))
	  	  {
	  		sync[dtsync10[start]]=""
	  
	 	   }

	        }
	    }
      }
	else
	{
	if(dtsync10[start+1]-dtsync10[start]<0.5*wc && !is.na(dtsync10[start+1]) && !is.na(dtsync10[start]))
	{
	  if(numbers[dtsync10[start]]<numbers[dtsync10[start+1]])
	  {
		  sync[dtsync10[start+1]]=""
	  }

	  if(numbers[dtsync10[start]]>numbers[dtsync10[start+1]] && !is.na(numbers[dtsync10[start+1]]))
	  {
	  	sync[dtsync10[start]]=""

	  }

       }

   }
   start=start+1
  # ii=ii+1
  }
stt=stt+1
}
return(sync)
}

# high function to find "high"(highest value) beween two consecutive lows.


high<-function(jack)
{
	dtsync10<-c()
	dtsync10<-which(jack[1:length(jack)]=="LOW")
	#dtsync10<-which(jack %in%  c("LOW","LOW*"))
	res=0
	stt=1
	dtsyn<-c()
	ii=1
	st=1
	while(st<length(dtsync10))
	{
	start=dtsync10[st]
	higval<-c()
	ii=1

		while(start<=dtsync10[st+1] && !is.na(dtsync10[st+1]) && !is.na(start))
		{
		higval[ii]=numbers[start]
		start=start+1
		ii=ii+1
		}
	if(!is.null(higval))
	jack[(which.max(higval)+dtsync10[st])-1]="HIGH"
	st=st+1
	}

	 jack = fillRows(jack)
	
	 return(jack)
}




# markfilling function marks the index of (low+0.5*window_cycle) if distance between the two lows is greater than (1.5* window_cycle)

markfilling<-function(sync10,wc){
kl=1
dt<-which(sync10 %in% c("LOW*","LOW")) # Stores index location where "LOW*" (backfilled low) and "LOW" are present.

while(kl<length(dt)){
    dt<-which(sync10 %in% c("LOW*","LOW"))
    if(dt[kl+1]-dt[kl]>=1.5*wc && length(dt)>0) {

        # low - window_cycle/2 will return (mark), if that point collides with HIGH next point will return mark.
        if(sync10[dt[kl+1]-wc/2]=="HIGH" && !is.na(sync10[dt[kl+1]-wc/2]))
        {
            sync10[(dt[kl+1]-wc/2)-1]="mark" 
        }
        else
        {
            sync10[dt[kl+1]-wc/2]="mark" 
        }

        # low + window_cycle/2 will return (mark), if that point collides with HIGH previous point will return mark.
        if(sync10[dt[kl]+wc/2]=="HIGH" && !is.na(sync10[dt[kl]+wc/2]) )
        {
            sync10[(dt[kl]+wc/2)+1]="mark"
        }
        else
        {
            sync10[dt[kl]+wc/2]="mark" 
        }


    }
    kl=kl+1
}

return(sync10) # sync10 returns the lows and mark points
}


# Backfilling function is invoked when the distance between two consecutive lows is more than 1.5 * window_cycle.
# Applicable for moving_average based on 10 days (window_cycle - 20 days).

backfilling <-function(wc, synccol) {
    sync10f <-c()
    sync10 = synccol
    dthind <-c()
    dtlind <-c()

    dtl <-which(sync10 %in% c("LOW*", "LOW")) # dtl variable stores index of "LOW"'s and "LOW*"s
    lk = 1
    while (lk < length(dtl) + 1) {
        res = 0
        rest = 0
        check <-c()
        i = 1
        r = 0
        dthh <-which(sync10 %in% c("HIGH*", "HIGH")) # dthh variable stores index of "HIGH"'s and "HIGH*"s
        dtm <-which(sync10[1: length(sync10)] == "mark") # dtm variable stores index of "mark"
        dtl <-which(sync10 %in% c("LOW*", "LOW")) # dtl stores index of "LOW"'s and "LOW*"s

        if (dtl[lk + 1] - dtl[lk] > 1.5 * wc && !is.na(dtl[lk + 1])) {
            dthh <-which(sync10[dtl[lk]: dtl[lk + 1]] == "HIGH") + dtl[lk]
            dthh1 <-which(sync10[dtl[lk]: dtl[lk + 1]] == "HIGH*") + dtl[lk]
            dthh = union(dthh, dthh1)
            dtm <-which(sync10[dtl[lk]: dtl[lk + 1]] == "mark") + dtl[lk]
            dtm2 = dtm
            dthh2 = dthh

            # Case 1 - When both marks are after the high between the two consecutive lows in which it's being backfilled.
            if (length(dthh) != 0) {
                if (dthh < dtm[1] && dthh < dtm[2] && !is.na(dtm[2])) 
                {
                    r = 0
                    check <-c()
                    dthind <-c()
                    dtlind <-c()
                    i = 1
                    r = dtm[2] - dtm[1]
                    for (a in seq(0, r, 1)) {
                        dtm = dtm2
                        first = dtm[1] + a - 1
                        check2 <-c()
                        while (dtm[1] + a < dtm[2]) {
                            rest = (numbers[dtm[1] + a] - numbers[first]) / numbers[dtm[1] + a]
                            if(rest>0){
					check[i] = rest
                              dthind[i] = first
                              dtlind[i] = dtm[1] + a}
                              dtm[1] = dtm[1] + 1
                            i = i + 1
                        }
                    }
                }
			if(!is.null(check)){

                sync10f[dthind[which.max(check)]] = "LOW*"
                sync10f[dtlind[which.max(check)]] = "HIGH*"}
            }

            # Case 2 - When first mark is before the high and the second one is after the high.
            if (length(dthh) != 0) {
                highestval1Pos <-c()
                highestval2Pos <-c()
                if (dtm[1] < dthh && dthh < dtm[2] && !is.na(dtm[2])) 
                {
                    r = 0
                    highestval1 <-c()
                    highestval2 <-c()
                    check <-c()
                    dthind <-c()
                    dtlind <-c()
                    i = 1
                    if (length(dthh) > 1) {
                        dthh = dthh[1]
                    }
                    r = dthh - dtm[1]
                    for (a in seq(0, r, 1)) {
                        dtm = dtm2
                        dthh = dthh2
                        first = dtm[1] + a - 1
                        check2 <-c()
                        while (dtm[1] + a < dthh - 1) {
                            rest = (numbers[dtm[1] + a] - numbers[first]) / numbers[dtm[1] + a]
                            if(rest<0){
					check[i] = rest
                            dthind[i] = first
                            dtlind[i] = dtm[1] + a}
                            dtm[1] = dtm[1] + 1
                            i = i + 1
                        }
                    }
			if(!is.null(check))
				{
                    highestval1 <-check[which.min(check)]
                    highestval1Pos <-which.min(check)
                    highestval1Pos1 <-dthind[which.min(check)]
                    highestval1Pos2 <-dtlind[which.min(check)]
			}
                    r = 0
                    check <-c()
                    dthind <-c()
                    dtlind <-c()
                    i = 1
                    if (!is.na(dtm[2])) {
                        if (length(dthh) > 1) {
                            dthh = dthh[1]
                        }
                        r = (dtm[2] - dthh)
                        if (r < 0) {
                            r = -r
                        }
                        for (a in seq(0, r, 1)) {
                            dtm = dtm2
                            dthh = dthh2
                            first = dthh + a
                            check2 <-c()
                            while (dthh + a < dtm[2] - 1 && length(dthh) > 0 && length(dtm[2]) > 0) {
                                rest = (numbers[dthh + 1 + a] - numbers[first]) / numbers[dthh + 1 + a]
                               if(rest>0)
					{
					 check[i] = rest
                                dthind[i] = first
                                dtlind[i] = dthh + 1 + a}
                                dthh = dthh + 1
				
                                i = i + 1
                            }
                        }
                    }
				if(!is.null(check)){
                    highestval2 <-check[which.max(check)]
                    highestval2Pos <-which.max(check)
                    lowhighval2 = max(check)}

                    if (highestval1 < 0 && !is.null(highestval1)) {
                        highestval1 = -highestval1
                    }
			 if (highestval1 < highestval2 && !is.null(highestval1) && !is.null(highestval2) & !is.null(check)) {
			 if (numbers[dtlind[which.max(check)]] < numbers[dthind[which.max(check)]]) {
                            sync10f[dthind[which.max(check)]] = "HIGH*"
                            sync10f[dtlind[which.max(check)]] = "LOW*"
					
                        } else {
                            sync10f[dtlind[which.max(check)]] = "HIGH*"
                            sync10f[dthind[which.max(check)]] = "LOW*"
                        }
                    }
                    if (is.null(highestval1) && !is.null(highestval2) && !is.null(check)  ) {
                        if (numbers[dtlind[which.max(check)]] < numbers[dthind[which.max(check)]]) {
                            sync10f[dthind[which.max(check)]] = "HIGH*"
                            sync10f[dtlind[which.max(check)]] = "LOW*"
                        } else {
                            sync10f[dtlind[which.max(check)]] = "HIGH*"
                            sync10f[dthind[which.max(check)]] = "LOW*"
                        }
                    }
                    if (!is.null(highestval1) && is.null(highestval2) ) {
                        if (numbers[highestval1Pos1] < numbers[highestval1Pos2] && !is.null(highestval1) && !is.null(highestval2)) {
                            sync10f[highestval1Pos1] = "LOW*"
                            sync10f[highestval1Pos2] = "HIGH*"
                        } else {
                            sync10f[highestval1Pos1] = "HIGH*"
                            sync10f[highestval1Pos2] = "LOW*"
                        }
                    }
                    if (highestval1 > highestval2 && !is.null(highestval1) && !is.null(highestval2)) {
 			 if (numbers[highestval1Pos1] < numbers[highestval1Pos2] && !is.null(highestval1) && !is.null(highestval2)) {
                            sync10f[highestval1Pos1] = "LOW*"
                            sync10f[highestval1Pos2] = "HIGH*"
					
                        } else {
                            sync10f[highestval1Pos1] = "HIGH*"
                            sync10f[highestval1Pos2] = "LOW*"
                        }
                    }
                }
            }

            # Case 3 - When both marks are before the high.
            if (length(dthh) != 0) {
                if (dtm[1] < dthh && dtm[2] < dthh && !is.na(dtm[2]))
                {
                    r = 0
                    check <-c()
                    dthind <-c()
                    dtlind <-c()
                    i = 1
                    r = dtm[2] - dtm[1] - 1
                    for (a in seq(0, r, 1)) {
                        dtm = dtm2
                        first = dtm[1] + a - 1
                        check2 <-c()
                        while (dtm[1] + a < dtm[2]) {
                            res = as.double(numbers[dtm[1] + a], options = 12) - as.double(numbers[first], options = 12)
                            rest = res / as.double(numbers[dtm[1] + a], options = 12)
                            if(rest<0){
					check[i] = rest
                            dthind[i] = first
                            dtlind[i] = dtm[1] + a}
                            dtm[1] = dtm[1] + 1
                            i = i + 1
                        }
                    }

			# if (numbers[dthind[which.min(check)]] < numbers[dtlind[which.min(check)]]) {
                      if(!is.null(check)){
				  sync10f[dthind[which.min(check)]] = "HIGH*"
                        sync10f[dtlind[which.min(check)]] = "LOW*"}
                   # } else {
                       # sync10f[dtlind[which.min(check)]] = "HIGH*"
                        #sync10f[dthind[which.min(check)]] = "LOW*"
                    #}
                }
            }
        }
        lk = lk + 1
    }
    sync10f = fillRows(sync10f)

    return (sync10f) # Backfill output is returned.
}

# syncing10 function synchronizes moving average low to closing prices low.

syncing10<-function(newCol,low10,wc){
syn10<-c()
dt<-which(newCol[1:length(numbers)]=="LOW")
dt20<-which(low10[1:length(numbers)]=="LOW")
jh=1
newc<-c()
while(jh<length(dt20)+1)#dt20
{
kh=jh


wd=wc/2
wds=-wd
i=1			
				
				if(dt20[jh]>wds && dt20[jh]+wds >0 )
	              {
			
			mv<-(which(newCol[dt20[jh]+wds:wd] %in% c("LOW","LOW*"))+dt20[jh]+wds-1)-dt20[kh]
			if(length(mv)>0 & length(mv[mv>0])>0 & length(mv[mv<0])>0){
			if(numbers[mv[mv>0][1]+dt20[kh]]<numbers[mv[mv<0][length(mv[mv<0])]+dt20[kh]])
				{
				 syn10[mv[mv>0][1]+dt20[kh]]="LOW"
				}
			     else
				{
				syn10[mv[mv<0][length(mv[mv<0])]+dt20[kh]]="LOW"
				
				}
				}
			else{
			if(length(mv[mv>0])==length(mv) & !is.null(mv)){
			syn10[min(mv,na.rm=TRUE)+dt20[kh]]="LOW"
			}

			if(length(mv[mv<0])==length(mv) & !is.null(mv)){
			syn10[max(mv,na.rm=TRUE)+dt20[kh]]="LOW"
			
			
				}
			


			}
                	
			 }
	       if(dt20[jh]<wds && dt20[jh]-wds >0 )
	              {
			
			mv<-(which(newCol[dt20[jh]-wds:wd] %in% c("LOW","LOW*"))+dt20[jh]-wds-1)-dt20[kh]
			if(length(mv)>0 & length(mv[mv>0])>0 & length(mv[mv<0])>0){
			if(numbers[mv[mv>0][1]+dt20[kh]]<numbers[mv[mv<0][length(mv[mv<0])]+dt20[kh]])
				{
				 syn10[mv[mv>0][1]+dt20[kh]]="LOW"
				}
			     else
				{
				syn10[mv[mv<0][length(mv[mv<0])]+dt20[kh]]="LOW"
			
				}

			}
			else
			{
			if(length(mv[mv>0])==length(mv) && !is.null(mv)){
			syn10[min(mv,na.rm=TRUE)+dt20[kh]]="LOW"}

			if(length(mv[mv<0])==length(mv) & !is.null(mv)){
			syn10[max(mv,na.rm=TRUE)+dt20[kh]]="LOW"}
			}
				 }

	jh=jh+1
}
	syn10=fillRows(syn10)
	return(syn10)
}
# sync2080 function synchronizes moving average low to closing prices low.

sync2080<-function(newCol, low10, wc)
{
syn10<-c()
#dt<-which(newCol[1:length(numbers)]=="LOW")
dt <-which(newCol %in% c("LOW","LOW*"))
dt20<-which(low10[1:length(numbers)]=="LOW")
j=1
while(j<length(dt20)+1)
{
	 k=j
	 while(k<length(dt)+1)
		{
		if(dt20[j]>dt[k] & dt[k+1]>dt20[j] & !is.na(dt20[j]) & !is.na(dt[k]) & !is.na(dt[k+1]))
		  {
			if(numbers[dt[k]]<numbers[dt[k+1]])
				{
					syn10[dt[k]]="LOW"
				}
			else
				{
					syn10[dt[k+1]]="LOW"
				}
		  }
		k=k+1
		}

  j=j+1
}
return(syn10)
}

# syncingFor10 function synchronizes moving average low to closing prices low.
syncingFor10 <-function(newCol, low10, wc) {
    syn10 <-c()
    dt <-which(newCol[1: length(numbers)] == "LOW")
    dt20 <-which(low10[1: length(numbers)] == "LOW")
    jh = 1

    while (jh < length(dt20) + 1) {
        kh = jh
        minval <-c()
        minvalLoc <-c()

        wd = wc / 2
        wds = -wd
        i = 1
        if (dt20[jh] > wds && dt20[jh] + wds > 0) {
            minval <-which.min(numbers[which(newCol[dt20[jh] + wds: wd] == "LOW") + dt20[jh] + wds - 1])
            minvalLoc <-which(newCol[dt20[jh] + wds: wd] == "LOW") + dt20[jh] + wds - 1
            syn10[minvalLoc[minval]] = "LOW"

        }
        if (dt20[jh] < wds && dt20[jh] - wds > 0) {
            minval <-which.min(numbers[which(newCol[dt20[jh] - wds: wd] == "LOW") + dt20[jh] - wds - 1])
            minvalLoc < -which(newCol[dt20[jh] - wds: wd] == "LOW") + dt20[jh] - wds - 1
            syn10[minvalLoc[minval]] = "LOW"
        }

        jh = jh + 1
    }
    syn10 = fillRows(syn10)

    return (syn10) # It returns the sync lows for moving average (10, 20 & 80 days respectively).
}

# lowafterHigh function find the lowest point (closing price) between the low and high.

lowafterHigh <-function(syn10) {
	index10 <-which(syn10 %in% c("LOW", "HIGH"))
	idx = 1
	while(idx < length(index10)) {
		syn10[which.min(numbers[index10[idx]: index10[idx + 1]]) + index10[idx] - 1] = "LOW"
		idx = idx + 1
	}

	syn10 = fillRows(syn10)
	return (syn10)
}
# lowafterHighC function find highest value among two low's and also find lowest value among two high's 
lowafterHighC <- function(syn10, wc) {
	idx = 1
	lowInde <-which(syn10 %in% c("LOW"))
	while(idx < length(lowInde)) {
		if(abs(lowInde[idx] - lowInde[idx + 1]) < wc * 1.5 & length(lowInde[idx - 1]) > 0 & !is.na(lowInde[idx + 2])) {
			highSafter <-which(syn10[seq(lowInde[idx + 1], lowInde[idx + 2], 1)] %in% c("HIGH*", "HIGH")) + lowInde[idx + 1] - 1
			highSbefore <-which(syn10[seq(lowInde[idx - 1], lowInde[idx], 1)] %in% c("HIGH*", "HIGH")) + lowInde[idx - 1] - 1
			highPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("HIGH")) + lowInde[idx] - 1
			rs = which.min(numbers[seq(highPos, highSafter[1], 1)]) + highPos - 1
			ers = which(syn10[seq(highPos, highSafter[1], 1)] %in% c("LOW")) + highPos - 1
			if(rs != ers) {
				syn10[ers] = "  "
				syn10[rs] = "LOW"
			}else
             	 syn10[rs] = "LOW"
			rs1 = which.min(numbers[seq(highSbefore[length(highSbefore)], highPos, 1)]) + highSbefore[length(highSbefore)] - 1
			ers1 = which(syn10[seq(highSbefore[length(highSbefore)], highPos, 1)] %in% c("LOW")) + highSbefore[length(highSbefore)] - 1
			if(rs1 != ers1) {
				syn10[ers1] = "  "
				syn10[rs1] = "LOW"
			} else 
              		syn10[rs1] = "LOW"
		}
		if(abs(lowInde[idx] - lowInde[idx + 1]) > wc * 1.5 & !is.null(lowInde[idx] + (wc / 2))) {
			highSPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("HIGH*")) + lowInde[idx] - 1
			lowSPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("LOW*")) + lowInde[idx] - 1
			highPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("HIGH")) + lowInde[idx] - 1
			lowPos <-lowInde[idx]
			i2 = 1
			if(lowInde[idx] < highPos & highPos < lowInde[idx] + (wc / 2)) {
				if(length(highSPos) >= i2) {
					rs = which.min(numbers[seq(highPos, highSPos[1], 1)]) + highPos - 1
					Ers = which(syn10[seq(highPos, highSPos[1], 1)] %in% c("LOW*")) + highPos - 1
					if(length(Ers) == 0)
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						syn10[rs] = "LOW*"
					} else
                     		 syn10[rs] = "LOW*"
					while(length(highSPos) > i2) {
						highPos = highSPos[i2]
						highSPosq = highSPos[i2 + 1]
						rs = which.min(numbers[seq(highPos, highSPosq, 1)]) + highPos - 1
						Ers = which(syn10[seq(highPos, highSPosq, 1)] %in% c("LOW*")) + highPos - 1
						if(length(Ers) == 0)
                         		 Ers = rs
						if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
							syn10[Ers] = " "
							syn10[rs] = "LOW*"
						} else 
                        		  syn10[rs] = "LOW*"
						i2 = i2 + 1
					}
				}
				i3 = 1
				lowSPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("LOW*")) + lowInde[idx] - 1
				if(length(lowSPos) >= i3) {
					rs = which.max(numbers[seq(lowPos, lowSPos[i3], 1)]) + lowPos - 1
					Ers = which(syn10[seq(lowPos, lowSPos[i3], 1)] %in% c("HIGH*", "HIGH")) + lowPos - 1
					if(length(Ers) == 0) 
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						syn10[rs] = "HIGH"
					} else 
                    		  syn10[rs] = "HIGH"
					while(length(lowSPos) > i3) {
						lowPos = lowSPos[i3]
						lowSPosq = lowSPos[i3 + 1]
						rs = which.max(numbers[seq(lowPos, lowSPosq, 1)]) + lowPos - 1
						Ers = which(syn10[seq(lowPos, lowSPosq, 1)] %in% c("HIGH*", "HIGH")) + lowPos - 1
						if(length(Ers) == 0)
                        			  Ers = rs
						if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
							syn10[Ers] = " "
							syn10[rs] = "HIGH*"
						} else
                          		syn10[rs] = "HIGH*"
						i3 = i3 + 1
					}
					rs = which.max(numbers[seq(lowSPos[i3], lowInde[idx + 1], 1)]) + lowSPos[i3] - 1
					Ers = which(syn10[seq(lowSPos[i3], lowInde[idx + 1], 1)] %in% c("HIGH*", "HIGH")) + lowSPos[i3] - 1
					if(length(Ers) == 0) 
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						syn10[rs] = "HIGH*"
					} else 
                     		 syn10[rs] = "HIGH*"
				}
			}
			highPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("HIGH")) + lowInde[idx] - 1
			if(lowInde[idx] + (wc / 2) < highPos & highPos < lowInde[idx + 1] - (wc / 2)) {
				lowSPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("LOW*")) + lowInde[idx] - 1
				i3 = 1
				if(length(lowSPos) >= i3) {
					rs = which.max(numbers[seq(lowPos, lowSPos[i3], 1)]) + lowPos - 1
					Ers = which(syn10[seq(lowPos, lowSPos[i3], 1)] %in% c("HIGH*", "HIGH")) + lowPos - 1
					if(length(Ers) == 0)
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						if(rs == highPos) 
                          			syn10[rs] = "HIGH"
						else 
                         			 syn10[rs] = "HIGH*"
					} else {
						if(rs == highPos) 
                         			 syn10[rs] = "HIGH"
						else 
                         			 syn10[rs] = "HIGH*"
					}
					while(length(lowSPos) > i3) {
						lowPos = lowSPos[i3]
						lowSPosq = lowSPos[i3 + 1]
						rs = which.max(numbers[seq(lowPos, lowSPosq, 1)]) + lowPos - 1
						Ers = which(syn10[seq(lowPos, lowSPosq, 1)] %in% c("HIGH*", "HIGH")) + lowPos - 1
						if(length(Ers) == 0)
                          			Ers = rs
						if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
							syn10[Ers] = " "
							if(rs == highPos) 
                            				  syn10[rs] = "HIGH"
							else 
                            				  syn10[rs] = "HIGH*"
						} else {
							if(rs == highPos)
                             				 syn10[rs] = "HIGH"
							else 
                             				 syn10[rs] = "HIGH*"
						}
						i3 = i3 + 1
					}
					rs = which.max(numbers[seq(lowSPos[i3], lowInde[idx + 1], 1)]) + lowSPos[i3] - 1
					Ers = which(syn10[seq(lowSPos[i3], lowInde[idx + 1], 1)] %in% c("HIGH*", "HIGH")) + lowSPos[i3] - 1
					if(length(Ers) == 0)
                    			 Ers = rs
					Ers = Ers[which.max(numbers[Ers])]
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						if(rs == highPos) 
                        			  syn10[rs] = "HIGH"
						else 
                        			  syn10[rs] = "HIGH*"
					} else {
						if(rs == highPos) 
                         			 syn10[rs] = "HIGH"
						else 
                          			syn10[rs] = "HIGH*"
					}
				}
				highSPos <-which(syn10[seq(lowInde[idx] + (wc / 2), lowInde[idx + 1], 1)] %in% c("HIGH*", "HIGH")) + lowInde[idx] + (wc / 2) - 1
				i2 = 1
				while(i2 < length(highSPos)) {
					rs = which.min(numbers[seq(highSPos[i2], highSPos[i2 + 1], 1)]) + highSPos[i2] - 1
					Ers = which(syn10[seq(highSPos[i2], highSPos[i2 + 1], 1)] %in% c("LOW*")) + highSPos[i2] - 1
					if(length(Ers) == 0)
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						syn10[rs] = "LOW**"
					} else 
                    			  syn10[rs] = "LOW*"
					i2 = i2 + 1
				}
			}
			highPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("HIGH")) + lowInde[idx] - 1
			if(lowInde[idx] + (wc / 2) < highPos & highPos > lowInde[idx + 1] - (wc / 2)) {
				lowSPos <-which(syn10[seq(lowInde[idx], lowInde[idx + 1], 1)] %in% c("LOW*")) + lowInde[idx] - 1
				i3 = 1
				if(length(lowSPos) >= i3) {
					rs = which.max(numbers[seq(lowPos, lowSPos[i3], 1)]) + lowPos - 1
					Ers = which(syn10[seq(lowPos, lowSPos[i3], 1)] %in% c("HIGH*", "HIGH")) + lowPos - 1
					if(length(Ers) == 0)
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						syn10[rs] = "HIGH*"
					} else 
                     			 syn10[rs] = "HIGH*"
					while(length(lowSPos) > i3) {
						lowPos = lowSPos[i3]
						lowSPosq = lowSPos[i3 + 1]
						rs = which.max(numbers[seq(lowPos, lowSPosq, 1)]) + lowPos - 1
						Ers = which(syn10[seq(lowPos, lowSPosq, 1)] %in% c("HIGH*", "HIGH")) + lowPos - 1
						if(length(Ers) == 0)
                         			 Ers = rs
						if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
							syn10[Ers] = " "
							syn10[rs] = "HIGH*"
						} else 
                       			     syn10[rs] = "HIGH*"
						i3 = i3 + 1
					}
					rs = which.max(numbers[seq(lowSPos[i3], lowInde[idx + 1], 1)]) + lowSPos[i3] - 1
					Ers = which(syn10[seq(lowSPos[i3], lowInde[idx + 1], 1)] %in% c("HIGH*", "HIGH")) + lowSPos[i3] - 1
					if(length(Ers) == 0)
                     			 Ers = rs
					if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
						syn10[Ers] = " "
						syn10[rs] = "HIGH"
					} else 
                     			 syn10[rs] = "HIGH"
				}
				i2 = 1
				highSPos <-which(syn10[seq(lowInde[idx] + (wc / 2), lowInde[idx + 1], 1)] %in% c("HIGH*")) + lowInde[idx] + (wc / 2) - 1
				if(length(highSPos) >= i2) {
					while(length(highSPos) > i2) {
						highPosn = highSPos[i2]
						highSPosq = highSPos[i2 + 1]
						rs = which.min(numbers[seq(highPosn, highSPosq, 1)]) + highPosn - 1
						Ers = which(syn10[seq(highPosn, highSPosq, 1)] %in% c("LOW*")) + highPosn - 1
						if(length(Ers) == 0) 
                         			 Ers = rs
						if(rs != Ers & length(rs) > 0 & length(Ers) > 0) {
							syn10[Ers] = " "
							syn10[rs] = "LOW*"
						} else 
                         			 syn10[rs] = "LOW*"
						i2 = i2 + 1
					}
				}
			}
		}
		idx = idx + 1
	}
	syn10 = fillRows(syn10)
	return (syn10)
}
deleteHigh <-function(syn10) {
	index10 <-which(syn10 %in% c("LOW", "HIGH","LOW*","HIGH*"))
	idx = 1
	while(idx < length(index10)) {
			if(syn10[index10[idx]]==syn10[index10[idx+1]] & syn10[index10[idx]]=="HIGH" )
				{
				loc<-c(index10[idx],index10[idx+1])
				maxres<-loc[which.max(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				minres<-loc[which.min(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				syn10[maxres]=syn10[maxres]
				syn10[minres]=""

				}
			if(syn10[index10[idx]]==syn10[index10[idx+1]] & syn10[index10[idx]]=="HIGH*" )
				{
				loc<-c(index10[idx],index10[idx+1])
				maxres<-loc[which.max(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				minres<-loc[which.min(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				syn10[maxres]=syn10[maxres]
				syn10[minres]=""

				}

			if(syn10[index10[idx+1]]=="HIGH*" & syn10[index10[idx]]=="HIGH" )
				{
				loc<-c(index10[idx],index10[idx+1])
				maxres<-loc[which.max(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				minres<-loc[which.min(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				syn10[maxres]=syn10[maxres]
				syn10[minres]=""
				}

			if(syn10[index10[idx+1]]=="HIGH" & syn10[index10[idx]]=="HIGH*" )
				{
				loc<-c(index10[idx],index10[idx+1])
				maxres<-loc[which.max(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				minres<-loc[which.min(c(numbers[index10[idx]],numbers[index10[idx+1]]))]
				syn10[maxres]=syn10[maxres]
				syn10[minres]=""

				}

		idx = idx + 1
	}

	syn10 = fillRows(syn10)
	return (syn10)
}

# backfilling Function is invoked when the distance between two consecutive lows is more than 1.5 * window_cycle 
# Applicable for moving_average based on 20 & 80 days (window_cycle - 80days and 320 days respectively).

backfilling2080<-function(wc,synccol,low){
sync10f<-c()
sync10=synccol
dtl<-which(low %in% c("LOW","LOW*"))
lk=1
	while(lk < length(dtl) + 1) {
		if(dtl[lk + 1] - dtl[lk] > 1.5 * wc && !is.na(dtl[lk + 1])) {
			fh <-c(which(low %in% c("HIGH","HIGH*")))
			f <-c(which(sync10 %in% c("LOW", "HIGH","LOW*","HIGH*")))
			m <-c(which(low %in% c("mark")))
			i = 1
			while(i < length(m)) {
				j = 1
				while(j < length(fh)) {
					if(length(m[m[i] < fh[j] & fh[j] < m[i + 1]]) > 0) {
						z = f[f > m[i] & f < fh[j]] # mark to high
						if(length(z) > 0) {
							kj = 1
							while(kj < length(z)) {
								if(sync10[z[kj]] == "HIGH" || sync10[z[kj]] == "HIGH*") {
									break
								}
								kj = kj + 1
							}
							value1 <-c(z[kj: length(z)])
							j1 = 1
							k1 = 1
							ans1 <-c()
							loc <-c()
							while(j1 < length(value1)) {
								ans1[k1] = abs((numbers[value1[j1 + 1]] - numbers[value1[j1]]) / numbers[value1[j1]])
								loc[k1] = j1
								j1 = j1 + 2
								k1 = k1 + 1
							}
							sync10f[value1[loc[which.max(ans1)]]] = sync10[value1[loc[which.max(ans1)]]]
							sync10f[value1[loc[which.max(ans1)] + 1]] = sync10[value1[loc[which.max(ans1)] + 1]]
						}
						z1 = f[f < m[i + 1] & f > fh[j]] #high to mark
						if(length(z1) > 0) {
							kjk = 1
							while(kjk < length(z1)) {
								if(sync10[z1[kjk]] == "LOW" || sync10[z1[kjk]] == "LOW*") {
									break
								}
								kjk = kjk + 1
							}
							value2 <-c(z1[kjk: length(z1)])
							j2 = 1
							k2 = 1
							ans2 <-c()
							loc2 <-c()
							while(j2 < length(value2)) {
								ans2[k2] = abs((numbers[value2[j2 + 1]] - numbers[value2[j2]] )/ numbers[value2[j2]])
								loc2[k2] = j2
								j2 = j2 + 2
								k2 = k2 + 1
							}
							sync10f[value2[loc2[which.max(ans2)]]] = sync10[value2[loc2[which.max(ans2)]]]
							sync10f[value2[loc2[which.max(ans2)] + 1]] = sync10[value2[loc2[which.max(ans2)] + 1]]
						}
					}
					j = j + 1
				}
				i = i + 2
			}
			#end of high mark
			if(dtl[lk] - dtl[lk + 1] > 1.5 * wc) {
				z = f[f < dtl[lk + 1] - 0.5 * wc & f > dtl[lk] + 0.5 * wc]

				zh = fh[fh > dtl[lk] & fh < dtl[lk] + 0.5 * wc]
				if(length(zh) > 0) {
					i = 1
					while(i < length(z)) {
						if(sync10[z[i]] == "LOW" ||sync10[z[i]] == "LOW*" ) {
							break
						}
						i = i + 1
					}
					value <-c(z[i: length(z)])
					j = 1
					k = 1
					ans <-c()
					while(j < length(value)) {
						ans[k] = abs((numbers[value[j + 1]] - numbers[value[j]]) / numbers[value[j]])
						j = j + 2
						k = k + 1
					}
					
					sync10f[value[which.max(ans)]] = sync10[value[which.max(ans)]]
					sync10f[value[which.max(ans) + 1]] = sync10[value[which.max(ans) + 1]]
				}
			}
			pra = fh[fh > dtl[lk + 1] - 0.5 * wc & fh < dtl[lk + 1]] # mark high low
			hz = f[f < dtl[lk + 1] - 0.5 * wc & f > dtl[lk] + 0.5 * wc]
			
			
			if(length(pra) > 0) {
				i = 1
				while(i < length(hz)) {
					if(sync10[hz[i]] == "HIGH" || sync10[hz[i]] == "HIGH*" ) {
						break
					}
					i = i + 1
				}
				value <-c(hz[i: length(hz)])
				j = 1
				k = 1
				ans <-c()
				loc <-c()
				while(j < length(value)) {
					ans[k] = abs((numbers[value[j + 1]] - numbers[value[j]]) / numbers[value[j]])
					loc[k] = j
					j = j + 2
					k = k + 1
				}
				sync10f[value[loc[which.max(ans)]]] = sync10[value[loc[which.max(ans)]]]
				sync10f[value[loc[which.max(ans)] + 1]] = sync10[value[loc[which.max(ans)] + 1]]
			}
		}
		lk = lk + 1
	}

sync10f= fillRows(sync10f)

return(sync10f)
}

# Functions invoked for moving_average 10 days (window_cycle - 20 days).

syn10<-c() 
syn80<-c()
sync10<-c()
syn20f<-c()
syn80f<-c()
syn10=syncingFor10(newCol,low10,20)

syn10=lowdel(syn10,20)
syn10=high(syn10)
syn10=lowafterHigh(syn10)
syn10=lowdel(syn10,20)
syn10=high(syn10)
dtkl<-which(syn10[1:length(numbers)]=="LOW")
dtkh<-which(syn10[1:length(numbers)]=="HIGH")
sync10[dtkl]="LOW"
sync10[dtkh]="HIGH"

syn10=markfilling(syn10,20)
syn10=backfilling(20,syn10)
dtkl2<-which(syn10[1:length(numbers)]=="LOW*")
dtkh2<-which(syn10[1:length(numbers)]=="HIGH*")
sync10[dtkl2]="LOW*"
sync10[dtkh2]="HIGH*"
sync10=lowafterHighC(sync10,20)
sync10=lowdel(sync10,20)
sync10=high(sync10)



syn101=markfilling(sync10,20)
syn101=backfilling(20,syn101)
dtkl3<-which(syn101[1:length(numbers)]=="LOW*")
dtkh3<-which(syn101[1:length(numbers)]=="HIGH*")
sync10[dtkl3]="LOW*"
sync10[dtkh3]="HIGH*"
sync10=lowafterHighC(sync10,20)
sync10=lowdel(sync10,20)
sync10=high(sync10)
#samp=sync10



syn102=markfilling(sync10,20)
syn102=backfilling(20,syn102)
dtkl4<-which(syn102[1:length(numbers)]=="LOW*")
dtkh4<-which(syn102[1:length(numbers)]=="HIGH*")
sync10[dtkl4]="LOW*"
sync10[dtkh4]="HIGH*"
sync10=lowafterHighC(sync10,20)
sync10=lowdel(sync10,20)
sync10=high(sync10)
sync10=deleteHigh(sync10) 



syn103=markfilling(sync10,20)
syn103=backfilling(20,syn102)
dtkl5<-which(syn103[1:length(numbers)]=="LOW*")
dtkh5<-which(syn103[1:length(numbers)]=="HIGH*")
sync10[dtkl5]="LOW*"
sync10[dtkh5]="HIGH*"
sync10=fillRows(sync10)
sync10=lowafterHighC(sync10,20)
sync10=lowdel(sync10,20)
sync10=high(sync10)
sync10=deleteHigh(sync10) 

# Functions invoked for moving_average 20 days. (window_cycle - 80 days).
syn20=sync2080(sync10,low20,80)
syn20=lowdel(syn20,80)
syn20=fillRows(syn20)
syn20=high(syn20)
syn20=lowafterHigh(syn20)
syn20=lowdel(syn20,80)
syn20=high(syn20)
syn20=deleteHigh(syn20) 



sync20k<-c()
dtkl20<-c()
dtkh20<-c()
dtkl20<-which(syn20[1:length(numbers)]=="LOW")
dtkh20<-which(syn20[1:length(numbers)]=="HIGH")

sync20k[dtkl20]="LOW"
sync20k[dtkh20]="HIGH"
sync20k=fillRows(sync20k)

# Backfill function is invoked
sync20k=markfilling(sync20k,80)
syn20f=backfilling2080(80,sync10,sync20k)
dtkl201<-c()
dtkh201<-c()

dtkl201<-which(syn20f[1:length(numbers)]=="LOW")
dtkh201<-which(syn20f[1:length(numbers)]=="HIGH")
sync20k[dtkl201]="LOW*"
sync20k[dtkh201]="HIGH*"
sync20k=fillRows(sync20k)
sync20k[which(sync20k[1:length(numbers)]=="mark")]=""
sync20k[intersect(dtkl20,dtkl201)]="LOW"
sync20k[intersect(dtkh20,dtkh201)]="HIGH"
sync20k=deleteHigh(sync20k) 
sync20k=lowafterHighC(sync20k,80)
sync20k=lowdel(sync20k,80)
sync20k=high(sync20k)
sync20k=deleteHigh(sync20k) 



# Backfill function is invoked

sync20k=markfilling(sync20k,80)
syn20ff=backfilling2080(80,sync10,sync20k)
dtkl202<-c()
dtkh202<-c()
dtkl2021<-c()
dtkh2021<-c()
dtkl202<-which(syn20ff[1:length(numbers)]=="LOW")
dtkh202<-which(syn20ff[1:length(numbers)]=="HIGH")
dtkl2021<-which(syn20ff[1:length(numbers)]=="LOW*")
dtkh2021<-which(syn20ff[1:length(numbers)]=="HIGH*")
sync20k[dtkl202]="LOW"
sync20k[dtkh202]="HIGH"
sync20k[dtkl2021]="LOW*"
sync20k[dtkh2021]="HIGH*"
sync20k[intersect(dtkl20,dtkl2021)]="LOW"
sync20k[intersect(dtkh20,dtkh2021)]="HIGH"
sync20k=fillRows(sync20k)


sync20k=lowafterHighC(sync20k,80)
sync20k=lowdel(sync20k,80)
sync20k=high(sync20k)
sync20k=deleteHigh(sync20k) 
sync20k[which(sync20k[1:length(numbers)]=="mark")]=""
sync20k=fillRows(sync20k)


# Functions invoked for moving_average 80 days. (window_cycle - 320 days).
syn80=sync2080(sync20k,low80,320)
syn80=lowdel(syn80,320)
syn80=high(syn80)
syn80=lowafterHigh(syn80)
syn80=lowdel(syn80,320)
syn80=high(syn80)


sync80<-c()
dtkl80<-c()
dtkh80<-c()
dtkl80<-which(syn80[1:length(numbers)]=="LOW")
dtkh80<-which(syn80[1:length(numbers)]=="HIGH")
sync80[dtkl80]="LOW"
sync80[dtkh80]="HIGH"


# Backfill function is invoked
syn80=markfilling(syn80,320)
syn80f=backfilling2080(320,sync20k,syn80)
dtkl201<-which(syn80f[1:length(numbers)]=="LOW")
dtkh201<-which(syn80f[1:length(numbers)]=="HIGH")
sync80[dtkl201]="LOW*"
sync80[dtkh201]="HIGH*"
sync80=fillRows(sync80)
sync80[which(sync80[1:length(numbers)]=="mark")]=""
sync80=lowafterHighC(sync80,320)
sync80=lowdel(sync80,320)
sync80=high(sync80)
sync80=deleteHigh(sync80) 

sync80=fillRows(sync80)
Turns_10D_Cycle=sync10
Turns_20D_Cycle=sync20k
Turns_80D_Cycle=sync80

# Main Variable defined for function cbind where it is invoked and all the output is stored.
bind<-cbind(data,ma10,newCol,low10,Turns_10D_Cycle,ma20,low20,Turns_20D_Cycle,ma80,low80,Turns_80D_Cycle)


# Append file to csv.
write.csv(bind,"output_etha.csv")