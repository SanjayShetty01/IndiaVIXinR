#Setting the wd
setwd('/home/sanjayashetty/Personal not your Shit/Projects/GithubStuff/IndiaVIXinR')

library(dplyr)

# India MIBOR from standard chartered 4.51%
# url = https://www.sc.com/in/business/external-benchmark-for-lending-facilities/

# For futures get Data from NSE

# get the data from NIFTY 50 website. 
# columns should contain 
# Strike | Call Bid | Call Ask | Put Ask | Put Bid

# June 14 2022
 

# Cleaning the data

cleaningData = function(data){
  reqCol = c('BID.PRICE', 'STRIKE.PRICE','ASK.PRICE')
  data = select(data,matches(reqCol))
  colnames(data) = c('callBid','putBid','strike','callAsk','putAsk' )
  
  return(data)
}

# Converting all the string character to numeric 
numConverer = function(data){
  
  data = lapply(data, readr::parse_number)
  data = lapply(data, as.numeric)
  
  return(as.data.frame(data))
}

# Getting the mid-quote
midQuote <-  function(data, ATMOptionPrice){
  # Calculating mid points
  data$callMid = (data$callAsk + data$callBid)/2
  data$putMid = (data$putAsk + data$putBid)/2
  
  # Calculating Spreads
  # Spread = (Ask – Bid) / Average of Bid and Ask
  
  data$callSpread = (data$callAsk - data$callBid)/((data$callAsk + data$callBid)/2)*100
  data$putSpread = (data$putAsk - data$putBid)/((data$putAsk + data$putBid)/2)*100
  
  # using cubic spline interpolations
  
  data$callSpline = spline(data$strike, data$callMid, 
                           method = "natural", n = nrow(data))[['y']]
  data$putSpline = spline(data$strike, data$putMid, 
                          method = "natural", n = nrow(data))[['y']]
  
  # assigning spline values for not appropriate (i.e spread > 30) and missing data

  data$callMid = if_else((data$strike >= ATMOptionPrice), 
                         if_else(is.na(data$callSpread) | data$callSpread > 30,
                                 data$callSpline,data$callMid),0 )
  
  data$putMid = if_else((data$strike <= ATMOptionPrice), 
                        if_else(is.na(data$putSpread) | data$putSpread > 30,
                                data$putSpline, data$putMid), 0)
  
  data$midPoint = if_else(data$callMid == 0, data$putMid, data$callMid)
  
  num = which(data$strike == ATMOptionPrice)
  
  data$midPoint[num] = (data$putMid[num] + data$callMid[num])/2
  
  
  return(data)
}

sigmaSqCalc = function(data,r, F,t,ATMOptionPrice){

  # Calculating sigma square.
  
  data$deltaK = (lead(data$strike) - lag(data$strike))/2

  wMaxStrike = which(data$strike == max(data$strike))
  wMinStrike = which(data$strike == min(data$strike))

  data$deltaK[wMinStrike] = abs(data$strike[wMinStrike] - data$strike[wMinStrike + 1])
  data$deltaK[wMaxStrike] = abs(data$strike[wMaxStrike] - data$strike[wMaxStrike - 1])

  data$deltaKbyK = (data$deltaK/data$strike^2)*exp(r*t)

  sumedStuff = sum(data$deltaKbyK*data$midPoint)
  const = (1/t)*(((F/ATMOptionPrice) - 1)^2)
  
  sigma = (2/t)*sumedStuff - const

return(sigma)
}


#-------------------------------------------------------------------------------

# Current Month Calculations

nearMonth = read.csv('data/option-chain-ED-NIFTY-30-Jun-2022.csv',  skip = 1)

# Assign the future price here. 
# Closing price of Future = 15701.0 on JUN 15 2022
ATMOptionPrice1 = 15700 #Check the proper NIFTY50 FUT value later 


date1 = as.POSIXlt('2022-06-14T15:30:00.000',format="%Y-%m-%dT%H:%M:%S")
date2 = as.POSIXlt('2022-06-30T15:30:00.000',format="%Y-%m-%dT%H:%M:%S")


NT1 = as.double.difftime(difftime(date2, date1, units = 'min'))

t1 = as.double.difftime(NT1/525600)

r1 = 0.0451 #1-Month MIBOR

F1 = 15701.0


nearMonthSigma = nearMonth %>%
                  cleaningData() %>%
                  numConverer() %>%
                  midQuote(.,ATMOptionPrice1) %>%
                  sigmaSqCalc(., r1, F1, t1, ATMOptionPrice1)

nearMonthSigma
#-------------------------------------------------------------------------------
# Next Month Calculations

nextMonth = read.csv('data/option-chain-ED-NIFTY-28-Jul-2022.csv', skip = 1)

# Assign the future price here. 
# Closing price of Future = 15722 on JUN 15 2022
ATMOptionPrice2 = 15700 #Check the proper NIFTY50 FUT value later 


date3 = as.POSIXlt('2022-06-14T15:30:00.000',format="%Y-%m-%dT%H:%M:%S")
date4 = as.POSIXlt('2022-07-28T15:30:00.000',format="%Y-%m-%dT%H:%M:%S")


NT2 = as.double.difftime(difftime(date4, date3, units = 'min'))

t2 = as.double.difftime(NT1/525600)

r2 = 0.0465 #3-Month MIBOR

F2 = 15722


nextMonthSigma = nextMonth %>%
                  cleaningData() %>%
                  numConverer() %>%
                  midQuote(.,ATMOptionPrice1) %>%
                  sigmaSqCalc(., r2, F2, t2, ATMOptionPrice2)


nextMonthSigma


N30 = 43200
N1Yr = 525600

NCon1 = (NT2 - N30)/(NT2 -NT1) 
NCon2 = (N30 - NT1)/(NT2 -NT1)

term = t1*nearMonthSigma*NCon1 + t2*nextMonthSigma*NCon2

IndiaVix = sqrt(term*(N1Yr/N30))

sprintf('The India VIX on JUNE 15 2022 is %.2f', IndiaVix*100)

