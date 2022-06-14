#Setting the wd
 setwd('/home/sanjayashetty/Personal not your Shit/Projects/GithubStuff/IndiaVIXinR')

library(dplyr)

# time = (x + 930 + y) / 525600

# x = time till midnight, y = time till the day of expiry

# India 1-Month MIBOR from standard chartered 4.51%
# url = https://www.sc.com/in/business/external-benchmark-for-lending-facilities/

# For futures get Data from NSE

# get the data from NIFTY 50 website. 
# columns should contain 
# Strike | Call Bid | Call Ask | Put Ask | Put Bid

# June 13 2022
 
# loading the required Options data 
data = read.csv('data/option-chain-ED-NIFTY-30-Jun-2022.csv', header = F)

# Cleaning the data
data = data[-1,-1] 
names = data[1,]
names = as.character(as.vector(names))
colnames(data) = names
data = data[-1,]
reqCol = c('BID PRICE', 'STRIKE PRICE','ASK PRICE')
data = data[names %in% reqCol]
colnames(data) = c('callBid', 'callAsk','strike','putBid','putAsk' )
row.names(data) = NULL

head(data)

data = data %>%
  mutate_at(1:5, readr::parse_number)

data = as.data.frame(lapply(data, as.numeric))

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

# Assign the future price here. 
# Closing price of Future = 15742 on JUN 14 2022
ATMOptionPrice = 15700 #Check the proper NIFTY50 FUT value later 


data$callMid = if_else((data$strike >= ATMOptionPrice), 
                       if_else(is.na(data$callSpread) | data$callSpread > 30,
                               data$callSpline,data$callMid),0 )

data$putMid = if_else((data$strike <= ATMOptionPrice), 
                      if_else(is.na(data$putSpread) | data$putSpread > 30,
                              data$putSpline, data$putMid), 0)

data$midPoint = if_else(data$callMid == 0, data$putMid, data$callMid)

num = which(data$strike == ATMOptionPrice)

data$midPoint[num] = (data$putMid[num] + data$callMid[num])/2

date1 = as.POSIXlt('2022-06-14T15:30:00.000',format="%Y-%m-%dT%H:%M:%S")
date2 = as.POSIXlt('2022-06-30T15:30:00.000',format="%Y-%m-%dT%H:%M:%S")


timeDiff = difftime(date2, date1, units = 'min')

t = as.double.difftime(timeDiff/525600)

r = 0.0451 #1-Month MIBOR

F = 15742
