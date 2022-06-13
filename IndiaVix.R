#Setting the wd
# setwd('/home/sanjayashetty/Personal not your Shit/Projects/GithubStuff/IndiaVIXinR')

library(dplyr)

# time = (x + 930 + y) / 525600

# x = time till midnight, y = time till the day of expiry

# India 3-Month MIBOR from standard chartered 4.51%
# url = https://www.sc.com/in/business/external-benchmark-for-lending-facilities/


# For futures get Data from NSE

# get the data from NIFTY 50 website. 
# columns should contain 
# Strike | Call Bid | Call Ask | Put Ask | Put Bid

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



