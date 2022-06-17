---
title: "Implementing INDIAVIX in R"
author: "Sanjaya J Shetty"
date: '2022-06-15'
output:
  html_document:
    keep_md: yes
---

### Calculating India Vix from scratch using R.

We would be referring to the [NSE Website](https://www1.nseindia.com/content/indices/white_paper_IndiaVIX.pdf) for calculating the India Vix Index.

The formula for calculating the Expected Volatility:

$$\sigma^2 = \frac{2}{T} \sum \frac{\Delta K_i} {K_i^2} e^{RT} Q(K_i) - \frac{1} {T}[\frac{F} {K_0} - 1]^2$$

$\sigma^2$ -\> Expected Volatility

$T$ -\> Time to expiry

$T$ -\> $\frac{M_{current day} +M_{settlement day} + M_{other day}}{M_{year}}$

$M_{current day}$ -\> number of minutes remaining till midnight on the current day

$M_{settlement day}$ -\> Number of minutes from midnight until closing hours of trading on expiry day

$M_{other day}$ -\> Total number of minutes in the days between current day and expiry day excluding both the days

$M_{year}$ -\> number of minutes in a year

$K_i$ -\> Strike price of $i^{th}$ out-of-the-money option; a call if $K_i > F$ and a put if $K_i < F$

$\Delta K_i$ -\> Interval between strike prices- half the distance between the strike on either side of $K_i$:

$$\Delta K_i = \frac{K_{i+1} - K_{i-1}}{2} $$

$R$ -\> Risk-free interest rate to expiration (we will use MIBOR)

$Q(K_i)$ -\> Midpoint of the bid ask quote for each option contract with strike $K_i$

$F$ -\> NIFTY50 Futures prices

$K_0$ -\> First strike below the forward index level, F


```r
# loading the necessary libraries

library(dplyr)
```

Let's start by creating functions to help to get the necessary results. The options chain data is downloaded from the NSE website [NSE Options Chain](https://www.nseindia.com/option-chain)

The required columns for the analysis are

Strike \| Call Bid \| Call Ask \| Put Ask \| Put Bid


```r
# function to get the necessary columns. 

cleaningData = function(data){
  reqCol = c('BID.PRICE', 'STRIKE.PRICE','ASK.PRICE')
  data = select(data,matches(reqCol))
  colnames(data) = c('callBid','putBid','strike','callAsk','putAsk' )
  
  return(data)
}
```

Since the prices are character with commas, we need to convert it to numeric form for further calculation.


```r
# Converting all the string character to numeric 

numConverer = function(data){
  
  data = lapply(data, readr::parse_number)
  data = lapply(data, as.numeric)
  
  return(as.data.frame(data))
}
```

#### Functions to calculate mid-quote of all strike prices.


```r
# Getting the mid-quote

midQuote <-  function(data, ATMOptionPrice){
  # Calculating mid points of all options
  data$callMid = (data$callAsk + data$callBid)/2
  data$putMid = (data$putAsk + data$putBid)/2
  
  # Calculating Spreads
  # Spread = (Ask – Bid) / Average of Bid and Ask
  
  data$callSpread = (data$callAsk - data$callBid)/((data$callAsk + data$callBid)/2)*100
  data$putSpread = (data$putAsk - data$putBid)/((data$putAsk + data$putBid)/2)*100
  
  # getting cubic spline interpolations for all the mid-price of options 
  
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
  
  # extracting the midpoints of OTM options
  data$midPoint = if_else(data$callMid == 0, data$putMid, data$callMid)

  # midpoint for the ATM option will be the average between call and options mid quote  
  num = which(data$strike == ATMOptionPrice)
  data$midPoint[num] = (data$putMid[num] + data$callMid[num])/2
  
  
  return(data)
}
```

#### Calculating sigma square (Expected Volatility)


```r
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
```

#### Getting the sigma value for the near month options


```r
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
```

#### Getting the sigma value for the next month options


```r
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
```

#### Calculating INDIAVIX.

Formula calculating INDIAVIX using Volatilities

$$\sigma = \sqrt{\{T_1 \sigma_1^2\}[\frac{N_{T_2} - N_{30}]}{N_{T_2} -N_{T_1}} +\frac{N_{30} - N_{T_1}]}{N_{T_2} -N_{T_1}} \times \frac{N_{365}}{N_{30}}} $$

$N_{T_1}$ -\> number of minutes to expiration of the near month options

$N_{T_2}$ -\> number of minutes to expiration of the next month options

$N_{30}$ -\> number of minutes in 30 days) =43200

$N_{365}$ -\> number of minutes in a 365-day year

$\sigma -> \frac{INDIAVIX}{100}$


```r
#Calculating IndiaVix

N30 = 43200
N1Yr = 525600

NCon1 = (NT2 - N30)/(NT2 -NT1) 
NCon2 = (N30 - NT1)/(NT2 -NT1)

term = t1*nearMonthSigma*NCon1 + t2*nextMonthSigma*NCon2

IndiaVix = sqrt(term*(N1Yr/N30))

sprintf('The India VIX on JUNE 15 2022 is %.2f', IndiaVix*100)
```

```
## [1] "The India VIX on JUNE 15 2022 is 21.99"
```
