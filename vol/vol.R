install.packages("zoo")
install.packages("PerformanceAnalytics")
install.packages("quantmod")

library(tidyverse)
library(zoo) 
library(PerformanceAnalytics)
library(quantmod)

Sys.setenv(TZ = 'utc')

price_Q = read.csv("E:/factor/vol/price_Q.csv", row.names = 1, header = TRUE)
price_Q[is.na(price_Q)] = 0  
ro = dim(price_Q)[1]          
co = dim(price_Q)[2] 

vol_Q = read.csv("E:/factor/vol/vol_Q.csv", row.names=1, header=TRUE)
vol_Q[is.na(vol_Q)] = 0
vol_Q[vol_Q == 0] = NA
vol_Q[price_Q == 0 ] = NA  #가격이 없는데 per 값이 있는 곳이 있다

price_Q[is.na(vol_Q)] = 0

ret_price_Q = matrix(0,ro,co)
for( i in 1: co){ret_price_Q[,i] = Delt((price_Q[,i]))}

write.csv(ret_price_Q, file = "E:/factor/vol/vol1.csv")
# kospi 
price_kospi = getSymbols('^KS11', from = '2000-01-01', auto.assign = FALSE) %>%
  Cl() %>%
  na.locf()
ret_kospi =Delt(price_kospi)




ret_vol_quan = matrix(0,ro,5)
ret_vol = matrix(0,ro,1)
rownames(ret_vol_quan) = rownames(price_Q)
rownames(ret_vol) = rownames(price_Q)




write.csv(vol_Q , file = "E:/factor/vol/vol_Q1.csv")
##################### ???׽?Ʈ ########################

for( date in (which(rownames(price_Q) == '2000-03-31')) : 76 ){
  grade = data.frame(matrix(0,2,co))
  grade[1,] = vol_Q[date, ]
  grade[, which(price_Q[date,] == 0)] = NA
  qt = data.frame(matrix(0,1,3))
  
  #na.rm = TRUE 
  for (q in 1 : 5) { qt[, q] = quantile(grade[1,], (q-1)*2/10, na.rm=TRUE)
  
    for (w in 1 : co) {
      if ( ( grade[1, w] > qt[, q] ) & (!is.na(grade[1,w])) ) {  grade[2, w] = q }
      }
    }
  
  for (q in 1 : 5) { 
    
    ret_vol_quan[date+1,q] = mean(ret_price_Q[date+1, which(grade[2, ] == q)  ])
    
  }
  ret_vol[i+1] = mean(ret_price_Q[date+1, which(rank(grade[1, ]) <= 30 ) ])
  print(date)
}



chart.CumReturns(ret_vol_quan)   
legend('topleft', c("1st","2nd","3rd"),col=1:3, lty=1, horiz = TRUE, cex = 0.5, bty="n")

chart.CumReturns(ret_vol)   
legend('topleft',c("Lowvol Index"),col=1:2, lty=1, horiz = TRUE, bty="n")

chart.Drawdown(ret_vol_quan)

legend('bottom', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

table.AnnualizedReturns(ret_vol_quan)
