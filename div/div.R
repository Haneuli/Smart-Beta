library(tidyverse)
library(zoo) 
library(PerformanceAnalytics)
library(quantmod)

Sys.setenv(TZ = 'utc')

price_Q = read.csv("E:/factor/div/price_Q.csv", row.names = 1, header = TRUE)
price_Q[is.na(price_Q)] = 0
ro = dim(price_Q)[1]
co = dim(price_Q)[2]
ret_price_Q = matrix(0,ro,co)
for( i in 1: co){ret_price_Q[,i] = Delt((price_Q[,i]))}

div_Q = read.csv("E:/factor/div/div_Q.csv", row.names=1, header=TRUE)
div_Q[price_Q == 0 ] =NA

ret_div_quan = matrix(0,ro,5)
ret_div = matrix(0,ro,1)
row.names(ret_div_quan) = row.names(price_Q)
row.names(ret_div) = row.names(price_Q)

for( date in (which(rownames(price_Q) == '2000-03-31')): 76 ){
  grade = data.frame(matrix(0,2,co))
  grade[1,] = div_Q[date,] #  just  put only 1 year PER  to  grade[1,]
  
  # grade = replace(grade== 0,NA)
  grade[1, which(grade[1,] == 0)] = NA # if PER = 0,  change  0 to NA
  qt = data.frame(matrix(0,1,5))
  
  for (q in 1 : 5) {
    qt[, q] = quantile(grade[1,], (q-1)*2/10, na.rm=TRUE) 
    for(w in 1: co) {
      if ((grade[1,w] > qt[,q]) & (!is.na(grade[1,w]))){
        grade[2,w] = q
      }
    }
  }
  for(q in 1:5){
    ret_div_quan[date+1,q] = mean(ret_price_Q[date+1,which((grade[2,] == q))])
    }
  ret_div[date+1] = mean(ret_price_Q[date+1, which(rank(grade[1, ]) <= 30 ) ])
  print(date)
}


chart.CumReturns(ret_div_quan)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")


chart.CumReturns(ret_div)   
legend('topleft',c("div Index"),col=1:2, lty=1, horiz = TRUE, bty="n")
