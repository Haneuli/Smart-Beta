library(tidyverse)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(xts)

Sys.setenv(TZ = 'utc')


price= read.csv("F:/factor/PBR/price.csv", row.names = 1, header = TRUE)
price[is.na(price)] = 0
ro = dim(price)[1]
co = dim(price)[2]
ret_price = matrix(0,ro,co)

for( i in 1: co){ret_price[,i] = Delt((price[,i]))}

PBR = read.csv("F:/factor/PBR/PBR.csv", row.names = 1, header = TRUE)
PBR[PBR == 0 ] = NA
PBR[price == 0 ] =NA

ret_PBR_quan = matrix(0,ro,5)
ret_PBR = matrix(0,ro,1)
row.names(ret_PBR_quan) = row.names(price)
row.names(ret_PBR) = row.names(price)






for( date in (which(rownames(price) == '2000-03-31')):76 ){
  grade = data.frame(matrix(0,2,co))
  grade[1,] = PBR[date,] #  just  put only 1 year PER  to  grade[1,]
  
  # grade = replace(grade== 0,NA)
  grade[1, which(grade[1,] == 0)] = NA # if PER = 0,  change  0 to NA
  qt = data.frame(matrix(0,1,5))
  
  for (q in 1 : 5) {
    qt[, q] = quantile(grade[1,], (q-1)*2/10, na.rm=TRUE) 
    for(w in 1: co) {
      print(3)
      if ((grade[1,w] > qt[,q]) & (!is.na(grade[1,w]))){
        grade[2,w] = q
        print(4) 
      }
    }
  }
  for(q in 1:5){
    ret_PBR_quan[date+1,q] = mean(ret_price[date+1,which((grade[2,] == q))])
    print("ssssssssss")
  }
  ret_PBR[date+1] = mean(ret_price[date+1, which(rank(grade[1, ]) <= 20 ) ])
}

# kospi return of ratio
price_kospi = getSymbols('^KS11', from = '2000-03-31', auto.assign = FALSE) %>%
  Cl() %>%
  na.locf()
price_kospi_ret=Delt(price_kospi)


Sys.setenv(TZ = 'utc')
PBR_With_kospi = merge(price_kospi_ret, ret_PBR_quan_tax)







chart.CumReturns(PBR_With_kospi)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

  #   include tax and Slippage
ret_PBR_quan_tax = ret_PBR_quan-0.005
chart.CumReturns(ret_PBR_quan_tax)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

chart.CumReturns(ret_PBR_quan)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

chart.CumReturns(ret_PBR)   
legend('topleft',c("PER Index"),col=1:2, lty=1, horiz = TRUE, bty="n")

chart.Drawdown(ret_PBR_quan)
legend('bottom', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")
table.AnnualizedReturns(ret_PBR_quan)



