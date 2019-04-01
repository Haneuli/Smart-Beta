library(tidyverse)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(xts)

Sys.setenv(TZ = 'utc')


price= read.csv("E:/factor/PBR/price.csv", row.names = 1, header = TRUE)
price[is.na(price)] = 0
ro = dim(price)[1]
co = dim(price)[2]
ret_price = matrix(0,ro,co)

for( i in 1: co){ret_price[,i] = Delt((price[,i]))}

PBR = read.csv("E:/factor/PBR/PBR.csv", row.names = 1, header = TRUE)
PBR[PBR == 0 ] = NA
PBR[price == 0 ] =NA

ret_PBR_quan = matrix(0,ro,5)
ret_PBR = matrix(0,ro,1)
row.names(ret_PBR_quan) = row.names(price)
row.names(ret_PBR) = row.names(price)


# read  20 year Quarter PER of all of stocks and  change na to 0  
PER = read.csv("E:/factor/PER.csv", row.names=1, header=TRUE)
PER[PER == 0] = NA
PER[price == 0 ] = NA  


ret_PBRPER_quan = matrix(0,ro,5) 
ret_PBRPER = matrix(0,ro,1)  
rownames(ret_PBRPER_quan) = rownames(price)
rownames(ret_PBRPER) = rownames(price)

for( date in (which(rownames(price) == '2000-03-31')):76 ){
grade = data.frame(matrix(0,6,co))
grade[1,] = PBR[date,]
grade[2,] = PER[date,]
grade[1, which(grade[1,] == 0)] = NA
grade[2, which(grade[2,] == 0)] = NA
grade[3,] = rank(grade[1,])
grade[4,] = rank(grade[2,])
grade[5,] = grade[3,] + grade[4,]
grade[5, which(is.na(grade[1,])|is.na(grade[2,]) )] = NA

qt = matrix(0,1,5)
for (q in 1 : 5) {
  qt[, q] = quantile(grade[5,], (q-1)*2/10, na.rm=TRUE) 
  for(w in 1: co) {
    if ((grade[5,w] > qt[,q]) & (!is.na(grade[5,w]))){
      grade[6,w] = q  }
    }
  }
  for(q in 1:5){
    ret_PBRPER_quan[date+1,q] = mean(ret_price[date+1,which((grade[6,] == q))])
    print("ssssssssss")
  }
  ret_PBR[date+1] = mean(ret_price[date+1, which(rank(grade[6, ]) <= 20 ) ])

 
}




# kospi return of ratio
price_kospi = getSymbols('^KS11', from = '2000-03-31', auto.assign = FALSE) %>%
  Cl() %>%
  na.locf()
price_kospi_ret=Delt(price_kospi)


Sys.setenv(TZ = 'utc')
PBR_With_kospi = merge(price_kospi_ret, ret_PBR_quan_tax)







chart.CumReturns(ret_PBRPER_quan)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

#   include tax and Slippage
ret_PBRPER_quan_tax = ret_PBRPER_quan-0.005
chart.CumReturns(ret_PBRPER_quan_tax)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

chart.CumReturns(ret_PBRPER_quan)
legend('topleft', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")

chart.CumReturns(ret_PBRPER_quan)   
legend('topleft',c("PER Index"),col=1:2, lty=1, horiz = TRUE, bty="n")

chart.Drawdown(ret_PBRPER_quan)
legend('bottom', c("1st","2nd","3rd","4th","5th"),col=1:5, lty=1, horiz = TRUE, cex = 0.5, bty="n")
table.AnnualizedReturns(ret_PBRPER_quan_tax)

