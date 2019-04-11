library(tidyverse)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(xts)

#시계열통합
Sys.setenv(TZ = 'utc')

#스팩,모기지,목적,.1(주식아니야)필요없어서 열에서 지움/1번째 열을 행으로 정한다.
price_Q= read.csv("F:/factor/factor_Q/price_Q.csv", row.names = 1, header = TRUE) %>%
  select(-contains('스팩')) %>%
  select(-contains('모기지')) %>% 
  select(-contains('목적')) %>%
  select(-contains('.1')) 
#값이 없는것 NA로 바꿧ㅇ
price_Q[price_Q == 0 ] = NA

#행의 수 열의 수 나타냄 
ro = dim(price_Q)[1]#열=2988
co = dim(price_Q)[2]#행=76

#수익율의 약자ret/가격/분기 = 분기수익율을 ret_price_Q
#matrix=이차원배열-> 새로운 값을 불러올 셀에 모든 데이터를 0으로 처리해주고 나중에 값을 저장할거것
#for문은 수익율을 저장해주는것/ Delt-> 한개의 열의 데이터를 처리(수익률처리)
ret_price_Q = data.frame(matrix(0,ro,co))
for( i in 1: co){ret_price_Q[,i] = Delt((price_Q[,i]))}


PBR_Q = read.csv("F:/factor/factor_Q/PBR_Q.csv", row.names = 1, header = TRUE) %>%
  select(-contains('스팩')) %>%
  select(-contains('모기지')) %>% 
  select(-contains('목적')) %>%
  select(-contains('.1'))
PBR_Q[PBR_Q == 0 ] = NA
PBR_Q[is.na(price_Q) ] =NA
#is.na -> price_Q안에 NA가 있으면 PBR_Q에 똑같은 행열도 NA로 바꿔라


# read  20 year Quarter PER of all of stocks and  change na to 0  
PER_Q = read.csv("F:/factor/factor_Q/PER_Q.csv", row.names=1, header=TRUE) %>%
  select(-contains('스팩')) %>%
  select(-contains('모기지')) %>% 
  select(-contains('목적')) %>%
  select(-contains('.1')) 
PER_Q[PER_Q == 0] = NA
PER_Q[is.na(price_Q) ] =NA 



ret_PBRPER = matrix(0,ro,1)  
rownames(ret_PBRPER) = rownames(price_Q)
#맨앞에있는게 객체임 로우네임을 날짜를 ret_PBRPER로 똑같ㅇ


#
for( date in 2:76){
  grade = data.frame(matrix(0,7,co))#grade라는 매트릭스를 만듬
  colnames(grade) = colnames(price_Q)
  rownames(grade) = c('Symbol','PBR','PER','PBR_rank','PER_rank','SUM','SUM_rank')
  
  grade[1,] = price_Q[1,]#종목이름 
  grade[2,] = PBR_Q[date,]
  grade[3,] = PER_Q[date,]
  grade[2, which(grade[2,] == 0)] = NA
  grade[3, which(grade[3,] == 0)] = NA
  grade = data.frame(t(grade)) %>%#fiter를 쓰려면 행과열 바꿔야해
    select(Symbol,PBR,PER,PBR_rank,PER_rank,SUM,SUM_rank) %>%
    filter(PBR > 0.5 & PER > 5)#5이하인건 망항기업일 확률이 높으니까 제거 2099개
  grade = data.frame(t(grade))#행과열을 다시 원상태로 복구
  grade[4,] = rank(grade[2,])
  grade[5,] = rank(grade[3,])
  grade[6,] = grade[4,] + grade[5,]
  grade_co = dim(grade)[2]#2099개인걸 나타내준다.
  grade[6, which(is.na(grade[2,])|is.na(grade[3,]) )] = NA #혹시라도 NA값 있을까봐
  #qt = matrix(0,1,5)
  #  for (q in 1 : 5) {
  #   qt[, q] = quantile(grade[6,], (q-1)*2/10, na.rm=TRUE) 
  #   for(w in 1: grade_co) {
  #     if ((grade[6,w] > qt[,q]) & (!is.na(grade[6,w]))){
  #       grade[7,w] = q  }
  
  #        }
  #   }
  #for(q in 1:5){
  #  ret_PBRPER_quan[date+1,q] = mean(ret_price_Q[date+1,which((grade[7,] == q))])
  #  }
  
  
  ret_PBRPER[date+1] = mean(unlist(ret_price_Q[date+1, which(rank(grade[6, ]) < 20) ] ),na.rm = TRUE)#수익률을 나온걸 ret_price_Q에다가 넣어서 평균화 시킴->ret_PBRPER에 넣어.31번째분기를 넣은거임->2000~2019년도까지의 수익률 넣게됨 .
  print(date)
}




ret_PBRPER = data.frame( ret_PBRPER[2:76,])  


charts.PerformanceSummary(ret_PBRPER)