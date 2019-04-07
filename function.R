library(tidyverse)
library(zoo)
library(tibble)
library(PerformanceAnalytics)
library(quantmod)
library(xts)

#########################   put csv file adress  ##########################

price = "E:/factor/factor_Q/price_Q.csv"
BM = "E:/factor/factor_Q/kospi_Q.csv"
factor_A = "E:/factor/factor_Q/PER_Q.csv"
factor_B = "E:/factor/factor_Q/PBR_Q.csv"
factor_C = "E:/factor/factor_Q/Vol_Q.csv"

start = "2000-03-31"
end = "2018-03-31"




###########################  Backtest_Tool funtion  ###########################

Backtest_Tool <- function(price ,factor_A ,factor_B = 0 ,factor_C = 0 , BM = 0, rank = 0, Percentile = 0, tax = 0 , start = row.names(price[1,0]) , end = row.names(price[ro,0]) ){
  
  Sys.setenv(TZ = 'utc')                                      # set the time series
  
  #######################  read price  #######################
  
  price = read.csv(price , row.names = 1, header = TRUE) %>%  # read  stocks price and exclude non-stock
    select(-contains('목적')) %>%
    select(-contains('스팩')) %>% 
    select(-contains('모기지')) %>% 
    select(-contains('.1')) %>%
    select(-contains('1호')) %>%
    select(-contains('2호')) %>%
    select(-contains('3호')) %>%
    select(-contains('4호')) 
  
  price[price == 0 ] = NA                                     # deal with missing values
  
  ro = dim(price)[1]                                          #  dim is length function  1 : row , 2 : column
  co = dim(price)[2]
  
  
  #################  create ratio of retun  ################
  ret_price = data.frame(matrix(0,ro,co))                     # create a ret_price matrix  having 0
  for( i in 1: co){ret_price[,i] = Delt((price[,i]))}         # put  return  into ret_price         
  rownames(ret_price) = rownames(price)                       # put  price rowname  into ret_price rowname 
  
  ##########################################################
  
  factors = c()                                               # create a  Empty list 
  
  if(factor_B ==  0){factors = c(factor_A)                    # if you put 1 factor  then  factors = ( factor_A)
  } else if(factor_C == 0){ factors = c(factor_A,factor_B)    # if you put 2 factor  then  factors = ( factor_A , factor_A )
  } else{ factors =c(factor_A,factor_B,factor_C)}             # if you put 3 factor  then  factors = ( factor_A , factor_B , factor_C)
  
  ##########################################################
  
  factor_1 = data.frame(matrix(0,ro,co))                      # create a factor_matrix having 0 
  factor_2 = data.frame(matrix(0,ro,co))        
  factor_3 = data.frame(matrix(0,ro,co))
  
  #####################  read factors  #####################
  
  for( i in factors){                                         # read  each factors  and exclude non-stock
    print(paste("reading", i ))
    factor = read.csv(i ,row.names = 1, header = TRUE) %>%
      select(-contains('목적')) %>%
      select(-contains('스팩')) %>% 
      select(-contains('모기지')) %>% 
      select(-contains('.1')) %>%
      select(-contains('1호')) %>%
      select(-contains('2호')) %>%
      select(-contains('3호')) %>%
      select(-contains('4호')) 
    factor[price == 0 ] = NA                                  # deal with missing values
    factor[is.na(price) ] = NA                                # deal with missing values
    
    ########################################################## 
    
    if(length(factors) == 3){                                 # put each factors into their factor matrixs  
      if(i == factors[1]){factor_1 = factor
      } else if(i == factors[2]){factor_2 = factor
      } else{factor_3 = factor}}
    else if(length(factors) == 2){
      if(i == factors[1]){factor_1 = factor
      } else{factor_3 = factor}}
    else{factor_1 = factor}
  }
  
  ##########################################################
  
  if(Percentile == 0){                                       #  choose  rank or Percentile and maek a matrixs having 0 for putting each ratio  
    ret_stragic = matrix(0,ro,1)  
    rownames(ret_stragic) = rownames(price)
  } else if(rank == 0){
    ret_stragic_quan = matrix(0,ro,Percentile)  
    rownames(ret_stragic_quan) = rownames(price)
  }
  
  print( "----- start back testing -----")
  
  ###########################################  calculate whole period ratio of return   ###########################################
  
  for( date in which(rownames(price) == start): (which(rownames(price) == end)-1)){           # select time period for backtest
    grade = data.frame(matrix(0,8,co))                                                        # create matrix for ranking factor 
    colnames(grade) = colnames(price)                                                         
    rownames(grade) = c('A','B','C','A_rank','B_rank','C_rank','SUM','SUM_rank') 
    
    ########################################################
    
    grade[1,] = factor_1[date,]                                                               # put factor_1  in grade[1,]
    grade[2,] = factor_2[date,]
    grade[3,] = factor_3[date,]
    # grade[1, which(grade[1,] == 0)] = NA
    # grade[2, which(grade[2,] == 0)] = NA
    # grade[3, which(grade[3,] == 0)] = NA
    
    ########################  filter  ######################
    
    grade = data.frame(t(grade)) %>%                       
      filter( A > 5 & B > 0.2    )                                                            # filter
    grade = data.frame(t(grade)) 
    
    ########################  rank  ########################
    grade[4,] = rank(grade[1,])                                                               # rank
    grade[5,] = rank(grade[2,])
    grade[6,] = rank(grade[3,])
    
    ################  sum factors ranking  #################
    
    if(factor_B ==  0){grade[7,] = grade[4,]                                                  # sum factors ranking 
    } else if(factor_C == 0){ grade[7,] = grade[4,] + grade[5,] 
    } else{ grade[7,] = grade[4,] + grade[5,] + grade[6,]}
    
    #########################################################
    
    grade_co = dim(grade)[2]                 
    grade[7, which(is.na(grade[1,])|is.na(grade[2,]) |is.na(grade[3,]))] = NA                 # deal with missing values
    
    ############  mean each perio return ratio ##############
    
    if(Percentile == 0){
      ret_stragic[date+1] = mean(unlist(ret_price[date+1, which(rank(grade[7, ]) < rank) ] ),na.rm = TRUE)     #  mean  each period retun ratio of rank 
    } else if(rank == 0 ){                                                                                 
      qt = matrix(0,1,Percentile)
      for (q in 1 : Percentile) {
        qt[, q] = quantile(grade[7,], (q-1)/Percentile, na.rm=TRUE)                                            # find percentile 
        for(w in 1: grade_co) {
          if ((grade[7,w] > qt[,q]) & (!is.na(grade[7,w]))){ grade[8,w] = q  }                                 #  rank all of stacks from 1 to percentile 
        }
      }
      for(q in 1:Percentile){
        ret_stragic_quan[date+1,q] = mean(unlist(ret_price[date+1,which((grade[8,] == q))]),na.rm=TRUE)        #  mean  each period retun ratio of percentile
      }
    }
    print( as.character( paste(  "-----", trunc(date/which(rownames(price) == end)*100), "% -----")   )  )
  }
  
  #######################################################  put nmae  #######################################################
  
  num_list = c()                                    
  
  if(Percentile != 0){
    Percentile_num = 1: Percentile
    for(n in Percentile_num){
      num_list = append(num_list,as.character(n))}
  } else {num_list = c("stragic")}
  
  #########################################  add tax , visualize static and chart  ##########################################
  
  if(Percentile == 0 & rank != 0){                                                                                  
    ret_stragic = data.frame(ret_stragic[which(rownames(price) == start): (which(rownames(price) == end)) ,])
    ret_stragic_T = as.xts(ret_stragic - tax)                                                                            #  add tax      
    colnames(ret_stragic_T) = num_list
    if(BM == 0){
      print(table.AnnualizedReturns(ret_stragic_T))                                                                      # visualize statics  
      charts.PerformanceSummary(ret_stragic_T )}                                                                         # visualize chart 
  }else {
    ret_stragic_quan = data.frame(ret_stragic_quan[which(rownames(price) == start): (which(rownames(price) == end)) ,])
    ret_stragic_quan_T = as.xts(ret_stragic_quan - tax)                                                                  #  add tax  
    colnames(ret_stragic_quan_T) = num_list                                                                             
    if(BM == 0){             
      print(table.AnnualizedReturns(ret_stragic_quan_T))                                                                 # visualize statics
      charts.PerformanceSummary(ret_stragic_quan_T, main =  paste(" Stragic Divide" ,Percentile, "Performance" ))}       # visualize chart
  }
  
  #####################################################  stragic vs kospi  ####################################################
  
  print("----- finish -----")
  if(BM != 0){
    BM = read.csv(BM, row.names = 1, header = TRUE)                                      #  read  benchmark  
    BM_ro = dim(BM)[1]
    BM_co = dim(BM)[2]
    ret_BM =data.frame( matrix(0,BM_ro,1))  
    ret_BM = Delt(BM[,1])
    rownames(ret_BM) = rownames(BM)  
    ret_BM= as.xts(ret_BM)
    
    ##########################################################
    
    if(Percentile == 0 & rank != 0 )  {                                                  # make a comparison between BM and stragic
      stragic_VS_BM = merge(ret_stragic_T , ret_BM , all = FALSE)%>%
        na.omit()
    }else {
      stragic_VS_BM = merge(ret_stragic_quan_T , ret_BM , all = FALSE)%>%
        na.omit()}
    
    ##########################################################
    
    num_list = append(num_list,"Benchmark")
    colnames(stragic_VS_BM) = num_list
    charts.PerformanceSummary(stragic_VS_BM ,main = "stragic vs Benchmark Performance")  # visulaize statics and chart 
    table.AnnualizedReturns(stragic_VS_BM)
  }
}





Backtest_Tool(price,factor_A,factor_B,factor_C , BM , rank =30  , tax = 0.01   )




