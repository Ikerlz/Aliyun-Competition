## 清除工作环境

rm(list = ls())

## 设置工作路径
setwd("/Users/gegavin/Desktop/阿里云")

## 加载包
library(forecast)
library(tseries)
data <- read.csv("处理后的数据.csv", header = TRUE)
data <- data[,-1]

arima_model <- function(vec, pred_num){
  # 输入数据与需要预测的数目
  ts_data <- ts(vec)  # 转为时间序列
  p_d_q=auto.arima(ts_data)
  res <- forecast(p_d_q, h=pred_num)
  return(as.numeric(res$mean))
}


## 计算MAPE
MAPE <- function(real_vec, pred_vec){
  if(length(real_vec) != length(pred_vec)){
    print("长度不同")
    return(NA)
  }else{
    len = length(real_vec)
    total <- 0
    for (i in c(1:len)) {
      total <- total + abs(pred_vec[i]/real_vec[i] - 1)
    }
    return(total/len)
  }
}

pred_mape <- rep(NA, 120)
for (i in c(1:120)) {
  pred_vec <- arima_model(as.numeric(data[i,2:145]), 24)
  real_vec <- as.numeric(data[i,146:169])
  pred_mape[i] <- MAPE(real_vec, pred_vec)
}

arima_model_mape <- data.frame(vm_name = data$vm_name, mape = pred_mape)

write.csv(arima_model_mape, "arima_model_mape.csv", fileEncoding = "UTF8")

