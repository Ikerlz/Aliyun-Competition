## 清除工作环境

rm(list = ls())


## 设置工作路径
setwd("/Users/gegavin/Desktop/阿里云")

## 加载包
library(forecast)
library(tseries)

cyclist_factor_model_mape <- read.csv("cyclist_factor_model_mape.csv", header = TRUE)
cyclist_factor_model_mape <- cyclist_factor_model_mape[,-1]
arima_model_mape <- read.csv("arima_model_mape.csv", header = TRUE)
arima_model_mape <- arima_model_mape[,-1]
lstm_model_mape <- read.csv("lstm_model_mape.csv", header = TRUE)
lstm_model_mape <- lstm_model_mape[,-1]

mape_total <- data.frame(vm_name = cyclist_factor_model_mape$vm_name,
                         cyclist_factor_model_mape = cyclist_factor_model_mape$mape,
                         arima_model_mape = arima_model_mape$mape,
                         lstm_model_mape = lstm_model_mape$mape)

data <- read.csv("处理后的数据.csv",header = TRUE)
data <- data[,-1]
#################################
### 周期因子模型

cyclist_factor_model <- function(vec){
  ## 输入长度为168的向量；
  ## 输出长度为72的预测值
  reminder <- length(vec) %% 24
  if(reminder != 0){
    print("输入的数据长度不是24的整数倍")
    return(NA)
  }else{
    day_num <- length(vec) / 24 # 天数
    data_matrix <- matrix(NA, day_num, 24)
    for (i in c(1:day_num)) {
      data_matrix[i,] <- vec[(24*(i-1)+1) : (24*i)]
    }
    data_df <- as.data.frame(data_matrix)
    names(data_df) <- c(1:24)
    # 第一步: 除以均值，得到一个比例
    data_percent <- data_df
    for (i in c(1:day_num)) {
      data_percent[i, ] <- data_df[i, ]/mean(as.numeric(data_df[i,c(1:24)]))
    }
    # 第二步: 按列取中位数
    Median_vec <- c()
    for (i in c(1:24)) {
      Median_vec <- c(Median_vec, median(as.numeric(data_percent[,i]))) # 周期因子
    }
    # 第三步：选取base
    base1 <- mean(as.numeric(data_df[1,c(1:24)]))
    base2 <- mean(as.numeric(data_df[2,c(1:24)]))
    base3 <- mean(as.numeric(data_df[3,c(1:24)]))
    
    # 第4步：预测：用base乘周期因子
    res1 <- Median_vec * base1
    res2 <- Median_vec * base2
    res3 <- Median_vec * base3
    return(c(res1, res2, res3))
  }
}



######### 预测 #########

pred_matrix <- matrix(NA, 120, 72)

for (i in c(1:120)) {
  pred_matrix[i, ] <- cyclist_factor_model(as.numeric(data[i, c(2:169)]))
}




pred_df <- as.data.frame(pred_matrix)
vm_name_df <- data.frame(vm_name = data$vm_name)

output_df <- cbind(vm_name_df, pred_df)
output_df$vm_name <- as.character(output_df$vm_name)

write.table(output_df, "output_df.txt", sep = ",")



