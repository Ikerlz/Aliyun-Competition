## 清除工作环境

rm(list = ls())

## 设置工作路径
setwd("/home/hadoop/R/aliyun")

data <- read.csv("处理后的数据.csv", header = TRUE)
data <- data[,-1]

### 周期因子模型

cyclist_factor_model <- function(vec){
  ## 输入一个向量，长度为24的整数倍；
  ## 输出长度为24的预测值
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
    base <- 0
    for (i in c(1:6)) {
      base <- base + mean(as.numeric(data_df[i,c(1:24)]))
    }
    base <- base / 6 # 取前6天的平均利用率为base
    
    # 第4步：预测：用base乘周期因子
    res <- Median_vec * base
    return(res)
  }
}

cyclist_factor_model(as.numeric(data[1,2:145]))

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
  pred_vec <- cyclist_factor_model(as.numeric(data[i,2:145]))
  real_vec <- as.numeric(data[i,146:169])
  pred_mape[i] <- MAPE(real_vec, pred_vec)
}

## 将误差写入csv文件
cyclist_factor_model_mape <- data.frame(vm_name = data$vm_name, mape = pred_mape)

write.csv(cyclist_factor_model_mape, "cyclist_factor_model_mape.csv", fileEncoding = "UTF-8")
