## 清除工作环境

rm(list = ls())

## 设置工作路径
setwd("/home/hadoop/R/aliyun")

## 读取数据
data <- read.table("dataset_campus_competition.txt", header = FALSE)

## 数据预处理


strTonum <- function(list_str){
  
  # 定义字符串转数字函数
  # input: list (eg list("1,2,3"))
  # output: vector (eg c(1,2,3))
  
  toVec <- as.vector(unlist(list_str[1]))         # 先转为向量
  str_vec <- strsplit(toVec, split = ",")[[1]]    # 以","为分割符分割字符串
  
  if(length(str_vec) != 168){
    print("数据分割错误，分割后数据长度不为168")
    return(NA)
  }else{
    num_vec <- as.numeric(str_vec)                # 字符型转为数值型
    return(num_vec)
  }
}

cpu_use_list <- lapply(data[,2], strTonum) # 将数据第二列所有元素转为数值型向量
#lapply函数出现5个warning，“In FUN(X[[i]], ...) : NAs introduced by coercion”,即有5组数据有NA

cpu_use_df <- as.data.frame(t(data.frame(cpu_use_list))) # 转为数据框
rownames(cpu_use_df) <- c(1:120)  # 改行名：1到120个服务器

## 合并服务器名字
data_df <- cbind(as.data.frame(data[,1]), cpu_use_df)
## 重命名列名
names(data_df) <- c("vm_name", c(1:168))  # 1到168个小时


plot(c(1:168), data_df[10, c(2:169)], type = "b")
abline(v = c(24, 48, 72, 96, 120, 144, 168),col = "red")
plot(c(1:168), data_df[22, c(2:169)], type = "b")
abline(v = c(24, 48, 72, 96, 120, 144, 168),col = "red")
plot(c(1:168), data_df[70, c(2:169)], type = "b")
abline(v = c(24, 48, 72, 96, 120, 144, 168),col = "red")
plot(c(1:168), data_df[113, c(2:169)], type = "b")
abline(v = c(24, 48, 72, 96, 120, 144, 168),col = "red")
plot(c(1:168), data_df[115, c(2:169)], type = "b")
abline(v = c(24, 48, 72, 96, 120, 144, 168),col = "red")


na_data_df <- data.frame(index = c(1:168), 
                         vm10 = as.numeric(data_df[10, c(2:169)]), 
                         vm22 = as.numeric(data_df[22, c(2:169)]), 
                         vm70 = as.numeric(data_df[70, c(2:169)]), 
                         vm113 = as.numeric(data_df[113, c(2:169)]), 
                         vm115 = as.numeric(data_df[115, c(2:169)]))

vm10_data_div <- data.frame(day1 = na_data_df$vm10[1:24], 
                            day2 = na_data_df$vm10[25:48], 
                            day3 = na_data_df$vm10[49:72], 
                            day4 = na_data_df$vm10[73:96], 
                            day5 = na_data_df$vm10[97:120], 
                            day6 = na_data_df$vm10[121:144], 
                            day7 = na_data_df$vm10[145:168])
vm22_data_div <- data.frame(day1 = na_data_df$vm22[1:24], 
                            day2 = na_data_df$vm22[25:48], 
                            day3 = na_data_df$vm22[49:72], 
                            day4 = na_data_df$vm22[73:96], 
                            day5 = na_data_df$vm22[97:120], 
                            day6 = na_data_df$vm22[121:144], 
                            day7 = na_data_df$vm22[145:168])
vm70_data_div <- data.frame(day1 = na_data_df$vm70[1:24], 
                            day2 = na_data_df$vm70[25:48], 
                            day3 = na_data_df$vm70[49:72], 
                            day4 = na_data_df$vm70[73:96], 
                            day5 = na_data_df$vm70[97:120], 
                            day6 = na_data_df$vm70[121:144], 
                            day7 = na_data_df$vm70[145:168])
vm113_data_div <- data.frame(day1 = na_data_df$vm113[1:24], 
                             day2 = na_data_df$vm113[25:48], 
                             day3 = na_data_df$vm113[49:72], 
                             day4 = na_data_df$vm113[73:96], 
                             day5 = na_data_df$vm113[97:120], 
                             day6 = na_data_df$vm113[121:144], 
                             day7 = na_data_df$vm113[145:168])
vm115_data_div <- data.frame(day1 = na_data_df$vm115[1:24], 
                             day2 = na_data_df$vm115[25:48], 
                             day3 = na_data_df$vm115[49:72], 
                             day4 = na_data_df$vm115[73:96], 
                             day5 = na_data_df$vm115[97:120], 
                             day6 = na_data_df$vm115[121:144], 
                             day7 = na_data_df$vm115[145:168])

## vm10_data_div
# 缺失132个点

# 先补齐第6行
na_data_df$vm10[6] = na_data_df$vm10[126] = na_data_df$vm10[150] <- 
  mean(c(na_data_df$vm10[30], na_data_df$vm10[54], na_data_df$vm10[78], na_data_df$vm10[102]))

# 线性插值加噪声

## 7到25
na_data_df$vm10[7:25] <- 
  (na_data_df$vm10[26]-na_data_df$vm10[6])/21 * c(1:19) + na_data_df$vm10[6] + 
  rnorm(19, sd = sqrt(na_data_df$vm10[26]))

## 31到49
na_data_df$vm10[31:49] <- 
  (na_data_df$vm10[50]-na_data_df$vm10[30])/21 * c(1:19) + na_data_df$vm10[30] + 
  rnorm(19, sd = sqrt(na_data_df$vm10[50]))

## 55到73
na_data_df$vm10[55:73] <- 
  (na_data_df$vm10[74]-na_data_df$vm10[54])/21 * c(1:19) + na_data_df$vm10[54] + 
  rnorm(19, sd = sqrt(na_data_df$vm10[74]))

## 79到97
na_data_df$vm10[79:97] <- 
  (na_data_df$vm10[98]-na_data_df$vm10[78])/21 * c(1:19) + na_data_df$vm10[78] + 
  rnorm(19, sd = sqrt(na_data_df$vm10[98]))
## 103到121
na_data_df$vm10[103:121] <- 
  (na_data_df$vm10[122]-na_data_df$vm10[102])/21 * c(1:19) + na_data_df$vm10[102] + 
  rnorm(19, sd = sqrt(na_data_df$vm10[122]))
## 127到145
na_data_df$vm10[127:145] <- 
  (na_data_df$vm10[146]-na_data_df$vm10[126])/21 * c(1:19) + na_data_df$vm10[126] + 
  rnorm(19, sd = sqrt(na_data_df$vm10[146]))

## 151到1
na_data_df$vm10[151:168] <- 
  (na_data_df$vm10[2]-na_data_df$vm10[150])/21 * c(1:18) + na_data_df$vm10[150] + 
  rnorm(18, sd = sqrt(na_data_df$vm10[2]))
na_data_df$vm10[1] <- 
  (na_data_df$vm10[2]-na_data_df$vm10[150])/21 * 19 + na_data_df$vm10[150] +
  rnorm(1, sd = sqrt(na_data_df$vm10[2]))


## vm22_data_div

## 11到28
na_data_df$vm22[11:28] <- 
  (na_data_df$vm22[29]-na_data_df$vm22[10])/20 * c(1:18) + na_data_df$vm22[10] + 
  rnorm(18, sd = sqrt(na_data_df$vm22[29]))
## 35到52
na_data_df$vm22[35:52] <- 
  (na_data_df$vm22[53]-na_data_df$vm22[34])/20 * c(1:18) + na_data_df$vm22[34] + 
  rnorm(18, sd = sqrt(na_data_df$vm22[53]))
## 59到76
na_data_df$vm22[59:76] <- 
  (na_data_df$vm22[77]-na_data_df$vm22[58])/20 * c(1:18) + na_data_df$vm22[58] + 
  rnorm(18, sd = sqrt(na_data_df$vm22[77]))
## 83到100
na_data_df$vm22[83:100] <- 
  (na_data_df$vm22[101]-na_data_df$vm22[82])/20 * c(1:18) + na_data_df$vm22[82] + 
  rnorm(18, sd = sqrt(na_data_df$vm22[101]))
## 107到124
na_data_df$vm22[107:124] <- 
  (na_data_df$vm22[125]-na_data_df$vm22[106])/20 * c(1:18) + na_data_df$vm22[106] + 
  rnorm(18, sd = sqrt(na_data_df$vm22[125]))
## 131到148
na_data_df$vm22[131:148] <- 
  (na_data_df$vm22[149]-na_data_df$vm22[130])/20 * c(1:18) + na_data_df$vm22[130] + 
  rnorm(18, sd = sqrt(na_data_df$vm22[149]))
## 155到4
na_data_df$vm22[155:168] <- 
  (na_data_df$vm22[5]-na_data_df$vm22[154])/20 * c(1:14) + na_data_df$vm22[154] + 
  rnorm(14, sd = sqrt(na_data_df$vm22[5]))
na_data_df$vm22[1:4] <- 
  (na_data_df$vm22[5]-na_data_df$vm22[154])/20 * c(15:18) + na_data_df$vm22[154] + 
  rnorm(4, sd = sqrt(na_data_df$vm22[5]))

## vm70_data_div

## 补全第二列
vm70_data_div$day2[24] <- 30
## 补全第三列
for (i in c(1:24)) {
  if(is.na(vm70_data_div$day3[i] == TRUE)){
    vm70_data_div$day3[i] = mean(as.numeric(vm70_data_div[i,c(1,2,4)]))
  }
}
## 补全第五列
for (i in c(1:24)) {
  if(is.na(vm70_data_div$day5[i] == TRUE)){
    vm70_data_div$day5[i] = mean(as.numeric(vm70_data_div[i,c(2:4)]))
  }
}

## 补全第六列
for (i in c(1:24)) {
  if(is.na(vm70_data_div$day6[i] == TRUE)){
    vm70_data_div$day6[i] = mean(as.numeric(vm70_data_div[i,c(3:5)]))
  }
}

## 补全第六列
for (i in c(1:24)) {
  if(is.na(vm70_data_div$day7[i] == TRUE)){
    vm70_data_div$day7[i] = mean(as.numeric(vm70_data_div[i,c(1:6)]))
  }
}

na_data_df$vm70 <- as.numeric(c(vm70_data_div[,1], vm70_data_div[,2], vm70_data_div[,3], 
                                vm70_data_div[,4], vm70_data_div[,5], vm70_data_div[,6], 
                                vm70_data_div[,7]))

## vm113_data_div
vm113_data_div[17,1] <-  mean(as.numeric(vm113_data_div[17,2:5]))

for (i in c(9:17)) {
  vm113_data_div[i,6] <- mean(as.numeric(vm113_data_div[i,1:4]))
  vm113_data_div[i,7] <- mean(as.numeric(vm113_data_div[i,1:6]))
}
na_data_df$vm113 <- as.numeric(c(vm113_data_div[,1], vm113_data_div[,2], vm113_data_div[,3], 
                               vm113_data_div[,4], vm113_data_div[,5], vm113_data_div[,6],
                               vm113_data_div[,7]))

for (i in c(1:6)) {
  na_data_df$vm113[(18+24*(i-1)):(32+24*(i-1))] <- (na_data_df$vm113[33+24*(i-1)]-na_data_df$vm113[17+24*(i-1)])/17 * c(1:15) + na_data_df$vm113[17+24*(i-1)] + 
    rnorm(15, sd =  na_data_df$vm113[17+24*(i-1)])
}

na_data_df$vm113[162:168] <- (na_data_df$vm113[9]-na_data_df$vm113[161])/17 * c(1:7) + na_data_df$vm113[161] + 
  rnorm(7, sd =  na_data_df$vm113[161])
na_data_df$vm113[1:8] <- (na_data_df$vm113[9]-na_data_df$vm113[161])/17 * c(8:15) + na_data_df$vm113[161] + 
  rnorm(8, sd =  na_data_df$vm113[161])

## vm115_data_div
## 数据有较强的比例关系
vm115_data_div[17,c(1:5)] <- 0.3818368 *vm115_data_div[18,c(1:5)]

for (i in c(8:10)) {
  vm115_data_div[i, 6] <- sum(vm115_data_div[i, c(1:5)])/ sum(vm115_data_div[18, c(1:5)]) * vm115_data_div[18, 6]
  vm115_data_div[i, 7] <- sum(vm115_data_div[i, c(1:6)])/ sum(vm115_data_div[18, c(1:6)]) * vm115_data_div[18, 7]
}

for (i in c(1:7)) {
  vm115_data_div[c(11:16), i] <- (vm115_data_div[17, i] - vm115_data_div[10, i])/8 * c(1:6) + vm115_data_div[10, i] + rnorm(6, sd = sqrt(vm115_data_div[10, i]))
}
for (i in c(1:7)) {
  vm115_data_div[c(1:7), i] <- (vm115_data_div[8, i] - vm115_data_div[24, i])/9 * c(1:7)+ vm115_data_div[24, i] + rnorm(7, sd = sqrt(vm115_data_div[8, i]))
}
na_data_df$vm115 <- as.numeric(c(vm115_data_div[,1], vm115_data_div[,2], vm115_data_div[,3], 
                                 vm115_data_div[,4], vm115_data_div[,5], vm115_data_div[,6], 
                                 vm115_data_div[,7]))

data_df[10, c(2:169)] <- na_data_df$vm10
data_df[22, c(2:169)] <- na_data_df$vm22
data_df[70, c(2:169)] <- na_data_df$vm70
data_df[113, c(2:169)] <- na_data_df$vm113
data_df[115, c(2:169)] <- na_data_df$vm115

write.csv(data_df, file = "处理后的数据.csv" , fileEncoding = "UTF-8")
