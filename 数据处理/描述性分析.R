## 清除工作环境

rm(list = ls())

## 设置工作路径
setwd("/home/hadoop/R/aliyun")

## 加载包
library(ggplot2)

## 设置ggplot2主题
mytheme <- theme(title = element_text(family = "Hei",size = 13)) +
  theme(axis.title = element_text(size = 12, family = "Hei")) +
  theme(axis.text.x = element_text(size = 11, family = "Hei")) +
  theme(axis.text.y = element_text(size = 11, family = "Hei")) +
  theme(legend.text = element_text(size = 11, family = "Hei")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))

## 读取数据
data <- read.csv("处理后的数据.csv", header = TRUE)
data <- data[,-1]

vm_index_list <- c()
cpu_use_list <- c()
for (i in c(1:120)) {
  vm_index_list <- c(vm_index_list, rep(i,168))
  cpu_use_list <- c(cpu_use_list, as.numeric(data[i,c(2:169)]))
}
boxplot_df <- data.frame(cpu_use = cpu_use_list, vm_index = vm_index_list)

Median <- function(x){
  return(median(x, na.rm = TRUE))
}

## 作图
ggplot(boxplot_df) + 
  geom_boxplot(aes(x = reorder(factor(vm_index), cpu_use, Median), y = log(cpu_use)), fill = rep("gold",120), outlier.size=0.5)+
  #  labs(x = "服务器", y = "对数CPU使用率", title = "各服务器CPU使用率箱线图") + 
  mytheme +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank())
