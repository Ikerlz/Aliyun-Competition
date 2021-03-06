# 弹性计算高校挑战赛-智能容量规划比赛

真实业务场景：在阿里集团应用全部上阿里云的背景下，基于智能的容量规划应用于双十一和618大促，在业务流量波峰期间利用弹性的能力提前预备好资源抗住流量洪峰，在业务流量低谷期间释放掉多余的资源以节约成本，通过对业务场最历史资源利用率的学习，来对未来资源利用率需求进行预测，最后基于资源利用率求换算出准确的虚拟机数量并输出容量弹性伸缩计划，同时要能高效的支持海量应用场景的计算。

## 1、题目要求：输入过去7天VM每小时的CPU利用率数据（168个点），预测未来三天CPU利用率数据（72个点）

- 需要做基本的数据分析，提供数据形态分析说明
- 数据中`NA`数据表示缺失数据点
- 需要考虑各种周期性，趋势来做对预测
- 预测结果和实际数据来作比对验证，`MAPE`来做衡量的标准
- 需要提供模型训练和线上`inference`代码

## 2、文件说明：

 - `data`文件夹包含原始数据`training_data.txt`和预测数据`generate_data.txt`
 - `中间数据`文件夹包含数据处理与模型训练过程中产生的数据
 - `数据处理`文件夹包含数据的预处理与数据缺失值补全部分的代码
 - `模型训练`文件夹包含三个模型训练的代码
 - `模型预测`文件夹包含模型预测的代码

## 3、模型说明见`报告.pdf`

## 4、成绩：第一名

![image](https://github.com/Ikerlz/Aliyun-Competition/blob/master/%E8%AF%81%E4%B9%A6.jpg)
