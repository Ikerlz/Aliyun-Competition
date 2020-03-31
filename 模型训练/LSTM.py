import numpy as np
from numpy import array
import pandas as pd
import math
from itertools import chain
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from sklearn.preprocessing import MinMaxScaler
import numpy


dataframe = pd.read_csv('处理后的数据.csv')
del dataframe['Unnamed: 0']

#dataset = dataframe.values


data_array = np.array(dataframe)    # np.ndarray()
data_list = data_array.tolist()  # list 共120个子列表，每一个子列表储存一个vm的数据

# 将一列变成两列，第一列是t时的数据，第二列是 t+1 时的数据
def create_dataset(dataset, look_back=1):
    # X为t时刻数据，Y为t+1时刻数据
    # convert an array of values into a dataset matrix
    dataX, dataY = [], []
    for i in range(len(dataset)-look_back-1):
        a = dataset[i:(i+look_back), 0]
        dataX.append(a)
        dataY.append(dataset[i + look_back, 0])
    return np.array(dataX), np.array(dataY)

# split a univariate sequence into samples
def split_sequence(sequence, n_steps):
    X, y = list(), list()
    for i in range(len(sequence)):
        # find the end of this pattern
        end_ix = i + n_steps
        # check if we are beyond the sequence
        if end_ix > len(sequence) - 1:
            break
        # gather input and output parts of the pattern
        seq_x, seq_y = sequence[i:end_ix], sequence[end_ix]
        X.append(seq_x)
        y.append(seq_y)
    return array(X), array(y)


## 计算MAPE
def MAPE(real_list, pred_list):
    if len(real_list) != len(pred_list):
        print("长度不等")
        return(None)
    else:
        Len = len(real_list)
        total = 0
        for i in range(Len):
            total = total + abs(pred_list[i]/real_list[i] - 1)
        return(total/Len)



## 创建model
look_back = 1
model = Sequential()
model.add(LSTM(32, input_shape=(24, 1)))
model.add(Dense(1))
model.compile(loss='mean_squared_error', optimizer='adam')

scaler = MinMaxScaler(feature_range=(0, 1)) # 正则化函数

MAPE_list = [0 for i in range(120)]
for i in range(120):
    train_data_norm = scaler.fit_transform(pd.DataFrame(data_list[i][1:145]))
    train_data_norm = np.array(train_data_norm).tolist()
    train_data_norm = list(chain.from_iterable(train_data_norm))
    # del(train_data_norm[0])
    X, y = split_sequence(train_data_norm, 24)
    n_features = 1
    X = X.reshape((X.shape[0], X.shape[1], n_features))
    # reshape input to be [samples, time steps, features]
    #trainX = numpy.reshape(trainX, (trainX.shape[0], 1, trainX.shape[1]))
    model.fit(X, y, epochs=50, batch_size=1, verbose=2)
    pred_val = []
    pred_data = train_data_norm[120:145]
    for i in range(24): # 预测一天
        pred_X = array(pred_data).reshape((1, 24, 1))
        pred = model.predict(pred_X)[:, 0].tolist()[0]
 #       print(pred)
        pred_val.append(pred)
        del(pred_data[0])
        pred_data.append(pred)
    pred_val = scaler.inverse_transform(pd.DataFrame(pred_val))
    pred_val = np.array(pred_val).tolist()
    pred_val = list(chain.from_iterable(pred_val))

    ## 计算MAPE
    MAPE_list[i] = MAPE(pred_val, data_list[i][145:169])
    print(MAPE_list[i])





