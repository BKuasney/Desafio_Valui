# -*- coding: utf-8 -*-
"""
Created on Mon Dec 11 19:29:39 2017

@author: bkuasney
"""

# libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os

# Import
os.chdir('C:/Users/bkuasney/Desktop/ML/FREELA')
dataset = pd.read_csv('base_python_casa.csv')
list(dataset)

X = dataset.iloc[:, 1:8]
y = dataset.iloc[:, 8]

# Treino e teste
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 5)

# Fitting Random Forest
from sklearn.ensemble import RandomForestRegressor
regressor = RandomForestRegressor(n_estimators = 100, oob_score = 'TRUE', random_state = 1)
regressor.fit(X_train, pd.DataFrame(y_train))

# Predicting
y_pred = regressor.predict(X_test)
y_pred_trat = pd.DataFrame(y_pred)
y_test_trat = pd.DataFrame(y_test)
y_pred_trat = (10**y_pred_trat).astype(np.int64)
y_test_trat = (10**y_test_trat).astype(np.int64)


# Salvando modelo em disco
filename = 'final_model.sav'
from sklearn.externals import joblib
joblib.dump(regressor, filename)



# Lendo o arquivo
loaded_model = joblib.load('final_model.sav')

# Fluxo #########

# Load
loaded_model = joblib.load('final_model.sav')
predict_model = np.array([3,0,1,1,1.78,1.78,3.47])
predict_model = predict_model.reshape(1,-1)
predict_model_log = 10**loaded_model.predict(predict_model)
predict_model_log.astype(np.int64)












