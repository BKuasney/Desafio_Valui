# libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os

# Import
os.chdir('C:/Users/bkuasney/Desktop/ML/FREELA')
dataset = pd.read_csv('base_python.csv')
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


# tunning
from sklearn.grid_search import GridSearchCV
from operator import itemgetter

clf = RandomForestRegressor(random_state = 84)
def evaluate_param(parameter, num_range, index):
    grid_search = GridSearchCV(clf, param_grid = {parameter: num_range})
    grid_search.fit(X_train, y_train)
    
    df={}
    for i, score in enumerate(grid_search.grid_scores_):
        df[score[0][parameter]] = score[1]

    df = pd.DataFrame.from_dict(df, orient='index')
    df.reset_index(level=0, inplace=True)
    df=df.sort_values(by='index')
    
    plt.subplot(3,2,index)
    plot = plt.plot(df['index'], df[0])
    plt.title(parameter)
    return plot, df


# parametros
param_grid = {"n_estimators": np.arange(2, 300, 2),
              "max_depth": np.arange(1, 28, 1),
              "min_samples_split": np.arange(2,150,2),
              "min_samples_leaf": np.arange(1,60,1),
              "max_leaf_nodes": np.arange(2,60,1),
              "min_weight_fraction_leaf": np.arange(0.1,0.4, 0.1)}

index = 1
plt.figure(figsize=(16,12))
for parameter, param_range in dict.items(param_grid):   
    evaluate_param(parameter, param_range, index)
    index += 1

from operator import itemgetter

# Utility function to report best scores
def report(grid_scores, n_top):
    top_scores = sorted(grid_scores, key=itemgetter(1), reverse=True)[:n_top]
    for i, score in enumerate(top_scores):
        print("Model with rank: {0}".format(i + 1))
        print("Mean validation score: {0:.4f})".format(
              score.mean_validation_score,
              np.std(score.cv_validation_scores)))
        print("Parameters: {0}".format(score.parameters))
        print("")

# parameters for GridSearchCV
param_grid2 = {"n_estimators": [10, 18, 22],
              "max_depth": [3, 5],
              "min_samples_split": [15, 20],
              "min_samples_leaf": [5, 10, 20],
              "max_leaf_nodes": [20, 40],
              "min_weight_fraction_leaf": [0.1]}

grid_search = GridSearchCV(clf, param_grid=param_grid2)
grid_search.fit(X_train, y_train)

report(grid_search.grid_scores_, 4)


param_grid3 = {"n_estimators": [20, 22, 25],
              "max_depth": [3, 5],
              "min_samples_split": [15, 20],
              "min_samples_leaf": [10, 20],
              "max_leaf_nodes": [20, 40]}


grid_search = GridSearchCV(clf, param_grid=param_grid3)
grid_search.fit(X_train, y_train)
report(grid_search.grid_scores_, 4)


param_grid4 = {"n_estimators": [22, 25],
              "max_depth": [5],
              "min_samples_split": [20, 22],
              "min_samples_leaf": [10, 20],
              "max_leaf_nodes": [40]}


grid_search = GridSearchCV(clf, param_grid=param_grid4)
grid_search.fit(X_train, y_train)
report(grid_search.grid_scores_, 4)


# Final
clf = RandomForestRegressor(min_samples_split = 22, 
                             max_leaf_nodes = 40, 
                             n_estimators = 100, 
                             max_depth = 5,
                             min_samples_leaf = 10)


clf.fit(X_train, y_train)

y_pred = clf.predict(X_test)

y_pred_trat = pd.DataFrame(y_pred)
y_test_trat = pd.DataFrame(y_test)

y_pred_trat = (10**y_pred_trat).astype(np.int64)
y_test_trat = (10**y_test_trat).astype(np.int64)


# Predicting
y_pred = regressor.predict(X_test)
y_pred_trat = pd.DataFrame(y_pred2)
y_test_trat = pd.DataFrame(y_test)
y_pred_trat = (10**y_pred_trat2).astype(np.int64)
y_test_trat = (10**y_test_trat2).astype(np.int64)


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












