#import necessary modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from sklearn.metrics import mean_absolute_error
import os
import sys
import os.path as path
from functions import (rmse)
import lightgbm as lgb
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
from pprint import pprint
##SOURCE: https://dataaspirant.com/lasso-regression/#t-1606404715785

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
# Define the path to the directory containing config_05.py (one level up)
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
# Append the directory to sys.path
sys.path.append(config_directory)

# Try importing the config_03 module
try:
    import config_05
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

dataset = config_05.input_dataset['globaldataset']
output_map = config_05.output['output_map']
model_metrics_lightgbm = config_05.output['model_metrics_lightgbm']
test_size = config_05.parameters['test_size']

data_path = path.abspath(path.join(__file__ ,"../../.."))

output_location = data_path  + output_map
#import dataset for modeling
df = pd.read_csv(data_path + dataset, sep=',')

#remove unique idenifier & geodata
df = df.drop(['Unnamed: 0', 'Longitude', 'Latitude'], axis=1)
#replace NaN's with 0's
df = df.fillna(0)

y = df['mean_value_NO2'] #specify target
x = df.drop(['mean_value_NO2'], axis=1) #predictors
feature_list = list(x.columns)

## == CONSTRUCTING RANDOM FOREST == ##

##GENERATING CROSS VALIDATION (75% TRAINING OF MODEL; 25% TESTING OF MODEL;EVALUATING MODEL PERFORMANCE)
X_train, X_test, Y_train, Y_test = train_test_split(x, y, test_size=test_size, random_state=20)
#random state is needed to ensure that same results are generated each time.

## STEP ENSURING THAT X- AND Y-TRAINING SET CONTAIN SAME AMOUNT OF ROWS. SAME FOR TESTING.
print('Training Features Shape:',X_train.shape)
print('Training Labels Shape:', Y_train.shape)
print('Testing Features Shape:', X_test.shape)
print('Testing Labels Shape:', Y_test.shape)

## PARAMETERS FOR MODEL
model = lgb.LGBMRegressor(random_state=42)
model.fit(X_train, Y_train)

##DEFINING ORIGINAL Y-VALUES AND PREDICTED Y-VALUES
expected_y  = Y_test
predicted_y = model.predict(X_test)

expected_y_tr  = Y_train
predicted_y_tr = model.predict(X_train)

## == EVALUATING PERFORMANCE OF LIGHTGBM MODEL == ##

# Open a file in append mode
with open(output_location  + model_metrics_lightgbm , "a") as file:
    # Writing the results to the file
    file.write("training rmse: " + str(rmse(predicted_y_tr, expected_y_tr)) + "\n")
    file.write("testing rmse: " + str(rmse(predicted_y, expected_y)) + "\n\n")

    file.write("R2 score training data: " + str(r2_score(expected_y_tr, predicted_y_tr)) + "\n")
    file.write("R2 score testing data: " + str(r2_score(expected_y, predicted_y)) + "\n\n")

    file.write("MAE training data: " + str(mean_absolute_error(expected_y_tr, predicted_y_tr)) + "\n")
    file.write("MAE testing data: " + str(mean_absolute_error(expected_y, predicted_y)) + "\n")


#HYPERPARAMETER TUNING
#VERIFYING CURRENT RANDOM FOREST HYPERPARAMETERS 

# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(model.get_params())

#CREATE PARAMETER GRID

# Number of trees in random forest
n_estimators = [int(x) for x in np.linspace(start = 200, stop = 2000, num = 10)]
# Number of features to consider at every split
max_features = ['auto', 'sqrt']
# Maximum number of levels in tree
max_depth = [int(x) for x in np.linspace(10, 110, num = 11)]
max_depth.append(None)
# Minimum number of samples required to split a node
min_samples_split = [2, 5, 10]
# Minimum number of samples required at each leaf node
min_samples_leaf = [1, 2, 4]
# Method of selecting samples for training each tree
bootstrap = [True, False]
# Create the random grid
random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap}
pprint(random_grid)
{'bootstrap': [True, False],
 'max_depth': [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, None],
 'max_features': ['auto', 'sqrt'],
 'min_samples_leaf': [1, 2, 4],
 'min_samples_split': [2, 5, 10],
 'n_estimators': [200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000]}

##RANDOM SEARCH TRAINING (https://towardsdatascience.com/hyperparameter-tuning-the-random-forest-in-python-using-scikit-learn-28d2aa77dd74)

# Use the random grid to search for best hyperparameters
# First create the base model to tune

model = lgb.LGBMRegressor(random_state=42)
# Random search of parameters, using 3 fold cross validation, 
# search across 100 different combinations, and use all available cores
model_random = RandomizedSearchCV(estimator = model, param_distributions = random_grid, n_iter = 100, cv = 3, verbose=2, random_state=42, n_jobs = -1)
# Fit the random search model
model_random.fit(X_train, Y_train)

model_random.best_params_

model_hyper = lgb.LGBMRegressor(n_estimators=200, min_samples_split=5, 
                                 min_samples_leaf=4, max_features='auto',
                                max_depth=10, bootstrap=True)

# Train the model on training data
model_hyper.fit(X_train, Y_train)

##DEFINING ORIGINAL Y-VALUES AND PREDICTED Y-VALUES
expected_y_H  = Y_test
predicted_y_H = model_hyper.predict(X_test)

expected_y_tr_H = Y_train
predicted_y_tr_H = model_hyper.predict(X_train)

# Open a file in append mode
with open(output_location  + model_metrics_lightgbm , "a") as file:
    # Writing the results to the file
    file.write('RMSE training dataset with hypertuning: '+ str(rmse(predicted_y_tr_H, expected_y_tr_H)) + "\n")
    file.write('RMSE testing dataset with hypertuning: '+ str(rmse(predicted_y_H, expected_y_H)) + "\n\n")

    file.write('R2 score training dataset with hypertuning: '+ str(r2_score(expected_y_tr_H, predicted_y_tr_H)) + "\n")
    file.write('R2 score testing dataset with hypertuning: '+ str(r2_score(expected_y_H, predicted_y_H)) + "\n\n")

    file.write('MAE training dataset with hypertuning: '+ str(mean_absolute_error(expected_y_tr_H, predicted_y_tr_H)) + "\n")
    file.write('MAE testing dataset with hypertuning: '+ str(mean_absolute_error(expected_y_H, predicted_y_H)) + "\n")

# Create the parameter grid based on the results of random search 
param_grid = {
    'bootstrap': [True],
    'max_depth': [80, 90, 100, 110],
    'max_features': [2, 3],
    'min_samples_leaf': [3, 4, 5],
    'min_samples_split': [8, 10, 12],
    'n_estimators': [100, 200, 300, 1000]
}
# Create a based model
model = lgb.LGBMRegressor(random_state=42)
# Instantiate the grid search model
grid_search = GridSearchCV(estimator = model, param_grid = param_grid, 
                          cv = 3, n_jobs = -1, verbose = 2)

# Fit the grid search to the data
grid_search.fit(X_train, Y_train)
grid_search.best_params_

# Create best grid model
model_grid = lgb.LGBMRegressor(random_state=42, bootstrap=True, max_depth=80, max_features=2, 
                         min_samples_leaf=3, min_samples_split=8, n_estimators=100)
model_grid.fit(X_train, Y_train)

##DEFINING ORIGINAL Y-VALUES AND PREDICTED Y-VALUES (FOR TESTING DATASET)
expected_ygrid  = Y_test
predicted_ygrid = model_grid.predict(X_test) #Predicting the test set results using the random forest regressor model

##DEFINING ORIGINAL Y-VALUES AND PREDICTED Y-VALUES (FOR TRAINING DATASET)

expected_y_tr_grid = Y_train
predicted_y_tr_grid = model_grid.predict(X_train)

with open(output_location  + model_metrics_lightgbm , "a") as file:
    # Writing the results to the file
    file.write('RMSE training dataset with hypertuning (GRID): '+ str(rmse(predicted_y_tr_grid, expected_y_tr_grid)) + "\n")
    file.write('RMSE testing dataset with hypertuning (GRID): '+ str(rmse(predicted_ygrid, Y_test))  + "\n\n")

    file.write('R2 score training dataset with hypertuning (GRID): '+ str(r2_score(expected_y_tr_grid, predicted_y_tr_grid))  + "\n")
    file.write('R2 score testing dataset with hypertuning (GRID): '+ str(r2_score(Y_test, predicted_ygrid))  + "\n\n")

    file.write('MAE training dataset with hypertuning (GRID): '+ str(mean_absolute_error(expected_y_tr_grid, predicted_y_tr_grid))  + "\n")
    file.write('MAE testing dataset with hypertuning (GRID): '+ str(mean_absolute_error(expected_ygrid, predicted_ygrid))  + "\n")