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
import xgboost as xgb
from pprint import pprint

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
model_metrics_xgboost = config_05.output['model_metrics_xgboost']
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

## == EVALUATING PERFORMANCE OF XGBOOST MODEL == ##
xg_reg = xgb.XGBRegressor()
xg_reg.fit(X_train, Y_train)

##DEFINING ORIGINAL Y-VALUES AND PREDICTED Y-VALUES
expected_y  = Y_test
predicted_y = xg_reg.predict(X_test)

expected_y_tr  = Y_train
predicted_y_tr = xg_reg.predict(X_train)

# Open a file in append mode
with open(output_location  + model_metrics_xgboost , "a") as file:
    # Writing the results to the file
    file.write("training rmse: " + str(rmse(predicted_y_tr, expected_y_tr)) + "\n")
    file.write("testing rmse: " + str(rmse(predicted_y, expected_y)) + "\n\n")

    file.write("R2 score training data: " + str(r2_score(expected_y_tr, predicted_y_tr)) + "\n")
    file.write("R2 score testing data: " + str(r2_score(expected_y, predicted_y)) + "\n\n")

    file.write("MAE training data: " + str(mean_absolute_error(expected_y_tr, predicted_y_tr)) + "\n")
    file.write("MAE testing data: " + str(mean_absolute_error(expected_y, predicted_y)) + "\n")


## == HYPERPARAMETER TUNING == ##
#VERIFYING CURRENT Xgboost HYPERPARAMETERS 

# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(xg_reg.get_params())

#Apply different parameter settings

xgboost_advanced = xgb.XGBRegressor(gamma = 5,  reg_alpha =2, reg_lambda=0, max_depth = 5, learning_rate = 0.002, n_estimators=2000, random_state=42)
xgboost_advanced.fit(X_train, Y_train)

##DEFINING ORIGINAL Y-VALUES AND PREDICTED Y-VALUES
expected_y_xgboost  = Y_test
predicted_y_xgboost_advanced = xgboost_advanced.predict(X_test)

expected_y_train_xgboost = Y_train
predicted_y_train_xgboost_advanced = xgboost_advanced.predict(X_train)


# Open a file in append mode
with open(output_location  + model_metrics_xgboost , "a") as file:
    # Writing the results to the file
    file.write("training rmse (ADVANCED): " + str(rmse(predicted_y_train_xgboost_advanced, Y_train)) + "\n")
    file.write("testing rmse (ADVANCED): " + str(rmse(predicted_y_xgboost_advanced, Y_test)) + "\n\n")

    file.write("R2 score training data (ADVANCED): " + str(r2_score(Y_train, predicted_y_train_xgboost_advanced)) + "\n")
    file.write("R2 score testing data (ADVANCED): " + str(r2_score(Y_test, predicted_y_xgboost_advanced)) + "\n\n")

    file.write("MAE training data (ADVANCED): " + str(mean_absolute_error(Y_train, predicted_y_train_xgboost_advanced)) + "\n")
    file.write("MAE testing data (ADVANCED): " + str(mean_absolute_error(Y_test, predicted_y_xgboost_advanced)) + "\n")

