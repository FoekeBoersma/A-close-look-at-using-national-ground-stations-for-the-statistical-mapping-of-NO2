# import modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
#import necessary module(s) for random forest
from sklearn.ensemble import RandomForestRegressor
#import necessary module(s) for LASSO
from sklearn.linear_model import Lasso
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
#import necessary module(s) for RIDGE
from sklearn.linear_model import Ridge, RidgeCV, Lasso, LassoCV
#import necessary module(s) for LightGBM
import lightgbm as lgb
#import necessary module(s) for XGBoost
import xgboost as xgb
import sys
import os

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
sys.path.append(config_directory)

main_dir = os.path.abspath(os.path.join(current_directory, '../../..'))

# Try importing the config_07 module
try:
    import config_06
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

predicting_dataset = config_06.input_data['predicting_dataset']
grid100_amsterdam_csv = config_06.input_data['grid100_amsterdam_csv']
grid100_amsterdam_ga_csv = config_06.input_data['grid100_amsterdam_ga_csv']
grid100_bayreuth_csv = config_06.input_data['grid100_bayreuth_csv']
grid100_hamburg_csv = config_06.input_data['grid100_hamburg_csv']
grid100_utrecht_csv = main_dir  + os.sep + config_06.input_data['grid100_utrecht_csv']
output_map=main_dir + os.sep  + config_06.output['output_map']
output_map_excel= main_dir  + os.sep + config_06.output['output_map_excel']
# specify city so that it is included in output-name.
city = "Amsterdam100_ga"

#import training dataset 
df_train = pd.read_csv(predicting_dataset, sep=',')
df_train

print(df_train)

#replace NA with 0
df_train=df_train.fillna(0)
#remove unnecessary information in training dataset
df_train = df_train.drop(['Unnamed: 0', 'Longitude', 'Latitude'], axis=1)

#import testing dataset - to this unprocessed dataset, the predicted NO2 will eventually be added.

#AMSTERDAM
# df_test_xy = pd.read_csv(grid100_amsterdam_csv, sep=',')

#AMSTERDAM - greater area
df_test_xy = pd.read_csv(grid100_amsterdam_ga_csv, sep=',')

# #HAMBURG
# df_test_xy = pd.read_csv(grid100_hamburg_csv, sep=',')

#Bayreuth
# df_test_xy = pd.read_csv(grid100_bayreuth_csv, sep=',')

# # # #Utrecht
# df_test_xy = pd.read_csv(grid100_utrecht_csv, sep=',')

print(df_test_xy)

#remove unnecessary information in testing dataset
df_test_processed = df_test_xy.drop(['Unnamed: 0', 'Longitude', 'Latitude'], axis=1, errors='ignore')
df_test_processed = df_test_processed.rename(columns={'trop_mean_filt': 'trop_mean_filt_2019'})
#define response- and predictor variables
Y_train = df_train['mean_value_NO2']
X_train = df_train.drop(['mean_value_NO2'], axis=1)
X_test = df_test_processed

print(X_test.columns)
print(X_train.columns)

#validate if columns of training and testing dataset are the same
print(len(X_test.columns))
print(len(X_train.columns))


# ## PARAMETERS FOR MODEL - RANDOM FOREST

# # Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 1000, random_state = 42, min_samples_split=10,
                          min_samples_leaf=5,max_features=4,max_depth=10, bootstrap=True )
# Train the model on training data
rf.fit(X_train, Y_train)

#predict NO2 values, based on random forest model that is trained by the training dataset
predicted_NO2 = rf.predict(X_test)

#add predicted values as a column to copy of original dataset ("df_test_xy")
df_test_xy_RF = df_test_xy
df_test_xy_RF['predicted_NO2_RF'] = predicted_NO2

#export to .xlsx & .csv
if not os.path.exists(output_map_excel):
    os.makedirs(output_map_excel)

if not os.path.exists(output_map):
    os.makedirs(output_map)
df_test_xy_RF.to_excel(output_map_excel+'Predicting NO2-RF'+ str(city)+'_xy.xlsx')
df_test_xy_RF.to_csv(output_map+'Predicting NO2-RF'+ str(city)+'_xy.csv')

#set alpha to 0.1 and train model based on training data
model_lasso = Lasso(alpha=0.1)
model_lasso.fit(X_train, Y_train)

#predict NO2 values, based on LASSO model that is trained by the training dataset
predicted_NO2= model_lasso.predict(X_test)

#add predicted values as a column to copy of the original dataset ("df_test_xy")
df_test_xy_LAS = df_test_xy
df_test_xy_LAS['predicted_NO2_LASSO'] = predicted_NO2

#export to .xlsx & .csv
df_test_xy_LAS.to_excel(output_map_excel+'Predicting NO2-RF-LAS-'+ str(city)+'_xy.xlsx')
df_test_xy_LAS.to_csv(output_map+'Predicting NO2-RF-LAS-'+ str(city)+'_xy.csv')

#set alpha to 0.1 and train model based on training data
model_ridge = Ridge(alpha = 0.1)
model_ridge.fit(X_train, Y_train) #again, data is standardized

#predict NO2 values, based on RIDGE model that is trained by the training dataset
predicted_NO2= model_ridge.predict(X_test)

#add predicted values as a column to copy of the original dataset ("df_test_xy")
df_test_xy_RID = df_test_xy
df_test_xy_RID['predicted_NO2_RIDGE'] = predicted_NO2

#export to .xlsx & .csv
df_test_xy_RID.to_excel(output_map_excel+'Predicting NO2-RF-LAS-RID-'+ str(city)+'_xy.xlsx')
df_test_xy_RID.to_csv(output_map+'Predicting NO2-RF-LAS-RID-'+ str(city)+'_xy.csv')

#set model parameters and train model on training dataset
model = lgb.LGBMRegressor(reg_alpha =2, reg_lambda = 0, max_depth = 5, learning_rate = 0.0005, n_estimators =50000, random_state=42)
model.fit(X_train, Y_train)

#predict NO2 values, based on LightGBM model that is trained by the training dataset
predicted_NO2 = model.predict(X_test)

#add predicted values as a column to copy of the original dataset ("df_test_xy")
df_test_xy_LGB = df_test_xy
df_test_xy_LGB['predicted_NO2_LightGBM'] = predicted_NO2

#export to .xlsx & .csv
df_test_xy_LGB.to_excel(output_map_excel+'Predicting NO2-RF-LAS-RID-LGB-'+ str(city)+'_xy.xlsx')
df_test_xy_LGB.to_csv(output_map+'Predicting NO2-RF-LAS-RID-LGB-'+ str(city)+'_xy.csv')

#set model parameters and train model on training dataset
xg_reg = xgb.XGBRegressor(gamma = 5,  reg_alpha =2, reg_lambda=0, max_depth = 5, learning_rate = 0.0005, n_estimators=10000, random_state=42)
xg_reg.fit(X_train, Y_train)

#predict NO2 values, based on XGBoost model that is trained by the training dataset
predicted_NO2 = xg_reg.predict(X_test)

#add predicted values as a column to copy of the original dataset ("df_test_xy")
df_test_xy_XGB = df_test_xy
df_test_xy_XGB['predicted_NO2_XGBoost'] = predicted_NO2

#export to .xlsx & .csv
df_test_xy_XGB.to_excel(output_map_excel+'Predicting NO2-AllModels_xgboost_n10000'+ str(city)+'_xy.xlsx')
df_test_xy_XGB.to_csv(output_map+'Predicting NO2-AllModels_xgboost_n10000'+ str(city)+'_xy.csv')