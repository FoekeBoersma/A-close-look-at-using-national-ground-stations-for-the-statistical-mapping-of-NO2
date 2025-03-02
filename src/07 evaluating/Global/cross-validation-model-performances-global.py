#import necessary modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
import xgboost as xgb
from sklearn.metrics import r2_score
from sklearn.metrics import mean_absolute_error
from sklearn.linear_model import Ridge, RidgeCV, Lasso, LassoCV
import lightgbm as lgb
import matplotlib.pyplot as plt
import re
import os
import sys

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__)) 
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
root_directory = os.path.abspath(os.path.join(current_directory, '../../..'))
sys.path.append(config_directory)
sys.path.append(root_directory)

# Try importing the config_07 module
try:
    import config_07
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")
from functions import cross_validate_models, create_new_map
predicting_dataset = config_07.input_data['predicting_dataset']

n_estimators_random_forest = config_07.parameters_random_forest ['n_estimators_random_forest']
random_state_random_forest = config_07.parameters_random_forest ['random_state_random_forest']
min_samples_split_random_forest = config_07.parameters_random_forest ['min_samples_split_random_forest']
min_samples_leaf_random_forest= config_07.parameters_random_forest ['min_samples_leaf_random_forest']
max_features_random_forest= config_07.parameters_random_forest ['max_features_random_forest']
max_depth_random_forest = config_07.parameters_random_forest ['max_depth_random_forest']
bootstrap_random_forest = config_07.parameters_random_forest ['bootstrap_random_forest']
test_size = config_07.parameters_random_forest ['test_size']

# Load parameters for LightGBM and XGBoost from config_07
reg_alpha_lightgbm = config_07.parameters_lightgbm['reg_alpha_lightgbm']
reg_lambda_lightgbm = config_07.parameters_lightgbm['reg_lambda_lightgbm']
max_depth_lightgbm = config_07.parameters_lightgbm['max_depth_lightgbm']
learning_rate_lightgbm = config_07.parameters_lightgbm['learning_rate_lightgbm']
n_estimators_lightgbm = config_07.parameters_lightgbm['n_estimators_lightgbm']
random_state_lightgbm = config_07.parameters_lightgbm['random_state_lightgbm']

# Load parameters for XGBoost from config_07
gamma_xgboost = config_07.parameters_xgboost['gamma_xgboost']
reg_alpha_xgboost = config_07.parameters_xgboost['reg_alpha_xgboost']
reg_lambda_xgboost = config_07.parameters_xgboost['reg_lambda_xgboost']
max_depth_xgboost = config_07.parameters_xgboost['max_depth_xgboost']
learning_rate_xgboost = config_07.parameters_xgboost['learning_rate_xgboost']
n_estimators_xgboost = config_07.parameters_xgboost['n_estimators_xgboost']
random_state_xgboost = config_07.parameters_xgboost['random_state_xgboost']

output_map = root_directory + '/' + config_07.output['output_map']
output_map_global_model_performance = config_07.output['output_map_global_model_performance']
create_new_map(output_map_global_model_performance, output_map)

output_map = output_map + '/' + output_map_global_model_performance + '/'

df = pd.read_csv(predicting_dataset, sep=',')
#remove geodata
df = df.drop(['Unnamed: 0', 'Longitude', 'Latitude'], axis=1)
#remove NAs and replace with 0
df=df.fillna(0)

y = df['mean_value_NO2'] #target
x = df.drop(['mean_value_NO2'], axis=1) #predictors
feature_list = list(x.columns)

#models

#Non-linear

#Random Forest
rf = RandomForestRegressor(n_estimators = n_estimators_random_forest, random_state = random_state_random_forest, 
                           min_samples_split=min_samples_split_random_forest,
                          min_samples_leaf=min_samples_leaf_random_forest,max_features=max_features_random_forest,
                          max_depth=max_depth_random_forest, bootstrap=bootstrap_random_forest )

#LightGBM
lightgbm_model = lgb.LGBMRegressor(
    reg_alpha=reg_alpha_lightgbm,
    reg_lambda=reg_lambda_lightgbm,
    max_depth=max_depth_lightgbm,
    learning_rate=learning_rate_lightgbm,
    n_estimators=n_estimators_lightgbm,
    random_state=random_state_lightgbm
)

#XGBoost
xgb = xgb.XGBRegressor(
    gamma=gamma_xgboost,
    reg_alpha=reg_alpha_xgboost,
    reg_lambda=reg_lambda_xgboost,
    max_depth=max_depth_xgboost,
    learning_rate=learning_rate_xgboost,
    n_estimators=n_estimators_xgboost,
    random_state=random_state_xgboost
)

#linear models

#Lasso
model_lasso = Lasso(alpha=0.1)
#Ridge
ridge = Ridge(alpha = 0.1)
#random states - CV = 20
random_states = [1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95]

#random state is needed to ensure that same results are generated each time.
#models
models = [rf, xgb, model_lasso, ridge]

# #model_names
model_names = ['rf', 'xgb', 'lasso', 'ridge']

#create dictionaries that eventually stores performance per model (CV)
total_rmse = {}
total_r2 = {}
total_mae = {}
rmse_vals_round = []

print(test_size)
# Call cross_validate_models and capture output in the dictionaries
total_rmse, total_r2, total_mae = cross_validate_models(models, model_names, x, y, random_states, test_size)

# Convert the dictionaries to pandas DataFrames for easier plotting
df_total_rmse = pd.DataFrame(total_rmse)
df_total_r2 = pd.DataFrame(total_r2)
df_total_mae = pd.DataFrame(total_mae)

## == plot metrics per model == ##

# RMSE Plot 
plt.rcParams["font.family"] = "serif"
fig, ax = plt.subplots()
fig.suptitle(f'Variability in Root Mean Square Error (CV={len(random_states)}) per model', fontsize=12, fontweight='bold')

# Use boxplot on DataFrame 
ax.boxplot([df_total_rmse[model] for model in df_total_rmse.columns])

# Set tick labels and axis labels
ax.set_xticks(range(1, len(df_total_rmse.columns) + 1))
ax.set_xticklabels(df_total_rmse.columns)
ax.set_xlabel('Models')
ax.set_ylabel('RMSE')

plot_filename = os.path.join(output_map, 'RMSE - CV20 - ModelPerformances')
plt.savefig(plot_filename, dpi=100)


# R2 Plot 
plt.rcParams["font.family"] = "serif"
fig, ax = plt.subplots()
fig.suptitle(f'Variability in R2 (CV={len(random_states)}) per model', fontsize=14, fontweight='bold')

# Use boxplot on each model’s R2 values 
ax.boxplot([df_total_r2[model] for model in df_total_r2.columns])

# Set tick labels and axis labels
ax.set_xticks(range(1, len(df_total_r2.columns) + 1))
ax.set_xticklabels(df_total_r2.columns)
ax.set_xlabel('Models')
ax.set_ylabel('R2')

plot_filename = os.path.join(output_map, 'R2 - CV20 - ModelPerformances')
plt.savefig(plot_filename, dpi=100)

# MAE Plot 
plt.rcParams["font.family"] = "serif"
fig, ax = plt.subplots()
fig.suptitle(f'Variability in MAE (CV={len(random_states)}) per model', fontsize=14, fontweight='bold')

# Use boxplot on each model’s MAE values 
ax.boxplot([df_total_mae[model] for model in df_total_mae.columns])

# Set tick labels and axis labels
ax.set_xticks(range(1, len(df_total_mae.columns) + 1))
ax.set_xticklabels(df_total_mae.columns)
ax.set_xlabel('Models')
ax.set_ylabel('MAE')

plot_filename = os.path.join(output_map, 'MAE - CV20 - ModelPerformances')
plt.savefig(plot_filename, dpi=100)


