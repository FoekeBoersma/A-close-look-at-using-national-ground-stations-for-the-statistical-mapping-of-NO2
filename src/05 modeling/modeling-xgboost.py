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
import os.path as path
from functions import (rmse)
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt


# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
# Define the path to the directory containing config_03.py (one level up)
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

print("Features used:", feature_list)


## == CONSTRUCTING RANDOM FOREST == ##

##GENERATING CROSS VALIDATION (75% TRAINING OF MODEL; 25% TESTING OF MODEL;EVALUATING MODEL PERFORMANCE)
X_train, X_test, Y_train, Y_test = train_test_split(x, y, test_size=test_size, random_state=20)
#random state is needed to ensure that same results are generated each time.

## STEP ENSURING THAT X- AND Y-TRAINING SET CONTAIN SAME AMOUNT OF ROWS. SAME FOR TESTING.
print('Training Features Shape:',X_train.shape)
print('Training Labels Shape:', Y_train.shape)
print('Testing Features Shape:', X_test.shape)
print('Testing Labels Shape:', Y_test.shape)

# define the n_estimators to test
n_estimators_range = [10, 50, 100, 500, 1000, 2000, 5000, 7500, 10000, 25000, 50000, 75000]
performance_results = []

# Loop through different estimator values
for n in n_estimators_range:
    xg_reg = xgb.XGBRegressor(gamma = 5,  reg_alpha =2, reg_lambda=0, max_depth = 5, learning_rate = 0.0005, n_estimators=n, random_state=42)
    ## == EVALUATING PERFORMANCE OF XGBOOST MODEL == ##
    xg_reg.fit(X_train, Y_train)

    # predictions
    y_pred = xg_reg.predict(X_test)

    # Evaluate performance
    r2 = r2_score(Y_test, y_pred)
    rmse_value = rmse(Y_test, y_pred)

    # store results
    performance_results.append([n, r2, rmse_value])
    print(f"n_estimators: {n}, RMSE: {rmse_value:.4f}, R²: {r2:.4f}")


# Convert results to DataFrame
performance_df = pd.DataFrame(performance_results, columns=['n_estimators', 'R2', 'RMSE'])

# Save CSV
csv_path = os.path.join(output_location, "xgboost_performance.csv")
performance_df.to_csv(csv_path, index=False)
print(f"Performance results saved to {csv_path}")

# Create subplots: 1 row, 2 columns
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

# First plot: R² Score
ax1.plot(performance_df['n_estimators'], performance_df['R2'], marker='o', linestyle='-', color='tab:blue', label="R² Score")
ax1.set_xlabel("Number of Estimators")
ax1.set_ylabel("R² Score", color='tab:blue')
ax1.set_title("XGBoost R² vs. Number of Estimators")
ax1.set_xscale("log")
ax1.grid(True)

# Second plot: RMSE Score
ax2.plot(performance_df['n_estimators'], performance_df['RMSE'], marker='s', linestyle='--', color='tab:red', label="RMSE Score")
ax2.set_xlabel("Number of Estimators")
ax2.set_ylabel("RMSE Score", color='tab:red')
ax2.set_title("XGBoost RMSE vs. Number of Estimators")
ax2.set_xscale("log")
ax2.grid(True)

# Adjust layout
fig.tight_layout()

# Save the figure
jpg_path = os.path.join(output_location, "xgboost_performance.jpg")
plt.savefig(jpg_path, dpi=300)
plt.show()  # Display the plots

