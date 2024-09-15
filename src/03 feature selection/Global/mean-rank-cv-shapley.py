import os
import os.path as path
import pandas as pd
import sys
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from functions import cvfi

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
# Define the path to the directory containing config_03.py (one level up)
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
# Append the directory to sys.path
sys.path.append(config_directory)

# Print sys.path to verify the directory was added
print("Current sys.path:")
for p in sys.path:
    print(p)

# Try importing the config_03 module
try:
    import config_03
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

# Access the dataset from the config_03 module
dataset = config_03.input_dataset['globaldataset']
output_map = config_03.output['output_map']
n_estimators = config_03.parameters['n_estimators_random_forest']
random_state = config_03.parameters['random_state']
nfolds = config_03.parameters['nfolds']
test_size = config_03.parameters['test_size']

data_path = path.abspath(path.join(__file__ ,"../../../.."))
dataset = pd.read_csv(data_path + dataset, sep=';')

## == data processing == ##

#get rid off unnecessary variables

#unique identifier and geodata
dataset = dataset.drop(['Longitude', 'Latitude'], axis=1)
#also drop temporal NO2 variables
dataset = dataset.drop(['FID', 'wkd_day_value', 'wnd_day_value', 'wkd_night_value', 'wnd_night_value'], axis=1)
#data characteristics
dataset.describe()
print(dataset.isna().sum().sum())
#remove NA values
dataset = dataset.dropna(axis = 0, how ='any', subset = None, inplace=False)


##CREATE DEPENDENT- AND INDEPENDENT VARIABLES
#store all column names
all_column_names = dataset.columns

#create dataset with only predictor variables
x = dataset.drop(["mean_value_NO2"], axis=1)
#store column names of predictor variables
feature_names = x.columns
#dependent variable
y = dataset["mean_value_NO2"]
#verify if dependent variable is out of dataset
print(len(feature_names))

#transform dependent- and independent variables to numpy arrays for calculations
x = np.array(x)
y = np.array(y)


#CONSTRUCT FUNCTION - CV FEATURE IMPORTANCE (MEAN)

#set initial model
rf = RandomForestRegressor(n_estimators = n_estimators, random_state = random_state) 

#generate function that creates K-fold CV, thereby creating shap summary plot for each loop/fold.
cvfi(x, y, rf, nfolds, test_size, feature_names, data_path, output_map) #argumens: dependent variable; independent variables; model; amount of k-fold; test size; feature names

#obtain mean statistics
mean = cvfi.meanfi

#translate the "features" and related "scores" into variable names and importance values.

df_importance = pd.Series(mean, index=feature_names)

df_importance = df_importance.to_frame('variable importance').reset_index() #converting series to columns

df_importance = df_importance.rename(columns={'index': 'variable'}) #change column name from index to variable
pd.set_option('display.max_rows', df_importance.shape[0]+1)

#order by importance
df_shap_feature_importance = df_importance.sort_values(by=['variable importance'], ascending=False)

#export option
df_shap_feature_importance.to_csv(data_path  + output_map + '/df mean shap cv10.csv')