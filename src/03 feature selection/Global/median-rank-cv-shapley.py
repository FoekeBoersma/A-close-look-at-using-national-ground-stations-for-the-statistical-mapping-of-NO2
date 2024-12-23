import os
import os.path as path
import pandas as pd
import sys
import numpy as np
from sklearn.ensemble import RandomForestRegressor

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
# Define the path to the directory containing config_03.py (one level up)
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
# Append the directory to sys.path
sys.path.append(config_directory)
from functions import cvfi_median, create_new_map
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
output_map = config_03.output['output_map_global']
global_shap_output_map = config_03.output['global_shap_output_map']
n_estimators = config_03.parameters['n_estimators_random_forest']
random_state = config_03.parameters['random_state']
nfolds = config_03.parameters['nfolds']
test_size = config_03.parameters['test_size']

data_path = path.abspath(path.join(__file__ ,"../../../.."))
dataset = pd.read_csv(data_path + dataset, sep=';')
map_path = data_path + output_map
create_new_map(global_shap_output_map, map_path)

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
#transform dependent- and independent variables to numpy arrays for calculations
x = np.array(x)
y = np.array(y)

## == CV FEATURE IMPORTANCE via Shapley values (median) == ##

#set initial model
rf = RandomForestRegressor(n_estimators = n_estimators, random_state = random_state) 

#generate function that creates K-fold CV, thereby creating shap summary plot for each loop/fold.
cvfi_median(x, y, rf, nfolds,test_size, feature_names, map_path, global_shap_output_map) #argumens: dependent variable; independent variables; model; amount of k-fold; test size; feature names

## == generate median == ##

# Assuming cvfi is an object with a ranking attribute - accessing rankings from function
rank = cvfi_median.ranking

# #create pandas dataframe from each fold of ranking
dfs = []
# Print the ranking for each fold dynamically based on nfolds
for i in range(nfolds):
    print(f"Round {i + 1}: {rank[i]}")
    df_nfold = pd.DataFrame(rank[i], columns = ['name', f'rank_fold_{i + 1}'])
    dfs.append(df_nfold)

#assign all dataframes to one dataframe that shows ranking per fold.
from functools import reduce
df_allranks = reduce(lambda left,right: pd.merge(left,right,on='name'), dfs)
print(df_allranks)

#drop string variable to do numpy calculations - necessary for obtaining median
df_fornumpy = df_allranks.drop(['name'], axis=1)

#convert dataframe to numpy array. Moreover sort values per list to obtain median.
df_num = df_fornumpy
num = df_num.to_numpy()
num_sort = np.sort(num)

##identify median per ranking
np_num = np.array(num_sort)
# print(np_num)
med_pervar = []
for i in np_num:
    #generate median for each variable (i.e. per list)
    k = np.median(i)
    print(k) 
    med_pervar.append(k)

#assign extra column to dataframe that calculates median of all rankings per variable
df_allranks['median'] = med_pervar
df_cvmedian = df_allranks.sort_values(by=['median'])

#export option
df_cvmedian.to_csv(map_path + global_shap_output_map + 'df median shap cv10.csv')