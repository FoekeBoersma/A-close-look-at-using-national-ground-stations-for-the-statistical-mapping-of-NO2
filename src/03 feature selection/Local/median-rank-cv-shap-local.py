#import necessary modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
#import necessary modules that are included into the function
import shap
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import KFold, RepeatedKFold, LeaveOneOut, LeavePOut,ShuffleSplit
import itertools 
import matplotlib.pyplot as plt
from functools import reduce
import os.path as path
import sys
import os


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
from functions import cvfi_median
# Access the dataset from the config_03 module
dataset = config_03.input_dataset['localdataset']
output_map = config_03.output['output_map_local']
n_estimators = config_03.parameters['n_estimators_random_forest']
random_state = config_03.parameters['random_state']
nfolds = config_03.parameters['nfolds']
test_size = config_03.parameters['test_size']

data_path = path.abspath(path.join(__file__ ,"../../../.."))
dataset = pd.read_csv(data_path + dataset, sep=';')


pd.set_option('display.max_columns', None)
dataset.describe()
dataset=dataset.fillna(0)

#unique identifier and geodata
dataset = dataset.drop(['Longitude', 'Latitude'], axis=1)
print(len(dataset.columns))

##CREATE DEPENDENT- AND INDEPENDENT VARIABLES
#store all column names
all_column_names = dataset.columns

#create dataset with only predictor variables
x = dataset.drop(["Lopend_gemiddelde"], axis=1)
#store column names of predictor variables
feature_names = x.columns
#dependent variable
y = dataset["Lopend_gemiddelde"]
#verify if dependent variable is out of dataset
print(len(feature_names))
#transform dependent- and independent variables to numpy arrays for calculations
x = np.array(x)
y = np.array(y)



#CONSTRUCT FUNCTION - CV FEATURE IMPORTANCE (MEAN)

#set initial model
rf = RandomForestRegressor(n_estimators = n_estimators, random_state = random_state) 

#generate function that creates K-fold CV, thereby creating shap summary plot for each loop/fold.
cvfi_median(x, y, rf, nfolds,test_size, feature_names, data_path, output_map) #argumens: dependent variable; independent variables; model; amount of k-fold; test size; feature names

# Assuming cvfi is an object with a ranking attribute
rank = cvfi_median.ranking

print(rank)

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
print(num_sort)

##identify median per ranking
np_num = np.array(num_sort)
# print(np_num)
med_pervar = []
for i in np_num:
    #generate median for each variable (i.e. per list)
    k = np.median(i)
    print(k) 
    med_pervar.append(k)

##assign extra column to dataframe that calculates median of all rankings per variable
df_allranks['median'] = med_pervar
print(df_allranks)

df_cvmedian = df_allranks.sort_values(by=['median'])
print(df_cvmedian)

#export option
df_cvmedian.to_csv(data_path  + output_map + 'df median shap cv10local.csv')