#import necessary modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
#import necessary modules that are included into the function
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot as plt
from math import log
import re
import sys
import os
import os.path as path


# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
# Define the path to the directory containing config_03.py (one level up)
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
# Append the directory to sys.path
sys.path.append(config_directory)
from functions import (cvaic)
# Try importing the config_03 module
try:
    import config_03
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

# Access the dataset from the config_03 module
dataset = config_03.input_dataset['localdataset']
output_map = config_03.output['output_map_local']
no_best_predictors = config_03.parameters['no_predictors_model_local']
no_predictors_model = config_03.parameters['no_predictors_model']
random_state = config_03.parameters['random_state']
nfolds = config_03.parameters['nfolds']
test_size = config_03.parameters['test_size']
data_path = path.abspath(path.join(__file__ ,"../../../.."))
output_location = data_path  + output_map
dataset = pd.read_csv(data_path + dataset, sep=';')

pd.set_option('display.max_columns', None)
dataset.describe()

#replace NA with 0
dataset=dataset.fillna(0)
#store geodata
data_xy = dataset[['Longitude', 'Latitude']]

#get rid off unnecessary variables

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


medianshap_dataset = pd.read_csv(data_path  + output_map + 'df median shap cv10local.csv')




random_state = 0
n_estimators = 1000
min_samples_split = 10
min_samples_leaf = 5
max_features="sqrt"
max_depth = 10
bootstrap = True

#set initial model
model = RandomForestRegressor(n_estimators = n_estimators, random_state = random_state, min_samples_split=min_samples_split,
                          min_samples_leaf=min_samples_leaf,max_features=max_features,max_depth=max_depth, bootstrap=bootstrap )


novar_list = []
i=1
novar = range(1,no_best_predictors,1) #30 best predictors are subject to predictor selection



while i < len(novar):
    sorted_vars_names = medianshap_dataset['name'].head(novar[i])

    sorted_vars_names = sorted_vars_names.to_string(index=False)
    names = '|'.join(sorted_vars_names.split())

    novar_list.append(names)
    i+=1


novar_regex = [re.sub(r'(?<=BldDen100)|(?<=road_class_3_300)\b|(?<=trafBuf100)\b|(?<=road_class_2_50)\b|(?<=road_class_2_100)\b|(?<=road_class_3_100)\b|(?<=road_class_3_50)\b' , '$', i) for i in novar_list]

i = 0 #number of independent variables needed


# Initialize lists to store results
CV_MSE = []
CV_R2 = []
CV_RMSE = []
Novar = []

while i < len(novar_regex):
    print(novar_regex[i])
   
    input_dataset = dataset.filter(regex=novar_regex[i])

    colen = len(input_dataset.columns)
    print(colen)
    best_x = np.array(input_dataset)

    cvaic(best_x, y, model, nfolds, test_size, feature_names, colen, random_state) #ind. vars; dep var; specific model; number of folds, amount of vars.
    #CV_aic = cvaic.meanaic
    CV_mse = cvaic.meanmse
    CV_r2 = cvaic.meanr2 
    CV_rmse = cvaic.meanrmse 

    #print(CV_aic)
    print(CV_mse)
    print(CV_r2)
    print(CV_rmse)
    
    # Store results in the corresponding lists
    CV_MSE.append(CV_mse)
    CV_R2.append(CV_r2)
    CV_RMSE.append(CV_rmse)
    
    # Append the number of variables (columns) to the Novar list
    Novar.append(colen)
    i+=1
    


# Construct a DataFrame from the results
Novar_performances = pd.DataFrame({
    'Novar': Novar,        # Number of variables in each model
    'CV_MSE': CV_MSE,      # Mean Squared Error for each model
    'CV_R2': CV_R2,        # R-squared for each model
    'CV_RMSE': CV_RMSE     # Root Mean Squared Error for each model
})

# Print the performance DataFrame
print(Novar_performances)

Novar_performances['CV_MSE'] = Novar_performances['CV_MSE'].str[0]
Novar_performances['CV_R2'] = Novar_performances['CV_R2'].str[0]
Novar_performances['CV_RMSE'] = Novar_performances['CV_RMSE'].str[0]

print(Novar_performances)

perfs = ['CV MSE', 'CV R2', 'CV RMSE']
perfs_names = ['CV_MSE', 'CV_R2', 'CV_RMSE']
perfs_abvs = ['MSE', 'R2', 'RMSE']
colors = ['darkblue', 'darkgreen', 'darkred']


a=0
b=0
c=0
d=0
while a < len(perfs_names):
    
    #define font
    plt.rcParams["font.family"] = "serif"
    
    x = Novar_performances["Novar"]
    y = Novar_performances[perfs_names[a]]
    fig, ax = plt.subplots()
    plt.title("No. of independent variables - "+  str(perfs[b]), fontweight='bold', pad=10)
    plt.xlabel("No. of independent variables")
    plt.ylabel(perfs_abvs[b] + " Score")
    
    plt.scatter(x, y, color = colors[d], zorder=10)
    ax.vlines([5, 10, 15, 20, 25, 30],min(y),max(y), linestyles='solid', colors='lightgrey', alpha=0.7, zorder=1)
    
    plt.savefig(output_location + 'Novars_' + str(perfs_names[a]) + '.jpg',bbox_inches='tight')
    a+=1
    b+=1
    c+=1
    d+=1


## Select dataset (no. vars) with best performance

#define no. of predictors that corresponds with best model performance
i = no_predictors_model - 2 #(a - b) - a is equal to preferred no. of predictors.
no_predictors_model_filter = novar_regex[i]

dataset_predicting = dataset.filter(regex = no_predictors_model_filter)
dataset_predicting = dataset_predicting.merge(data_xy, left_index=True, right_index=True)

dataset_predicting.to_csv(output_location + '/PredictingDataset.csv')