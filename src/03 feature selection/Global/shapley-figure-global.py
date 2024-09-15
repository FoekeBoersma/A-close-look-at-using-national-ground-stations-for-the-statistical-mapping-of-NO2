import os
import os.path as path
import pandas as pd
import sys
import numpy as np
from sklearn.ensemble import RandomForestRegressor
import shap
import matplotlib.pyplot as plt

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
test_size = config_03.parameters['test_size']
data_path = path.abspath(path.join(__file__ ,"../../../.."))
dataset = pd.read_csv(data_path + dataset, sep=';')

from sklearn.model_selection import train_test_split

#unique identifier and geodata
dataset = dataset.drop(['Longitude', 'Latitude'], axis=1)
#also drop temporal NO2 variables
dataset = dataset.drop(['wkd_day_value', 'wnd_day_value', 'wkd_night_value', 'wnd_night_value'], axis=1)

# Remove rows with any NA values
dataset = dataset.dropna(axis=0, how='any', inplace=False)

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

#CONSTRUCT FUNCTION - FEATURE IMPORTANCE 

#set initial model
rf = RandomForestRegressor(n_estimators = n_estimators, random_state = random_state)

#split dataset into training- and testing dataset
X_train, X_test, Y_train, Y_test = train_test_split(x, y, test_size=test_size, random_state=random_state)

# Train the model on training data
rf.fit(X_train, Y_train)

## == create shapley figure == ##

shap_values = shap.TreeExplainer(rf).shap_values(X_train)

#Define colormap
my_cmap = plt.get_cmap("RdBu_r")

shap.summary_plot(shap_values, X_train, feature_names=feature_names, show=False)
        
# Get the current figure and axes objects.
fig, ax = plt.gcf(), plt.gca()
#define font
plt.rcParams["font.family"] = "serif"

# Change the colormap of the artists
for fc in plt.gcf().get_children():
    for fcc in fc.get_children():
        if hasattr(fcc, "set_cmap"):
            fcc.set_cmap(my_cmap)

output_location = data_path  + output_map
plot_filename = os.path.join(output_location, 'Shapley_RdBu.png')
fig.savefig(plot_filename, bbox_inches='tight',
           facecolor=(1, 1, 1))
