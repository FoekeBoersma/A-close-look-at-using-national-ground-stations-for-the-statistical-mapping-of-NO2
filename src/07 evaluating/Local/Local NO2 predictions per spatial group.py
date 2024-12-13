import numpy as np # linear algebra
import pandas as pd
import os
import sys
import matplotlib.pyplot as plt
import seaborn as sns

## == path configuration and connecting == ##

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__)) 
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
sys.path.append(config_directory)

# Try importing the config_07 module
try:
    import config_07
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

local_predicting_dataset = config_07.input_data['local_prediction_columns']
output_map = config_07.output['output_map']

# read csv data
local = pd.read_csv(local_predicting_dataset, sep=';')
local = local.rename(columns ={"Unnamed: 0": "FID"  })

def f(row):
    if row['spachar'] == 1:
        val = 'Urban'
    elif row['spachar'] == 2:
        val = 'Suburban'
    else:
        val = 'Rural'
    return val

local['SpatialGroup'] = local.apply(f, axis=1)

#drop irrelevant column
local = local.drop(columns=['spachar'], axis=1)
local_melt = pd.melt(local, id_vars=['SpatialGroup'], var_name=['Model'])

## == raw plot == ##

sns.set(rc={'figure.figsize':(11.7,8.27)})
#define font
plt.rcParams["font.family"] = "serif"
Model_colors=["darkred","indianred","lightcoral","steelblue","lightskyblue", "blueviolet"]
ax = sns.boxplot(x="SpatialGroup", y="value", hue="Model", data=local_melt, palette=Model_colors) 
plot_filename_raw = os.path.join(output_map, 'Distribution predicted NO2 per model')
plt.savefig(plot_filename_raw, dpi=100)

# Clear the current figure to avoid overlapping
plt.clf()

## == filtered plot == ## 

#Remove outliers (below 0 and above 85)
local_0_85 = local_melt[(local_melt['value'] > 0) & (local_melt['value'] < 85)]
sns.set(rc={'figure.figsize':(11.7,8.27)})
#define font
plt.rcParams["font.family"] = "serif"
Model_colors=["darkred","indianred","lightcoral","steelblue","lightskyblue", "blueviolet"]
ax = sns.boxplot(x="SpatialGroup", y="value", hue="Model", data=local_0_85, palette=Model_colors)  
plot_filename_filtered = os.path.join(output_map, 'Distribution predicted NO2 per model NO2 values between 0_85')
plt.savefig(plot_filename_filtered, dpi=100)
