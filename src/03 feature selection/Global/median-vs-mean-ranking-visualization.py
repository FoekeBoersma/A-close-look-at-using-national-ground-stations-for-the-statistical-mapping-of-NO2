#import necessary modules
import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import sys
import os
import os.path as path

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

data_path = path.abspath(path.join(__file__ ,"../../../.."))

mean_sort = pd.read_csv(data_path  + output_map + '/df mean shap cv10.csv', sep=',', index_col=0)
median_sort = pd.read_csv(data_path  + output_map + '/df median shap cv10.csv', sep=',', index_col=0)

mean_sort['rank sorted by mean shap'] = range(1, len(mean_sort)+1,1)
#rename columns
median_sort = median_sort.rename(columns={'median':'rank sorted by median', 'name':'variable'})

#merge datasets
df_new = pd.merge(mean_sort,median_sort,on='variable',how='outer')
#to list 
cols = df_new.columns.tolist()
cols = cols[1:] + cols[:1]
df_new = df_new[cols]

#rename columns
df_new = df_new.rename(columns={'rank sorted by median': 'rank sorted by median (CV=10)', 'rank sorted by mean shap': 'rank sorted by mean shap (CV=10)'})

#assign to variables
x = df_new['rank sorted by mean shap (CV=10)']
y = df_new['rank sorted by median (CV=10)']

#define font
plt.rcParams["font.family"] = "serif"

plt.figure(figsize= (12,12))
plt.title("Mean vs median approach - CV ranking", fontweight='bold', pad=10)
plt.xlabel("CV rank via mean shap", fontsize=12)
plt.xticks(np.arange(min(x)-1, max(x)+5, 5))
plt.ylabel("CV rank via median", fontsize=12)
plt.yticks(np.arange(min(x)-1, max(x)+5, 5))
plt.grid()
plt.scatter(x, y)

output_location = data_path  + output_map
plot_filename = os.path.join(output_location, 'CV rank via median.png')
plt.savefig(plot_filename,bbox_inches='tight')