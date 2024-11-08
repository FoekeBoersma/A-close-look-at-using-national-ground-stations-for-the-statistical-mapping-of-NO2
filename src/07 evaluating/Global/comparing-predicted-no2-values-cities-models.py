# import necessary modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt
import seaborn as sns
import os
import sys

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
sys.path.append(config_directory)

# Try importing the config_07 module
try:
    import config_07
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

amsterdam_predicting_no2_allmodels = config_07.input_data['amsterdam_predicting_no2_allmodels']
bayreuth_predicting_no2_allmodels = config_07.input_data['bayreuth_predicting_no2_allmodels']
hamburg_predicting_no2_allmodels = config_07.input_data['hamburg_predicting_no2_allmodels']
utrecht_predicting_no2_allmodels = config_07.input_data['amsterdam_predicting_no2_allmodels']
output_map = config_07.output['output_map']

amsterdam_predicting_no2_allmodels = pd.read_csv(amsterdam_predicting_no2_allmodels, sep=',')
bayreuth_predicting_no2_allmodels = pd.read_csv(bayreuth_predicting_no2_allmodels, sep = ',')
hamburg_predicting_no2_allmodels = pd.read_csv(hamburg_predicting_no2_allmodels, sep=',')
utrecht_predicting_no2_allmodels  = pd.read_csv(utrecht_predicting_no2_allmodels , sep=',')

amsterdam_predicting_no2_allmodels = amsterdam_predicting_no2_allmodels.rename(columns ={"Unnamed: 0": "FID"  })
bayreuth_predicting_no2_allmodels = bayreuth_predicting_no2_allmodels.rename(columns ={"Unnamed: 0": "FID"  })
hamburg_predicting_no2_allmodels = hamburg_predicting_no2_allmodels.rename(columns ={"Unnamed: 0": "FID"  })
utrecht_predicting_no2_allmodels = utrecht_predicting_no2_allmodels.rename(columns ={"Unnamed: 0": "FID"  })

Amsterdam = amsterdam_predicting_no2_allmodels[['predicted_NO2_RF', 'predicted_NO2_LightGBM',
       'predicted_NO2_XGBoost', 'predicted_NO2_RIDGE',
       'predicted_NO2_LASSO']].assign(location='Amsterdam')

Bayreuth = bayreuth_predicting_no2_allmodels[['predicted_NO2_RF', 'predicted_NO2_LightGBM',
       'predicted_NO2_XGBoost', 'predicted_NO2_RIDGE',
       'predicted_NO2_LASSO']].assign(location='Bayreuth')

Hamburg = hamburg_predicting_no2_allmodels[['predicted_NO2_RF', 'predicted_NO2_LightGBM',
       'predicted_NO2_XGBoost', 'predicted_NO2_RIDGE',
       'predicted_NO2_LASSO']].assign(location='Hamburg')

Utrecht = utrecht_predicting_no2_allmodels[['predicted_NO2_RF', 'predicted_NO2_LightGBM',
       'predicted_NO2_XGBoost', 'predicted_NO2_RIDGE',
       'predicted_NO2_LASSO']].assign(location='Utrecht')


cities = pd.concat([Amsterdam, Bayreuth, Hamburg, Utrecht])
cities_melt = pd.melt(cities, id_vars=['location'], var_name=['Model'])


sns.set(rc={'figure.figsize':(11.7,8.27)})

#define font
plt.rcParams["font.family"] = "serif"
Model_colors=["darkred","indianred","lightcoral","steelblue","lightskyblue"]
ax = sns.boxplot(x="location", y="value", hue="Model", data=cities_melt, palette=Model_colors)    

# Save plot
plot_filename = os.path.join(output_map, f'Distribution predicted NO2 per model.png')
plt.savefig(plot_filename, dpi=100)
plt.show()
