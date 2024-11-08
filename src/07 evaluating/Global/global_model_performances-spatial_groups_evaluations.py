#import necessary modules

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from sklearn.metrics import mean_absolute_error
from sklearn.linear_model import Ridge, RidgeCV, Lasso, LassoCV
import lightgbm as lgb
import geopandas as gdp
import os 
import sys
from functions import cross_validate_models_chars

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
sys.path.append(config_directory)

# Try importing the config_07 module
try:
    import config_07
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")


modeling_dataset = config_07.input_data['modeling_dataset']
global_spachar_dataset = config_07.input_data['global_spachar']

#import dataset 
df = pd.read_csv(modeling_dataset, sep=';')
output_map = config_07.output['output_map']
output_map_excel = config_07.output['output_map_excel']

pd.set_option('display.max_columns', None)

#replace NA with 0
df=df.fillna(0)
#create unique identifier
df['FID'] = range(1,len(df)+1, 1)

df['road_class_3_100'].describe()
df['population_1000'].describe()

df['Urban'] = np.where(df['population_1000'].gt(df['population_1000'].quantile(0.75)) & (df['road_class_1_100'].gt(0) | df['road_class_2_100'].gt(0) | df['road_class_3_100'].gt(df['road_class_3_100'].quantile(0.75))), 1, 0)
df['Urban'].value_counts()

df['LowPop'] = np.where(df['population_1000'].lt(df['population_1000'].quantile(0.75)) & (df['road_class_1_100'].gt(0) | df['road_class_2_100'].gt(0)  | df['road_class_3_100'].gt(df['road_class_3_100'].quantile(0.75))), 1, 0)
df['LowPop'].value_counts()

df['FarFromRoad'] = np.where(df['Urban'] | df['LowPop'] == 1, 0, 1)
df['FarFromRoad'].value_counts()

def spachar(row):
    if row['Urban'] == 1:
        return 1
    if row['LowPop'] == 1:
        return 2
    if row['FarFromRoad'] == 1:
        return 3
    else:
        return 0
    
df['spachar'] = df.apply (lambda row: spachar(row), axis=1)

df_sel = df[['Longitude', 'Latitude', 'mean_value_NO2', 'population_1000', 'road_class_1_100', 'road_class_2_100', 'road_class_3_100', 'spachar']]

#create different datasets so that NO2 distribution per spatial character can be easily examined

Urb = df_sel.loc[df_sel['spachar'] == 1]
Lowpop = df_sel.loc[df_sel['spachar'] == 2]
FFR = df_sel.loc[df_sel['spachar'] == 3]

Urb['mean_value_NO2'].describe()
Lowpop['mean_value_NO2'].describe()
FFR['mean_value_NO2'].describe()

#export option (geopackage) to examine spatial distribution of different groups (spatial characters)
gdf = gdp.GeoDataFrame(df_sel, geometry=gdp.points_from_xy(df.Longitude, df.Latitude), crs=4326)
gdf.to_file(global_spachar_dataset, driver="GPKG")

#remove unique idenifier & geodata
df = df.drop(['Longitude', 'Latitude', 'wkd_day_value', 'wnd_day_value','wkd_night_value', 'wnd_night_value', 'spachar'], axis=1)

df_urban = df[['FID', 'mean_value_NO2', 'nightlight_450', 'nightlight_3150', 'population_1000', 'population_3000', 'road_class_2_25', 'road_class_3_300', 'road_class_3_3000', 'trop_mean_filt_2019', 'BldDen100', 'NDVI', 'trafBuf25', 'trafBuf50', 'Urban']]
df_lowpop = df[['FID','mean_value_NO2','nightlight_450', 'nightlight_3150', 'population_1000', 'population_3000', 'road_class_2_25',  'road_class_3_300', 'road_class_3_3000', 'trop_mean_filt_2019', 'BldDen100', 'NDVI', 'trafBuf25', 'trafBuf50', 'LowPop']]
df_farfromroad = df[['FID','mean_value_NO2','nightlight_450', 'nightlight_3150', 'population_1000', 'population_3000', 'road_class_2_25', 'road_class_3_300', 'road_class_3_3000', 'trop_mean_filt_2019', 'BldDen100', 'NDVI', 'trafBuf25', 'trafBuf50', 'FarFromRoad']]

#only select rows that correspond to 1 for each characteristic column.
df_urban_TEST = df_urban.loc[df_urban['Urban'] == 1]
df_urban_TRAIN = df_urban.loc[df_urban['Urban'] == 0]
df_lowpop_TEST = df_lowpop.loc[df_lowpop['LowPop'] == 1]
df_lowpop_TRAIN = df_lowpop.loc[df_lowpop['LowPop'] == 0]
df_farfromroad_TEST = df_farfromroad.loc[df_farfromroad['FarFromRoad'] == 1]
df_farfromroad_TRAIN = df_farfromroad.loc[df_farfromroad['FarFromRoad'] == 0]

#get index of column with spatial characteristics. This column will be dropped for the
#modelling which is done via the index
df_urban_TRAIN.columns.get_loc("Urban")
df_lowpop_TRAIN.columns.get_loc("LowPop")
df_farfromroad_TRAIN.columns.get_loc("FarFromRoad")

#models

#Non-linear

#Random Forest
rf = RandomForestRegressor(n_estimators = 1000, random_state = 42, min_samples_split=10,
                          min_samples_leaf=5,max_features=4,max_depth=10, bootstrap=True )

#LightGBM
lightgbm_model = lgb.LGBMRegressor(reg_alpha =2, reg_lambda = 0, max_depth = 5, learning_rate = 0.0005, n_estimators =50000, random_state=42)
#XGBoost
xgb= xgb.XGBRegressor(gamma = 5,  reg_alpha =2, reg_lambda=0, max_depth = 5, learning_rate = 0.0005, n_estimators=50000, random_state=42)


#linear models

#Lasso
model_lasso = Lasso(alpha=0.1)
#Ridge
ridge2 = Ridge(alpha = 0.3)

#random states
#CROSS VALIDATION = 20 FOLD
# random_states = [1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95]
random_states = [1, 5]
# #models
models = [rf, lightgbm_model, xgb, model_lasso, ridge2]
#model_names
model_names = ['rf', 'lgb',  'xgb', 'lasso', 'ridge']

#training datasets
characteristics = [df_urban_TEST, df_lowpop_TEST, df_farfromroad_TEST]
#other
others = [df_urban_TRAIN, df_lowpop_TRAIN, df_farfromroad_TRAIN]
#char names
char_names = ['Urban', 'LowPop', 'FFR']

#use function
rmse_results, r2_results, mae_results = cross_validate_models_chars(
    models=models,
    model_names=model_names,
    characteristics=[df_urban_TEST, df_lowpop_TEST, df_farfromroad_TEST],
    others=[df_urban_TRAIN, df_lowpop_TRAIN, df_farfromroad_TRAIN],
    char_names=['Urban', 'LowPop', 'FFR'],
    random_states=[1, 5]  
)

# Pivot tables for analysis - modified to create separate tables by model
def create_pivot_tables_by_model(results: pd.DataFrame, metric: str) -> dict:
    # Create a RUN column for grouping
    results['RUN'] = (
        results.groupby(results['Char'].ne(results['Char'].shift()).cumsum())
        .cumcount()
        .add(1)
    )
    
    # Create a dictionary to store pivot tables for each model
    pivot_tables = {}
    
    # Get unique models
    models = results['Model'].unique()
    
    for model in models:
        # Filter the results for the current model
        model_results = results[results['Model'] == model]
        
        # Create the pivot table for this model
        pivot_table = model_results.pivot_table('Model Perf', ['Model', 'RUN'], 'Char')
        
        # Store the pivot table in the dictionary using model name as key
        pivot_tables[model] = pivot_table
        print(f"{metric} Pivot Table for {model}:\n{pivot_table}\n")
    
    return pivot_tables

# Create pivot tables for RMSE, R2, and MAE for each model separately
rmse_pivot_tables = create_pivot_tables_by_model(rmse_results, 'RMSE')
r2_pivot_tables = create_pivot_tables_by_model(r2_results, 'R2')
mae_pivot_tables = create_pivot_tables_by_model(mae_results, 'MAE')

# store into separate variables for each model
RMSE_RF_PIV = rmse_pivot_tables.get('rf')
RMSE_RID_PIV = rmse_pivot_tables.get('ridge')

RMSE_LGB_PIV= rmse_pivot_tables.get('lgb')
RMSE_XGB_PIV= rmse_pivot_tables.get('xgb')
RMSE_LAS_PIV = rmse_pivot_tables.get('lasso')

R2_RF_PIV = r2_pivot_tables.get('rf')
R2_RID_PIV = r2_pivot_tables.get('ridge')
R2_XGB_PIV = r2_pivot_tables.get('xgb')
R2_LGB_PIV = r2_pivot_tables.get('lgb')
R2_LAS_PIV = r2_pivot_tables.get('lasso')

MAE_RF_PIV = mae_pivot_tables.get('rf')
MAE_RID_PIV = mae_pivot_tables.get('ridge')
MAE_XGB_PIV = mae_pivot_tables.get('xgb')
MAE_LGB_PIV = mae_pivot_tables.get('lgb')
MAE_LAS_PIV = mae_pivot_tables.get('lasso')

PIV_Models_RMSE = [RMSE_RF_PIV,  RMSE_LGB_PIV, RMSE_XGB_PIV, RMSE_LAS_PIV, RMSE_RID_PIV]
PIV_Models_RMSE_names = ['RMSE_RF',  'RMSE_LGB', 'RMSE_XGB', 'RMSE_LAS', 'RMSE_RID']
PIV_Models_R2 = [R2_RF_PIV,  R2_LGB_PIV , R2_XGB_PIV, R2_LAS_PIV, R2_RID_PIV]
PIV_Models_R2_names = ['R2_RF',  'R2_LGB' ,'R2_XGB', 'R2_LAS', 'R2_RID']
PIV_Models_MAE = [MAE_RF_PIV,  MAE_LGB_PIV , MAE_XGB_PIV, MAE_LAS_PIV, MAE_RID_PIV]
PIV_Models_MAE_names = ['MAE_RF', 'MAE_LGB' , 'MAE_XGB', 'MAE_LAS', 'MAE_RID']


import matplotlib.pyplot as plt
i=0
j=0
while i < len(PIV_Models_RMSE):
    
    #define font
    plt.rcParams["font.family"] = "serif"
    
    fig = plt.figure()
    fig.suptitle('Variance in Root Mean Square Error (CV=20)', fontsize=12, fontweight='bold')

    ax = fig.add_subplot(111)
    ax.boxplot(PIV_Models_RMSE[i])

    ax.set_xticks([1, 2, 3], ['FFR', 'LowPop', 'Urban'])

    ax.set_xlabel('Spatial Group')
    ax.set_ylabel(PIV_Models_RMSE_names[j])
    plot_filename = os.path.join(output_map, PIV_Models_RMSE_names[j] + '.jpg')
    fig.savefig(plot_filename,  bbox_inches='tight',
           facecolor=(1, 1, 1))
    # plt.show()
    i+=1
    j+=1

## == R2 == ##

i=0
j=0
while i < len(PIV_Models_R2):
    
    #define font
    plt.rcParams["font.family"] = "serif"
    
    fig = plt.figure()
    fig.suptitle('Variance in R2 (CV=20)', fontsize=12, fontweight='bold')

    ax = fig.add_subplot(111)
    ax.boxplot(PIV_Models_R2[i])

    ax.set_xticks([1, 2, 3], ['FFR', 'LowPop', 'Urban'])

    ax.set_xlabel('Spatial Group')
    ax.set_ylabel(PIV_Models_R2_names[j])
    plot_filename = os.path.join(output_map, PIV_Models_R2_names[j] + '.jpg')
    fig.savefig(plot_filename,  bbox_inches='tight',
           facecolor=(1, 1, 1))
    # plt.show()
    i+=1
    j+=1


## == MAE == ##

import matplotlib.pyplot as plt
i=0
j=0
while i < len(PIV_Models_MAE):
    #define font
    plt.rcParams["font.family"] = "serif"
    
    fig = plt.figure()
    fig.suptitle('Variance in Mean Absolute Error (CV=20)', fontsize=12, fontweight='bold')

    ax = fig.add_subplot(111)
    ax.boxplot(PIV_Models_MAE[i])

    ax.set_xticks([1, 2, 3], ['FFR', 'LowPop', 'Urban'])

    ax.set_xlabel('Spatial Group')
    ax.set_ylabel(PIV_Models_MAE_names[j])
    plot_filename = os.path.join(output_map, PIV_Models_MAE_names[j] + '.jpg')
    fig.savefig(plot_filename,  bbox_inches='tight',
           facecolor=(1, 1, 1))

    # plt.show()
    i+=1
    j+=1

## == export option == ##
RMSE_RF_PIV.to_excel(os.path.join(output_map_excel, 'RMSE_RF_spachar.xlsx'))
RMSE_LGB_PIV.to_excel(os.path.join(output_map_excel, 'RMSE_LGB_spachar.xlsx'))
RMSE_XGB_PIV.to_excel(os.path.join(output_map_excel, 'RMSE_XGB_spachar.xlsx'))
RMSE_LAS_PIV.to_excel(os.path.join(output_map_excel, 'RMSE_LAS_spachar.xlsx'))
RMSE_RID_PIV.to_excel(os.path.join(output_map_excel, 'RMSE_RID_spachar.xlsx'))

R2_RF_PIV.to_excel(os.path.join(output_map_excel, 'R2_RF_spachar.xlsx'))
R2_LGB_PIV.to_excel(os.path.join(output_map_excel, 'R2_LGB_spachar.xlsx'))
R2_XGB_PIV.to_excel(os.path.join(output_map_excel, 'R2_XGB_spachar.xlsx'))
R2_LAS_PIV.to_excel(os.path.join(output_map_excel, 'R2_LAS_spachar.xlsx'))
R2_RID_PIV.to_excel(os.path.join(output_map_excel, 'R2_RID_spachar.xlsx'))

MAE_RF_PIV.to_excel(os.path.join(output_map_excel, 'MAE_RF_spachar.xlsx'))
MAE_LGB_PIV.to_excel(os.path.join(output_map_excel, 'MAE_LGB_spachar.xlsx'))
MAE_XGB_PIV.to_excel(os.path.join(output_map_excel, 'MAE_XGB_spachar.xlsx'))
MAE_LAS_PIV.to_excel(os.path.join(output_map_excel, 'MAE_LAS_spachar.xlsx'))
MAE_RID_PIV.to_excel(os.path.join(output_map_excel,'MAE_RID_spachar.xlsx'))











































