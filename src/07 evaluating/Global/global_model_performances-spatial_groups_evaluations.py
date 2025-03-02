#import necessary modules

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.ensemble import RandomForestRegressor
import xgboost as xgb
from sklearn.linear_model import Ridge, Lasso
import lightgbm as lgb
import geopandas as gdp
import os 
import sys
import matplotlib.pyplot as plt

## == connecting to relevant import files == ##
# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
sys.path.append(config_directory)

from functions import cross_validate_models_chars

# Try importing the config_07 module
try:
    import config_07
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

modeling_dataset = config_07.input_data['modeling_dataset']
global_spachar_dataset = config_07.input_data['global_spachar']

#import dataset 
df = pd.read_csv(modeling_dataset, sep=';')
#define output locations
output_map = config_07.output['output_map']
output_map_excel = config_07.output['output_map_excel']

#replace NA with 0
df=df.fillna(0)
#create unique identifier
df['FID'] = range(1,len(df)+1, 1)

## == create spatia groups == ##
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

print("urban: ", len(Urb))
print("suburban: ", len(Lowpop))
print("rural: ", len(FFR))

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
# lightgbm_model = lgb.LGBMRegressor(reg_alpha =2, reg_lambda = 0, max_depth = 5, learning_rate = 0.0005, n_estimators =50000, random_state=42)
#XGBoost
xgb= xgb.XGBRegressor(gamma = 5,  reg_alpha =2, reg_lambda=0, max_depth = 5, learning_rate = 0.0005, n_estimators=10000, random_state=42)

#linear models

#Lasso
model_lasso = Lasso(alpha=0.1)
#Ridge
ridge = Ridge(alpha = 0.1)

#random states
#CROSS VALIDATION = 20 FOLD
random_states = [1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95]

# #models
# models = [rf, lightgbm_model, xgb, model_lasso, ridge]
models = [rf,  xgb, model_lasso, ridge]
# model names
# model_names = ['rf', 'lgb',  'xgb', 'lasso', 'ridge']
model_names = ['rf',   'xgb', 'lasso', 'ridge']

#testing datasets
testing_datasets = [df_urban_TEST, df_lowpop_TEST, df_farfromroad_TEST]
#other
training_datasets = [df_urban_TRAIN, df_lowpop_TRAIN, df_farfromroad_TRAIN]
#char names
char_names = ['Urban', 'LowPop', 'FFR']

#use function
rmse_results, r2_results, mae_results = cross_validate_models_chars(
    models=models,
    model_names=model_names,
    characteristics=testing_datasets,
    others=training_datasets,
    char_names=char_names,
    random_states=random_states  
)

# Pivot tables function for creating result summaries
def create_pivot_tables_by_model(results: pd.DataFrame, metric: str) -> dict:
    results['RUN'] = results.groupby(results['Char'].ne(results['Char'].shift()).cumsum()).cumcount() + 1
    pivot_tables = {}
    
    for model in results['Model'].unique():
        model_results = results[results['Model'] == model]
        pivot_tables[model] = model_results.pivot_table('Model Perf', ['Model', 'RUN'], 'Char')
        print(f"{metric} Pivot Table for {model}:\n{pivot_tables[model]}\n")
    
    return pivot_tables

# Generate pivot tables for each metric
rmse_pivot_tables = create_pivot_tables_by_model(rmse_results, 'RMSE')
r2_pivot_tables = create_pivot_tables_by_model(r2_results, 'R2')
mae_pivot_tables = create_pivot_tables_by_model(mae_results, 'MAE')

# Store pivot results into separate variables for each model
def store_pivot_to_excel(pivot_tables, prefix, output_dir):
    for model, pivot_table in pivot_tables.items():
        pivot_table.to_excel(os.path.join(output_dir, f"{prefix}_{model}_spachar.xlsx"))

store_pivot_to_excel(rmse_pivot_tables, 'RMSE', output_map_excel)
store_pivot_to_excel(r2_pivot_tables, 'R2', output_map_excel)
store_pivot_to_excel(mae_pivot_tables, 'MAE', output_map_excel)

# Plotting function to avoid redundancy
def plot_pivot_tables(pivot_tables, metric_name, output_dir):
    for model, pivot_table in pivot_tables.items():
        fig, ax = plt.subplots()
        ax.boxplot(pivot_table.values)
        ax.set_xticklabels(['FFR', 'LowPop', 'Urban'])
        ax.set_xlabel('Spatial Group')
        ax.set_ylabel(f'{metric_name} for {model}')
        ax.set_title(f'Variance in {metric_name} (CV={len(random_states)})')
        plot_filename = os.path.join(output_dir, f'{metric_name}_{model}.jpg')
        fig.savefig(plot_filename, bbox_inches='tight', facecolor='w')


# Plot RMSE, R2, and MAE
plot_pivot_tables(rmse_pivot_tables, 'RMSE', output_map)
plot_pivot_tables(r2_pivot_tables, 'R2', output_map)
plot_pivot_tables(mae_pivot_tables, 'MAE', output_map)



import os
import numpy as np

def save_results_to_txt(rmse_results, r2_results, mae_results, output_dir):
    output_file = os.path.join(output_dir, "model_performance_results.txt")
    
    with open(output_file, "w", encoding="utf-8") as f:
        f.write("\\newpage\n\\onecolumn\n")
        f.write("\\begin{table}[H]\n\\centering\n")
        f.write("\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|}\n")
        f.write("\\hline\n")
        f.write("\\multicolumn{3}{|c|}{} &\\multicolumn{3}{c|}{Urban} & ")
        f.write("\\multicolumn{3}{c|}{Suburban} & ")
        f.write("\\multicolumn{3}{c|}{Rural}\\\\\n")
        f.write("\\cline{3-12}\n")
        f.write("\\hline\n")
        f.write("\\multicolumn{3}{|c|}{Models} & R$^{2}$ & RMSE & MAE & R$^{2}$ & RMSE & MAE & R$^{2}$ & RMSE & MAE\\\\\n")
        f.write("\\hline\\hline\n")
        
        categories = {"Non-linear": ['rf', 'xgb'], "Linear": ['ridge', 'lasso']}
        
        for category, model_list in categories.items():
            f.write(f"\\multirow{{{len(model_list)*2}}}{{*}}{{{category}}}")
            
            for model in model_list:
                rmse_vals = rmse_results[rmse_results['Model'] == model].groupby('Char')['Model Perf'].agg([np.mean, np.std])
                r2_vals = r2_results[r2_results['Model'] == model].groupby('Char')['Model Perf'].agg([np.mean, np.std])
                mae_vals = mae_results[mae_results['Model'] == model].groupby('Char')['Model Perf'].agg([np.mean, np.std])
                
                f.write(f"&\\multirow{{2}}{{*}}{{{model.upper()}}} & Mean ")
                for char in ['Urban', 'LowPop', 'FFR']:
                    f.write(f"& {r2_vals.loc[char, 'mean']:.3f} & {rmse_vals.loc[char, 'mean']:.3f} & {mae_vals.loc[char, 'mean']:.3f} ")
                f.write("\\\\\n")
                
                f.write("& & SD ")
                for char in ['Urban', 'LowPop', 'FFR']:
                    f.write(f"& {r2_vals.loc[char, 'std']:.3f} & {rmse_vals.loc[char, 'std']:.3f} & {mae_vals.loc[char, 'std']:.3f} ")
                f.write("\\\\\n\\cline{2-12}\n")
        
        f.write("\\hline\n")
        f.write("\\end{tabular}\n")
        f.write("\\caption{Model performance per spatial group (CV = 20). RMSE and MAE are represented in NO$_{2}$ ($\\mu$g/m\\textsuperscript{3}).}\n")
        f.write("\\label{table:model performance per spatial group - global}\n")
        f.write("\\end{table}\n")
        f.write("\\normalsize\n")

save_results_to_txt(rmse_results, r2_results, mae_results, output_map_excel)