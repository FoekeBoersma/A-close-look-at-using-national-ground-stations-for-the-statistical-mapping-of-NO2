#import necessary modules
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.model_selection import train_test_split
from sklearn.linear_model import Lasso
from sklearn.metrics import r2_score
from sklearn.metrics import mean_absolute_error
import os
import sys
import os.path as path
from functions import (alp, rmse)

##SOURCE: https://dataaspirant.com/lasso-regression/#t-1606404715785

# Get the directory of the current script
current_directory = os.path.dirname(os.path.abspath(__file__))
# Define the path to the directory containing config_03.py (one level up)
config_directory = os.path.abspath(os.path.join(current_directory, '..'))
# Append the directory to sys.path
sys.path.append(config_directory)

# Try importing the config_03 module
try:
    import config_05
except ModuleNotFoundError as e:
    print(f"Error importing module: {e}")

dataset = config_05.input_dataset['globaldataset']
output_map = config_05.output['output_map']
model_metrics_lasso = config_05.output['model_metrics_lasso']
random_state = config_05.parameters['random_state']
test_size = config_05.parameters['test_size']

data_path = path.abspath(path.join(__file__ ,"../../.."))

output_location = data_path  + output_map
#import dataset for modeling
df = pd.read_csv(data_path + dataset, sep=',')


#remove unique idenifier & geodata
df = df.drop(['Unnamed: 0', 'Longitude', 'Latitude'], axis=1)
#replace NaN's with 0's
df = df.fillna(0)

y = df['mean_value_NO2'] #specify target
x = df.drop(['mean_value_NO2'], axis=1) #predictors
feature_list = list(x.columns)

X_train, X_test, Y_train, Y_test = train_test_split(x, y, test_size=test_size, random_state=random_state)
#random state is needed to ensure that same results are generated each time.

## ENSURING THAT X- AND Y-TRAINING SET CONTAIN SAME AMOUNT OF ROWS. SAME FOR TESTING.
print('Training Features Shape:',X_train.shape)
print('Training Labels Shape:', Y_train.shape)
print('Testing Features Shape:', X_test.shape)
print('Testing Labels Shape:', Y_test.shape)

#examine model performance for each alpha parameter
o = np.arange(0.1, 1.1, 0.1)
store = []
for i in o:
    alp(i, X_train, Y_train, X_test, Y_test)

## Build the lasso model with best alpha
model_lasso = Lasso(alpha=0.1)
model_lasso.fit(X_train, Y_train)
pred_train_lasso= model_lasso.predict(X_train)
pred_test_lasso= model_lasso.predict(X_test)

# Open a file in write mode
with open(output_location  + model_metrics_lasso , "w") as file:
    # Writing the results to the file
    file.write("training rmse: " + str(rmse(pred_train_lasso, Y_train)) + "\n")
    file.write("testing rmse: " + str(rmse(pred_test_lasso, Y_test)) + "\n\n")

    file.write("R2 score training data: " + str(r2_score(Y_train, pred_train_lasso)) + "\n")
    file.write("R2 score testing data: " + str(r2_score(Y_test, pred_test_lasso)) + "\n\n")

    file.write("MAE training data: " + str(mean_absolute_error(Y_train, pred_train_lasso)) + "\n")
    file.write("MAE testing data: " + str(mean_absolute_error(Y_test, pred_test_lasso)) + "\n")