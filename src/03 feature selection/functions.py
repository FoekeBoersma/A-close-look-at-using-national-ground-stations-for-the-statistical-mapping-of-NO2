import numpy as np
import shap
from sklearn.model_selection import ShuffleSplit
import itertools 
import matplotlib.pyplot as plt
import os
import pandas as pd
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score



def cvfi(data_x, data_y, model, nofolds, test_size, names, data_path, output_map):
    ss = ShuffleSplit(n_splits=nofolds, test_size=test_size, random_state=0)
    
    total = list(itertools.repeat(0, len(names))) #88 copies of 0 - needed for generating mean of CV
    output_location = data_path  + output_map
    
    for fold, (train, test) in enumerate(ss.split(data_x, data_y)):
        data_x_train = np.array(data_x[train])
        data_y_train = np.array(data_y[train])

        # Train the model on training data
        model.fit(data_x_train, data_y_train)
        print(model)
        
        shap_values = shap.TreeExplainer(model).shap_values(data_x_train)
        # Create a new figure
        plt.figure(figsize=(15, 10))

        shap.summary_plot(shap_values, data_x_train, feature_names=names, plot_type='bar',show=False)
        
        # Save SHAP summary plot
        plot_filename = os.path.join(output_location, f'shap_summary_plot_fold_{fold}.png')
        plt.savefig(plot_filename, dpi=100)
        
        #convert importance values to numpy array
        vals= np.abs(shap_values).mean(0)

        print("")
        print(f"next iteration, fold: {fold}")
        print("")
        total += vals
    print(total)
    cvfi.meanfi = total/nofolds #allows use of local variable outside function
    return(cvfi.meanfi)



def cvfi_median(data_x, data_y, model, nofolds, test_size, names, data_path, output_map):
    ss = ShuffleSplit(n_splits=nofolds, test_size=test_size, random_state=0)
    
    total = list(itertools.repeat(0, len(names)))  #88 copies of 0 - needed for generating mean of CV
    output_location = data_path  + output_map
    ranking = []
    
    for fold, (train, test) in enumerate(ss.split(data_x, data_y)):
        data_x_train = np.array(data_x[train])
        data_y_train = np.array(data_y[train])

        # Train the model on training data
        model.fit(data_x_train, data_y_train)

        
        shap_values = shap.TreeExplainer(model).shap_values(data_x_train)
        # Create a new figure
        plt.figure(figsize=(15, 10))

        shap.summary_plot(shap_values, data_x_train, feature_names=names, plot_type='bar',show=False)
        
        # Save SHAP summary plot
        plot_filename = os.path.join(output_location, f'shap_summary_plot_fold_{fold}.png')
        plt.savefig(plot_filename, dpi=100)
        
        #https://towardsdatascience.com/explain-your-model-with-the-shap-values-bc36aac4de3d
        # according to this source, training dataset is used to calculate shap values
        
        df = pd.DataFrame(names, np.abs(shap_values).mean(0))
        df['index'] = df.index
        df.reset_index(drop=True, inplace=True)
        df.columns = [''] * len(df.columns)
        
        df.columns = ['name','shap']
        print(df)
        df_sort = df.sort_values(by = ['shap'], ascending=False)
        df_sort['rank'] = np.arange(1, len(df) * 1 + 1, 1)
        df_new = df_sort.drop(['shap'], axis=1)
        print(df_new)
#         rnk_pd.append(df_new, ignore_index = True)
        rank_array = df_new.values
        
        print(rank_array)
        ranking.append(rank_array)
  
        #convert importance values to numpy array
        vals= np.abs(shap_values).mean(0)
        print(vals)
   
        print(len(vals))
        total += vals
    
    print(total)
    print(ranking)
    cvfi_median.ranking = ranking
    
    # cvfi.meanfi = total/nofolds #allows use of local variable outside function



#generate function that creates K-fold CV, thereby creating shap summary plot for each loop/fold.

def cvaic(data_x, data_y, model, nofolds, test_size, names, novar, random_state):
    ss = ShuffleSplit(n_splits=nofolds, test_size=test_size, random_state=random_state)

    total_msetest = [0]
    total_r2 = [0]
    total_rmse = [0]
   
    
    for train, test in ss.split(data_x, data_y):
        data_x_train = np.array(data_x[train])
        data_y_train = np.array(data_y[train])
        data_x_test = np.array(data_x[test])
        data_y_test = np.array(data_y[test])
        
        num_params = novar + 1 #+1 for intercept

        model.fit(data_x_train, data_y_train)

        y_predicted = model.predict(data_x_test) #predict dependent var based on x number of most important ind. var.

        mse_test = mean_squared_error(data_y_test, y_predicted)
        
        r2 = r2_score(data_y_test, y_predicted)
        
        def rmse(predictions, targets):
                return np.sqrt(((predictions - targets) ** 2).mean())
        rmse_val = rmse(y_predicted, data_y_test)

        mse_vals= np.abs(mse_test)

        r2_vals = np.abs(r2)

        rmse_vals = np.abs(rmse_val)

        total_msetest += mse_vals
        total_r2 += r2_vals
        total_rmse += rmse_vals

    cvaic.meanmse = total_msetest/nofolds #allows use of local variable outside function
    cvaic.meanr2 = total_r2/nofolds #allows use of local variable outside function
    cvaic.meanrmse = total_rmse/nofolds #allows use of local variable outside function

