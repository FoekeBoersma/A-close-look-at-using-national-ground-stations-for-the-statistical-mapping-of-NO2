import numpy as np
import shap
from sklearn.model_selection import ShuffleSplit
import itertools 
import matplotlib.pyplot as plt
import os
import pandas as pd
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score

def create_new_map(map_name, location):
    """
    Creates a new directory (map) with the given name at the specified location.
    
    Args:
        map_name (str): Name of the new directory.
        location (str): Path where the new directory should be created.
    """
    try:
        # Combine the location and map name to form the full path
        full_path = os.path.join(location, map_name)
        
        # Create the directory
        os.makedirs(full_path, exist_ok=True)
        
        print(f"Directory '{map_name}' created at location: {location}")
    except Exception as e:
        print(f"Error creating directory: {e}")

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
    
    total = list(itertools.repeat(0, len(names)))  # Initialize with zeros for mean calculation
    output_location = os.path.join(data_path, output_map)
    ranking = []
    
    for fold, (train, test) in enumerate(ss.split(data_x, data_y)):
        print('fold: ', fold)
        data_x_train = np.array(data_x[train])
        data_y_train = np.array(data_y[train])

        # Train the model on training data
        model.fit(data_x_train, data_y_train)
        
        # SHAP summary plot (bar)
        shap_values = shap.TreeExplainer(model).shap_values(data_x_train)
        plt.figure(figsize=(15, 10))
        shap.summary_plot(shap_values, data_x_train, feature_names=names, plot_type='bar', show=False)
        
        # Save SHAP bar plot
        plot_filename_bar = os.path.join(output_location, f'shap_summary_plot_fold_{fold}.png')
        plt.savefig(plot_filename_bar, dpi=100)
        plt.close()
        
        # SHAP summary plot (RdBu_r colormap)
        shap.summary_plot(shap_values, data_x_train, feature_names=names, show=False)
        fig, ax = plt.gcf(), plt.gca()
        plt.rcParams["font.family"] = "serif"  # Set font style
        
        # Update colormap for artists
        for fc in fig.get_children():
            for fcc in fc.get_children():
                if hasattr(fcc, "set_cmap"):
                    fcc.set_cmap(plt.get_cmap("RdBu_r"))
        
        # Save SHAP colormap plot
        plot_filename_colormap = os.path.join(output_location, f'Shapley_RdBu_fold_{fold}.png')
        fig.savefig(plot_filename_colormap, bbox_inches='tight', facecolor=(1, 1, 1))
        plt.close(fig)
        
        # DataFrame for SHAP values
        df = pd.DataFrame({'name': names, 'shap': np.abs(shap_values).mean(0)})
        df_sort = df.sort_values(by='shap', ascending=False).reset_index(drop=True)
        df_sort['rank'] = np.arange(1, len(df) + 1)
        df_new = df_sort[['name', 'rank']]
        
        rank_array = df_new.values
        ranking.append(rank_array)
        
        # Calculate total importance
        vals = np.abs(shap_values).mean(0)
        total += vals

        print('next fold')
        print('\n')
    
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

