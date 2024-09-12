import numpy as np
import shap
from sklearn.model_selection import ShuffleSplit
import itertools 
import matplotlib.pyplot as plt
import os

def cvfi(data_x, data_y, model, nofolds, test_size, names, data_path, output_map):
    ss = ShuffleSplit(n_splits=nofolds, test_size=test_size, random_state=0)
    
    total = list(itertools.repeat(0, 88)) #85 copies of 0 - needed for generating mean of CV
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
