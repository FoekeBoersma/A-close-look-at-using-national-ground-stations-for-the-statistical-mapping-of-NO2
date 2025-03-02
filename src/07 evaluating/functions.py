from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
from typing import List, Tuple, Dict, Any
import os

# Define the function
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

def cross_validate_models(models, model_names, X, y, random_states, test_size=0.25):
    """
    Perform cross-validation for multiple models and store performance metrics (RMSE, R2, MAE) for each model.

    Parameters:
    models: list of model objects
    model_names: list of model names corresponding to the models
    X: feature matrix (numpy array or pandas DataFrame)
    y: target vector (numpy array or pandas Series)
    random_states: list of random states for reproducibility of train/test splits
    test_size: proportion of the dataset to include in the test split (default: 0.25)

    Returns:
    total_rmse: dict containing RMSE scores for each model
    total_r2: dict containing R2 scores for each model
    total_mae: dict containing MAE scores for each model
    """
    
    # Create dictionaries that eventually store performance metrics per model
    total_rmse = {}
    total_r2 = {}
    total_mae = {}

    # Define RMSE calculation function
    def rmse(predictions, targets):
        return np.sqrt(((predictions - targets) ** 2).mean())

    # Start cross-validation loop for each model
    for model, model_name in zip(models, model_names):
        # Initialize lists where performances will be stored
        rmse_scores = []
        r2_scores = []
        mae_scores = []
        
        print(f"\nNext model: {model_name}\n")
        
        # Loop through different random states for train/test splits
        for random_state in random_states:
            # Split dataset into training and testing sets
            X_train, X_test, Y_train, Y_test = train_test_split(X, y, test_size=test_size, random_state=random_state)
            print(f"Random state: {random_state}")
            print(f"training samples: {len(X_train)}")
            print(f"testing samples: {len(X_test)}")

            # Train the model
            model.fit(X_train, Y_train)

            # Predict based on the test data
            preds_test = model.predict(X_test)

            # Calculate RMSE
            rmse_val = rmse(preds_test, Y_test)
            print(f"RMSE (test): {rmse_val}")
            rmse_scores.append(rmse_val)

            # Calculate R2 score
            r2_val = r2_score(Y_test, preds_test)
            print(f"R2 score (test): {r2_val}")
            r2_scores.append(r2_val)

            # Calculate Mean Absolute Error (MAE)
            mae_val = mean_absolute_error(Y_test, preds_test)
            print(f"MAE (test): {mae_val}")
            mae_scores.append(mae_val)

        # Store scores in the total dictionaries
        total_rmse[model_name] = rmse_scores
        total_r2[model_name] = r2_scores
        total_mae[model_name] = mae_scores

    return total_rmse, total_r2, total_mae



def cross_validate_models_chars(
    models: List[Any], model_names: List[str], characteristics: List[pd.DataFrame],
    others: List[pd.DataFrame], char_names: List[str], random_states: List[int]
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    this function validates the performance of the model on each spatial group (urban, suburban rural) via n times bootstrapping (random states)
    in this function, 30 samples are randomly chosen (per random state) in spatial group x
    that will be used for testing while the remainder is used for training.
    the not chosen samples are merged with the the other spatial groups that together is the training data.

    """

    # Storage lists for model performance metrics
    Model_RMSE_scores = []
    Model_R2_scores = []
    Model_MAE_scores = []
    model_list = []
    characteristic_list = []

    # Loop through each model
    for model, model_name in zip(models, model_names):
        print(f"\n\nNext model: {model_name}\n\n")
        
        # Loop through each characteristic data set
        for char, other, char_name in zip(characteristics, others, char_names):
            print(f"\nCHARACTERISTIC: {char_name}\n")
            
            for random_state in random_states:
                print(f"Random State: {random_state}")
                
                # Split data into training and testing sets
                chosen = char.sample(n=30, random_state=random_state)
                not_used = char[~char.isin(chosen)].dropna()
                
                print("Chosen:", len(chosen))
                print("Not used: ", len(not_used))
                print("other: ", len(other))
                print("Not Used, will be assigned to training data:", len(not_used))
                
                # Define test data
                Y_test = chosen['mean_value_NO2']
                X_test = chosen.drop(columns=['mean_value_NO2', 'FID'], errors='ignore')
                
                # Define train data
                train = pd.concat([other, not_used])
                Y_train = train['mean_value_NO2']
                X_train = train.drop(columns=['mean_value_NO2', 'FID'], errors='ignore')
                
                # Fit the model
                model.fit(X_train, Y_train)
                
                # Predict on test data
                preds_test = model.predict(X_test)
                
                # Calculate performance metrics
                rmse_val = np.sqrt(((preds_test - Y_test) ** 2).mean())
                r2_val = r2_score(Y_test, preds_test)
                mae_val = mean_absolute_error(Y_test, preds_test)
                
                # Store results
                Model_RMSE_scores.append(rmse_val)
                Model_R2_scores.append(r2_val)
                Model_MAE_scores.append(mae_val)
                model_list.append(model_name)
                characteristic_list.append(char_name)
                
                # Print metrics
                print(f"RMSE testing: {rmse_val}")
                print(f"R2 score testing: {r2_val}")
                print(f"MAE testing: {mae_val}")

    # Convert results to DataFrames
    rmse_df = pd.DataFrame({
        'Model Perf': Model_RMSE_scores,
        'Char': characteristic_list,
        'Model': model_list
    })
    r2_df = pd.DataFrame({
        'Model Perf': Model_R2_scores,
        'Char': characteristic_list,
        'Model': model_list
    })
    mae_df = pd.DataFrame({
        'Model Perf': Model_MAE_scores,
        'Char': characteristic_list,
        'Model': model_list
    })

    return rmse_df, r2_df, mae_df