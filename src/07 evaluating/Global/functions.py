from sklearn.metrics import r2_score, mean_absolute_error
from sklearn.model_selection import train_test_split
import numpy as np

# Define the function
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