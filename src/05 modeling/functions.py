import numpy as np # linear algebra
from sklearn.linear_model import Lasso, Ridge
from sklearn.metrics import r2_score
from sklearn.metrics import mean_absolute_error
from sklearn.preprocessing import StandardScaler

#determining alpha parameter
def alp(g, X_train, Y_train, X_test, Y_test):
    model_lasso = Lasso(alpha=g)
    model_lasso.fit(X_train, Y_train)
    pred_train_lasso= model_lasso.predict(X_train)
    pred_test_lasso= model_lasso.predict(X_test)
    
    ##RMSE
    print(g)
    def rmse(predictions, targets):
        return np.sqrt(((predictions - targets) ** 2).mean())
    
    print("testing rmse: ", rmse(pred_test_lasso, Y_test))

    ##R2
    print("R2 score testing data: ", r2_score(Y_test, pred_test_lasso))

    ##MEAN ABSOLUTE ERROR 
    print("MAE testing data: ", mean_absolute_error(Y_test, pred_test_lasso))

##RMSE 
def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())

def alp_ridge(g, X_train, Y_train, X_test, Y_test):
    # Initialize the scaler
    scaler = StandardScaler()

    # Normalize the training data
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    # Create Ridge model with the given alpha
    model_ridge = Ridge(alpha=g)
    model_ridge.fit(X_train_scaled, Y_train)

    # Make predictions
    predtrain = model_ridge.predict(X_train_scaled)
    prediction_adjusted_alpha = model_ridge.predict(X_test_scaled)

    ##RMSE
    print(g)

    def rmse(predictions, targets):
        return np.sqrt(((predictions - targets) ** 2).mean())

    print("testing rmse: ", rmse(prediction_adjusted_alpha, Y_test))

    ##R2
    print("R2 score testing data: ", r2_score(Y_test, prediction_adjusted_alpha))

    ##MEAN ABSOLUTE ERROR 
    print("MAE testing data: ", mean_absolute_error(Y_test, prediction_adjusted_alpha))