import numpy as np # linear algebra
from sklearn.linear_model import Lasso
from sklearn.metrics import r2_score
from sklearn.metrics import mean_absolute_error

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