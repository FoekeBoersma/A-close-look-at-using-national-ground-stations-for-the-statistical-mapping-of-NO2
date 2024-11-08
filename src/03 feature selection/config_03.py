from sklearn.ensemble import RandomForestRegressor
import os
import sys


input_dataset = {
    "globaldataset": "/data/input/GlobalModelData/ModellingDataset-Global.csv",
    "medianshapcv10": "/data/input/GlobalModelData/df median shap cv10.csv",
    "localdataset": "/data/input/LocalModelData/ModellingDataset-Local.csv",
    "medianshaplocal": "/data/input/LocalModelData/df_cv median-Local.csv"
}

output = {
    "output_map": "/data/output/03 feature selection/",
    "output_map_local": "/data/output/03 feature selection/local/"
}


parameters ={
    'n_estimators_random_forest': 100,
    'random_state': 42,
    'test_size': 0.25,
    'nfolds': 10,
    'no_best_predictors': 30,
    'no_predictors_model': 12,
    'no_predictors_model_local': 30
}