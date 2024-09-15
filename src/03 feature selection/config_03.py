from sklearn.ensemble import RandomForestRegressor
import os
import sys


input_dataset = {
    "globaldataset": "/data/input/GlobalModelData/ModellingDataset-Global.csv",
    "medianshapcv10": "/data/input/GlobalModelData/df median shap cv10.csv"
}

output = {
    "output_map": "/data/output/03 feature selection/"
}


parameters ={
    'n_estimators_random_forest': 100,
    'random_state': 42,
    'test_size': 0.25,
    'nfolds': 10
}