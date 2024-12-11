

input_data = {
'amsterdam_predicting_no2_allmodels' : 'data/input/TooBigData/Amsterdam/Predicting NO2-AllModelsAmsterdam100_xy.csv',

'bayreuth_predicting_no2_allmodels': 'data/input/TooBigData/Bayreuth/Predicting NO2-AllModelsBayreuth100_xy.csv',

'hamburg_predicting_no2_allmodels': 'data/input/TooBigData/Hamburg/Predicting NO2-AllModelsHamburg100_xy.csv',

'utrecht_predicting_no2_allmodels' : 'data/input/TooBigData/Utrecht/Predicting NO2-AllModelsUtrecht100_xy.csv',

'predicting_dataset': 'data/input/GlobalModelData/PredictingDataset.csv',

'predicting_dataset_local': 'data/input/LocalModelData/PredictingDataset.csv',

'modeling_dataset': 'data/input/GlobalModelData/ModellingDataset-Global.csv',

'global_spachar': 'data/input/TooBigData/Global_spachar.gpkg',
} 

parameters_random_forest = {
    'n_estimators_random_forest': 1000,
    'random_state_random_forest': 42,
     'min_samples_split_random_forest':10,
     'min_samples_leaf_random_forest':5,
     'max_features_random_forest':4,
     'max_depth_random_forest':10,
     'bootstrap_random_forest':True,

     'test_size': 0.25
}

# LightGBM Parameters
parameters_lightgbm = {
    'reg_alpha_lightgbm': 2,
    'reg_lambda_lightgbm': 0,
    'max_depth_lightgbm': 5,
    'learning_rate_lightgbm': 0.0005,
    'n_estimators_lightgbm': 50000,
    'random_state_lightgbm': 42
}

# XGBoost Parameters
parameters_xgboost = {
    'gamma_xgboost': 5,
    'reg_alpha_xgboost': 2,
    'reg_lambda_xgboost': 0,
    'max_depth_xgboost': 5,
    'learning_rate_xgboost': 0.0005,
    'n_estimators_xgboost': 50000,
    'random_state_xgboost': 42
}


output = {
    "output_map": "data/output/07 evaluating",
    "output_map_excel": "data/output/07 evaluating/excel",
    "output_map_global_model_performance": "global_model_performances_cv"
}


