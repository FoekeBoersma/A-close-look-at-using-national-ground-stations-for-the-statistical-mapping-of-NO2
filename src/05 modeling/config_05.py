input_dataset = {
    "globaldataset": "/data/input/GlobalModelData/PredictingDataset.csv",
}

output = {
    "model_metrics_lasso": "model_metrics_lasso.txt",
    "model_metrics_ridge": "model_metrics_ridge.txt",
    "model_metrics_random_forest": "model_metrics_random_forest.txt",
    "output_map": "/data/output/05 modeling//"
}


parameters ={
    'random_state': 20,
    'test_size': 0.25,
    'n_estimators_random_forest': 1000,
    'random_state_random_forest': 42,
}