input_dataset = {
    "globaldataset": "/data/input/GlobalModelData/PredictingDataset.csv",
}

output = {
    "model_metrics_lasso": "model_metrics_lasso.txt",
    "model_metrics_ridge": "model_metrics_ridge.txt",
    "output_map": "/data/output/05 modeling//"
}


parameters ={
    'random_state': 20,
    'test_size': 0.25,
}