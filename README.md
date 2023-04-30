# Content

## 01 adding variables to initial datasets


Scripts that relate to this map process the additional predictors with the purpose of assigning predictor information to the measurement stations. 
Additional predictors consist of:
  * Building density
  * NDVI
  * Precipitation
  * Traffic volume
  
## 02 composing initial dataset

At this stage, the initial dataset is merged with the additional variables, both applicable to the local- and global dataset.
  
## 03 feature selection

Once the global- and local datasets are composed, feature selection is applied whereby only the most important features (i.e. having the highest influence on variation in the target variable) are considered for modeling. Important steps in this part of the process are:
 * determining the influence of every feature via Shapley values by means of:
   * a 10-fold cross validation where the average mean is taken
   * a 10-fold cross validation where the average median is taken
   * a comparison between the average mean and median
 * determining the number of most important features to be included for the global and local dataset by means of several metrics:
   * for the global dataset these metrics are the R2 and RMSE
   * for the local dataset these metrics are the (adjusted) R2, CP, and BIC
   
## 04 composing modeling datasets

In this phase, the most important features are identified and included in the global models which will be projected onto Amsterdam, Bayreuth, Hamburg, and Utrecht and the local model.

After the most important features are selected for the models, the next step is to compose the most important features into one new dataset that will be used for modeling. This part of the process applies to global- and local analyses hence two subfolders are present here: global and local. The global folder consists of several aspects that are applied to the cities of Bayreuth, Hamburg, and Utrecht:
 * defining the area of interest through a 100 meter grid for every city.
 * composing modeling dataset. Most of the predictors derive from TIFS that are projected onto the 100 meter grid for every city. Several other of the most important      features are deduced from other data sources than the TIFS and require slightly different data processing operations.

## 05 modeling

Scripts in this subfolder relate to the different global- and local-, as well as linear- and non-linear models. An examination to parameter settings for each model is done too.

## 06 predicting

Two subfolders again relate to local- and global predicting. The local folder concerns the following scripts for modeling:
* linear modeling
* linear modeling, while accounting for spatial groups (being "urban", "low population", and "far from road")
* mixed effects modeling
* ordinary kriging
* universal kriging
* universal kriging, while accounting for spatial groups (being "urban", "low population", and "far from road")

The other folder concerns the global predictions, which are derived from the following models:
* random forest
* lightgbm
* xgboost
* ridge
* lasso

Scripts in the global folder cover the following processes: based on the predictor information, predict NO2 per model; and assigning the model predictions to the grid to make spatial processing and predicting possible.

## 07 evaluating

A global/local division is apparent in the folder structure. Evaluation of global models is done via a 20-fold cross validation while a leave-one-out cross validation is applied n the local dataset which is characterized by fewer samples.
Also included at this stage is the mapping of model predictions whereby different cities are subject to spatial prediction patterns for global models while air pollution mapping is relevant for the Amsterdam area for local models. One script 
bundles the local and global predictions, as well as the Amsterdam no2 tif together. This script is outside the local and global folders.

## data

Multiple datasets are identified including the scripts/processes in which every dataset is of use:
* **5TIFS**: contains most of the predictors projected onto the Amsterdam area. Relevant in the following scripts:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Amsterdam.R 
	* 04 composing dataset/Local/ComposingGrid-AllPredictors.R
* **polygonbuilding_studyArea**: contains buildings in The Netherlands and Germany, represented by polygons, in shapefile-format:
	* 01 adding variables to initial datasets/Global/AssignBuildingDensity to NO2MSs.R
	* 01 adding variables to initial datasets/Local/AssignBuildingDensity to NO2MSs.R
	* 02 composing initial dataset/Global/ComposingModellingDataset-Global.R (processed building density data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Bayreuth.R (processed building density data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Hamburg.R (processed building density data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Utrecht.R (processed building density data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Amsterdam.R (processed building density data)
	* 02 composing dataset/Local/ComposingModellingDataset-Local.R (processed building density data)
	
* **InitialGlobalDataset**: measurement stations including predictor information
	* 01 adding variables to initial datasets/Global/AssignBuildingDensity to NO2MSs.R
	* 01 adding variables to initial datasets/Global/AssignNDVIToNO2MSs.R
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToNO2MSs.R
	* 02 composing initial dataset/ComposingModellingDataset-Global.R
	
* **mod13q1**: TIF file of NDVI:
	* 01 adding variables to initial datasets/Global/AssignNDVIToNO2MSs.R
	* 01 adding variables to initial datasets/Local/AssignNDVIToNO2MSs.R
	* 02 composing initial dataset/Global/ComposingModellingDataset-Global.R (processed NDVI data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Bayreuth.R (processed NDVI data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Hamburg.R (processed NDVI data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Utrecht.R (processed NDVI data)
	* 02 composing dataset/Local/ComposingModellingDataset-Local.R  (processed NDVI data)
* **MonthlyPrecipitation**: monthly precipitation represented by 11322 stations across The Netherlands and Germany:
	* 01 adding variables to initial datasets/Global/AssignPrecipitationToMSs.R
	* 02 composing dataset/Global/ComposingModellingDataset-Global.R (processed precipitation data)
	* 02 composing dataset/Local/ComposingModellingDataset-Local.R (processed precipitation data)
* **Jawe2017.csv**: traffic count in Germany via traffic counting stations (raw):
	* 01 adding variables to initial datasets/Global/Processing_JaweData.R
* **Motorway_Primary_Germany**: all motorways and primary roads in Germany, represented by lines in shapefile-format:
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoads.R
* **Motorway_Primary_NL**: all motorways and primary roads in The Netherlands, represented by lines in shapefile-format:
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoads.R
	
	
* **Jawe_processed**: traffic count in Germany via traffic counting stations in shapefile-format(processed):
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoads.R
* **intensiteit-snelheid-export- (RotterdamDenHaag/zuid/noord/West/Oost/Noord/overig)**:
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoads.R
* **Telpunten_WGS84**: traffic station locations in The Netherlands:
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoads.R

* **TrafficVolume_RoadsNetherlands**: roads in The Netherlands with traffic volume assigned, in shapefile-format
	* 01 adding variables to initial datasets/Local/AssignTrafficVolumeToRoads.R
	
* **TrafficVolume_StudyArea**: traffic volume represented by roads for The Netherlands and Germany:
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToNO2MSs.R
	* 01 adding variables to initial datasets/Local/AssignTrafficVolumeToNO2MSs.R
	* 02 composing initial dataset/Global/ComposingModellingDataset-Global.R (processed traffic data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Bayreuth.R (processed traffic data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Hamburg.R (processed traffic data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Utrecht.R (processed traffic data)
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Amsterdam.R (processed traffic data)
	* 04 composing dataset/Local/ComposingModellingDataset-Local.R (processed traffic data)
	* 04 composing dataset/Local/ComposingGrid-AllPredictors.R (processed traffic data)

* **ModellingDataset-Global**: all predictors and target (NO2) related to the (global) measurement stations, in csv-format: 
	* 03 feature selection/Global/cv-aic-mse-r2-median-global-random-forest.ipynb 
	* 03 feature selection/Global/mean-cv-shap.ipynb 
	* 03 feature selection/Global/median-cv-shap.ipynb 
	* 03 feature selection/Global/shapley-figure-global.ipynb 
	* 07 evaluating/Global/globalmodelperformances-spatialgroupsevaluations.ipynb
* **df_cv_mean**: the sequential order of importance for every predictor variable (global) measured via cross-validated mean, in csv-format:
	* 03 feature selection/Global/median-vs-mean-ranking-visualization.ipynb 
* **df_cv_median (global)**: the sequential order of importance for every predictor variable (global) measured via cross-validated median, in csv-format:
	* 03 feature selection/Global/cv-aic-mse-r2-median-global-random-forest.ipynb 
	* 03 feature selection/Global/median-vs-mean-ranking-visualization.ipynb 
	
* **df_cv_median (local)**: the sequential order of importance for every predictor variable (local) measured via cross-validated median, in csv-format:
	* 03 feature selection/Local/cv-aic-mse-r2-median-global-random-forest.ipynb 
	
* **grid100Amsterdam**
	* 04 composing dataset/Local/ComposingGrid-AllPredictors.R
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Amsterdam
* **grid100Bayreuth**: 100 meter by 100 meter grid for the Bayreuth area, in geopackage-format:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Bayreuth.R
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Bayreuth
	_NOTE: due to a large file size, this data is not included in the repository._
* **Bayreuth/TIFFS/8**: most predictors, in TIF-format:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Bayreuth.R
	_NOTE: due to a large file size, this data is not included in the repository._
* **grid100_GlobalPredictors-Bayreuth**: result of the most important features in the local dataset and the grid100Bayreuth projection
	* 06 predicting/Global/predictions-all-models-global.ipynb
* **Bayreuth_NO2PredictionPerModel** 100 meter grid containing all NO2 model predictions
	* 07 evaluating/Global/visualization_purposes-Bayreuth_Hamburg_Utrecht
* **Predicting NO2-AllModelsBayreuth100_xy** * csv of 100 meter grid containing all NO2 model predictions
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Bayreuth.R
	* 07 evaluating/Global/comparing-predicted-no2-values-model.ipynb
	
* **grid100Hamburg**: 100 meter by 100 meter grid for the Hamburg area, in geopackage-format:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Hamburg.R
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Hamburg
	_NOTE: due to a large file size, this data is not included in the repository._
* **Hamburg/TIFFS/1**: most predictors, in TIF-format:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Hamburg.R
	_NOTE: due to a large file size, this data is not included in the repository._
* **grid100_GlobalPredictors-Hamburg**: result of the most important features in the local dataset and the grid100Hamburg projection
	* 06 predicting/predictions-all-models-global.ipynb
* **Hamburg_NO2PredictionPerModel** 100 meter grid containing all NO2 model predictions
	* 07 evaluating/Global/visualization_purposes-Bayreuth_Hamburg_Utrecht
* **Predicting NO2-AllModelsHamburg100_xy** * csv of 100 meter grid containing all NO2 model predictions
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Hamburg.R
	* 07 evaluating/Global/comparing-predicted-no2-values-model.ipynb
	
* **grid100Utrecht**: 100 meter by 100 meter grid for the Utrecht area, in geopackage-format:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Utrecht.R
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Utrecht.R
	_NOTE: due to a large file size, this data is not included in the repository._
* **Utrecht/TIFFS/7**: most predictors, in TIF-format:
	* 04 composing dataset/Global/ComposingTestingGridDatasetGlobal-Utrecht.R
	_NOTE: due to a large file size, this data is not included in the repository._
* **grid100_GlobalPredictors-Utrecht**: result of the most important features in the local dataset and the grid100Utrecht projection
	* 06 predicting/Global/predictions-all-models-global.ipynb
	

	
* **Utrecht_NO2PredictionPerModel** 100 meter grid containing all NO2 model predictions
	* 07 evaluating/Global/visualization_purposes-Bayreuth_Hamburg_Utrecht
	
	
* **Predicting NO2-AllModelsUtrecht100_xy** csv of 100 meter grid containing all NO2 model predictions
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Utrecht
	* 07 evaluating/Global/comparing-predicted-no2-values-model.ipynb
	
* **Predicting NO2-AllModelsAmsterdam100_xy** csv of 100 meter grid containing all NO2 model predictions
	* 06 predicting/Global/AssigningModelPredictionsTo100mGrid-Amsterdam.R
	* 07 evaluating/Global/comparing-predicted-no2-values-model.ipynb
	
* **Amsterdam_NO2PredictionPerModel** 100 meter grid containing all NO2 model predictions
	* 07 evaluating/Global/visualization_purposes-Amsterdam	
	* 07 evaluating/MergingGlobalAndLocalModels.R
	
* **NO2_4WEKEN**: containing NO2 values from measurement stations in the Amsterdam area:
	* 01 adding variables/Local/ComposingLocalMeasurementStations_Amsterdam

* **NO2_LOCATIES**: containing the coordinates of the measurement stations in the Amsterdam area:
	* 01 adding variables/Local/ComposingLocalMeasurementStations_Amsterdam
	
* **LocalMeasurementStations**: result from merging NO2_4WEKEN & NO2_LOCATIES:
	* 01 adding variables to initial datasets/Local/AssignBuildingDensity to NO2MSs.R
	* 01 adding variables to initial datasets/Local/AssignNDVIToNO2MSs.R
	* 01 adding variables to initial datasets/Local/AssignTrafficVolumeToRoads.R
	* 01 adding variables to initial datasets/Local/Assigning TrafficVolume to NO2MSs.R
	* 02 composing dataset/Local/ComposingModellingDataset-Local.R 
	
* **ModellingDataset-Local**: all predictors and target (NO2) related to the (local) measurement stations, in csv-format:
	* 03 feature selection/Local/cv-aic-mse-r2-median-local.ipynb
	* 03 feature selection/Local/median-rank-cv-shap-local.ipynb
	* 06 predicting/Local/PredictingNO2_Linear-separating_spatial_groups.R
	* 06 predicting/Local/PredictingNO2_MEM.R
	* 06 predicting/Local/PredictingNO2_UniversalKriging.R
	* 06 predicting/Local/PredictingNO2_UniversalKriging-separating_spatial_groups.R
	* 07 evaluating/Local/PredictingNO2_Linear_SpatialGroupEvaluations-AddingKrigedResiduals.R
	* 07 evaluating/Local/PredictingNO2_Linear_SpatialGroupEvaluations-ownLOOCVfunction.R
	* 07 evaluating/Local/PredictingNO2_MEM_SpatialGroupEvaluations-ownLOOCVfunction.R
	* 07 evaluating/Local/PredictingNO2_OK_SpatialGroupEvaluations.R

* **PredictingDataset (global)**: the resulting dataset of the most important features combined with the number of most important features that yield the best metrics:
	* 05 modeling/modelling-lasso.ipynb
	* 05 modeling/modelling-lightgbm.ipynb
	* 05 modeling/modelling-random forest.ipynb
	* 05 modeling/modelling-ridge.ipynb
	* 05 modeling/modelling-xgboost.ipynb
	* 06 predicting/Global/predictions-all-models-global.ipynb
	* 07 evaluating/Global/cross-validation-model-performances-global.ipynb 
	
* **PredictingDataset (local)**: the resulting dataset of the most important features combined with the number of most important features that yield the best metrics:
	* 06 predicting/Local/PredictingNO2_Linear.R
	* 06 predicting/Local/PredictingNO2_OrdinaryKriging.R
	* 07 evaluating/Local/cross-validation-model-performances-local.ipynb

* **dataset_bestMetrics_longlat-Local-top30**: best ranked top 30 variables deriving from local dataset.
	* 03 feature selection/Local/local_feature_selection.R
		
* **predictedNO2_Linear**:
	* 07 evaluating/MergingGlobalAndLocalModels.R

* **predictedNO2_Linear_SeparatingSpatialGroups**:
	* 07 evaluating/MergingGlobalAndLocalModels.R

* **predictedNO2_MEM**:
	* 07 evaluating/MergingGlobalAndLocalModels.R

* **predictedNO2_OK**:
	* 07 evaluating/MergingGlobalAndLocalModels.R

* **predictedNO2_UK_formula**:
	* 07 evaluating/MergingGlobalAndLocalModels.R

* **predictedNO2_UK_SeparatingSpatialGroups**:
	* 07 evaluating/MergingGlobalAndLocalModels.R
	
* **all_models**: merge of local- and global model predictions, as well as no2 Amsterdam tif.
	* 07 evaluating/Local/visualization_purposes.R
	* 07 evaluating/Local/Spachar_Amsterdam_visualization.R

* **localmodel_predictions**: all local model predictions into one dataset

* **Germany**:shapefile of Germany
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoadsToNO2MSs.R
	
* **NL**: shapefile of The Netherlands
	* 01 adding variables to initial datasets/Global/AssignTrafficVolumeToRoadsToNO2MSs.Rs

* **Grid100_LocalPredictors_Amsterdam: Grid of Amsterdam with predictor information
	* 06 predicting/Local/PredictingNO2_Linear.R
	* 06 predicting/Local/PredictingNO2_Linear-separating_spatial_groups.R
	* 06 predicting/Local/PredictingNO2_MEM.R
	* 06 predicting/Local/PredictingNO2_OrdinaryKriging.R
	* 06 predicting/Local/PredictingNO2_UniversalKriging.R
	* 06 predicting/Local/PredictingNO2_UniversalKriging-separating_spatial_groups.R

	
## Python

To run the scripts on python, several packages are necessary. It is recommended to use a requirements.txt file. The following packages can be included in this file:
certifi==2022.12.7
charset-normalizer==3.1.0
docopt==0.6.2
idna==3.4
pipreqs==0.4.11
requests==2.28.2
tomli==2.0.1
urllib3==1.26.15
yarg==0.1.9
numpy
pandas
shap
scikit-learn
matplotlib
openpyxl
lightgbm
xgboost
seaborn
geopandas

A best practice is to use a virtual environment to run the scripts related to this repository. 

## Disclaimer

A slight difference in data processing can have effects on further output and final results. Therefore, results should be interpreted with caution. 
Still, while a slight variation in data processing may result in different results, global results are expected to be relatively persistant. Moreover, the 
availabilty of scripts encourages not only transparency, but it can also be used as inspiration for future research. 
