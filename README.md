# Content

## 01 adding variables to intitial datasets


Scripts that relate to this map process the additional predictors with the purpose of assigning predictor information to the measurement stations. 
Additional predictors consist of:
  * Building density
  * NDVI
  * Precipitation
  * Traffic volume
  
## 02 feature selection


   
## 03 composing modeling datasets

After the most important features are selected for the models, the next step is to compose the most important features into one new dataset that will be used for modeling. This part of the process applies to global- and local analyses hence two subfolders are present here: global and local. The global folder consists of several aspects that are applied to the cities of Bayreuth, Hamburg, and Utrecht:
 * defining the area of interest through a 100 meter grid for every city.
 * composing modeling dataset. Most of the predictors derive from TIFS that are projected onto the 100 meter grid for every city. Several other of the most important      features are deduced from other data sources than the TIFS and require slightly different data processing operations.

## 04 modeling

## 05 predicting

## 06 evaluating
