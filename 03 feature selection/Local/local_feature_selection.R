#necessary libraries
library(tidyverse)
library(caret)
library(leaps)

## == import geodata == ##

local_data_top30 <- read.csv('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/LocalModelData/dataset_bestMetrics_longlat-Local-top30.csv', sep=',')
#deselect irrelevant column(s)
local_data_top30 <- local_data_top30 %>% dplyr::select(-c(X))
#replace NA with 0
local_data_top30[is.na(local_data_top30)] <- 0
local_data_top30

## == determine number of features and which features for modelling == ##
#source:http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/
models <- regsubsets(Lopend_gemiddelde~., data = local_data_top30, nvmax = 30,really.big=T)
summary(models)

#The summary() function returns some metrics - Adjusted R2, Cp and BIC
#(see Chapter @ref(regression-model-accuracy-metrics)) -
#allowing us to identify the best overall model, where best is defined as the model that
#maximize the adjusted R2 and minimize the prediction error (RSS, cp and BIC).
#The adjusted R2 represents the proportion of variation, in the outcome, that are explained by
#the variation in predictors values. the higher the adjusted R2, the better the model.
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

# K-fold cross-validation
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
#get_model_formula(), allowing to access easily the formula of the models returned by the function regsubsets()
get_model_formula(10, models, "Lopend_gemiddelde")
#get_cv_error(), to get the cross-validation (CV) error for a given model
get_cv_error <- function(model.formula, data_top30){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = local_data_top30, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
#Finally, use the above defined helper functions to compute the prediction error of the
#different best models returned by the regsubsets() function
# Compute cross-validation error
model.ids <- 1:30
cv.errors <-  map(model.ids, get_model_formula, models, "Lopend_gemiddelde") %>%
  map(get_cv_error, data = local_data_top30) %>%
  unlist()
cv.errors 
# Select the model that minimize the CV error (outputs 9)
which.min(cv.errors)

#examine which related variables
coef(models, 9)

predicting_dataset <- local_data_top30[, c("Lopend_gemiddelde", "nightlight_450", "nightlight_4950", "population_3000", "road_class_1_5000", "road_class_2_1000", "road_class_2_5000", "road_class_3_100", "road_class_3_300", "trafBuf50", "Longitude", "Latitude")]
predicting_dataset

#csv
write.csv(predicting_dataset, 'C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/LocalModelData/PredictingDataset.csv',col.names = TRUE)

