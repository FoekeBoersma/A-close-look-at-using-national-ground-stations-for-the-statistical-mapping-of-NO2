# Necessary libraries
library(tidyverse)
library(caret)
library(leaps)
library(yaml)
library(rstudioapi)

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_03.yml")
config_03 <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# Define output path
out_location <- config_03$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location), winslash = "/")

# Import geodata
local_top30_dataset <- config_03$local$local_top30
local_top30_map_dir <- normalizePath(file.path(parent_directory, local_top30_dataset), winslash = "/")
local_data_top30 <- read.csv(local_top30_map_dir, sep = ',')

# Deselect irrelevant column(s)
local_data_top30 <- local_data_top30 %>% dplyr::select(-c(X))

# Replace NA with 0
local_data_top30[is.na(local_data_top30)] <- 0

# Determine number of features and which features for modelling
models <- regsubsets(Lopend_gemiddelde ~ ., data = local_data_top30, nvmax = 30, really.big = TRUE)
res.sum <- summary(models)

# Compute best model IDs based on Adjusted R2, CP, and BIC
best_adj_r2_id <- which.max(res.sum$adjr2)
best_cp_id <- which.min(res.sum$cp)
best_bic_id <- which.min(res.sum$bic)

# Compute cross-validation error for each model
get_model_formula <- function(id, object, outcome) {
  models <- summary(object)$which[id, -1]
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data_top30) {
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = data_top30, method = "lm", trControl = train.control)
  cv$results$RMSE
}

model.ids <- 1:30
cv.errors <- map(model.ids, get_model_formula, models, "Lopend_gemiddelde") %>%
  map(get_cv_error, data = local_data_top30) %>%
  unlist()

# Identify the best model ID based on minimum cross-validation error
best_model_id <- which.min(cv.errors)

# Display the best model formula
best_model_formula <- get_model_formula(best_model_id, models, "Lopend_gemiddelde")
best_model_formula

# Show the coefficients of the best model
best_model_coefs <- coef(models, best_model_id)
best_model_coefs

# Identify the top 9 variables by absolute value of coefficients
top_9_vars <- sort(abs(best_model_coefs[-1]), decreasing = TRUE)[1:9]
top_9_vars

# Display the sequence of the top 9 variables
top_9_vars_seq <- names(top_9_vars)
top_9_vars_seq

# Prepare dataset with the top 9 variables
predicting_dataset <- local_data_top30[, c("Lopend_gemiddelde", top_9_vars_seq)]

# Write to CSV
# write.csv(predicting_dataset, file.path(out_location_dir, 'PredictingDataset1.csv'), row.names = FALSE)

# Display the sequence of top 9 variables
print("sequence")
print(top_9_vars_seq)
