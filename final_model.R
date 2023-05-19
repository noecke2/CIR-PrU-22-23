#---------------------------------FINAL_MODEL--------------------------------------------#
# In keeping with our philosophy and research question, this .R script will take the dataset created
# in ag_dataset.R and come up with some cool models. I'll start by creating a model just from the few natural/ecological/
# environmental variables we have, then one from just the social/cultural variables, then one that combines the two. I'll
# keep track of some important diagnostics along the way. Running this whole script will generate some gt tables with these.

library(tidyverse)
library(rlang)
library(gt)
library(caret)
library(nnet)
library(broom)

ag_dataset <- read_csv('final_dataset.csv')
# grab the columns of the dataset, sort out the ones that are "social" from the 5 that are purely "natural"
cols <- colnames(ag_dataset)
social <- cols[c(2:41, 47:50)]
natural <- cols[c(42:46)]
full <- c(social, natural)

# the shuraku we've already classified from the case studies become the modelling dataset - there are 1330 of these, we've classified them as urban, okuyama, satoyama
ag_dataset_model <- ag_dataset %>% 
  filter(is_satoyama2 %in% c('urban', 'okuyama', 'satoyama')) %>% 
  mutate(is_satoyama2 = fct_relevel(is_satoyama2, 'satoyama'))

# the rest become the "rest-of-japan" dataset, the shuraku to be classified
ag_dataset_japan <- ag_dataset %>% 
  filter(is_satoyama2 == 'unknown')

# split up the already-classfied shuraku, the ag_dataset_model, into training and testing set (80-20 split) - 1065 to train and 265 to test
set.seed(1)
training_samples <- ag_dataset_model$is_satoyama2 %>% 
  createDataPartition(p = 0.8, list = FALSE)
training_data  <- ag_dataset_model[training_samples, ]
testing_data <- ag_dataset_model[-training_samples, ]

# functions to spit out accuracy on testing set and aic for each model
accuracy <- function(x) {
  print(mean(x %>% predict(testing_data) == testing_data$is_satoyama2))
}

aic <- function(x) {
  print(glance(x)[['AIC']])
}


#--------------------------------------NATURAL MODELS-------------------------------#

#---------------------------------model_natural_1------------------------------------#

# dataframe to keep track of the variables
df_natural <- data.frame(name = vector(mode = "character", length = length(natural)),
                         aic = vector(mode = "double", length = length(natural)))

# see which variable we should add to create the best model by AIC
modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(natural))) {
  results <- modeler(natural[i])
  df_natural$name[i] <- results[[1]]
  df_natural$aic[i] <- results[[2]]
}

# display top 4 add-one models
df_natural %>% 
  slice_min(aic, n = 4) %>% 
  gt()

# the best variable was avg_altitude, create model_natural_1
model_natural_1 <- multinom(is_satoyama2 ~ avg_altitude, data = training_data)

# display its diagnostics
data.frame(accuracy = accuracy(model_natural_1), aic = aic(model_natural_1)) %>% 
  gt()

#---------------------------------model_natural_2------------------------------------#

# get rid of the variable we added to the model last time
natural <- natural[!natural == 'avg_altitude']

df_natural <- data.frame(name = vector(mode = "character", length = length(natural)),
                         aic = vector(mode = "double", length = length(natural)))

# test the rest of the variables- which one should we add to improve AIC the most?
modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(natural))) {
  results <- modeler(natural[i])
  df_natural$name[i] <- results[[1]]
  df_natural$aic[i] <- results[[2]]
}

df_natural %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_natural_2 <- multinom(is_satoyama2 ~ avg_altitude + prop_paddy_area, data = training_data)

data.frame(accuracy = accuracy(model_natural_2), aic = aic(model_natural_2)) %>% 
  gt()

#------------------------------model_natural_3-----------------------------------#

natural <- natural[!natural == 'prop_paddy_area']

df_natural <- data.frame(name = vector(mode = "character", length = length(natural)),
                         aic = vector(mode = "double", length = length(natural)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_paddy_area + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}


for (i in seq(length(natural))) {
  results <- modeler(natural[i])
  df_natural$name[i] <- results[[1]]
  df_natural$aic[i] <- results[[2]]
}

df_natural %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_natural_3 <- multinom(is_satoyama2 ~ avg_altitude + prop_paddy_area + prop_forest_area, data = training_data)

data.frame(accuracy = accuracy(model_natural_3), aic = aic(model_natural_3)) %>% 
  gt()


#--------------------------------------SOCIAL MODELS-------------------------------#

#------------------------------model_social_1-----------------------------------#

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}


for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_1 <- multinom(is_satoyama2 ~ pop_density, data = training_data)

data.frame(accuracy = accuracy(model_social_1), aic = aic(model_social_1)) %>% 
  gt()

#------------------------------model_social_2-----------------------------------#

social <- social[!social == 'pop_density']

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2 ~ pop_density + ', var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_2 <- multinom(is_satoyama2 ~ pop_density + time_to_did, data = training_data)

data.frame(accuracy = accuracy(model_social_2), aic = aic(model_social_2)) %>% 
  gt()

#------------------------------model_social_3-----------------------------------#

social <- social[!social == 'time_to_did']

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2 ~ pop_density + time_to_did + ', var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}


for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_3 <- multinom(is_satoyama2 ~ pop_density + time_to_did + mtn_program_area, data = training_data)

data.frame(accuracy = accuracy(model_social_3), aic = aic(model_social_3)) %>% 
  gt()

#------------------------------model_social_4-----------------------------------#

social <- social[!social == 'mtn_program_area']

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + ', var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}


for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_4 <- multinom(is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area, data = training_data)

data.frame(accuracy = accuracy(model_social_4), aic = aic(model_social_4)) %>% 
  gt()

#------------------------------model_social_5-----------------------------------#

social <- social[!social == 'depopulated_area']

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area + ', var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_5 <- multinom(is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area + specially_approved_area, data = training_data)

data.frame(accuracy = accuracy(model_social_5), aic = aic(model_social_5)) %>% 
  gt()

#------------------------------model_social_6-----------------------------------#

social <- social[!social == 'specially_approved_area']

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area + specially_approved_area + ', var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}


for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_6 <- multinom(is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area + specially_approved_area + num_orgs_mfngrants, data = training_data)

data.frame(accuracy = accuracy(model_social_6), aic = aic(model_social_6)) %>% 
  gt()

#------------------------------model_social_7-----------------------------------#

social <- social[!social == 'num_orgs_mfngrants']

df_social <- data.frame(name = vector(mode = "character", length = length(social)),
                        aic = vector(mode = "double", length = length(social)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area + specially_approved_area + num_orgs_mfngrants + ', var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}


for (i in seq(length(social))) {
  results <- modeler(social[i])
  df_social$name[i] <- results[[1]]
  df_social$aic[i] <- results[[2]]
}

df_social %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_social_7 <- multinom(is_satoyama2 ~ pop_density + time_to_did + mtn_program_area + depopulated_area + specially_approved_area + num_orgs_mfngrants + prop_under6_families, data = training_data)

data.frame(accuracy = accuracy(model_social_7), aic = aic(model_social_7)) %>% 
  gt()

#------------------------------model_full_5-----------------------------------#

full <- full[!full %in% c('avg_altitude', 'prop_forest_area', 'pop_density', 'mtn_program_area')]

df_full <- data.frame(name = vector(mode = "character", length = length(full)),
                        aic = vector(mode = "double", length = length(full)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(full))) {
  results <- modeler(full[i])
  df_full$name[i] <- results[[1]]
  df_full$aic[i] <- results[[2]]
}

df_full %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_full_5 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag, data = training_data)

data.frame(accuracy = accuracy(model_full_5), aic = aic(model_full_5)) %>% 
  gt()

#------------------------------model_full_6-----------------------------------#

full <- full[!full == 'meetings_ag']

df_full <- data.frame(name = vector(mode = "character", length = length(full)),
                      aic = vector(mode = "double", length = length(full)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(full))) {
  results <- modeler(full[i])
  df_full$name[i] <- results[[1]]
  df_full$aic[i] <- results[[2]]
}

df_full %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_full_6 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope, data = training_data)

data.frame(accuracy = accuracy(model_full_6), aic = aic(model_full_6)) %>% 
  gt()

#------------------------------model_full_7-----------------------------------#

full <- full[!full == 'avg_slope']

df_full <- data.frame(name = vector(mode = "character", length = length(full)),
                      aic = vector(mode = "double", length = length(full)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(full))) {
  results <- modeler(full[i])
  df_full$name[i] <- results[[1]]
  df_full$aic[i] <- results[[2]]
}

df_full %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_full_7 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses, data = training_data)

data.frame(accuracy = accuracy(model_full_7), aic = aic(model_full_7)) %>% 
  gt()

#------------------------------model_full_8-----------------------------------#

full <- full[!full == 'ag_businesses']

df_full <- data.frame(name = vector(mode = "character", length = length(full)),
                      aic = vector(mode = "double", length = length(full)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(full))) {
  results <- modeler(full[i])
  df_full$name[i] <- results[[1]]
  df_full$aic[i] <- results[[2]]
}

df_full %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_full_8 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + no_meetings, data = training_data)

data.frame(accuracy = accuracy(model_full_8), aic = aic(model_full_8)) %>% 
  gt()

#------------------------------model_full_9-----------------------------------#

full <- full[!full == 'no_meetings']

df_full <- data.frame(name = vector(mode = "character", length = length(full)),
                      aic = vector(mode = "double", length = length(full)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + no_meetings + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(full))) {
  results <- modeler(full[i])
  df_full$name[i] <- results[[1]]
  df_full$aic[i] <- results[[2]]
}

df_full %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_full_9 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + no_meetings + special_ag_mtn_villages, data = training_data)

data.frame(accuracy = accuracy(model_full_9), aic = aic(model_full_9)) %>% 
  gt()

#------------------------------model_full_10-----------------------------------#

full <- full[!full == 'special_ag_mtn_villages']

df_full <- data.frame(name = vector(mode = "character", length = length(full)),
                      aic = vector(mode = "double", length = length(full)))

modeler <- function(var) {
  name <- var
  model <- multinom(as.formula(paste0('is_satoyama2', " ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + no_meetings + special_ag_mtn_villages + ", var)), data = training_data)
  aic <- summary(model)['AIC']
  return(c(name, aic))
}

for (i in seq(length(full))) {
  results <- modeler(full[i])
  df_full$name[i] <- results[[1]]
  df_full$aic[i] <- results[[2]]
}

df_full %>% 
  slice_min(aic, n = 4) %>% 
  gt()

model_full_10 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + no_meetings + special_ag_mtn_villages + depopulated_area, data = training_data)

data.frame(accuracy = accuracy(model_full_10), aic = aic(model_full_10)) %>% 
  gt()



#-----------------------------------DIAGNOSTICS------------------------------------#

## This function will show how the model would classify our 9 cases and the rest of Japan's shuraku.
get_breakdown_preds <- function(x) {
  ag_dataset$predictions <- predict(x, newdata = ag_dataset)
  
  ag_dataset %>% 
    mutate(case2 = as.factor(case2),
           case2 = fct_relevel(case2, 'Saitama', 'Hamamatsu', 'Fukuoka', 'Noto', 'Nishi-Awa', 'Minabe-Tanabe', 'Tadami', 'Iide', 'Ishikawa', 'Japan')) %>% 
    group_by(case2) %>% 
    summarize(prop_urban = mean(predictions == 'urban'),
              prop_satoyama = mean(predictions == 'satoyama'),
              prop_okuyama = mean(predictions == 'okuyama'))
}


# Summary shows the estimated coefficients and std. errors for each variable in each model
summary(model_natural_1)
summary(model_natural_2)
summary(model_natural_3)
summary(model_social_1)
summary(model_social_2)
summary(model_social_3)
summary(model_social_4)
summary(model_social_5)
summary(model_social_6)
summary(model_social_7)
summary(model_full_5)
summary(model_full_6)
summary(model_full_7)
summary(model_full_8)
summary(model_full_9)
summary(model_full_10)

# Breakdown how each model classifies our cases and all of Japan
get_breakdown_preds(model_natural_1)
get_breakdown_preds(model_natural_2)
get_breakdown_preds(model_natural_3)
get_breakdown_preds(model_social_1)
get_breakdown_preds(model_social_2)
get_breakdown_preds(model_social_3)
get_breakdown_preds(model_social_4)
get_breakdown_preds(model_social_5)
get_breakdown_preds(model_social_6)
get_breakdown_preds(model_social_7)
get_breakdown_preds(model_full_5)
get_breakdown_preds(model_full_6)
get_breakdown_preds(model_full_7)
get_breakdown_preds(model_full_8)
get_breakdown_preds(model_full_9)
get_breakdown_preds(model_full_10)



#-------------------------------------WRITE----------------------------------------#
# This code adds the predictions from each of our final models to the final dataset so they can go to ArcGIS.
# For each model, it stores the straight-up predictions (urban, satoyama, or okuyama), the predictions as numbers 
# (0 = urban, 1 = satoyama, 2 = okuyama, because ArcGIS needs a numeric variable to create our desired bivariate
# cloropleth map), and the probability that the model assigns to that prediction.

# Model Alpha (old model from mid-july, for comparison)
model_alpha <- multinom(is_satoyama2 ~ prop_forest_area + prop_paddy_area + meetings_events + avg_slope + depopulated_area + special_ag_mtn_villages + avg_age,  data = training_data)
predictions_alpha <- predict(model_natural_3, ag_dataset)
predictions_alpha_probs <- as.data.frame(predict(model_alpha, ag_dataset, type = 'probs')) %>% 
  rowwise() %>% 
  mutate(predictions_alpha_probmaxs = max(satoyama, okuyama, urban)) %>% 
  mutate(predictions_alpha_num = ifelse(satoyama > okuyama & satoyama > urban, '1',
                                          ifelse(okuyama > satoyama & okuyama > urban, '2',
                                                 ifelse(urban > satoyama & urban > okuyama, '0', NA))))
ag_dataset$predictions_alpha <- predictions_alpha
ag_dataset$predictions_alpha_probmaxs <- predictions_alpha_probs$predictions_alpha_probmaxs
ag_dataset$predictions_alpha_num <- predictions_alpha_probs$predictions_alpha_num



# Natural Model 3 (Final natural-only model) 
model_natural_3 <- multinom(is_satoyama2 ~ avg_altitude + prop_paddy_area + prop_forest_area, data = training_data)
predictions_natural_3 <- predict(model_natural_3, ag_dataset)
predictions_natural_3_probs <- as.data.frame(predict(model_natural_3, ag_dataset, type = 'probs')) %>% 
  rowwise() %>% 
  mutate(predictions_natural_3_probmaxs = max(satoyama, okuyama, urban)) %>% 
  mutate(predictions_natural_3_num = ifelse(satoyama > okuyama & satoyama > urban, '1',
                                              ifelse(okuyama > satoyama & okuyama > urban, '2',
                                                     ifelse(urban > satoyama & urban > okuyama, '0', NA))))
ag_dataset$predictions_natural_3 <- predictions_natural_3
ag_dataset$predictions_natural_3_probmaxs <- predictions_natural_3_probs$predictions_natural_3_probmaxs
ag_dataset$predictions_natural_3_num <- predictions_natural_3_probs$predictions_natural_3_num



# Social Model 7 (Final social-only model)
model_social_7 <- multinom(is_satoyama2 ~ pop_density + time_to_did + prop_over65_families + mtn_program_area + depopulated_area + num_orgs_mfngrants, data = training_data)
predictions_social_7 <- predict(model_social_7, ag_dataset)
predictions_social_7_probs <- as.data.frame(predict(model_social_7, ag_dataset, type = 'probs')) %>% 
  rowwise() %>% 
  mutate(predictions_social_7_probmaxs = max(satoyama, okuyama, urban)) %>% 
  mutate(predictions_social_7_num = ifelse(satoyama > okuyama & satoyama > urban, '1',
                                             ifelse(okuyama > satoyama & okuyama > urban, '2',
                                                    ifelse(urban > satoyama & urban > okuyama, '0', NA))))
ag_dataset$predictions_social_7 <- predictions_social_7
ag_dataset$predictions_social_7_probmaxs <- predictions_social_7_probs$predictions_social_7_probmaxs
ag_dataset$predictions_social_7_num <- predictions_social_7_probs$predictions_social_7_num



# Full model 6 (Final full model) 
model_full_6 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope, data = training_data)
predictions_full_6 <- predict(model_full_6, ag_dataset)
predictions_full_6_probs <- as.data.frame(predict(model_full_6, ag_dataset, type = 'probs')) %>% 
  rowwise() %>% 
  mutate(predictions_full_6_probmaxs = max(satoyama, okuyama, urban)) %>% 
  mutate(predictions_full_6_num = ifelse(satoyama > okuyama & satoyama > urban, '1',
                                          ifelse(okuyama > satoyama & okuyama > urban, '2',
                                                 ifelse(urban > satoyama & urban > okuyama, '0', NA))))
ag_dataset$predictions_full_6 <- predictions_full_6
ag_dataset$predictions_full_6_probmaxs <- predictions_full_6_probs$predictions_full_6_probmaxs
ag_dataset$predictions_full_6_num <- predictions_full_6_probs$predictions_full_6_num



# Full model 10 
model_full_10 <- multinom(is_satoyama2 ~ avg_altitude + prop_forest_area + pop_density + mtn_program_area + meetings_ag + avg_slope + ag_businesses + no_meetings + special_ag_mtn_villages + depopulated_area, data = training_data)
predictions_full_10 <- predict(model_full_10, ag_dataset)
predictions_full_10_probs <- as.data.frame(predict(model_full_10, ag_dataset, type = 'probs')) %>% 
  rowwise() %>% 
  mutate(predictions_full_10_probmaxs = max(satoyama, okuyama, urban)) %>% 
  mutate(predictions_full_10_num = ifelse(satoyama > okuyama & satoyama > urban, '1',
                                           ifelse(okuyama > satoyama & okuyama > urban, '2',
                                                  ifelse(urban > satoyama & urban > okuyama, '0', NA))))
ag_dataset$predictions_full_10 <- predictions_full_10
ag_dataset$predictions_full_10_probmaxs <- predictions_full_10_probs$predictions_full_10_probmaxs
ag_dataset$predictions_full_10_num <- predictions_full_10_probs$predictions_full_10_num


# Write to CSV
ag_dataset %>% 
  # We need to add 'KEY' in front of the 10-digit shuraku key code, otherwise GIS will interpret it as an integer, not a string.
  # I took these out with the Calculate Field tool in order to go back to the straight-up 10-digit code (key = !key![3:])
  mutate(gis_key = paste0('KEY', key)) %>% 
  write_csv('final_dataset.csv')

# Code to export to ArcGIS to map full model comps - not really necessary 
# ag_dataset %>%
#   select(key, 63:74) %>%
#   mutate(gis_key = paste0('KEY', key)) %>%
#   write_csv("data/will_final_preds.csv")
