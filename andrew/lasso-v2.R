library(Matrix)
library(glmnet)
library(tidyverse)
library(rlang)
library(gt)
library(caret)
library(nnet)
library(broom)


# Load in Will's Final Data
final_data <- read.csv("final_dataset.csv")

# Load in Will's Training/Testing Data
ag_dataset <- read_csv('final_dataset.csv')

ag_dataset_model <- ag_dataset %>% 
  filter(is_satoyama2 %in% c('urban', 'okuyama', 'satoyama')) %>% 
  mutate(is_satoyama2 = fct_relevel(is_satoyama2, 'satoyama'))

# Creating testing/training data
set.seed(1)
training_samples <- ag_dataset_model$is_satoyama2 %>% 
  createDataPartition(p = 0.8, list = FALSE)
training_data  <- ag_dataset_model[training_samples, ]
testing_data <- ag_dataset_model[-training_samples]


# Training Data 
