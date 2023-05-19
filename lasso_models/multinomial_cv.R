### Function to cross validate multinomial logistic model 

library(nnet)
library(caret)
library(tidyverse)

# 1. Assign a fold value to each row in the data (I think this should be the same everytime we run the model)

# 2. Within map_dfr - for each unique fold, define the following function(fold)

  # 3. test_data = data[id == fold]

  # 4. train_data = data[id != fold]

  # 5. Build multinomial model with train data

  # 6. Predict and return tibble of predicted values

  # 7. map_dfr will iterate through all the folds and the final tibble will be the compiled rows (aka all rows)




# Uncomment 15 lines below to load in data if necessary -------------------



# # Load in Will's Training/Testing Data
# ag_dataset <- read_csv('final_dataset.csv')
# 
# ag_dataset_model <- ag_dataset %>% 
#   filter(is_satoyama2 %in% c('urban', 'okuyama', 'satoyama')) %>% 
#   mutate(is_satoyama2 = fct_relevel(is_satoyama2, 'satoyama'))
# 
# 
# # Sort out the different columns for social/natural/full
# cols <- colnames(ag_dataset)
# social <- cols[c(2:41, 47:50)]
# natural <- cols[c(42:46)]
# full <- c(social, natural)
# 
# # Select the relevant columns
# ag_dataset_model <- ag_dataset_model[c(full, "is_satoyama2")]


# Cross validation function -----------------------------------------------


multinomial_cv <- function(formula) {
  # Assign random folds - step 1 from above
  set.seed(1)
  model_data <- ag_dataset_model %>%
    mutate(fold_id = sample(rep(1:10, length.out = 1330), size = 1330))
  
  # use map_dfr to iterate through every fold
  cv_preds <- map_dfr(unique(model_data$fold_id), 
          function(fold) {
            # Create test/train data
            test_data <- model_data %>%
              filter(fold_id == fold)
            train_data <- model_data %>%
              filter(fold_id != fold)
            
            # Train model
            mult_model <- multinom(as.formula(paste0("is_satoyama2", " ~ ", formula)), data = train_data)
          
            # Return tibble of results on test data
            tibble(test_pred = predict(mult_model, test_data),
                   test_actual = test_data$is_satoyama2,
                   fold_id = test_data$fold_id, 
                   num_preds = length(mult_model$coefnames) - 1
                   )
            }
          )
  
  cv_preds
}

