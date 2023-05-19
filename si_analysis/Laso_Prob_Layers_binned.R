prob_layers <- read_csv("/home/rstudio/users/noecke2/CIR-PrU-22-23/model_preds/lasso_prob_layers.csv")

#Here is where we create the bins for the different categories of model uncertainty.
#We created four extra bins for max probabilities less than 0.75
prob_layers <- prob_layers %>%
  mutate(predpairs_lasso_full_9 = ifelse((predictions_lasso_full_9 == 'satoyama' & satoyama_prob < 0.75 & okuyama_prob > 0.25), 'satoyama_okuyama', 
                                         ifelse((predictions_lasso_full_9 == 'satoyama' & satoyama_prob < 0.75 & urban_prob > 0.25), 'satoyama_urban',
                                                ifelse((predictions_lasso_full_9 == 'okuyama' & okuyama_prob < 0.75 & satoyama_prob > 0.25), 'okuyama_satoyama',
                                                       ifelse((predictions_lasso_full_9 == 'urban' & urban_prob < 0.75 & satoyama_prob > 0.25), 'urban_satoyama', predictions_lasso_full_9))
    
  )))

prob_layers %>%
  count(predpairs_lasso_full_9)


prob_layers_nodup <- distinct(prob_layers)


write_csv(prob_layers_nodup,"/home/rstudio/users/noecke2/CIR-PrU-22-23/model_preds/lasso_pair_prob_layers.csv")
