library(tidyverse)
final_data <- read_csv("~/CIR-PrU-22-23/final_dataset.csv")

final_data$gis_key


colnames(final_data)
(final_data_small <- final_data %>%
  select(72:75))


  
