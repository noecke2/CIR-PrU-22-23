
#This document shows my initial analysis of SI values by group(okuyama, Satoyama, Urban).
#There are some interesting plots but overall I think our assumptions of normality are violated
#and we should consider non parametric tests.



#--------------------------------------Libraries-------------------------------#
library(tidyverse)
library(ggResidpanel)
library(mosaic)



#-------------------------------------WILLSPREDICTIONS---------------------------------#
#--------------------------------------Creating Data Set-------------------------------#


si <- read_csv("~/CIR-PrU-22-23/data/Zonal_SI.csv")

will_predictions <- read_csv("~/CIR-PrU-22-23/final_dataset.csv") %>%
  select(1, 72)

will_predictions %>%
  count(key, sort = TRUE) %>%
  filter(n>1)

si %>%
  count(KEY, sort = TRUE) %>%
  filter(n>1)


(will_predictions_si <- si %>%
  inner_join(will_predictions, by = c("KEY" = "key")) %>%
    rename(si = MEAN,
           land_type = predictions_full_10) %>%
  select(2,3,6,7))

will_predictions_si %>%
  count(KEY, sort = TRUE) %>%
  filter(n>1)
  
#---------------------------find outliers---------------------------------#

gisexport <- will_predictions_si %>%
  filter(si > 0.6)

gisexport %>%
  count(KEY, sort = TRUE) %>%
  filter(n>1)

# write_csv(gisexport,"~/CIR-PrU-22-23/data/high_si.csv")

gisexport %>%
  mutate(gis_key = paste0('KEY', KEY)) %>%
  write_csv("~/CIR-PrU-22-23/data/high_si.csv")


will_predictions_si %>%
  group_by(land_type) %>%
  summarize(mean_si = mean(si), occurrences = n(), sd_si = sd(si))

#------------------------------------Running Anova/t-tests--------------------------#

will_predictions_si <- will_predictions_si %>%
  mutate(land_type_fac=as.factor(land_type))

#ANOVA
ggplot(data = will_predictions_si, aes(y = si, x = land_type_fac)) +
  geom_violin(color = NA, alpha = 0.75) +
  geom_boxplot(width = 0.25) +
  labs(x = "Land Type", y = "Satoyama Index")

model1 <- aov(si ~ land_type_fac, data = will_predictions_si)
summary(model1)

favstats(~ si | land_type, data = will_predictions_si)

TukeyHSD(model1)

#---------------t-test----------------------------------------------------------#

will_predictions_si_no_urban <- will_predictions_si %>%
  filter(!(land_type == "urban"))

t.test(si ~ land_type, data = will_predictions_si_no_urban)


#---------------data not normally distributed----------------------------------#
ggplot(data = will_predictions_si) +
  geom_histogram(aes(x = si))

ggplot(data = will_predictions_si) +
  geom_histogram(aes(x = si)) +
  facet_wrap(~land_type)

#These take awhile to run
plot(model1, which =2)
plot(model1, which =1)


#-------------------------------------LASSOFULLPREDICTIONS---------------------------------#
#--------------------------------------Creating Data Set-------------------------------#
 
lasso_predictions <- read_csv("~/CIR-PrU-22-23/model_preds/full_model_preds.csv")

lasso_predictions <- lasso_predictions %>%
  mutate(KEY = str_replace(gis_key, "KEY", "")) %>%
  select(-gis_key,-3,-4)



lasso_predictions_si <- si %>%
  inner_join(lasso_predictions, by = c("KEY")) %>%
  rename(si = MEAN,
         land_type = predictions_lasso_full_9) %>%
  select(2,3,6,7)

lasso_predictions_si %>%
  group_by(land_type) %>%
  summarize(mean_si = mean(si), occurrences = n(), sd_si = sd(si))

#------------------------------------Running Anova/t-tests--------------------------#

lasso_predictions_si <- lasso_predictions_si %>%
  mutate(land_type_fac=as.factor(land_type))

#ANOVA
ggplot(data = lasso_predictions_si, aes(y = si, x = land_type_fac)) +
  geom_violin(color = NA, alpha = 0.75) +
  geom_boxplot(width = 0.25) +
  labs(x = "Land Type", y = "Satoyama Index")

model2 <- aov(si ~ land_type_fac, data = lasso_predictions_si)
summary(model1)

favstats(~ si | land_type, data = lasso_predictions_si)

TukeyHSD(model2)

#---------------t-test----------------------------------------------------------#

lasso_predictions_si_no_urban <- lasso_predictions_si %>%
  filter(!(land_type == "urban"))

t.test(si ~ land_type, data = lasso_predictions_si_no_urban)


#---------------data not normally distributed----------------------------------#
ggplot(data = lasso_predictions_si) +
  geom_histogram(aes(x = si))

ggplot(data = lasso_predictions_si) +
  geom_histogram(aes(x = si)) +
  facet_wrap(~land_type)

#These take awhile to run
plot(model2, which =2)
plot(model2, which =1)









