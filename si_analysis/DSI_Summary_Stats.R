
#This document shows my initial analysis of DSI values by group(okuyama, Satoyama, Urban).
#There are some interesting plots but overall I think our assumptions of normality are violated
#and we should consider non parametric tests.



#--------------------------------------Libraries-------------------------------#
library(tidyverse)
library(ggResidpanel)
library(mosaic)



#-------------------------------------WILLSPREDICTIONS---------------------------------#
#--------------------------------------Creating Data Set-------------------------------#


dsi <- read_csv("~/CIR-PrU-22-23/data/Zonal_DSI.csv")

will_predictions <- read_csv("~/CIR-PrU-22-23/final_dataset.csv") %>%
  select(1, 72)

(will_predictions_dsi <- dsi %>%
    inner_join(will_predictions, by = c("KEY" = "key")) %>%
    rename(dsi = MEAN,
           land_type = predictions_full_10) %>%
    select(2,3,6,7))


will_predictions_dsi %>%
  group_by(land_type) %>%
  summarize(mean_dsi = mean(dsi))

#------------------------------------Running Anova/t-tests--------------------------#

will_predictions_dsi <- will_predictions_dsi %>%
  mutate(land_type_fac=as.factor(land_type))

#ANOVA
ggplot(data = will_predictions_dsi, aes(y = dsi, x = land_type_fac)) +
  geom_violin(color = NA, alpha = 0.75) +
  geom_boxplot(width = 0.25) +
  labs(x = "Land Type", y = "Satoyama Index")

model1b <- aov(dsi ~ land_type_fac, data = will_predictions_dsi)
summary(model1b)

favstats(~ dsi | land_type, data = will_predictions_dsi)

TukeyHSD(model1b)

#---------------t-test----------------------------------------------------------#

will_predictions_dsi_no_urban <- will_predictions_dsi %>%
  filter(!(land_type == "urban"))

t.test(dsi ~ land_type, data = will_predictions_dsi_no_urban)


#---------------data not normally distributed----------------------------------#
ggplot(data = will_predictions_dsi) +
  geom_histogram(aes(x = dsi))

ggplot(data = will_predictions_dsi) +
  geom_histogram(aes(x = dsi)) +
  facet_wrap(~land_type)

#These take awhile to run
#plot(model1b, which =2)
#plot(model1b, which =1)


#-------------------------------------LASSOFULLPREDICTIONS---------------------------------#
#--------------------------------------Creating Data Set-------------------------------#

lasso_predictions <- read_csv("~/CIR-PrU-22-23/model_preds/full_model_preds.csv")

lasso_predictions <- lasso_predictions %>%
  mutate(KEY = str_replace(gis_key, "KEY", "")) %>%
  select(-gis_key,-3,-4)



lasso_predictions_dsi <- dsi %>%
  inner_join(lasso_predictions, by = c("KEY")) %>%
  rename(dsi = MEAN,
         land_type = predictions_lasso_full_9) %>%
  select(2,3,6,7)

lasso_predictions_dsi %>%
  group_by(land_type) %>%
  summarize(mean_dsi = mean(dsi), n = n())

#------------------------------------Running Anova/t-tests--------------------------#

lasso_predictions_dsi <- lasso_predictions_dsi %>%
  mutate(land_type_fac=as.factor(land_type))

#ANOVA
ggplot(data = lasso_predictions_dsi, aes(y = dsi, x = land_type_fac)) +
  geom_violin(color = NA, alpha = 0.75) +
  geom_boxplot(width = 0.25) +
  labs(x = "Land Type", y = "Satoyama Index")

model2b <- aov(dsi ~ land_type_fac, data = lasso_predictions_dsi)
summary(model2b)

favstats(~ dsi | land_type, data = lasso_predictions_dsi)

TukeyHSD(model2b)

#---------------t-test----------------------------------------------------------#

lasso_predictions_dsi_no_urban <- lasso_predictions_dsi %>%
  filter(!(land_type == "urban"))

t.test(dsi ~ land_type, data = lasso_predictions_dsi_no_urban)


#---------------data not normally distributed----------------------------------#
ggplot(data = lasso_predictions_dsi) +
  geom_histogram(aes(x = dsi))

ggplot(data = lasso_predictions_dsi) +
  geom_histogram(aes(x = dsi)) +
  facet_wrap(~land_type)

#These take awhile to run
#plot(model2b, which =2)
#plot(model2b, which =1)