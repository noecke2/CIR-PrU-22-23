
# Purpose: Use Land Area to analyze difference between prefecture  --------

# Load in data and libraries ----------------------------------------------

library(tidyverse)

attribute_table <- read_csv("envt_footprint/prob_layers_table.csv") %>%
  mutate(PREF = parse_number(PREF))

footprint_scaled <- read_csv("envt_footprint/FootprintData_Scaled.csv")


# Summarize prefecture data by land proportion -----------------------------------------------

pref_summary <- attribute_table %>%
  group_by(PREF) %>%
  summarize(total_area = sum(Shape_Area),
            prop_satoyama = sum(((predictions_lasso_full_9_num_1 == 1)*Shape_Area)) / total_area,
            # if preds = 1, then it's satoyama
            prop_okuyama = sum(((predictions_lasso_full_9_num_1 == 2)*Shape_Area)) / total_area,
            # if preds = 2, then it's okuyama
            prop_urban = sum(((predictions_lasso_full_9_num_1 == 0)*Shape_Area)) / total_area
            # if preds = 0, then it's urban
            )



# Plots -------------------------------------------------------------------

# Urban hex code: #bf7000ff

# Satoyama hex code:#38a800ff

# Okuyama hex code: #00674cff




# Dataset for easier plotting of boxplots 

### One point for each prefecture

max_pref_summary <- pref_summary %>%
  left_join(footprint_scaled, by = c("PREF" = "Prefecture Number")) %>%
  pivot_longer(cols = 3:5, names_to = "land_type", values_to = "prop") %>%
  # Assigning each prefecture it's max proportion
  group_by(`Prefecutre Name`) %>%
  filter(prop == max(prop)) %>%
  ungroup()


### 5 different ecological footprint metrics plotted

# Boxplots ----------------------------------------------------------------

# Food

max_pref_summary %>%
  mutate(land_type = fct_recode(land_type, 
                                Okuyama = "prop_okuyama", 
                                Satoyama = "prop_satoyama", 
                                Urban = "prop_urban")) %>%
  ggplot(aes(x = land_type, y = `food_non_alcohol_z`, color = land_type)) + 
  geom_boxplot(varwidth = TRUE)+
  # varwidth assigns width of the box to how many are in the data
  theme_bw() + 
  labs(x = "Max Land Type",
       y = "Footprint",
       title = "Fig. 1: Footprint metrics by land type classification",
       subtitle = "Food and non-alcoholic beverages") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))


### Housing, water, electricity, gas, and other fuels
max_pref_summary %>%
  mutate(land_type = fct_recode(land_type, 
                                Okuyama = "prop_okuyama", 
                                Satoyama = "prop_satoyama", 
                                Urban = "prop_urban")) %>%
  ggplot(aes(x = land_type, y = `housing_water_electric_gas_other_z`, color = land_type)) + 
  geom_boxplot(varwidth = TRUE)+
  theme_bw() + 
  labs(x = "Max Land Type",
       y = "Footprint",
       title = "Fig. 2: Footprint metrics by land type classification",
       subtitle = "Housing, water, electricity, gas and other fuels") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))


### Recreation and Culture
max_pref_summary %>%
  mutate(land_type = fct_recode(land_type, 
                                Okuyama = "prop_okuyama", 
                                Satoyama = "prop_satoyama", 
                                Urban = "prop_urban")) %>%
  ggplot(aes(x = land_type, y = `recreation_culture_z`, color = land_type)) + 
  geom_boxplot(varwidth = TRUE)+
  theme_bw() + 
  labs(x = "Max Land Type",
       y = "Footprint",
       title = "Fig. 3: Footprint metrics by land type classification",
       subtitle = "Recreation and culture") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))



### Electricity, gas, other fuels
max_pref_summary %>%
  mutate(land_type = fct_recode(land_type, 
                                Okuyama = "prop_okuyama", 
                                Satoyama = "prop_satoyama", 
                                Urban = "prop_urban")) %>%

  ggplot(aes(x = land_type, y = `electric_gas_other_fuel_z`, color = land_type)) + 
  geom_boxplot(varwidth = TRUE)+
  theme_bw() + 
  labs(x = "Max Land Type",
       y = "Footprint",
       title = "Fig. 4: Footprint metrics by land type classification",
       subtitle = "Electricity, gas and other fuels") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))


### Water supply
max_pref_summary %>%
  mutate(land_type = fct_recode(land_type, 
                                Okuyama = "prop_okuyama", 
                                Satoyama = "prop_satoyama", 
                                Urban = "prop_urban")) %>%
  ggplot(aes(x = land_type, y = `water_misc_services_z`, color = land_type)) + 
  geom_boxplot(varwidth = TRUE)+
  theme_bw() + 
  labs(x = "Max Land Type",
       y = "Footprint",
       title = "Fig. 5: Footprint metrics by land type classification",
       subtitle = "Water supply") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))

# Scatterplots ------------------------------------------------------------


# Data to make scatterplots - this way there are 3 points for each prefecture
long_pref_summary <- pref_summary %>%
  left_join(footprint_scaled, by = c("PREF" = "Prefecture Number")) %>%
  pivot_longer(cols = 3:5, names_to = "land_type", values_to = "prop") %>%
  mutate(land_type = fct_recode(land_type, 
                                Okuyama = "prop_okuyama", 
                                Satoyama = "prop_satoyama", 
                                Urban = "prop_urban"))



### Food 
long_pref_summary %>%
  # filter(land_type == "prop_satoyama") %>%
  ggplot(aes(x = prop, y = `food_non_alcohol_z`, color = land_type)) + 
  geom_point(alpha = 0.5) + 
   geom_smooth(se = FALSE, method = "lm") +#
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~land_type) + 
  labs(x = "Proportion of land type",
       y = "Footprint",
       title = "Fig. 6: Footprint metrics by land type classification",
       subtitle = "Food and non-alcoholic beverages") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))

### Housing, water, electricity, gas, and other fuels
long_pref_summary %>%
  ggplot(aes(x = prop, y = `housing_water_electric_gas_other_z`, color = land_type)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~land_type) + 
  labs(x = "Proportion of land type",
       y = "Footprint",
       title = "Fig. 7: Footprint metrics by land type classification",
       subtitle = "Housing, water, electricity, gas and other fuels") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))


### Recreation and Culture
long_pref_summary %>%
  ggplot(aes(x = prop, y = `recreation_culture_z`, color = land_type)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE, method = "lm") + 
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~land_type) + 
  labs(x = "Proportion of land type",
       y = "Footprint",
       title = "Fig. 8: Footprint metrics by land type classification",
       subtitle = "Recreation and culture") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))



### Electricity, gas, other fuels
long_pref_summary %>%
  ggplot(aes(x = prop, y = `electric_gas_other_fuel_z`, color = land_type)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~land_type) + 
  labs(x = "Proportion of land type",
       y = "Footprint",
       title = "Fig. 9: Footprint metrics by land type classification",
       subtitle = "Electricity, gas and other fuels") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))


### Water supply
long_pref_summary %>%
  ggplot(aes(x = prop, y = `water_misc_services_z`, color = land_type)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() + 
  theme(legend.position = "none") +
  facet_wrap(~land_type) + 
  labs(x = "Proportion of land type",
       y = "Footprint",
       title = "Fig. 10: Footprint metrics by land type classification",
       subtitle = "Water supply") + 
  scale_color_manual(name = "Land type",
                     values = c("#00674cff", "#38a800ff", "#bf7000ff"))

