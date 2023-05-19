
# Purpose: Load in envt_footprint data from Google Sheets------------------------------------

library(tidyverse)
library(googlesheets4)

# Unique token for sheet

ss <- "1K5HF3e2qPQJLazcVgacUC8-teTxdsd5HGI9Zv7KMxJc"

googlesheets4::gs4_auth(email = "noecke2@stolaf.edu", token=ss)

# Read in the 'transposed' sheet
footprint <- googlesheets4::read_sheet(ss = ss,
                          sheet = "transposed")

# Standardizing Function

Zscore <- function(x){
  (x-mean(x))/sd(x)
}

# Rename columns

colnames(footprint)[6:20] <- c("gross_capital_formation",                    # "Gross Fixed Capital Formation"
                               "food_non_alcohol",                           # "1. Food and non-alcoholic beverages"
                               "alcohol_tobacco_narcotics",                  # "2. Alcoholic beverages, tobacco and narcotics"
                               "clothes_shoes",                              # "3. Clothing and footwear" 
                               "housing_water_electric_gas_other",           # "4. Housing, water, electricity, gas and other fuels"  
                               "house_furnish_equip_maintain",               # "5. Household furnishings, equipment and maint." 
                               "health",                                     # "6. Health"   
                               "transportation",                             # "7. Transportation"     
                               "communication",                              # "8. Communication" 
                               "recreation_culture",                         # "9. Recreation and culture"      
                               "education",                                  # "10. Education" 
                               "restaurants_hotels",                         # "11. Restaurants and hotels"        
                               "misc_goods_services",                        # "12. Miscellaneous goods and services"  
                               "electric_gas_other_fuel",                    # "Electricity, gas other fuels"
                               "water_misc_services")                        # "Water supply and miscellaneous dwelling services" 


# Standardize
Footprint_Scaled <- footprint %>%
  mutate(across(.cols = -(1:2), .fns = Zscore, .names = "{.col}_z"))

Footprint_Scaled %>% 
  select(ends_with("z"), contains("Name")) 


write_csv(Footprint_Scaled, "envt_footprint/FootprintData_Scaled.csv")



# #footprint %>%
#   summarize_all(sd) %>% 
#   print(width = Inf) %>%
#   pivot_longer(3:20) %>%
#   arrange(-value) %>%
#   select(-1,-2)
# 
# 
# footprint %>% mutate(household_scale=scale(Household)) %>%
#   select(household_scale, Household, `Prefecutre Name`) %>%
#   arrange(-household_scale) %>%
#   print(n = 47)
