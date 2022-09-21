#---------------------------------FINAL_DATASET--------------------------------------------#
# This .R script is designed to cook the raw data.
# We start with 752 tables, one per prefecture per variable group, and turn them into one
# tibble with data on all of the variables across all of the shuraku.

library(tidyverse); library(readxl)

# Create a vector [01, 02, ... 47] as suffixes to the data table files for the 47 prefectures
prefs <- as.character(seq(47))
for (i in seq(9)) {
  prefs[i] <- paste0(0, prefs[i])
}



#--------------------------EXTRACTING FROM TABLES-------------------------------#
# The following code is organized by variable group (AAXXXX, where AA is the survey that 
# the data come from and XXXX is a 4-digit code identifying the table). Each chunk of code
# defines a function to read tables of that variable group and then apply that function to 
# all 47 tables from that variable group and put it together into one tibble per variable 
# group. I've annotated the code for variable group SA0001 as an aid. More functions should
# be developed if other variable groups are found to be of interest.



#-----------------------------Variable group SA0001----------------------------------#
### We start with 47 tables for the data on this variable group for each prefecture.

# Function to read each individual table with SA0001 data
read_SA0001 <- function(filepath = 'raw_data/ag_data/SA0001_2020_2020_01.xlsx') {
  # Read the excel worksheet
  read_xlsx(filepath) %>%
    # Translate the Japanese
    rename(ag_businesses = 農業経営体,
           forestry_businesses = 林業経営体) %>% 
    # The hyphens ("-") represent zero values, replace them so the columns can be numeric
    mutate(ag_businesses = as.numeric(ifelse(ag_businesses == '-', 0, ag_businesses)),
           forestry_businesses = as.numeric(ifelse(forestry_businesses == '-', 0, forestry_businesses))) %>% 
    # Keep the key (AABBBCCCDD, AA = prefecture, BBB = municipality, CCC = city, DD = shuraku), and the variables
    select(key, ag_businesses, forestry_businesses)
}

# Instantiate a vector, "tables" to hold all 47 prefectures' data, store what the filepath to this variable group is.
tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA0001_2020_2020_'

# Iterate through all 47 prefectures, grabbing each table and applying the read_SA0001 function, then putting them in the tables vector
for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA0001(filepath)
  tables[[i]] <- table
}

# Bind all the individual tables in the tables vector into a final tibble for variable group SA0001
SA0001 <- bind_rows(tables)

### Cool! Now we have 1 tibble with all the interesting variables across all the prefectures from variable group SA0001.

#-----------------------------Variable group SA5194----------------------------------#
read_SA5194 <- function(filepath = 'raw_data/ag_data/SA5200_2015_2015_01.xlsx') {
  read_xlsx(filepath, na = 'x') %>%
    mutate('key' = KEY) %>% 
    rename('total_num_farmers' = 総農家,
           'total_num_nonfarmers' = 土地持ち非農家,
    ) %>% 
    select(key, total_num_farmers, total_num_nonfarmers) %>% 
    mutate(total_num_farmers = parse_number(ifelse(total_num_farmers == '-', 0, total_num_farmers)),
           total_num_nonfarmers = parse_number(ifelse(total_num_nonfarmers == '-', 0, total_num_nonfarmers))) %>% 
    return()
}


tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA5194_2015_2015_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA5194(filepath)
  tables[[i]] <- table
}

SA5194 <- bind_rows(tables)



#-----------------------------Variable group SA5200----------------------------------#
read_SA5200 <- function(filepath = 'raw_data/ag_data/SA5200_2015_2015_01.xlsx') {
  read_xlsx(filepath, na = 'x') %>%
    mutate('key' = KEY) %>% 
    rename('num_farmers_cultivated' = 総農家_経営耕地のある農家数,
           'area_farmers_cultivated' = 総農家_経営耕地面積,
    ) %>% 
    select(key, num_farmers_cultivated, area_farmers_cultivated) %>% 
    mutate(num_farmers_cultivated = parse_number(ifelse(num_farmers_cultivated == '-', 0, num_farmers_cultivated)),
           area_farmers_cultivated = parse_number(ifelse(area_farmers_cultivated == '-', 0, area_farmers_cultivated))) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA5200_2015_2015_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA5200(filepath)
  tables[[i]] <- table
}

SA5200 <- bind_rows(tables)



#-----------------------------Variable group SA5201----------------------------------#
read_SA5201 <- function(filepath = 'raw_data/ag_data/SA5201_2015_2015_01.xlsx') {
  read_xlsx(filepath, na = 'x') %>%
    mutate('key' = KEY) %>% 
    rename('num_farmers_abandoned' = 総農家_耕作放棄地のある農家数,
           'area_farmers_abandoned' = 総農家_耕作放棄地面積,
           'num_nonfarmers_abandoned' = 土地持ち非農家_耕作放棄地のある世帯数,
           'area_nonfarmers_abandoned' = 土地持ち非農家_耕作放棄地面積) %>% 
    select(key, num_farmers_abandoned, area_farmers_abandoned, num_nonfarmers_abandoned, area_nonfarmers_abandoned) %>% 
    mutate(num_farmers_abandoned = parse_number(ifelse(num_farmers_abandoned == '-', 0, num_farmers_abandoned)),
           area_farmers_abandoned = parse_number(ifelse(area_farmers_abandoned == '-', 0, area_farmers_abandoned)),
           num_nonfarmers_abandoned = parse_number(ifelse(num_nonfarmers_abandoned == '-', 0, num_nonfarmers_abandoned)),
           area_nonfarmers_abandoned = parse_number(ifelse(area_nonfarmers_abandoned == '-', 0, area_nonfarmers_abandoned))) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA5201_2015_2015_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA5201(filepath)
  tables[[i]] <- table
}

SA5201 <- bind_rows(tables)



#-----------------------------Variable group SA7001----------------------------------#
read_SA7001 <- function(filepath = 'raw_data/ag_data/SA7001_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('total_did' = 計,
           'did_lessthanfifteen' = `15分未満`,
           'did_fifteen_to_thirty' = `15分～30分`,
           'did_thirty_to_sixty' = `30分～1時間`,
           'did_sixty_to_ninety' = `1時間～1時間半`,
           'did_morethanninety' = `1時間半以上`,
           'did_inaccessible' = 計測不能) %>% 
    select(key, total_did, did_lessthanfifteen, did_fifteen_to_thirty, did_thirty_to_sixty, did_sixty_to_ninety, did_morethanninety, did_inaccessible) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7001_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7001(filepath)
  tables[[i]] <- table
}

SA7001 <- bind_rows(tables)



#-----------------------------Variable group SA7002----------------------------------#
read_SA7002 <- function(filepath = 'raw_data/ag_data/SA7002_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('total_surveyed_ag_area' = `耕地面積`,
           'total_surveyed_paddy_area' = `田面積`) %>% 
    select(key, total_surveyed_ag_area, total_surveyed_paddy_area) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7002_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7002(filepath)
  tables[[i]] <- table
}

SA7002 <- bind_rows(tables)



#-----------------------------Variable group SA7003----------------------------------#
read_SA7003 <- function(filepath = 'raw_data/ag_data/SA7003_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('meetings_ag' = `寄り合いの議題_農業生産にかかる事項`,
           'meetings_irrig' = `寄り合いの議題_農道・農業用用排水路・ため池の管理`,
           'meetings_facilities' = `寄り合いの議題_集落共有財産・共用施設の管理`,
           'meetings_environmental' = `寄り合いの議題_環境美化・自然環境の保全`,
           'meetings_events' = `寄り合いの議題_農業集落行事（祭り・イベント等）の計画・推進`,
           'meetings_welfare' = `寄り合いの議題_農業集落内の福祉・厚生`,
           'no_meetings' = `寄り合いを開催しなかった`) %>% 
    select(key, meetings_ag, meetings_irrig, meetings_facilities, meetings_environmental, meetings_events, meetings_welfare, no_meetings) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7003_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7003(filepath)
  tables[[i]] <- table
}

SA7003 <- bind_rows(tables)



#-----------------------------Variable group SA7006----------------------------------#
read_SA7006 <- function(filepath = 'raw_data/ag_data/SA7006_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('mtn_program_area' = `振興山村地域`,
           'remote_island_program_area' = `離島振興対策実施地域`,
           'special_ag_mtn_villages' = `特定農山村地域`,
           'depopulated_area' = `過疎地域`,
           'specially_approved_area' = `特認地域`) %>% 
    select(key, mtn_program_area, remote_island_program_area, special_ag_mtn_villages, depopulated_area, specially_approved_area) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7006_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7006(filepath)
  tables[[i]] <- table
}

SA7006 <- bind_rows(tables)



#-----------------------------Variable group SA7007----------------------------------#
read_SA7007 <- function(filepath = 'raw_data/ag_data/SA7007_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('yes_enviro_activities' = `いずれかの活動を行っている`,
           'conservation_activities' = `環境美化・自然環境の保全`,
           'event_activities' = `農業集落行事（祭り・イベントなど）の実施`,
           'welfare_activities' = `農業集落内の福祉・厚生`,
           'settlement_activities' = `定住を推進する取組`,
           'ecotourism_activities' = `グリーン・ツーリズムの取組`,
           'sixthindustrial_activities' = `６次産業化への取組`,
           'renewable_activities' = `再生可能エネルギーへの取組`,
           'no_enviro_activities' = `いずれの活動も行っていない`) %>% 
    select(key, yes_enviro_activities, conservation_activities, event_activities, welfare_activities, settlement_activities, ecotourism_activities, sixthindustrial_activities, renewable_activities, no_enviro_activities) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7007_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7007(filepath)
  tables[[i]] <- table
}

SA7007 <- bind_rows(tables)



#-----------------------------Variable group SA7008----------------------------------#
read_SA7008 <- function(filepath = 'raw_data/ag_data/SA7008_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('with_urban_all_activities' = `いずれかの活動で都市住民との交流を行っている`,
           'with_orgs_all_activities' = `いずれかの活動でNPO・学校・企業との連携を行っている`) %>% 
    select(key, with_urban_all_activities, with_orgs_all_activities) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7008_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7008(filepath)
  tables[[i]] <- table
}

SA7008 <- bind_rows(tables)



#-----------------------------Variable group SA7009----------------------------------#
read_SA7009 <- function(filepath = 'raw_data/ag_data/SA7009_2020_2020_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('with_urban_conservation' = `いずれかの地域資源を都市住民と連携して保全している`,
           'with_orgs_conservation' = `いずれかの地域資源をＮＰＯ・学校・企業と連携して保全している`) %>% 
    select(key, with_urban_conservation, with_orgs_conservation) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SA7009_2020_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SA7009(filepath)
  tables[[i]] <- table
}

SA7009 <- bind_rows(tables)



#-----------------------------Variable group SB0001----------------------------------#
read_SB0001 <- function(filepath = 'raw_data/ag_data/SB0001_2015_2020_01.xlsx') {
  read_xlsx(filepath) %>%
    rename('total_num_households' = 一般世帯総数,
           'num_under6_families' = `6歳未満世帯員のいる一般世帯総数`,
           'num_over65_families' = `65歳未満世帯員のいる一般世帯総数`) %>% 
    mutate(prop_under6_families = num_under6_families / total_num_households, 
           prop_over65_families = num_over65_families / total_num_households) %>% 
    select(key, total_num_households, prop_under6_families, prop_over65_families) %>%
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SB0001_2015_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SB0001(filepath)
  tables[[i]] <- table
}

SB0001 <- bind_rows(tables)



#-----------------------------Variable group SB0002----------------------------------#
read_SB0002 <- function(filepath = 'raw_data/ag_data/SB0002_2015_2020_01.xlsx') {
  read_xlsx(filepath) %>%
    rename('data_year' = データ年次,
           'total_population' = `総数、年齢「不詳」含む`,
           'pop_0_4' = `総数0～ 4歳`,
           'pop_5_9' = `総数5～ 9歳`,
           'pop_10_14' = `総数10～14歳`,
           'pop_15_19' = `総数15～19歳`,
           'pop_20_24' = `総数20～24歳`,
           'pop_25_29' = `総数25～29歳`,
           'pop_30_34' = `総数30～34歳`,
           'pop_35_39' = `総数35～39歳`,
           'pop_40_44' = `総数40～44歳`,
           'pop_45_49' = `総数45～49歳`,
           'pop_50_54' = `総数50～54歳`,
           'pop_55_59' = `総数55～59歳`,
           'pop_60_64' = `総数60～64歳`,
           'pop_65_69' = `総数65～69歳`,
           'pop_70_74' = `総数70～74歳`,
           'pop_75_over' = `総数75歳以上`,
           'pop_age_unknown' = `総人口不詳`,
           'total_male_population' = `男の総数`,
           'male_pop_0_4' = `男0～ 4歳`,
           'male_pop_5_9' = `男5～ 9歳`,
           'male_pop_10_14' = `男10～14歳`,
           'male_pop_15_19' = `男15～19歳`,
           'male_pop_20_24' = `男20～24歳`,
           'male_pop_25_29' = `男25～29歳`,
           'male_pop_30_34' = `男30～34歳`,
           'male_pop_35_39' = `男35～39歳`,
           'male_pop_40_44' = `男40～44歳`,
           'male_pop_45_49' = `男45～49歳`,
           'male_pop_50_54' = `男50～54歳`,
           'male_pop_55_59' = `男55～59歳`,
           'male_pop_60_64' = `男60～64歳`,
           'male_pop_65_69' = `男65～69歳`,
           'male_pop_70_74' = `男70～74歳`,
           'male_pop_75_over' = `男75歳以上`,
           'male_pop_age_unknown' = `男人口不詳`,
           'total_female_population' = `女の総数`,
           'female_pop_0_4' = `女0～ 4歳`,
           'female_pop_5_9' = `女5～ 9歳`,
           'female_pop_10_14' = `女10～14歳`,
           'female_pop_15_19' = `女15～19歳`,
           'female_pop_20_24' = `女20～24歳`,
           'female_pop_25_29' = `女25～29歳`,
           'female_pop_30_34' = `女30～34歳`,
           'female_pop_35_39' = `女35～39歳`,
           'female_pop_40_44' = `女40～44歳`,
           'female_pop_45_49' = `女45～49歳`,
           'female_pop_50_54' = `女50～54歳`,
           'female_pop_55_59' = `女55～59歳`,
           'female_pop_60_64' = `女60～64歳`,
           'female_pop_65_69' = `女65～69歳`,
           'female_pop_70_74' = `女70～74歳`,
           'female_pop_75_over' = `女75歳以上`,
           'female_pop_age_unknown' = `女人口不詳`) %>% 
    mutate(avg_age = (pop_0_4 * 2.5 +
                        pop_5_9 * 7.5 + 
                        pop_10_14 * 12.5 +
                        pop_15_19 * 17.5 +
                        pop_20_24 * 22.5 +
                        pop_25_29 * 27.5 +
                        pop_30_34 * 32.5 +
                        pop_35_39 * 37.5 +
                        pop_40_44 * 42.5 +
                        pop_45_49 * 47.5 +
                        pop_50_54 * 52.5 +
                        pop_55_59 * 57.5 +
                        pop_60_64 * 62.5 +
                        pop_65_69 * 67.5 +
                        pop_70_74 * 72.5 +
                        pop_75_over * 77.5) / total_population,
           avg_age = ifelse(is.nan(avg_age), 0, avg_age),
           prop_elderly = (pop_60_64 * 62.5 + pop_65_69 * 67.5 + pop_70_74 * 72.5 + pop_75_over * 77.5) / total_population,
           prop_elderly = ifelse(is.nan(prop_elderly), 0, prop_elderly)) %>% 
    select(key, total_population, avg_age, prop_elderly) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/SB0002_2015_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_SB0002(filepath)
  tables[[i]] <- table
}

SB0002 <- bind_rows(tables)



#-----------------------------Variable group GC0001----------------------------------#
read_GC0001 <- function(filepath = 'raw_data/ag_data/GC0001_2019_2020_01.xlsx') {
  read_xlsx(filepath) %>%
    rename('num_orgs_mfngrants' = 組織数) %>% 
    select(key, num_orgs_mfngrants) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/GC0001_2019_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_GC0001(filepath)
  tables[[i]] <- table
}

GC0001 <- bind_rows(tables)



#-----------------------------Variable group GD0001----------------------------------#
read_GD0001 <- function(filepath = 'raw_data/ag_data/GD0001_2019_2020_01.xlsx') {
  read_xlsx(filepath) %>%
    rename('is_directpayments' = 対象農用地の有無) %>% 
    select(key, is_directpayments) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/GD0001_2019_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_GD0001(filepath)
  tables[[i]] <- table
}

GD0001 <- bind_rows(tables)



#-----------------------------Variable group KA0001----------------------------------#
read_KA0001 <- function(filepath = 'raw_data/ag_data/KA0001_2016_2015_01.xlsx') {
  read_xlsx(filepath, na = '…') %>%
    rename('municipality' = city_name,
           'shuraku_name' = rcom_name,
           'key' = key,
           'avg_altitude' = 中心地平均標高,
           'avg_slope' = 中心地平均傾斜度,
           'total_land_area' = 総土地面積,
           'total_forest_area' = 森林地域_森林地域,
           'total_ag_area' = 農業地域_農業地域,
           'total_urban_area' = 都市地域_都市地域) %>% 
    select(key, shuraku_name, municipality, avg_altitude, avg_slope, total_land_area, total_forest_area, total_ag_area, total_urban_area) %>%
    mutate(avg_altitude = parse_double(avg_altitude),
           avg_slope = parse_double(avg_slope),
           total_land_area = parse_double(total_land_area),
           total_forest_area = parse_double(total_forest_area),
           total_ag_area = parse_double(total_ag_area),
           total_urban_area = parse_double(total_urban_area)) %>% 
    return()
}

tables <- vector('list', length = length(prefs))
stem <- 'raw_data/ag_data/KA0001_2021_2020_'

for (i in seq(length(prefs))) {
  filepath <- paste0(stem, prefs[i], '.xlsx')
  table <- read_KA0001(filepath)
  tables[[i]] <- table
}

KA0001 <- bind_rows(tables)



#-----------------------------------BUILD AG DATASET------------------------------#
### We have 16 tibbles representing 16 variables groups. We want to stitch these together into a final dataset.

## STITCH TOGETHER 
# Start with a list of all 16
var_groups <- list(SA0001, SA5194, SA5200, SA5201, SA7001, SA7002, SA7003, SA7006, SA7007, SA7008, SA7009, SB0001, SB0002, GC0001, GD0001, KA0001)

# We also will need a tibble of all the shuraku on the map, so that we can exclude the summary rows. Grabbing all
# the rows from the MAFF data will include summary rows, bringing the total to ~160,000; however, there are only about
# 154,637 shuraku on the map, so we need to exclude those summary rows.
all_japan_shuraku <- read_csv('gis_imports/all_japan_shuraku.csv', col_types = 'dc') %>% 
  select(KEY) %>% 
  rename(key = KEY)

# Join the variable group tibbles together
ag_dataset <- reduce(var_groups, full_join) %>%
  # Filter out all the rows that don't correspond to a shuraku on the map (hint- all shuraku whose keys end in
  # 000 or similar represent summaries, not actual shuraku)
  right_join(all_japan_shuraku) %>% 
  # Fix up the municipality and shuraku names for each shuraku
  mutate(municipality = ifelse(is.na(municipality), 0, municipality)) %>% 
  mutate(shuraku_name = ifelse(is.na(shuraku_name), 0, shuraku_name)) %>% 
  relocate(municipality, .after = total_urban_area) %>% 
  relocate(shuraku_name, .after = municipality)

  

# Imput 0s whereever there is missing data- NAs almost always mean data censored because it was too close to 0, this is reasonable
ag_dataset[is.na(ag_dataset)] <- 0




## UNITED NATIONS UNIVERSITY SATOYAMAS (UNU- "NEW SATOYAMAS")

# Grab the new satoyamas Excel sheet
new_satoyamas <- read_xlsx('gis_imports/pauls_satoyamas.xlsx') %>% 
  # Get the important parts and translate them
  rename(municipality = `市区町村`,
         satoyama_name = `名称`,
         satoyama_criterion_1 = 基準１,
         satoyama_criterion_2 = 基準２,
         satoyama_criterion_3 = 基準３) %>% 
  select(municipality, satoyama_name, satoyama_criterion_1, satoyama_criterion_2, satoyama_criterion_3) %>% 
  # Add in numeric columns for the satoyama criteria
  mutate(satoyama_criterion_1 = ifelse(satoyama_criterion_1 == '○', 1, 0),
         satoyama_criterion_2 = ifelse(satoyama_criterion_2 == '○', 1, 0),
         satoyama_criterion_3 = ifelse(satoyama_criterion_3 == '○', 1, 0)) %>% 
  # Make the dataset have one row for each distinct municipality
  mutate(municipality = str_split(municipality, '、')) %>% 
  unnest(municipality) %>% 
  distinct(municipality, .keep_all = TRUE)

# Build a vector with all the individual UNU satoyama names
satoyamas_vec <- new_satoyamas %>% 
  select(satoyama_name) %>%
    # Manipulate the satoyama strings to get out the specific names
    mutate(satoyama_name = str_remove_all(satoyama_name, '地区'),
           satoyama_name = str_squish(satoyama_name),
           satoyama_name = str_split(satoyama_name, '（|・|）|、|「|」')) %>% 
  unchop(satoyama_name) %>% 
  filter(satoyama_name != '') %>% 
  .$satoyama_name

# FINAL PRODUCT- a tibble with one row for each distinct municipality with a UNU satoyama; a vector of names of these distinct satoyama


## CASE STUDIES (I kept the name cases2 so we remember that these are the cases from the second round, 3 for each of 3 classification categories)

# Grab all the cases from the csv
cases2 <- read_csv('gis_imports/cases2.csv', col_types = 'cccccccccc') %>%
  # Excel messes up the leading 0s for prefectures that only have one digit (01 = hokkaido, etc)
  mutate(tadami_key = ifelse(nchar(tadami_key) == 9, paste0('0', tadami_key), tadami_key), 
         iide_key = ifelse(nchar(iide_key) == 9, paste0('0', iide_key), iide_key))


## MUTATE NEW VARIABLES

ag_dataset <- ag_dataset %>% 
  # Calculate proportions of area, other interesting things
  mutate(prop_ag_area = total_ag_area / total_land_area,
         prop_ag_area = ifelse(is.nan(prop_ag_area), 0, prop_ag_area),
         prop_forest_area = total_forest_area / total_land_area,
         prop_forest_area = ifelse(is.nan(prop_forest_area), 0, prop_forest_area),
         prop_paddy_area = total_surveyed_paddy_area / total_land_area,
         prop_paddy_area = ifelse(is.nan(prop_paddy_area), 0, prop_paddy_area),
         prop_area_abandoned = area_farmers_abandoned / area_farmers_cultivated,
         prop_farmers_abandoned = num_farmers_abandoned / num_farmers_cultivated,
         pop_density = total_population / total_land_area,
         pop_density = ifelse(is.infinite(pop_density), 0, pop_density),
         prop_over65_families = ifelse(is.infinite(prop_over65_families), 0, prop_over65_families),
         time_to_did = ifelse(did_lessthanfifteen == 1, 7.5, 
                              ifelse(did_fifteen_to_thirty == 1, 17.5, 
                                     ifelse(did_thirty_to_sixty == 1, 45, 
                                            ifelse(did_sixty_to_ninety == 1, 75, 
                                                   ifelse(did_morethanninety == 1, 105, 0))))))

# one more imputation by 0
ag_dataset[is.na(ag_dataset)] <- 0

# CASES
ag_dataset <- ag_dataset %>% 
  # The case2 variable represents which of our cases each shuraku is in. 
  mutate(case2 = ifelse(key %in% cases2$saitama_key, 'Saitama', 
                        ifelse(key %in% cases2$hamamatsu_key, 'Hamamatsu', 
                               ifelse(key %in% cases2$fukuoka_key, 'Fukuoka',
                                      ifelse(key %in% cases2$tadami_key, 'Tadami',
                                             ifelse(key %in% cases2$iide_key, 'Iide',
                                                    ifelse(key %in% cases2$ishikawa_key, 'Ishikawa', 
                                                           ifelse(key %in% cases2$noto_key, 'Noto',
                                                                  ifelse(key %in% cases2$nishi_awa_key, 'Nishi-Awa',
                                                                         #If it's not in any of the cases, cases2 = 'Japan'
                                                                         ifelse(key %in% cases2$minabe_tanabe_key, 'Minabe-Tanabe', 'Japan')))))))))) %>% 
  # I kept the name is_satoyama2 to remember that we're in the second iteration, where we think of 
  # satoyama as either yes, 'satoyama', or no, ('urban' or 'okuyama')
  mutate(is_satoyama2 = ifelse(case2 %in% c('Saitama', 'Hamamatsu', 'Fukuoka'), 'urban',
                               ifelse(case2 %in% c('Noto', 'Nishi-Awa', 'Minabe-Tanabe'), 'satoyama',
                                      ifelse(case2 %in% c('Tadami', 'Iide', 'Ishikawa'), 'okuyama', 'unknown')))) %>% 
  # Add the new_satoyamas (UNU) information
  left_join(new_satoyamas) %>% 
  # is_new_satoyama represents whether an area is a UNU satoyama, the new definition we introduced
  mutate(is_new_satoyama = ifelse((shuraku_name %in% satoyamas_vec)&!is.na(satoyama_name), 1, 0)) %>%
  # move a few variables to the final columns for tidiness
  relocate(c(satoyama_name:is_new_satoyama, municipality, shuraku_name), .after = time_to_did) %>% 
  # get rid of the variables that we're no longer interested in
  select(-c(total_num_farmers:area_nonfarmers_abandoned, total_surveyed_ag_area:total_surveyed_paddy_area, total_num_households, total_population, total_land_area:total_urban_area))

## PLACES THAT GET IDENTIFIED AS UNU (new_satoyama) CASES: if the shuraku name matches a fragment of the satoyama name from the list of UNU satoyama names AND the municipality matches one of the municipalities known to contain UNU satoyama
# WRITE THE FINAL DATASET !!!
ag_dataset %>% 
  write_csv('final_dataset.csv')