# CIR-PrU-22-23

Repository for St. Olaf CIR Project for 2022-23 academic year titled "Mapping *satoyama* - where are they and what do they look like?"

[Link to final maps of our model](https://stolaf.maps.arcgis.com/apps/dashboards/9325753e4b3847618cb7aa6c374c89dd)

[Link to Will's github](https://github.com/asingerwill/mappingsatoyama) (for additional reference)

## Directory Information

### `lasso_models`

This folder holds various scripts that were used to **build our lasso and multinomial models**. In the `full_model.R` file, the lasso models were built to select variables. The selected variables were then used to build a multinomial logistic regression model, which predicted land type for each shuraku.

### `envt_footprint`

This folder contains two scripts and some datasets that relate to our work on the environmental footprint data, which was at the prefectural level. `footprint_googlesheets.R` loads in the data from a Google Sheet located in the shared Google Drive, and `footprint_satoyama_analysis` creates some plots by aggregating the model predictions into prefectures and analyzing the relationship between land types in a prefecture and that prefecture's ecological footprint.

### `final_dataset.R` and `final_model.R`

In `final_dataset.R`, Will builds the comprehensive dataset using all the variables from the Ag census and other data sources that he colleceted during Summer 2022. This final dataset is housed in `final_dataset.csv`

In `final_model.R`, Will builds the multinomial logistic regression using stepwise variable selection.

### `si_analysis`

This folder contains some code used to generate analysis based on the SI mapping and zonal analysis we did in GIS. Results can be found in Google Slides --\> "PrU w/CIR 2022-23/A-M-R Materials/Comparison of Means Analysis: Plots and Statistics"

## Data + Other Work Not Included Here

#### The following folders can be found in the R project shared with Paul on the St. Olaf R Server. 

### `model_preds`

This folder contains various different `.csv` files that hold different model predictions. The most notable of these are the `lasso_pair_prob_layers.csv` and `will_final_preds.csv`. The former holds our final model predictions, as well as the various probabilities assigned to each land type and whether they were solely one land type such as *satoyama* or were a mix of land types such as *satoyama_urban*. The latter holds Will's model's final predictions.

### `si_data`

This folder primarily contains the resulting `.csv` from our SI and DSI zonal analysis in GIS. It also contains two files that relate to the low and high SI outliers.

### `raw_data`

This folder houses data that Will used to compile all the AG census data. It includes `ag_data` (agricultural census), `pop_data` (population data), and `si_data` (SI data used in GIS).

### `gis_imports`

This folder includes some files that were used by Will when first developing *shuraku* maps in GIS.

From Will: "Contains csvs: all the 154,637 shuraku that we’ve mapped across Japan, the cases that we chose for our second round of analysis, and the UNU satoyamas as cleaned by Paul."
