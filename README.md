# Wenatchee case study: stream temperature and Chinook pre-spawn mortality

### Aimee H Fullerton, Arianna Goodman, and Tracy Bowerman

aimee.fullerton\@noaa.gov

This repository includes scripts used to make daily predictions of past stream temperatures and to compute future changes in temperature for the Wenatchee River (Columbia) using methods from Siegel et al. 2023, PLoS Water 2(8) 30000119 and data from Fullerton et al. in prep, and to compute predicted pre-spawn mortality for Chinook salmon. Below are listed each script and the associated the data files called by the script, followed by the data's source (listed only on its the first appearance). Data necessary for running scripts are available and described at URL.

### 1.1.prepare_fitting_dataset.R

NorWeST_obs.csv : Siegel et al. (2023)

COMID_to_HUC12.csv : Siegel et al. (2023)

spatial_data.csv : Siegel et al. (2023)

PNW_covariates/huc_huc10.fst : Siegel et al. (2023), Riverscapes Exchange <https://data.riverscapes.net/pt/streamtemp>

WenTemps3Jun25.csv : T. Bowerman, pers comm

### 1.2.antecedent_airT.R

fitting_data.fst : Siegel et al. (2023)

### 1.3.prepare_covariates.R

antec_air_temp_duration_models.RData : Produced in step 1.2

### 1.4.fit_model.R

freeflow_data.fst : Produced in step 1.2

### 1.5.make_predictions.R

covariates_mean_Wenatchee.fst : Produced in step 1.3

fitted_model_mean.RData : Produced in step 1.4

### 2.1_deltaST.R

st_pred_UC.csv : Produced in step 1.5

cc_preds_1702001101.csv, cc_preds_1702001102.csv, etc. : PNW predictions, this study

### 2.2_Mean2Max.R

Wenatchee_spawning_reaches.csv : WDFW

ST_delta_17020011.csv : Produced in step 2.1

### 2.3a_7DADM.R

ST_max_17020011.csv : Produced in step 2.2

### 2.3b_AugMn.R

ST_max_17020011.csv : Produced in step 2.2, uses 'mean'

### 2.4_map_metrics.R

shp/Wenatchee_boundary.shp : NHD v2 Watershed Boundary Dataset

shp/upper_columbia_streams_nhd2.shp : Subset of NHD v2 Hydrography

shp/Wenatchee_NHDv2_SOgt2.shp : Subset of NHD v2 Hydrography

7DADM_17020011.csv : Produced in step 2.3a

AugMn_17020011.csv : Produced in step 2.3b

### 3.1_CalcPSM.R

UCpublishedCOEF.csv : Bowerman et al. 2021 <https://doi.org/10.1016/j.fishres.2021.105874>

UCPublished_VCOV.csv : Bowerman et al. 2021 <https://doi.org/10.1016/j.fishres.2021.105874>

prd.AugMn_17020011.csv : Produced in step 2.3b

### 3.2_map_PSM.R

UC_PSM_17020011_AugMn.csv : Produced in step 3.1

# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
