# S2-PADDY-MAPPING

# Mapping of rice condition on dry season using predictive analysis
please cite this work with:
Ramadhani, F., Pullanagari, R., Kereszturi, G., & Procter, J. (2020). Automatic Mapping of Rice Growth Stages Using the Integration of SENTINEL-2, MOD13Q1, and SENTINEL-1. Remote Sensing, 12(21), 3613. https://www.mdpi.com/2072-4292/12/21/3613 

This is the steps to recreate the mapping. Please change the path of folder accordingly. 

Folder ML-S2_S1v1/ML_LS8_S1_LEE_S2_2019_5class_surveyV2 the results of the classification model

Folder ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR contains the final classification images of the classification results from the model

I. Download Data Sentinel-1
1. Install Python and GEE login. Please refer to https://developers.google.com/earth-engine/python_install

2. Download S1 VH with LEE filtering data using dl_S1_VH_DESC_LEE_7regency_full1.py

2. Download MOD13Q1 using dl_MOD13Q1_ALL_250_JAVA_v1.ipynb

II. Classification S-2 process
A. Cliping the S2-Bands with A-S2_L2A_BAND_PR_ARCPY_PADDY1.R and A-S1_LEE_split_paddy_VH1 for Sentinel-1 VH band (with ArcGIS installed. adjust the folder of ArcPy)

B. Building the model
1. Labelling on points from field survey just for five rice condition: 1. Bare land, 2. Flooding, 3. Vegetative, 4. Reproductive, 5. Ripening.

2. Only data with pixel quality (2,4,5,6,7) is processed

3. Building the model using SVM Radial and tuning it using B-ML_LS8_S1_LEE_S2_2019_5class_surveyV2.r

4. Change RDS path file to best model on C-step1-2019_classify_S2_V4.R

C. Running the model
1. Run C-step1-2019_classify_S2_V4.R for classifying

2. Run C-step2-2019_merge_S2v2.R for merging into one image

3. Run C-step3-2019_CLEAR_S2VH_ALLv1.R for filtering low VH value (-20 db) and bare land.

4. Run C-step4-2019_CLEAR_ALLv4.R for merging with Sentinel-1 VH to detect rice period 

D. Temporal changes

1. Run D-step1-2019_CHANGE_ALLv1 for detecting change classes with 5,10,15,20,25,30 lag days

2. Run D-step2-Recap-Change_V4.R for recaping the change detection data

III. Classification MOD13Q1 process
A. Pre-processing
1. Separating bands from original MOD13Q1 with A.1.MOD13Q1_separate_band.R 

2. Cliping the MOD13Q1-Bands with A.2.MOD13Q1_ARCPY_PADDY1.R and A.3.MOD13Q1_split_band_region.R (with ArcGIS installed. adjust the folder of ArcPy)

3. resampling MOD13Q1 bands into 10m A.4.MOD13Q1_resample10m_band_region.R

B. Building the model 
1. Labelling on points from field survey just for five rice condition: 1. Bare land, 2. Flooding, 3. Vegetative, 4. Reproductive, 5. Ripening.

2. Building the model using SVM Radial with dataset S1_MOD13Q1_ML_v5.xlsx and tuning it using B.S1_MOD13Q1_ML_v5.r

3. Change RDS path file to best model on C.Run-ML-MOD13Q1_S1_classify_v5_2019_32_TS.R and C.Run-ML-MOD13Q1_S1_classify_v5_2019_35_TS.R

C. Running the model

1. Run C-Run-ML-MOD13Q1_S1_classify_v5_2019.R for each district.

D. Temporal changes

1. Run D.MOD13Q1_change_v1.R for detecting change classes with 16 lag days

2. Run D.Recap-Change_MOD13Q1.R for recaping the change detection data
