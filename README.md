# S2-PADDY-MAPPING

# Mapping of rice condition on dry season using predictive analysis

This is the steps to recreate the mapping. Please change the path of folder accordingly. 

Folder ML-S2_S1v1/ML_LS8_S1_LEE_S2_2019_5class_surveyV2 the results of the classification model

Folder ML-S2_S1v1/CLASSIFY-MASK-PADDY-CLEAR contains the final classification images of the classification results from the model

I. Download Data Sentinel-1
1. Install Python and GEE login. Please refer to https://developers.google.com/earth-engine/python_install

2. Download S1 VH with LEE filtering data using dl_S1_VH_DESC_LEE_7regency_full1.py

II. Classification process
A. Cliping the S2-Bands with A-S2_L2A_BAND_PR_ARCPY_PADDY1.R and A-S1_LEE_split_paddy_VH1 for Sentinel-1 VH band (with ArcGIS installed. adjust the folder of ArcPy)

B. Building the model
1. Labelling on points from field survey just for five rice condition: 1. Bare land, 2. Flooding, 3. Vegetative, 4. Reproductive, 5. Ripening.

2. Only data with pixel quality (2,4,5,6,7) is processed

3. Building the model using SVM Radial and tuning it using B-ML_LS8_S1_LEE_S2_2019_5class_surveyV2.r

5. Change RDS path file to best model on C-step1-2019_classify_S2_V4.R

C. Running the model
1. Run C-step1-2019_classify_S2_V4.R for classifying

2. Run C-step2-2019_merge_S2v2.R for merging into one image

3. Run C-step3-2019_CLEAR_ALLv4.R for merging with Sentinel-1 VH to detect rice period 

D. Temporal changes

3. Run D-step1-2019_CHANGE_ALLv1 for detecting change classes with 5,10,15,20,25,30 lag days

4. Run D-step2-Recap-Change_V4.R for recaping the change detection data

