#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 14 22:17:22 2021

@author: lucasliu
"""
from random import sample
import random
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
import shap
from sklearn import metrics

def plot_shap_summary(explainer, X, output_dir, outfile):
    shap_values_all = explainer.shap_values(X) #compute shap values for all X
    f = plt.figure()
    shap.summary_plot(shap_values_all, X)
    f.savefig(output_dir + outfile, bbox_inches='tight', dpi=600)

def plot_individual_shap(explainer, Sample_X,value_threshold, output_dir, outfile):
    shap_value = explainer.shap_values(Sample_X)
    #Using logit will change log-odds numbers into probabilities, the defualt shows the the log-odds
    shap.force_plot(explainer.expected_value, shap_value, Sample_X,contribution_threshold= value_threshold,show = False,matplotlib=True).savefig(output_dir + outfile,bbox_inches='tight',dpi = 500)


data_dir = "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"

#######################################################################################
# Load data
#######################################################################################
#use not normed feature for the plot
feature_df = pd.read_csv(data_dir + "All_Feature_imputed.csv", index_col="STUDY_PATIENT_ID") # Load the data
outcome_df = pd.read_csv(data_dir + "All_outcome.csv", index_col="STUDY_PATIENT_ID")
outcome_df = outcome_df.reindex(feature_df.index) #reorder to match ID

##########################################################
#Mortality
##########################################################
comb_df = feature_df.join(outcome_df['Death_inHOSP'])
selected_features = ["UrineOutput_D0toD3" , "Vasopressor_ICUD0toD3","FI02_D1_HIGH","Platelets_D1_LOW","AGE",
                     "BUN_D0toD3_HIGH","HR_D1_HIGH","LAST_KDIGO_ICU_D0toD3","PH_D1_LOW","Bilirubin_D1_HIGH",
                     "MAX_KDIGO_ICU_D0toD3","ECMO_ICUD0toD3","Hours_inICUD0toD3", 
                     "Temperature_D1_LOW", "Temperature_D1_HIGH"]
Y =  comb_df['Death_inHOSP']
X =  comb_df[selected_features]


#Use All data to train and get plot
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(X, Y)

# Create object that can calculate shap values
explainer = shap.TreeExplainer(model) #
    

#Output dir
outdir = "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0708/CV_performance/mortality/Visual_Shap/"

# Plot shap summary 
plot_shap_summary(explainer, X, outdir, "RF_ClinicalF2_AllUK_SHAP_HospMortality.png")
explainer.expected_value #This is the base value = Y.mean(), if we know nothing about this instance, the prediction is this value

#check predicted value
y_pred = model.predict(X)

#Find which y_pred > 0.5 and y_ture == 1
predicted_death_indexes = [i for i, value in enumerate(y_pred) if value > 0.5]
true_death_indexes = [i for i, value in enumerate(Y) if value == 1]
predicted_correct_death_index = [value for value in predicted_death_indexes if value in true_death_indexes] #intersection of two list
predicted_correct_death_IDs = list(X.index[predicted_correct_death_index])

#Find which y_pred < 0.5 and y_ture == 0
predicted_survive_indexes = [i for i, value in enumerate(y_pred) if value < 0.5]
true_survive_indexes = [i for i, value in enumerate(Y) if value == 0]
predicted_correct_survive_index = [value for value in predicted_survive_indexes if value in true_survive_indexes] #intersection of two list
predicted_correct_survive_IDs = list(X.index[predicted_correct_survive_index])

# plot Indiviaual
random.seed(0)
n = 10
Survivors_IDs = sample(predicted_correct_survive_IDs,n)
Death_IDs = sample(predicted_correct_death_IDs,n)
Sample_IDs = Survivors_IDs + Death_IDs

for pt in Sample_IDs:
    sample_data = np.around(X.loc[pt],2) #round feature to 2 digit
    outcome_label = Y.loc[pt] 
    plot_individual_shap(explainer, sample_data,0.1,outdir,  "RF_Mortality" + str(outcome_label) + "_ID" + str(pt) + ".png")
 
##########################################################
#MAKE
##########################################################
comb_df = feature_df.join(outcome_df['MAKE_HOSP120_Drop50'])
selected_features = ["LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                     "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                     "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW",
                     "Hours_inICUD0toD3", "Temperature_D1_LOW", "Temperature_D1_HIGH"]

Y =  comb_df['MAKE_HOSP120_Drop50']
X =  comb_df[selected_features]

#Use All data to train and get plot
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(X, Y)

# Create object that can calculate shap values
explainer = shap.TreeExplainer(model) #
    

#Output dir
outdir = "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0708/CV_performance/make120_drop50/Visual_Shap/"

# Plot shap summary 
plot_shap_summary(explainer, X, outdir, "RF_ClinicalF2_AllUK_SHAP_MAKE50.png")
explainer.expected_value #This is the base value = Y.mean(), if we know nothing about this instance, the prediction is this value

#check predicted value
y_pred = model.predict(X)

#Find which y_pred > 0.5 and y_ture == 1
predicted_make_indexes = [i for i, value in enumerate(y_pred) if value > 0.5]
true_make_indexes = [i for i, value in enumerate(Y) if value == 1]
predicted_correct_make_index = [value for value in predicted_make_indexes if value in true_make_indexes] #intersection of two list
predicted_correct_make_IDs = list(X.index[predicted_correct_make_index])

#Find which y_pred < 0.5 and y_ture == 0
predicted_survive_indexes = [i for i, value in enumerate(y_pred) if value < 0.5]
true_survive_indexes = [i for i, value in enumerate(Y) if value == 0]
predicted_correct_survive_index = [value for value in predicted_survive_indexes if value in true_survive_indexes] #intersection of two list
predicted_correct_survive_IDs = list(X.index[predicted_correct_survive_index])

# plot Indiviaual
random.seed(1)
n = 10
Survivors_IDs = sample(predicted_correct_survive_IDs,n)
MAKE_IDs = sample(predicted_correct_make_IDs,n)
Sample_IDs = Survivors_IDs + MAKE_IDs

for pt in Sample_IDs:
    sample_data = np.around(X.loc[pt],2) #round feature to 2 digit
    outcome_label = Y.loc[pt] 
    plot_individual_shap(explainer, sample_data, 0.1, outdir,  "RF_MAKE" + str(outcome_label) + "_ID" + str(pt) + ".png")

