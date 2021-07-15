#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 14 22:17:22 2021

@author: lucasliu
"""


import pandas as pd
import numpy as np
np.random.seed(0)
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn import preprocessing
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
import shap
from sklearn import metrics
import matplotlib.pyplot as plt


data_dir = "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"

#use not normed feature for the plot
feature_df = pd.read_csv(data_dir + "All_Feature_imputed.csv", index_col="STUDY_PATIENT_ID") # Load the data
outcome_df = pd.read_csv(data_dir + "All_outcome.csv", index_col="STUDY_PATIENT_ID")
outcome_df = outcome_df.reindex(feature_df.index) #reorder to match ID

#############################
#Mortality
#############################
comb_df = feature_df.join(outcome_df['Death_inHOSP'])
selected_features = ["UrineOutput_D0toD3" , "Vasopressor_ICUD0toD3","FI02_D1_HIGH","Platelets_D1_LOW","AGE",
                       "BUN_D0toD3_HIGH","HR_D1_HIGH","LAST_KDIGO_ICU_D0toD3","PH_D1_LOW","Bilirubin_D1_HIGH",
                       "MAX_KDIGO_ICU_D0toD3","ECMO_ICUD0toD3"]


# The target variable is 'quality'.
Y =  comb_df['Death_inHOSP']
X =  comb_df[selected_features]

# Split the data into train and test data:
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.1)
# Build the model with the random forest regression algorithm:
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(X_train, Y_train)

#Check prediction results
#y_pred_class = model.predict(X_test) #returns class if use RF classifer not the regressor
#y_pred_prob =  model.predict_proba(X_test)
#print("Accuracy:",metrics.accuracy_score(Y_test, y_pred_class))

# Create object that can calculate shap values
explainer = shap.TreeExplainer(model)

# Calculate shap_values for all of val_X rather than a single row, to have more data for plot.
shap_values_train = explainer.shap_values(X_train)
shap_values_test = explainer.shap_values(X_test)


#train
f = plt.figure()
shap.summary_plot(shap_values_train, X_train)
f.savefig("/Users/lucasliu/Desktop/RF_Train_SHAP.png", bbox_inches='tight', dpi=600)

#test
f = plt.figure()
shap.summary_plot(shap_values_test, X_test)
f.savefig("/Users/lucasliu/Desktop/RF_Test_SHAP.png", bbox_inches='tight', dpi=600)



#Use All data to train and get plot
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(X, Y)
explainer = shap.TreeExplainer(model)


f = plt.figure()
shap_values_all = explainer.shap_values(X)
shap.summary_plot(shap_values_all, X)
f.savefig("/Users/lucasliu/Desktop/RF_AllUK_SHAP_HospMortality.png", bbox_inches='tight', dpi=600)

# Indiviaual plot
Y.mean() # this is the base value: if we know nothing about this instance, the prediction is this value
y_pred = model.predict(X)
#Pred > 0.5 and true = 1
s1_ind = 231
y_pred[s1_ind] #check predicted
Y.iloc[s1_ind] #check true
S1 = np.around(X.iloc[s1_ind],3)
shap_value_sp1 = explainer.shap_values(S1)
shap.force_plot(explainer.expected_value, shap_value_sp1, S1, show = False,matplotlib=True).savefig('/Users/lucasliu/Desktop/RF_SHAP_MortalitySp1.png',bbox_inches='tight', dpi=600)

#Pred < 0.5 and true = 0
s0_ind = 1
y_pred[s0_ind] #check predicted
Y.iloc[s0_ind] #check true
S0 = np.around(X.iloc[s0_ind])
shap_value_sp0 = explainer.shap_values(S0)
shap.force_plot(explainer.expected_value, shap_value_sp0, S0, show = False,matplotlib=True).savefig('/Users/lucasliu/Desktop/RF_SHAP_MortalitySp0.png',bbox_inches='tight', dpi=600)


#############################
#MAKE
#############################
comb_df = feature_df.join(outcome_df['MAKE_HOSP120_Drop50'])
selected_features = ["LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                     "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                     "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW"]

Y =  comb_df['MAKE_HOSP120_Drop50']
X =  comb_df[selected_features]
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(X, Y)
explainer = shap.TreeExplainer(model)


f = plt.figure()
shap_values_all = explainer.shap_values(X)
shap.summary_plot(shap_values_all, X)
f.savefig("/Users/lucasliu/Desktop/RF_AllUK_SHAP_MAKE50.png", bbox_inches='tight', dpi=600)

# Indiviaual plot
Y.mean() # this is the base value: if we know nothing about this instance, the prediction is this value
y_pred = model.predict(X)
#Pred > 0.5 and true = 1
s1_ind = 4844
y_pred[s1_ind] #check predicted
Y.iloc[s1_ind] #check true
S1 = np.around(X.iloc[s1_ind],3)
shap_value_sp1 = explainer.shap_values(S1)
shap.force_plot(explainer.expected_value, shap_value_sp1, S1, show = False,matplotlib=True).savefig('/Users/lucasliu/Desktop/RF_SHAP_MAKE50_Sp1.png',bbox_inches='tight', dpi=600)

#Pred < 0.5 and true = 0
s0_ind = 38
y_pred[s0_ind] #check predicted
Y.iloc[s0_ind] #check true
S0 = np.around(X.iloc[s0_ind])
shap_value_sp0 = explainer.shap_values(S0)
shap.force_plot(explainer.expected_value, shap_value_sp0, S0, show = False,matplotlib=True).savefig('/Users/lucasliu/Desktop/RF_SHAP_MAKE50_Sp0.png',bbox_inches='tight', dpi=600)
