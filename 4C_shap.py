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

def plot_shap_summary_trainAndTest(explainer, TRAIN_X, TEST_X, ticks_value, outdir,outfile):
    LARGE_SIZE =  50
    MEDIUM_SIZE = 40
    SMALL_SIZE = 30
    TINY_SIZE = 20

    #Train Values
    shap_values_train = explainer.shap_values(TRAIN_X) #compute shap values for all X

    #Test Values
    shap_values_test = explainer.shap_values(TEST_X) #compute shap values for all X
    
    plt.figure(figsize=(16, 64))
    plt.subplot(1,2,1)
    shap.summary_plot(shap_values_train, TRAIN_X, plot_size=(32, 16), show=False, color_bar = False)
    plt.title('UK', fontsize= LARGE_SIZE)
    plt.xlabel('SHAP value',fontsize= MEDIUM_SIZE)
    plt.yticks(fontsize= SMALL_SIZE)
    plt.xticks(fontsize= SMALL_SIZE)
    
    cb = plt.colorbar(ticks = ticks_value,aspect=1000)
    cb.set_ticklabels(['Low/Non-EXP.', 'High/EXP.'])
    cb.set_label(label='Feature Value',size=SMALL_SIZE, labelpad=-100)
    cb.ax.tick_params(labelsize=SMALL_SIZE, length=0)
    cb.set_alpha(1)
    cb.outline.set_visible(False)
    bbox = cb.ax.get_window_extent().transformed(plt.gcf().dpi_scale_trans.inverted())
    cb.ax.set_aspect((bbox.height - 0.9) * 20)

    plt.subplot(1,2,2)
    shap.summary_plot(shap_values_test, TEST_X, plot_size=(32, 16), show=False, color_bar = False)
    plt.title('UTSW', fontsize= LARGE_SIZE)
    plt.xlabel('SHAP value',fontsize= MEDIUM_SIZE)
    plt.yticks(fontsize= SMALL_SIZE)
    plt.xticks(fontsize= SMALL_SIZE)

    cb = plt.colorbar(ticks = ticks_value,aspect=1000)
    cb.set_ticklabels(['Low/Non-EXP.', 'High/EXP.'])
    cb.set_label(label='Feature Value',size=SMALL_SIZE, labelpad=-100)
    cb.ax.tick_params(labelsize=SMALL_SIZE, length=0)
    cb.set_alpha(1)
    cb.outline.set_visible(False)
    bbox = cb.ax.get_window_extent().transformed(plt.gcf().dpi_scale_trans.inverted())
    cb.ax.set_aspect((bbox.height - 0.9) * 20)
    
    plt.subplots_adjust(wspace=0.8)
    plt.savefig(outdir + outfile, bbox_inches='tight', dpi=300)


def plot_shap_summary(explainer, X, output_dir, outfile):
    shap_values_all = explainer.shap_values(X) #compute shap values for all X
    f = plt.figure()
    shap.summary_plot(shap_values_all, X)
    f.savefig(output_dir + outfile, bbox_inches='tight', dpi=600)
    

def plot_individual_shap(explainer, Sample_X,value_threshold, output_dir, outfile):
    shap_value = explainer.shap_values(Sample_X)
    #Using logit will change log-odds numbers into probabilities, the defualt shows the the log-odds
    #We do not have to chaknge link to "logit", because we use RF_regressor classfier, the y_pred is alreayd in proabily form
    shap.force_plot(explainer.expected_value, shap_value, Sample_X,contribution_threshold= value_threshold,show = False,matplotlib=True).savefig(output_dir + outfile,bbox_inches='tight',dpi = 500)

def get_mean_abs_shap_values (explainer,X,output_dir,out_file):
    shap_values_all = explainer.shap_values(X) #compute shap values for all X
    f = plt.figure()
    #plot mean abs value of shap
    shap.summary_plot(shap_values_all, features=X, feature_names=X.columns, plot_type='bar')
    f.savefig(output_dir + out_file + ".png", bbox_inches='tight', dpi=600)
    
    shap_values_all_df = pd.DataFrame(shap_values_all)
    #get  mean  abs value shap for each feature
    mean_abs_shap_values = []
    for f in shap_values_all_df.columns:
        curr_val = round(np.mean(abs(shap_values_all_df[f])),4)
        mean_abs_shap_values.append(curr_val)
    mean_abs_shap_values_df = pd.DataFrame({'Feature': X.columns, 'Mean_Abs_SHAP': mean_abs_shap_values })
    mean_abs_shap_values_df.to_csv(output_dir + out_file + ".csv")
        

def get_correct_predicted_IDs(sample_IDs,y_pred,y_true,label_class):
    
    #get true indexes for label_class
    true_indexes = [i for i, value in enumerate(y_true) if value == label_class]
    
    if label_class == 1:
        #get predicted class 1
        predicted_label1_indexes = [i for i, value in enumerate(y_pred) if value >= 0.5]
        #get corrected preidct class 1
        correct_pred_index = [value for value in predicted_label1_indexes if value in true_indexes] 
    elif label_class == 0:
        #get predicted class 0
        predicted_label0_indexes = [i for i, value in enumerate(y_pred) if value < 0.5]
        #get corrected preidct class 1
        correct_pred_index = [value for value in predicted_label0_indexes if value in true_indexes] 

    corrected_prediction_IDs = list(sample_IDs[correct_pred_index])
    
    return corrected_prediction_IDs


def find_decreaseIn_KDIGO_pts(feature_data, max_kdigo, last_kdigo):
    hightolow_kdigo_df = feature_data.loc[(feature_data['Maximum KDIGO'].isin(max_kdigo)) & (feature_data['Last KDIGO'] == last_kdigo)]
    return hightolow_kdigo_df[['Maximum KDIGO','Last KDIGO']]

def intersection(lst1, lst2):
    return list(set(lst1) & set(lst2))

def change_feature_name(df):
    df = df.rename(columns={"UrineOutput_D0toD3": "Urine Output", 
                            "Vasopressor_ICUD0toD3": "Vasopressor",
                            "FI02_D1_HIGH": "FiO2 (High)",
                            "Platelets_D1_LOW": "Platelets (Low)",
                            "AGE": "Age",
                            "BUN_D0toD3_HIGH": "BUN (High)",
                            "HR_D1_HIGH": "Heart Rate (High)",
                            "LAST_KDIGO_ICU_D0toD3": "Last KDIGO",
                            "PH_D1_LOW": "pH (Low)",
                            "Bilirubin_D1_HIGH": "Bilirubin (High)",
                            "MAX_KDIGO_ICU_D0toD3": "Maximum KDIGO",
                            "ECMO_ICUD0toD3": "ECMO",
                            "Hours_inICUD0toD3": "Hours in ICU",
                            "Temperature_D1_LOW": "Temperature (Low)",
                            "Temperature_D1_HIGH": "Temperature (High)",
                            "Hemoglobin_D1_LOW": "Hemoglobin (Low)",
                            "Admit_sCr": "ICU admission sCr",
                            "Sodium_D1_LOW": "Serum Sodium (Low)"})
    return df

#######################################################################################
#data dir
#######################################################################################
UK_data_dir = "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"
UTSW_data_dir = "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/utsw/Model_Feature_Outcome/"
outdir = "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/Shap/"

#######################################################################################
# Load data
#######################################################################################
#use not normed feature for the plot
UK_feature_df = pd.read_csv(UK_data_dir + "All_Feature_imputed.csv", index_col="STUDY_PATIENT_ID") # Load the data
UK_outcome_df = pd.read_csv(UK_data_dir + "All_outcome.csv", index_col="STUDY_PATIENT_ID")
UK_outcome_df = UK_outcome_df.reindex(UK_feature_df.index) #reorder to match ID

UTSW_feature_df = pd.read_csv(UTSW_data_dir + "All_Feature_imputed.csv", index_col="STUDY_PATIENT_ID") # Load the data
UTSW_outcome_df = pd.read_csv(UTSW_data_dir + "All_outcome.csv", index_col="STUDY_PATIENT_ID")
UTSW_outcome_df = UTSW_outcome_df.reindex(UTSW_outcome_df.index) #reorder to match ID


##########################################################
#Mortality
##########################################################
selected_features = ["UrineOutput_D0toD3" , "Vasopressor_ICUD0toD3","FI02_D1_HIGH","Platelets_D1_LOW","AGE",
                     "BUN_D0toD3_HIGH","HR_D1_HIGH","LAST_KDIGO_ICU_D0toD3","PH_D1_LOW","Bilirubin_D1_HIGH",
                     "MAX_KDIGO_ICU_D0toD3","ECMO_ICUD0toD3","Hours_inICUD0toD3", 
                     "Temperature_D1_LOW", "Temperature_D1_HIGH"]
#UK
UK_comb_df = UK_feature_df.join(UK_outcome_df['Death_inHOSP'])
train_Y =  UK_comb_df['Death_inHOSP']
train_X =  UK_comb_df[selected_features]

#UTSW
UTSW_comb_df = UTSW_feature_df.join(UTSW_outcome_df['Death_inHOSP'])
test_Y =  UTSW_comb_df['Death_inHOSP']
test_X =  UTSW_comb_df[selected_features]

#Change feature name for publication
train_X = change_feature_name(train_X)
test_X = change_feature_name(test_X)


#Use All data to train and get plot
#model = RandomForestClassifier(max_depth=6, random_state=0, n_estimators=500)
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(train_X, train_Y)

# Create object that can calculate shap values
explainer = shap.TreeExplainer(model) #

#Plot shap summary for UK and UTSW together
plot_shap_summary_trainAndTest(explainer,train_X, test_X, [0,1], outdir, "mortality/RF_15vars_UKandUTSW_Mortality.tiff")

# # Plot shap summary for UK
# plot_shap_summary(explainer, train_X, outdir, "mortality/RF_15vars_AllUK_SHAP_HospMortality.png")
# explainer.expected_value #This is the base value = Y.mean(), if we know nothing about this instance, the prediction is this value

#Mean abs shap value for UK
get_mean_abs_shap_values(explainer, train_X, outdir,"mortality/RF_15vars_AllUK_Mean_ABS_SHAP_HospMortality")

# # Plot shap summaryfor UTSW
# plot_shap_summary(explainer, test_X, outdir, "mortality/RF_15vars_AllUTSW_SHAP_HospMortality.png")
# explainer.expected_value #This is the base value = Y.mean(), if we know nothing about this instance, the prediction is this value

#Mean abs shap value for UTSW
get_mean_abs_shap_values(explainer, test_X, outdir,"mortality/RF_15vars_AllUTSW_Mean_ABS_SHAP_HospMortality")


#check predicted value
#y_pred = model.predict_proba(train_X) #if using RandomForestClassifier
y_pred = model.predict(train_X) #y_pred in RandomForestRegressor is equalient to the probabily of being in the class 1 in using RandomForestClassifier


#Get all Train ID (UK ID)
all_train_IDs = train_X.index

#Find which y_pred > 0.5 and y_ture == 1
predicted_correct_death_IDs = get_correct_predicted_IDs(all_train_IDs,y_pred,train_Y,1)

#Find which y_pred < 0.5 and y_ture == 0
predicted_correct_survive_IDs = get_correct_predicted_IDs(all_train_IDs,y_pred,train_Y,0)

#Find MAX KDIGO 3or4 with lower Last kdigo IDs
MAX3or4_LAST0 = find_decreaseIn_KDIGO_pts(train_X,[3,4],0) #141
MAX3or4_LAST1 = find_decreaseIn_KDIGO_pts(train_X,[3,4],1) #93
MAX3or4_LAST2 = find_decreaseIn_KDIGO_pts(train_X,[3,4],2) #231
MAX2_LAST1    = find_decreaseIn_KDIGO_pts(train_X,[2],1) #444
MAX2_LAST0    = find_decreaseIn_KDIGO_pts(train_X,[2],0) #488
MAX1_LAST0    = find_decreaseIn_KDIGO_pts(train_X,[1],0) #2012
MAX1_LAST1   = find_decreaseIn_KDIGO_pts(train_X,[1],1) #2012

#All high to low IDs
MAX3or4_LAST0_IDs = list(MAX3or4_LAST0.index)
MAX3or4_LAST1_IDs = list(MAX3or4_LAST1.index)
MAX3or4_LAST2_IDs = list(MAX3or4_LAST2.index)
MAX2_LAST1_IDs = list(MAX2_LAST1.index)
MAX2_LAST0_IDs = list(MAX2_LAST0.index)
MAX1_LAST0_IDs = list(MAX1_LAST0.index)
MAX1_LAST1_IDs = list(MAX1_LAST1.index)

#For survivors: Correct predicted and high to low IDs
correct_MAX3or4_LAST0_IDs1 = intersection(predicted_correct_survive_IDs,MAX3or4_LAST0_IDs)
correct_MAX3or4_LAST1_IDs1 = intersection(predicted_correct_survive_IDs,MAX3or4_LAST1_IDs)
correct_MAX3or4_LAST2_IDs1 = intersection(predicted_correct_survive_IDs,MAX3or4_LAST2_IDs)
correct_MAX2_LAST1_IDs1 = intersection(predicted_correct_survive_IDs,MAX2_LAST1_IDs)
correct_MAX2_LAST0_IDs1 = intersection(predicted_correct_survive_IDs,MAX2_LAST0_IDs)
correct_MAX1_LAST0_IDs1 = intersection(predicted_correct_survive_IDs,MAX1_LAST0_IDs)


#For death: Correct predicted and high to low IDs
correct_MAX3or4_LAST0_IDs2 = intersection(predicted_correct_death_IDs,MAX3or4_LAST0_IDs) #0
correct_MAX3or4_LAST1_IDs2 = intersection(predicted_correct_death_IDs,MAX3or4_LAST1_IDs) #1
correct_MAX3or4_LAST2_IDs2 = intersection(predicted_correct_death_IDs,MAX3or4_LAST2_IDs) #10
correct_MAX2_LAST1_IDs2 = intersection(predicted_correct_death_IDs,MAX2_LAST1_IDs) #13
correct_MAX2_LAST0_IDs2 = intersection(predicted_correct_death_IDs,MAX2_LAST0_IDs) #7
correct_MAX1_LAST0_IDs2 = intersection(predicted_correct_death_IDs,MAX1_LAST0_IDs) #28


#Sample  IDs to plot
random.seed(1)
Survivors_IDs1 = sample(correct_MAX3or4_LAST0_IDs1,2)
Survivors_IDs2 = sample(correct_MAX2_LAST0_IDs1,2)
Survivors_IDs3 = sample(correct_MAX1_LAST0_IDs1,2)

Death_IDs1 = sample(correct_MAX3or4_LAST2_IDs2,2)
Death_IDs2 = sample(correct_MAX2_LAST0_IDs2,2)
Death_IDs3 = sample(correct_MAX1_LAST0_IDs2,2)

Sample_IDs = Survivors_IDs1 + Survivors_IDs2 + Survivors_IDs3 + Death_IDs1 + Death_IDs2 + Death_IDs3

for pt in Sample_IDs:
    sample_data = np.around(train_X.loc[pt],2) #round feature to 2 digit
    outcome_label = train_Y.loc[pt] 
    MAX_KDIGO = train_X.loc[pt,'Maximum KDIGO'] 
    LAST_KDIGO = train_X.loc[pt,'Last KDIGO']
    plot_individual_shap(explainer, sample_data,0.1,outdir,"/mortality/Examples/UK/" + 'Outcome' + str(outcome_label) + '_MAXKDIGO'+ str(MAX_KDIGO) + '_LASTKDIGO'+ str(LAST_KDIGO) +"_ID" + str(pt) + ".png")
 
##########################################################
#MAKE
##########################################################
selected_features1 = ["LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                     "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                     "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW",
                     "Admit_sCr","Sodium_D1_LOW"]
                        
#UK
UK_comb_df = UK_feature_df.join(UK_outcome_df['MAKE_HOSP120_Drop50'])
train_Y =  UK_comb_df['MAKE_HOSP120_Drop50']
train_X =  UK_comb_df[selected_features1]

#UTSW
UTSW_comb_df = UTSW_feature_df.join(UTSW_outcome_df['MAKE_HOSP120_Drop50'])
test_Y =  UTSW_comb_df['MAKE_HOSP120_Drop50']
test_X =  UTSW_comb_df[selected_features1]


#Change feature name for publication
train_X = change_feature_name(train_X)
test_X = change_feature_name(test_X)

#Use All data to train and get plot
model = RandomForestRegressor(max_depth=6, random_state=0, n_estimators=500)
model.fit(train_X, train_Y)

# Create object that can calculate shap values
explainer = shap.TreeExplainer(model) #

#Plot shap summary for UK and UTSW together
plot_shap_summary_trainAndTest(explainer,train_X, test_X,[0,4], outdir,"make120drop50/RF_14vars_UKandUTSW_MAKE50.tiff")

# # Plot shap summary UK
# plot_shap_summary(explainer, train_X, outdir, "make120drop50/RF_14vars_AllUK_SHAP_MAKE50.png")
# explainer.expected_value #This is the base value = Y.mean(), if we know nothing about this instance, the prediction is this value

#Mean abs shap value for UK
get_mean_abs_shap_values(explainer, train_X, outdir,"make120drop50/RF_14vars_AllUK_Mean_ABS_SHAP_MAKE50")


# # Plot shap summaryfor UTSW
# plot_shap_summary(explainer, test_X, outdir, "/make120drop50/RF_14vars_AllUTSW_SHAP_MAKE50.png")
# explainer.expected_value #This is the base value = Y.mean(), if we know nothing about this instance, the prediction is this value

#Mean abs shap value for UTSW
get_mean_abs_shap_values(explainer, test_X, outdir,"make120drop50/RF_14vars_AllUTSW_Mean_ABS_SHAP_MAKE50")


#check predicted value
y_pred = model.predict(train_X)

#Get all Train ID (UK ID)
all_train_IDs = train_X.index

#Find which y_pred > 0.5 and y_ture == 1
predicted_correct_MAKE1_IDs = get_correct_predicted_IDs(all_train_IDs,y_pred,train_Y,1)

#Find which y_pred < 0.5 and y_ture == 0
predicted_correct_MAKE0_IDs = get_correct_predicted_IDs(all_train_IDs,y_pred,train_Y,0)


#For MAKE0: Correct predicted and high to low IDs
correct_MAX3or4_LAST0_IDs1 = intersection(predicted_correct_MAKE0_IDs,MAX3or4_LAST0_IDs)
correct_MAX3or4_LAST1_IDs1 = intersection(predicted_correct_MAKE0_IDs,MAX3or4_LAST1_IDs)
correct_MAX3or4_LAST2_IDs1 = intersection(predicted_correct_MAKE0_IDs,MAX3or4_LAST2_IDs)
correct_MAX2_LAST1_IDs1 = intersection(predicted_correct_MAKE0_IDs,MAX2_LAST1_IDs)
correct_MAX2_LAST0_IDs1 = intersection(predicted_correct_MAKE0_IDs,MAX2_LAST0_IDs)
correct_MAX1_LAST0_IDs1 = intersection(predicted_correct_MAKE0_IDs,MAX1_LAST0_IDs)


#For MAKE1: Correct predicted and high to low IDs
correct_MAX3or4_LAST0_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX3or4_LAST0_IDs) #5
correct_MAX3or4_LAST1_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX3or4_LAST1_IDs) #3
correct_MAX3or4_LAST2_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX3or4_LAST2_IDs) #31
correct_MAX2_LAST1_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX2_LAST1_IDs) #20
correct_MAX2_LAST0_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX2_LAST0_IDs) #11
correct_MAX1_LAST0_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX1_LAST0_IDs) #33
correct_MAX1_LAST1_IDs2 = intersection(predicted_correct_MAKE1_IDs,MAX1_LAST1_IDs) #66


#Sample  IDs to plot
random.seed(1)
MAKE0_IDs1 = sample(correct_MAX3or4_LAST0_IDs1,2)
MAKE0_IDs2 = sample(correct_MAX2_LAST0_IDs1,2)
MAKE0_IDs3 = sample(correct_MAX1_LAST0_IDs1,2)

MAKE1_IDs1 = sample(correct_MAX3or4_LAST2_IDs2,2)
MAKE1_IDs2 = sample(correct_MAX2_LAST1_IDs2,2)
MAKE1_IDs3 = sample(correct_MAX1_LAST1_IDs2,2)

Sample_IDs = MAKE0_IDs1 + MAKE0_IDs2 + MAKE0_IDs3 + MAKE1_IDs1 + MAKE1_IDs2 + MAKE1_IDs3

for pt in Sample_IDs:
    sample_data = np.around(train_X.loc[pt],2) #round feature to 2 digit
    outcome_label = train_Y.loc[pt] 
    MAX_KDIGO = train_X.loc[pt,'Maximum KDIGO'] 
    LAST_KDIGO = train_X.loc[pt,'Last KDIGO']
    plot_individual_shap(explainer, sample_data,0.1,outdir,"/make120drop50/Examples/UK/" + 'Outcome' + str(outcome_label) + '_MAXKDIGO'+ str(MAX_KDIGO) + '_LASTKDIGO'+ str(LAST_KDIGO) +"_ID" + str(pt) + ".png")
