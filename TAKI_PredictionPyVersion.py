#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 20 01:40:12 2021

@author: lucasliu
"""

import numpy as np
import pandas as pd
from sklearn.model_selection import KFold
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score, confusion_matrix
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier
import scipy.stats as st
import pickle

def downsample_func(in_data,out_col,ran_state):
    # data of each class
    in_class0 = in_data[in_data[out_col] == 0]
    in_class1 = in_data[in_data[out_col] == 1]

    #num of class 1
    n_class1 = in_class1.shape[0]
    
    #random sample num = n_class1 of class 0
    in_class0_downsampled = in_class0.sample(n= n_class1, replace=False, random_state=ran_state)
    
    #down sampled data
    in_data_downsampled = pd.concat([in_class1, in_class0_downsampled], axis = 0)
    
    return in_data_downsampled


def generate_CV_folds(in_data, label_col, selected_features, bootstrap_flag = True) :
    
    #create folds
    kf = KFold(n_splits=10, shuffle = True, random_state = 1) #First create 10 folds for UK data
    fold_i = 1
    
    train_X_list = [] 
    train_Y_list = []
    valid_X_list = []
    valid_Y_list = []
    for train_index, validation_index in kf.split(in_data):
        
        #Load the other 9 folds for train
        train_comb = in_data.iloc[train_index]  
            
        if bootstrap_flag == False: #no bootstrap, only down sample
           
            #Down sample
            train_comb_ds = downsample_func(train_comb,label_col,ran_state = fold_i)
            
            #Get train X and Y
            train_X =  train_comb_ds[selected_features]
            train_Y =  train_comb_ds[label_col]
            
        elif bootstrap_flag == True: #bootstrap and down sample
            
            #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
            train_comb_bts = train_comb.sample(frac=0.8, replace=True, random_state= fold_i)
            #Down sample
            train_comb_bts_ds = downsample_func(train_comb_bts,label_col,ran_state = fold_i)
    
            #Get train X and Y
            train_X =  train_comb_bts_ds[selected_features]
            train_Y =  train_comb_bts_ds[label_col]
    
        #Get Validation X and Y
        valid_comb   = in_data.iloc[validation_index]         
        validation_X =  valid_comb[selected_features]
        validation_Y =  valid_comb[label_col]
            
        train_X_list.append(train_X)
        train_Y_list.append(train_Y)
        valid_X_list.append(validation_X)
        valid_Y_list.append(validation_Y)
        
        fold_i += 1 #for random state
        
    return train_X_list,train_Y_list,valid_X_list,valid_Y_list


def main(train_X_list,train_Y_list,valid_X_list,valid_Y_list,test_X,test_Y):
    validation_perf_list = []
    test_perf_list = []
    for i in range(10):
        train_X = train_X_list[i]
        train_Y = train_Y_list[i]
        valid_X = valid_X_list[i]
        valid_Y = valid_Y_list[i]
            
        RF_model = RandomForestClassifier(max_depth=6, random_state=0, n_estimators=500)
        RF_model.fit(train_X, train_Y)
        
        #prediction for validation 
        pred_prob_valid = RF_model.predict_proba(valid_X)[: ,1] #probabily of prediction class label 1
        pred_classes_valid = RF_model.predict(valid_X)
        curr_perf_valid = compute_performance(valid_Y,pred_prob_valid,pred_classes_valid,"Fold" + str(i))
        validation_perf_list.append(curr_perf_valid)
        
        #prediction for test
        pred_prob_test = RF_model.predict_proba(test_X)[: ,1] #probabily of prediction class label 1
        pred_classes_test = RF_model.predict(test_X)
        curr_perf_test = compute_performance(test_Y,pred_prob_test,pred_classes_test,"Fold" + str(i))
        test_perf_list.append(curr_perf_test)
    
    validation_perf = pd.concat(validation_perf_list)
    test_perf = pd.concat(test_perf_list)
    
    return validation_perf,test_perf




def compute_performance(y_true,y_pred_prob,y_pred_class,cohort_name):
    confusion_matrix(y_true, y_pred_class) #CM
    fpr, tpr, thresholds = metrics.roc_curve(y_true, y_pred_prob, pos_label=1)
    
    AUC = round(metrics.auc(fpr, tpr),2)
    ACC = round(accuracy_score(y_true, y_pred_class),2)
    F1 = round(f1_score(y_true, y_pred_class),2)
    Recall = round(recall_score(y_true, y_pred_class),2)
    Precision = round(precision_score(y_true, y_pred_class),2)
    perf_tb = pd.DataFrame({"AUC": AUC, 
                            "ACC": ACC,
                            "F1": F1,
                            "Recall": Recall,
                            "Precision":Precision},index = [cohort_name])
    
    return perf_tb




def comptute_avg_CI_perf (perf_tb): 
    #perf_tb = validation_perf

    #avg
    avg_perf = round(pd.DataFrame(perf_tb.mean()).transpose(),2)
    avg_perf = avg_perf.rename(index={0: "AVG"})

    
    #95% CI
    metrics = ['AUC','ACC','F1','Recall','Precision']
    CI_low = []
    CI_high = []
    for i in range(5):
       curr_perf = perf_tb[metrics[i]]
       curr_CI =  st.t.interval(alpha=0.95, df=len(curr_perf)-1, loc=np.mean(curr_perf), scale=st.sem(curr_perf)) 
       CI_low.append(round(curr_CI[0],2))
       CI_high.append(round(curr_CI[1],2))

    CI_low_df =  pd.DataFrame(CI_low).transpose()
    CI_low_df = CI_low_df.rename(columns={0: "AUC", 1: "ACC", 2: "F1",3: "Recall",4: "Precision"})
    CI_low_df = CI_low_df.rename(index={0: "CI_L"})

    CI_high_df = pd.DataFrame(CI_high).transpose()
    CI_high_df = CI_high_df.rename(columns={0: "AUC", 1: "ACC", 2: "F1",3: "Recall",4: "Precision"})
    CI_high_df = CI_high_df.rename(index={0: "CI_H"})

    final_perf_tb = pd.concat([perf_tb,avg_perf,CI_low_df,CI_high_df])
    return final_perf_tb

#######################################################################################
#data dir
#######################################################################################
UK_data_dir = "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"
UTSW_data_dir = "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/utsw/Model_Feature_Outcome/"
outdir = "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/Python_Version/"

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
outcome_col = 'Death_inHOSP'

#UK
UK_comb_df = UK_feature_df.join(UK_outcome_df[outcome_col])

#UTSW
UTSW_comb_df = UTSW_feature_df.join(UTSW_outcome_df[outcome_col])
test_Y =  UTSW_comb_df[outcome_col]
test_X =  UTSW_comb_df[selected_features]


#Get train and validtion date from 10 folds CV 
train_X_list,train_Y_list,valid_X_list,valid_Y_list = generate_CV_folds(UK_comb_df,outcome_col,selected_features,bootstrap_flag = False)

#Train and predction for valdiation and testing data
#returns list of performance from each fold (one fold for valid, 9 fold for training, and 9 fold used for prediction of test)
UK_validation_perf,UTSW_test_perf = main(train_X_list,train_Y_list,valid_X_list,valid_Y_list,test_X,test_Y)

#Add CI
UK_validation_perf = comptute_avg_CI_perf(UK_validation_perf)
UTSW_test_perf = comptute_avg_CI_perf(UTSW_test_perf)


UK_validation_perf.to_csv(outdir + "Py_TAKI_Moratlity_UK_Perf.csv")
UTSW_test_perf.to_csv(outdir + "Py_TAKI_Moratlity_UTSW_Perf.csv")

#Training the whole UK set, prediction for UTSW
train_comb_ds = downsample_func(UK_comb_df,outcome_col,ran_state = 123) #Down sample
train_X = train_comb_ds[selected_features]
train_Y = train_comb_ds[outcome_col]

RF_model = RandomForestClassifier(max_depth=6, random_state=0, n_estimators=500)
RF_model.fit(train_X, train_Y)

#prediction for test
pred_prob_test = RF_model.predict_proba(test_X)[: ,1] #probabily of prediction class label 1
pred_classes_test = RF_model.predict(test_X)
perf_test = compute_performance(test_Y,pred_prob_test,pred_classes_test,"ALLUK")
perf_test.to_csv(outdir + "Py_TAKI_Moratlity_UTSW_Perf_TrainedWithAllUK.csv")

# save the full model to disk
filename = outdir + 'TAKI_Mortality_Fullmodel.sav'
pickle.dump(RF_model, open(filename, 'wb'))

##########################################################
#MAKE
##########################################################
selected_features = ["LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                     "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                     "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW",
                     "Admit_sCr","Sodium_D1_LOW"]

outcome_col = 'MAKE_HOSP120_Drop50'
                        
#UK
UK_comb_df = UK_feature_df.join(UK_outcome_df[outcome_col])

#UTSW
UTSW_comb_df = UTSW_feature_df.join(UTSW_outcome_df[outcome_col])
test_Y =  UTSW_comb_df[outcome_col]
test_X =  UTSW_comb_df[selected_features]


#Get train and validtion date from 10 folds CV 
train_X_list,train_Y_list,valid_X_list,valid_Y_list = generate_CV_folds(UK_comb_df,outcome_col,selected_features,bootstrap_flag = False)

#Train and predction for valdiation and testing data
#returns list of performance from each fold (one fold for valid, 9 fold for training, and 9 fold used for prediction of test)
UK_validation_perf,UTSW_test_perf = main(train_X_list,train_Y_list,valid_X_list,valid_Y_list,test_X,test_Y)

#Add CI
UK_validation_perf = comptute_avg_CI_perf(UK_validation_perf)
UTSW_test_perf = comptute_avg_CI_perf(UTSW_test_perf)


UK_validation_perf.to_csv(outdir + "Py_TAKI_MAKE_UK_Perf.csv")
UTSW_test_perf.to_csv(outdir + "Py_TAKI_MAKE_UTSW_Perf.csv")


#Training the whole UK set, prediction for UTSW
train_comb_ds = downsample_func(UK_comb_df,outcome_col,ran_state = 123) #Down sample
train_X = train_comb_ds[selected_features]
train_Y = train_comb_ds[outcome_col]

RF_model = RandomForestClassifier(max_depth=6, random_state=0, n_estimators=500)
RF_model.fit(train_X, train_Y)

#prediction for test
pred_prob_test = RF_model.predict_proba(test_X)[: ,1] #probabily of prediction class label 1
pred_classes_test = RF_model.predict(test_X)
perf_test = compute_performance(test_Y,pred_prob_test,pred_classes_test,"ALLUK")
perf_test.to_csv(outdir + "Py_TAKI_MAKE_UTSW_Perf_TrainedWithAllUK.csv")

# save the full model to disk
filename = outdir + 'TAKI_MAKE_Fullmodel.sav'
pickle.dump(RF_model, open(filename, 'wb'))
