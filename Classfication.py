#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 10 22:12:10 2021

@author: lucasliu
"""

import numpy as np
import pandas as pd

from sklearn import svm, metrics
from sklearn.model_selection import KFold

#Load UK data
UK_dir = "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/TAKI_Data/uky/"
UK_feature_df =  pd.read_csv(UK_dir + "clinical_model_mortality_norm.csv",index_col= 0)
UK_Label_df = pd.read_csv(UK_dir + "Old_outcomes.csv",index_col= 0)

#Match label df Id with feature df
UK_Comb_df = UK_feature_df.join(UK_Label_df)
UK_Comb_df['died_inp'].value_counts() #0    5233  #1    1618

X = UK_feature_df.reset_index(drop=True)
y = UK_Comb_df['died_inp'].reset_index(drop=True)
IDs = pd.DataFrame(UK_Comb_df.index)


#Load UTSW for validation data


#Create fold index
n_folds = 10
kf = KFold(n_splits=n_folds, shuffle=True, random_state=1)

for train_index, test_index in kf.split(X):
    X_train, X_test = X.iloc[train_index], X.iloc[test_index]
    y_train, y_test = y.iloc[train_index], y.iloc[test_index]
    svm_model = svm.SVC(probability=True, kernel='rbf',class_weight =  {0:1,1:2}).fit(X_train, y_train)
    pred_prob_table = svm_model.predict_proba(X_test)
    pred_class = svm_model.predict(X_test)

#DS
metrics.accuracy_score(y_test, pred_class)
metrics.precision_score(y_test, pred_class,pos_label=1)
metrics.recall_score(y_test, pred_class,pos_label=1)

import collections
collections.Counter(y_train)

X = [[0, 0], [1, 1]]
y = [0, 1]
clf = svm.SVC(probability=True)
clf.fit(X, y)
v = [1., 2.1]
clf.predict_proba([v])
clf.predict([v])
