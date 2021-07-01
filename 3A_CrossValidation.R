source("TAKI_Ultility.R")
library(rms)
library(PredictABEL)
library(pROC) #can also use this one for delong's methods
library(Rmisc)
library(caret)

#this script do 10 folds CV on UK data 
#1. for each fold , down sampling 10 time, each instance get 10 predicted results
#2. compute confidence interval for performance metrics for each fold with each sampling index


#Data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"

#out dir
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0629/"
#feature file and outcome file names
outcome_file <- "All_outcome.csv"




####################################################################################### 
######                           Mortality Prediction   1                   ############
#feature file: 1. All_Feature_imputed_normed.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_Feature_imputed_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/AllClinicalFeature/")

#Outcome column name
outcome_colname <- "Death_inHOSP"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$Death_inHOSP)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}



####################################################################################### 
######                           Mortality Prediction   2                   ############
#feature file: 
#              2. Selected features
#Outcome file: All_outcome.csv
####################################################################################### 
# #1.Selected features
# importance_files <- paste0(outdir1, c("Importance_AVG_SVM.csv","Importance_AVG_RF.csv","Importance_AVG_LogReg.csv","Importance_AVG_XGB.csv"))
# importance_df <- lapply(importance_files,read.csv)
# top25feautres1 <- importance_df[[1]][1:25,"Feature"]
# top25feautres2 <- importance_df[[2]][1:25,"Feature"]
# top25feautres3 <- importance_df[[3]][1:25,"Feature"]
# top25feautres4 <- importance_df[[4]][1:25,"Feature"]
# 
# intersect_features <- Reduce(intersect, list(top25feautres1,top25feautres2,top25feautres3,top25feautres4))
# 
# feature_file <- c("All_Feature_imputed_normed.csv")
# 
# #Outdir for mortality
# outdir1 <- paste0(out_dir,"mortality/Selected_Clinical_Feature/")
# 
# #Outcome column name
# outcome_colname <- "Death_inHOSP"
# 
# #1.Get model data
# model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
# model_data <- model_data[,c(intersect_features,outcome_colname)]
# table(model_data$Death_inHOSP)
# colnames(model_data)
# 
# #2.CV
# upsample_flag <- 0
# N_sampling <- 10
# NFolds <- 10
# 
# model_name_list <- c("SVM","RF","LogReg","XGB")
# for (m in 1:length(model_name_list)){
#   model_name <- model_name_list[m]
#   #CV
#   cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
#   final_pred <- cv_res[[1]]
#   write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
#   
#   #compute avg performance 
#   final_importance_matrix <- cv_res[[2]]
#   feature_indexes<- which(colnames(model_data) != outcome_colname)
#   features <- colnames(model_data)[feature_indexes]
#   avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
#   write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
#   
#   #Compute perforamnce for each fold with each sampling
#   eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
#   
#   write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
#   
#   #get CI and mean perforamnce
#   CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
#   write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
# }


####################################################################################### 
######                           Mortality Prediction   3                   ############
#feature file: 
#              3. SOFA.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_SOFA_TOTAL_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/SOFA/")

#Outcome column name
outcome_colname <- "Death_inHOSP"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$Death_inHOSP)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}


####################################################################################### 
######                           Mortality Prediction   4                  ############
#feature file: 
#              4. APACHE.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_APACHE_TOTAL_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/APACHE/")

#Outcome column name
outcome_colname <- "Death_inHOSP"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$Death_inHOSP)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}



####################################################################################### 
######                MAKE with drop50 Prediction   1                      ############
#feature file: 1. All_Feature_imputed_normed.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_Feature_imputed_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"make120_drop50/AllClinicalFeature/")

#Outcome column name
outcome_colname <- "MAKE_HOSP120_Drop50"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$MAKE_HOSP120_Drop50)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}




####################################################################################### 
######                MAKE with drop50 Prediction   2                      ############
#feature file: 1. KDIGO.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_MAX_KDIGO_ICUD0toD3_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"make120_drop50/KDIGO/")

#Outcome column name
outcome_colname <- "MAKE_HOSP120_Drop50"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$MAKE_HOSP120_Drop50)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}


####################################################################################### 
######                MAKE with drop30 Prediction   1                      ############
#feature file: 1. All_Feature_imputed_normed.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_Feature_imputed_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"make120_drop30/AllClinicalFeature/")

#Outcome column name
outcome_colname <- "MAKE_HOSP120_Drop30"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$MAKE_HOSP120_Drop30)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds,svmkernel = 'svmRadial')
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}




####################################################################################### 
######                MAKE with drop30 Prediction   2                      ############
#feature file: 1. KDIGO.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.All_Feature_imputed_normed.csv
feature_file <- c("All_MAX_KDIGO_ICUD0toD3_normed.csv")

#Outdir for mortality
outdir1 <- paste0(out_dir,"make120_drop30/KDIGO/")

#Outcome column name
outcome_colname <- "MAKE_HOSP120_Drop30"

#1.Get model data
model_data <- construct_model_data_func(data_dir,feature_file,outcome_file,outcome_colname)
table(model_data$MAKE_HOSP120_Drop50)
colnames(model_data)

#2.CV
upsample_flag <- 0
N_sampling <- 10
NFolds <- 10

model_name_list <- c("SVM","RF","LogReg","XGB")
for (m in 1:length(model_name_list)){
  model_name <- model_name_list[m]
  #CV
  cv_res <- cv2_func(model_data,outcome_colname,model_name,upsample_flag,N_sampling,NFolds)
  final_pred <- cv_res[[1]]
  write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
  
  #compute avg performance 
  final_importance_matrix <- cv_res[[2]]
  feature_indexes<- which(colnames(model_data) != outcome_colname)
  features <- colnames(model_data)[feature_indexes]
  avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
  write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
  
  #Compute perforamnce for each fold with each sampling
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,final_pred)
  
  write.csv(final_pred, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
  
  #get CI and mean perforamnce
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14])
  write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
}


