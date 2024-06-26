source("TAKI_Ultility.R")
#this script use entire UK data, and validation on utsw data (Use down sampling and bootstrapping for CI )
main_func <-function(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list,n_tress_RF=500,svmkernel = 'svmLinear2',random_perc=0.8){
  for (m in 1:length(method_list)){
    model_name <- method_list[m]
    
    #External validation training with downsampled UK data 10 times and validate on UTSW data
    res <- external_validation_func(train_data,Validation_data,outcome_colname,model_name,upsample_flag,N_sampling,n_tress_RF,svmkernel,random_perc)
    final_pred <- res[[1]]
    write.csv(final_pred, paste0(outdir1,"Prediction_", model_name, ".csv"),row.names = F)
    
    #compute avg performance
    final_importance_matrix <- res[[2]]
    features <- colnames(train_data)[which(colnames(train_data) != outcome_colname)]
    avg_importance_matrix <- compute_avg_importance(final_importance_matrix,features,model_name)
    write.csv(avg_importance_matrix, paste0(outdir1,"Importance_AVG_", model_name, ".csv"),row.names = F)
    
    #Compute perforamnce for each sampling
    eachSample_perf_tb <- compute_performance_ExternalValidation_func(N_sampling,final_pred)
    write.csv(eachSample_perf_tb, paste0(outdir1,"Performance_PerFoldPerSample_", model_name, ".csv"),row.names = F)
    
    #get CI and mean perforamnce
    eachSample_perf_tb[which(is.na(eachSample_perf_tb)==T,arr.ind = T)] <- 0 #basicaly prediction only 1 class in these samples
    CI_perf_tb <- perf_Mean_CI_func(eachSample_perf_tb[,2:13])
    write.csv(CI_perf_tb, paste0(outdir1,"Performance_AVG_CI_", model_name, ".csv"),row.names = T)
  }
  
}



#Data dir
UK_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/uky/Model_Feature_Outcome/"
UTSW_data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/utsw/Model_Feature_Outcome/"

#out dir
out_dir <- "//Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/ExternalV_performance/"


####################################################################################### 
######                           Mortality Prediction   1                  ############
#feature file: SOFA.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.Feature file
feature_file <- "All_SOFA_TOTAL_normed.csv"

#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "Death_inHOSP"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/SOFA/")


#1.Get model data
train_data <- construct_model_data_func(UK_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- construct_model_data_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$Death_inHOSP) #5742 1612
table(Validation_data$Death_inHOSP) #2011  222 

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list)



####################################################################################### 
######                           Mortality Prediction   2                  ############
#feature file: APACHE.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.Feature file
feature_file <- "All_APACHE_TOTAL_normed.csv"

#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "Death_inHOSP"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/APACHE/")


#1.Get model data
train_data <- construct_model_data_func(UK_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- construct_model_data_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$Death_inHOSP) #5742 1612
table(Validation_data$Death_inHOSP) #2011  222 

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list)



####################################################################################### 
######                           Mortality Prediction   3                  ############
#feature file: Selected features with 15 var
#Outcome file: All_outcome.csv
####################################################################################### 
##1.Feature file
feature_file <- c("All_Feature_imputed_normed.csv")
selected_features <- c("UrineOutput_D0toD3" , "Vasopressor_ICUD0toD3","FI02_D1_HIGH","Platelets_D1_LOW","AGE",
                       "BUN_D0toD3_HIGH","HR_D1_HIGH","LAST_KDIGO_ICU_D0toD3","PH_D1_LOW","Bilirubin_D1_HIGH",
                       "MAX_KDIGO_ICU_D0toD3","ECMO_ICUD0toD3","Hours_inICUD0toD3", "Temperature_D1_LOW", "Temperature_D1_HIGH")
#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "Death_inHOSP"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/SelectedClinicalFeature15Vars/")


#1.Get model data
train_data <- construct_model_data_func(UK_data_dir,feature_file,outcome_file,outcome_colname)
train_data <- train_data[,c(selected_features,outcome_colname)]

Validation_data <- construct_model_data_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- Validation_data[,c(selected_features,outcome_colname)]

table(train_data$Death_inHOSP) #5742 1612
table(Validation_data$Death_inHOSP) #2011  222 

colnames(train_data)
colnames(Validation_data)

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list)



####################################################################################### 
######                           Mortality Prediction   4                  ############
#feature file: SOFA.csv, directly get risk w/o train models on UK
#Outcome file: All_outcome.csv
#Rules: "Serial evaluation of the SOFA score to predict outcome in critically ill patients"
#Initial SOFA, Mortality Risk 
#0-1,   0
#2-3,   0.06
#4-5,   0.20
#6-7,   0.22
#8-9,   0.33
#10-11, 0.50
#>11,   0.96
####################################################################################### 
#1.Feature file
feature_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/utsw/xilong_extracted/"
feature_file <- "All variables for each patients 07212021.csv"

#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "Death_inHOSP"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/SOFA_Direct/")


#1.Get model data
feature_data <- read.csv(paste0(feature_dir,feature_file),stringsAsFactors = F)
Outcome_data    <- read.csv(paste0(UTSW_data_dir,outcome_file),stringsAsFactors = F)
#2.Computer predcition
pred_df <- as.data.frame(matrix(NA, nrow = nrow(feature_data),ncol = 4))
colnames(pred_df) <- c("ID","SOFA","pred_prob","Label")
for (i in 1:nrow(feature_data)){
  curr_id    <- feature_data[i,"STUDY_PATIENT_ID"]
  curr_sofa  <- feature_data[i,"SOFA"]
  curr_label <- Outcome_data[which(Outcome_data$STUDY_PATIENT_ID == curr_id),"Death_inHOSP"]
  curr_risk  <- compute_sofa_direct_risk(curr_sofa)

  pred_df[i,"ID"] <- curr_id
  pred_df[i,"SOFA"] <- curr_sofa
  pred_df[i,"pred_prob"] <- curr_risk
  pred_df[i,"Label"] <- curr_label
  
}

#get prediction class from predicted risk
pred_df$pred_class <- get_pred_class_func(pred_df[,"pred_prob"])
write.csv(pred_df, paste0(outdir1,"Prediction_SOFADirect.csv"),row.names = F)

#Get performance
pred_df$TrainingSample_Index <- "S1" #Add a training index so that the following perforamcne fucntion can be used
eachSample_perf_tb <- compute_performance_ExternalValidation_func(1,pred_df)
write.csv(eachSample_perf_tb, paste0(outdir1,"Performance_PerFoldPerSample_SOFADirect.csv"),row.names = F)

#'@NOTE: Cannot get CI and mean perforamnce since the score is directly comptued, there is no training sample differntce compring to other methods


####################################################################################### 
######                           Mortality Prediction   5                  ############
#feature file: APACHE.csv,  directly get risk w/o train models on UK
#Outcome file: All_outcome.csv
#Rules: "APACHE II: A severity of disease classification system"
#APACHE II, Mortality Risk 
#0-4,   0.04
#5-9,   0.07
#10-14, 0.13
#15-19, 0.25
#20-24, 0.42
#25-29, 0.55
#30-34, 0.73
#>=35,  0.84
####################################################################################### 
#1.Feature file
feature_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/utsw/xilong_extracted/"
feature_file <- "All variables for each patients 07212021.csv"

#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "Death_inHOSP"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"mortality/APACHE_Direct/")

#1.Get model data
feature_data <- read.csv(paste0(feature_dir,feature_file),stringsAsFactors = F)
Outcome_data    <- read.csv(paste0(UTSW_data_dir,outcome_file),stringsAsFactors = F)

#2.Computer predcition
pred_df <- as.data.frame(matrix(NA, nrow = nrow(feature_data),ncol = 4))
colnames(pred_df) <- c("ID","APACHE","pred_prob","Label")
for (i in 1:nrow(feature_data)){
  curr_id    <- feature_data[i,"STUDY_PATIENT_ID"]
  curr_apache  <- feature_data[i,"APACHE"]
  curr_label <- Outcome_data[which(Outcome_data$STUDY_PATIENT_ID == curr_id),"Death_inHOSP"]
  curr_risk  <- compute_apache_direct_risk(curr_apache)
  
  pred_df[i,"ID"] <- curr_id
  pred_df[i,"APACHE"] <- curr_apache
  pred_df[i,"pred_prob"] <- curr_risk
  pred_df[i,"Label"] <- curr_label
  
}

#get prediction class from predicted risk
pred_df$pred_class <- get_pred_class_func(pred_df[,"pred_prob"])
write.csv(pred_df, paste0(outdir1,"Prediction_APACHEDirect.csv"),row.names = F)

#Get performance
pred_df$TrainingSample_Index <- "S1" #Add a training index so that the following perforamcne fucntion can be used
eachSample_perf_tb <- compute_performance_ExternalValidation_func(1,pred_df)
write.csv(eachSample_perf_tb, paste0(outdir1,"Performance_PerFoldPerSample_APACHEDirect.csv"),row.names = F)

#'@NOTE: Cannot get CI and mean perforamnce since the score is directly comptued, there is no training sample differntce compring to other methods



####################################################################################### 
######                MAKE with drop50 Prediction   1                      ############
#feature file: 1. KDIGO.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.Feature file
feature_file <- c("All_MAX_KDIGO_ICUD0toD3_normed.csv")

#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "MAKE_HOSP120_Drop50"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"make120_drop50/KDIGO/")


#1.Get model data
train_data <- construct_model_data_func(UK_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- construct_model_data_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$MAKE_HOSP120_Drop50) #4972 2382 
table(Validation_data$MAKE_HOSP120_Drop50) #1659  574 

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list,n_tress_RF=500,svmkernel = 'svmLinear2',random_perc=0.8)


####################################################################################### 
######                MAKE with drop50 Prediction   2                      ############
#feature file: Selected Features with 14 var (without onRRT)
#Outcome file: All_outcome.csv
####################################################################################### 
#1.Feature file
feature_file <- c("All_Feature_imputed_normed.csv")
selected_features <- c("LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                       "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                       "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW",
                       "Admit_sCr","Sodium_D1_LOW")
#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "MAKE_HOSP120_Drop50"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"make120_drop50/SelectedClinicalFeature14Vars/")


#1.Get model data
train_data <- construct_model_data_func(UK_data_dir,feature_file,outcome_file,outcome_colname)
train_data <- train_data[,c(selected_features,outcome_colname)]

Validation_data <- construct_model_data_func(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- Validation_data[,c(selected_features,outcome_colname)]

table(train_data$MAKE_HOSP120_Drop50) #4972 2382 
table(Validation_data$MAKE_HOSP120_Drop50) #1659  574 

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list)


####################################################################################### 
######                MAKE with drop50 Prediction   4                      ############
#'@NOTE: only do prediction for survirors (pts who  did not die from hosp start to hosp 120)
#feature file: Selected Features 14 vars (prediction3 without onRRT)
#Outcome file: All_outcome.csv
####################################################################################### 
#1.Feature file
feature_file <- c("All_Feature_imputed_normed.csv")
selected_features <- c("LAST_KDIGO_ICU_D0toD3","UrineOutput_D0toD3","MAX_KDIGO_ICU_D0toD3","Bilirubin_D1_HIGH",
                       "AGE","BUN_D0toD3_HIGH","Hemoglobin_D1_LOW","Platelets_D1_LOW","FI02_D1_HIGH",
                       "Vasopressor_ICUD0toD3","HR_D1_HIGH","PH_D1_LOW",
                       "Admit_sCr","Sodium_D1_LOW")
#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "MAKE_HOSP120_Drop50"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"Surviors_make120_drop50/SelectedClinicalFeature14Vars/")


#1.Get model data
train_data <- construct_model_data_func_survirors(UK_data_dir,feature_file,outcome_file,outcome_colname)
train_data <- train_data[,c(selected_features,outcome_colname)]

Validation_data <- construct_model_data_func_survirors(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- Validation_data[,c(selected_features,outcome_colname)]

table(train_data$MAKE_HOSP120_Drop50) #4972  423 
table(Validation_data$MAKE_HOSP120_Drop50) #1659  205

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list)


####################################################################################### 
######                MAKE with drop50 Prediction   5                      ############
##'@NOTE: only do prediction for survirors (pts who  did not die from hosp start to hosp 120)
#feature file: 1. KDIGO.csv, 
#Outcome file: All_outcome.csv
####################################################################################### 
#1.Feature file
feature_file <- c("All_MAX_KDIGO_ICUD0toD3_normed.csv")

#2.Outcome column name
outcome_file <- "All_outcome.csv"
outcome_colname <- "MAKE_HOSP120_Drop50"

#3.Outdir for mortality
outdir1 <- paste0(out_dir,"Surviors_make120_drop50/KDIGO/")


#1.Get model data
train_data <- construct_model_data_func_survirors(UK_data_dir,feature_file,outcome_file,outcome_colname)
Validation_data <- construct_model_data_func_survirors(UTSW_data_dir,feature_file,outcome_file,outcome_colname)
table(train_data$MAKE_HOSP120_Drop50) #4972  423
table(Validation_data$MAKE_HOSP120_Drop50) #1659  205 

#2.For each method, do boostraps 10 times on entire UK data, and valdition on UTSW data
upsample_flag <- 3 #random sample 0.8 of train data with replacement for bootstrapping and then down sample for training
N_sampling <- 10
method_list <- c("SVM","RF","LogReg","XGB")
main_func(train_data,Validation_data,outcome_colname,upsample_flag,N_sampling,outdir1,method_list,n_tress_RF=500,svmkernel = 'svmLinear2',random_perc=0.8)


