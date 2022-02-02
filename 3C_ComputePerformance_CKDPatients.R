source("TAKI_Ultility.R")

compute_perf_externalV_func <- function(N_sampling,pred_df_CKD){
  eachSample_perf_tb <- compute_performance_ExternalValidation_func(N_sampling,pred_df_CKD) ##Compute perforamnce for each sampling
  eachSample_perf_tb[which(is.na(eachSample_perf_tb)==T,arr.ind = T)] <- 0 #basicaly prediction only 1 class in these samples
  CI_perf_tb <- perf_Mean_CI_func(eachSample_perf_tb[,2:13])
  return(CI_perf_tb)
}

compute_perf_CV_func <- function(N_sampling,NFolds, pred_df_CKD){
  eachfold_eachSample_perf_tb <- compute_performance_TrainCV_func(N_sampling,NFolds,pred_df_CKD) ##Compute perforamnce for each fold with each sampling
  CI_perf_tb <- perf_Mean_CI_func(eachfold_eachSample_perf_tb[,3:14]) ##get CI and mean perforamnce
  return(CI_perf_tb)
}

report_outcome_instanceN <- function(pred_df){
  pred_df_all <- pred_df[which(pred_df[,"TrainingSample_Index"]=="S1"),] #for One Training Cycle
  print(table(pred_df_all[,"Label"]))
  N_POS <- length(which(pred_df_all[,"Label"]==1))
  N_TOTAL <- nrow(pred_df_all)
  POS_RATIO <- round(N_POS/N_TOTAL*100,2)
  print(paste0("N_POS:",N_POS))
  print(paste0("N_POS (%):",N_POS , "(",POS_RATIO,")"))
  
}

perf_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/"

##########################################################################################
#    Cross validation UK for CKD pateints
##########################################################################################
#1. get CKD patiets
cohort_name <- "uky"
ckd_file <- paste0(data_dir,cohort_name,"/","All_Charlson_ELIX_Diabetes_Hypertension_CKD.csv")

#Load CKD info
ckd_df  <- read.csv(ckd_file,stringsAsFactors = F)
ckd_ids1 <- ckd_df[which(ckd_df[,"CKD"] == 1),"STUDY_PATIENT_ID"]
ckd_ids2 <- ckd_df[which(ckd_df[,"CKD_UseDiagTerms"] == 1),"STUDY_PATIENT_ID"]
ckd_ids3 <- ckd_df[which(ckd_df[,"CKD_UseICDCodes"] == 1),"STUDY_PATIENT_ID"]


#2. Mortality prediction
#Clinical model : SelectedClinicalFeature15Vars
# Method:  RF
folder_name <- "CV_performance/mortality"
model_name  <- "SelectedClinicalFeature15Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10
NFolds <- 10

#Load prediction df for all patients
pred_df <- read.csv(prediction_file,stringsAsFactors = F)

#Load prediction df for CKD patients
pred_df_ckd1 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids1),] #CKD (baseline eGFR < 60)
pred_df_ckd2 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids2),] #CKD (diagnosis terms)
pred_df_ckd3 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids3),] #CKD (diagnosis ICD codes)

#Report Mortality Ratio
report_outcome_instanceN(pred_df_ckd1)
report_outcome_instanceN(pred_df_ckd2)
report_outcome_instanceN(pred_df_ckd3)

#Compute perforance
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")
CI_perf_tb1 <- compute_perf_CV_func(N_sampling,NFolds, pred_df_ckd1)
write.csv(CI_perf_tb1, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_eGFR_PTs",".csv"),row.names = T)

CI_perf_tb2 <- compute_perf_CV_func(N_sampling,NFolds, pred_df_ckd2)
write.csv(CI_perf_tb2, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_Diag_PTs",".csv"),row.names = T)

CI_perf_tb3 <- compute_perf_CV_func(N_sampling,NFolds, pred_df_ckd3)
write.csv(CI_perf_tb3, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_ICD_PTs",".csv"),row.names = T)



#3. MAKE prediction
#Clinical model : SelectedClinicalFeature14Vars
#Method:  RF
folder_name <- "CV_performance/make120_drop50"
model_name  <- "SelectedClinicalFeature14Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10
NFolds <- 10

#Load prediction df for all patients
pred_df <- read.csv(prediction_file,stringsAsFactors = F)

#Load prediction df for CKD patients
pred_df_ckd1 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids1),] #CKD (baseline eGFR < 60)
pred_df_ckd2 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids2),] #CKD (diagnosis terms)
pred_df_ckd3 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids3),] #CKD (diagnosis ICD codes)

#Report MAKE Ratio
report_outcome_instanceN(pred_df_ckd1)
report_outcome_instanceN(pred_df_ckd2)
report_outcome_instanceN(pred_df_ckd3)

#Compute perforance
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")
CI_perf_tb1 <- compute_perf_CV_func(N_sampling,NFolds, pred_df_ckd1)
write.csv(CI_perf_tb1, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_eGFR_PTs",".csv"),row.names = T)

CI_perf_tb2 <- compute_perf_CV_func(N_sampling,NFolds, pred_df_ckd2)
write.csv(CI_perf_tb2, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_Diag_PTs",".csv"),row.names = T)

CI_perf_tb3 <- compute_perf_CV_func(N_sampling,NFolds, pred_df_ckd3)
write.csv(CI_perf_tb3, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_ICD_PTs",".csv"),row.names = T)


##########################################################################################
#    External validation UTSW for CKD pateints
##########################################################################################
#1. get CKD patiets
cohort_name <- "utsw"
ckd_file <- paste0(data_dir,cohort_name,"/","All_Charlson_ELIX_Diabetes_Hypertension_CKD.csv")

#Load CKD info
ckd_df  <- read.csv(ckd_file,stringsAsFactors = F)
ckd_ids1 <- ckd_df[which(ckd_df[,"CKD"] == 1),"STUDY_PATIENT_ID"]
ckd_ids2 <- ckd_df[which(ckd_df[,"CKD_UseDiagTerms"] == 1),"STUDY_PATIENT_ID"]
ckd_ids3 <- ckd_df[which(ckd_df[,"CKD_UseICDCodes"] == 1),"STUDY_PATIENT_ID"]


#2. Mortality prediction
#Clinical model : SelectedClinicalFeature15Vars
# Method:  RF
folder_name <- "ExternalV_performance/mortality"
model_name  <- "SelectedClinicalFeature15Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10

#Load prediction df for all patients
pred_df <- read.csv(prediction_file,stringsAsFactors = F)

#Load prediction df for CKD patients
pred_df_ckd1 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids1),] #CKD (baseline eGFR < 60)
pred_df_ckd2 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids2),] #CKD (diagnosis terms)
pred_df_ckd3 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids3),] #CKD (diagnosis ICD codes)

#Report Mortality Ratio
report_outcome_instanceN(pred_df_ckd1)
report_outcome_instanceN(pred_df_ckd2)
report_outcome_instanceN(pred_df_ckd3)

#Compute perforance
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")
CI_perf_tb1 <- compute_perf_externalV_func(N_sampling,pred_df_ckd1)
write.csv(CI_perf_tb1, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_eGFR_PTs",".csv"),row.names = T)

CI_perf_tb2 <- compute_perf_externalV_func(N_sampling,pred_df_ckd2)
write.csv(CI_perf_tb2, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_Diag_PTs",".csv"),row.names = T)

CI_perf_tb3 <- compute_perf_externalV_func(N_sampling,pred_df_ckd3)
write.csv(CI_perf_tb3, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_ICD_PTs",".csv"),row.names = T)


#3. MAKE prediction
#Clinical model : SelectedClinicalFeature14Vars
#Method:  RF
folder_name <- "ExternalV_performance/make120_drop50"
model_name  <- "SelectedClinicalFeature14Vars"
method_name <- "RF"
prediction_file <- paste0(perf_dir,folder_name,"/",model_name,"/Prediction_",method_name,".csv")
N_sampling <- 10

#Load prediction df for all patients
pred_df <- read.csv(prediction_file,stringsAsFactors = F)

#Load prediction df for CKD patients
pred_df_ckd1 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids1),] #CKD (baseline eGFR < 60)
pred_df_ckd2 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids2),] #CKD (diagnosis terms)
pred_df_ckd3 <- pred_df[which(pred_df[,"ID"] %in% ckd_ids3),] #CKD (diagnosis ICD codes)

#Report MAKE Ratio
report_outcome_instanceN(pred_df_ckd1)
report_outcome_instanceN(pred_df_ckd2)
report_outcome_instanceN(pred_df_ckd3)

#Compute perforance
outdir <- paste0(perf_dir,folder_name,"/",model_name,"/")
CI_perf_tb1 <- compute_perf_externalV_func(N_sampling,pred_df_ckd1)
write.csv(CI_perf_tb1, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_eGFR_PTs",".csv"),row.names = T)

CI_perf_tb2 <- compute_perf_externalV_func(N_sampling,pred_df_ckd2)
write.csv(CI_perf_tb2, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_Diag_PTs",".csv"),row.names = T)

CI_perf_tb3 <- compute_perf_externalV_func(N_sampling,pred_df_ckd3)
write.csv(CI_perf_tb3, paste0(outdir,"Performance_AVG_CI_", method_name,"CKD_ICD_PTs",".csv"),row.names = T)
