source("TAKI_Ultility.R")
compute_avg_pred_risk_and_risk_category <- function(cohort_name,outcome_name,perf_dir,method_name,featureset_folder){
  # perf_dir <- UTSW_MAKE_dir
  # cohort_name <- "UTSW"
  # outcome_name <- "MAKE"
  # method_name <- "RF"
  
  #1. Load pred table
  pred_df <- read.csv(paste0(perf_dir, featureset_folder, "/Prediction_",method_name,".csv"),stringsAsFactors = F)
  
  #2.Compute avg pred risk
  avg_risk <- get_avg_pred_func(pred_df)
  write.csv(avg_risk,paste0(perf_dir,cohort_name,"_",featureset_folder,"_",outcome_name,"_AVG_Pred_Risk_",method_name,".csv"))
  
  #3.Count risk category
  risk_category1 <- c(0.1,0.5)
  risk_count1 <- count_risk_category(avg_risk,risk_category1)
  write.csv(risk_count1,paste0(perf_dir,cohort_name,"_",featureset_folder,"_",outcome_name,"_Risk_Catogory1_",method_name,".csv"))
  
  
  risk_category2 <- c(0.2,0.5)
  risk_count2 <- count_risk_category(avg_risk,risk_category2)
  write.csv(risk_count2,paste0(perf_dir,cohort_name,"_",featureset_folder,"_",outcome_name,"_Risk_Catogory2_",method_name,".csv"))
  
  risk_category3 <- c(0.1,0.3,0.5)
  risk_count3 <- count_risk_category(avg_risk,risk_category3)
  write.csv(risk_count3,paste0(perf_dir,cohort_name,"_",featureset_folder,"_",outcome_name,"_Risk_Catogory3_",method_name,".csv"))
  
}

proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"

################################################################################################## 
######                         Mortality                                            ############## 
################################################################################################## 
outcome_name <- "Mortality"
method_name <- "RF"
featureset_folder <- "SelectedClinicalFeature15Vars"
#1. UK
UK_mortality_dir <- paste0(proj_dir,"CV_performance/mortality/")
compute_avg_pred_risk_and_risk_category("UK",outcome_name,UK_mortality_dir,method_name,featureset_folder)

#2.UTSW
UTSW_mortality_dir <- paste0(proj_dir,"ExternalV_performance/mortality/")
compute_avg_pred_risk_and_risk_category("UTSW",outcome_name,UTSW_mortality_dir,method_name,featureset_folder)

################################################################################################## 
############## MAKE ############## 
################################################################################################## 
#1. UK
method_name <- "RF"
featureset_folder <- "SelectedClinicalFeature14Vars"
outcome_name <- "MAKE"
#1. UK
UK_MAKE_dir <- paste0(proj_dir,"CV_performance/make120_drop50/")
compute_avg_pred_risk_and_risk_category("UK",outcome_name,UK_MAKE_dir,method_name,featureset_folder)

#2.UTSW
UTSW_MAKE_dir <- paste0(proj_dir,"ExternalV_performance/make120_drop50/")
compute_avg_pred_risk_and_risk_category("UTSW",outcome_name,UTSW_MAKE_dir,method_name,featureset_folder)


################################################################################################## 
############## MAKE ############## 
#'@ADDITONAL MAKE for survivors
################################################################################################## 
#1. UK
method_name <- "RF"
featureset_folder <- "SelectedClinicalFeature14Vars"
outcome_name <- "MAKE"
#1. UK
UK_MAKE_dir <- paste0(proj_dir,"CV_performance/Surviors_make120_drop50/")
compute_avg_pred_risk_and_risk_category("UK",outcome_name,UK_MAKE_dir,method_name,featureset_folder)

#2.UTSW
UTSW_MAKE_dir <- paste0(proj_dir,"ExternalV_performance/Surviors_make120_drop50/")
compute_avg_pred_risk_and_risk_category("UTSW",outcome_name,UTSW_MAKE_dir,method_name,featureset_folder)
