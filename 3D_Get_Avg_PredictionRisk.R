source("TAKI_Ultility.R")

proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0708/"

################################################################################################## 
######                         Mortality                                            ############## 
################################################################################################## 
#1. UK
UK_mortality_dir <- paste0(proj_dir,"CV_performance/mortality/")
method_name <- "RF"
featureset_folder <- "SelectedClinicalFeature2"
#1. Load pred table
pred_df <- read.csv(paste0(UK_mortality_dir, featureset_folder, "/Prediction_",method_name,".csv"),stringsAsFactors = F)

#2.Compute avg pred risk
risk_mortality_UK <- get_avg_pred_func(pred_df)
write.csv(risk_mortality_UK,paste0(UK_mortality_dir,"UK_",featureset_folder,"_Mortality_AVG_Pred_Risk_",method_name,".csv"))

#3.Count risk category
risk_mortality_UK_count <- count_risk_category(risk_mortality_UK)
write.csv(risk_mortality_UK_count,paste0(UK_mortality_dir,"UK_",featureset_folder,"_Mortality_Risk_Catogory_",method_name,".csv"))

#2.UTSW

################################################################################################## 
############## MAKE ############## 
################################################################################################## 
#1. UK
UK_MAKE_dir <- paste0(proj_dir,"CV_performance/make120_drop50/")
method_name <- "RF"
featureset_folder <- "SelectedClinicalFeature2"
#1. Load pred table
pred_df <- read.csv(paste0(UK_MAKE_dir,featureset_folder,"/Prediction_",method_name,".csv"),stringsAsFactors = F)

#2.Compute avg pred risk
risk_make_UK <- get_avg_pred_func(pred_df)
write.csv(risk_make_UK,paste0(UK_MAKE_dir,"UK_",featureset_folder,"_MAKE_AVG_Pred_Risk_",method_name,".csv"))

#3.Count risk category
risk_make_UK_count <- count_risk_category(risk_make_UK)
write.csv(risk_make_UK_count,paste0(UK_MAKE_dir,"UK_",featureset_folder,"_MAKE_Risk_Catogory_",method_name,".csv"))

#UTSW
