source("TAKI_Ultility.R")


proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/All_AKI_Projects/Other_Project/TAKI_Project/Intermediate_Results/Prediction_results0806/"

###############################################################
#1. For mortality, compare models with IDI, NRI 
###############################################################
#1. For UK 
perf_dir <- paste0(proj_dir,"CV_performance/mortality/")
baseline_model_file  <- "/SOFA/Prediction_RF.csv"
comprison_model_file1 <- "/APACHE/Prediction_RF.csv"
comprison_model_file2 <- "/SelectedClinicalFeature15Vars/Prediction_RF.csv"

#compare APAHCE and SOFA
reclass_res1 <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1,cutoff = c(0,0.5,1))
colnames(reclass_res1)[2] <- paste0("APACHEvsSOFA_",colnames(reclass_res1)[2])
#compare clinical model and SOFA
reclass_res2 <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2,cutoff = c(0,0.5,1))
colnames(reclass_res2)[2] <- paste0("SelectedClinicalFeature15VarsvsSOFA_",colnames(reclass_res2)[2])
#compare clinical model and APAHCE
reclass_res3 <- compute_IDI_NRI_func(perf_dir,comprison_model_file1,comprison_model_file2,cutoff = c(0,0.5,1))
colnames(reclass_res3)[2] <- paste0("SelectedClinicalFeature15VarsvsAPACHE_",colnames(reclass_res3)[2])
comb_res <- cbind(reclass_res1,reclass_res2,reclass_res3)
write.csv(comb_res,paste0(perf_dir,"UK_SelectedClinicalFeature15Vars_Mortality_ReclassResults_RF.csv"))

#2. UTSW
perf_dir <- paste0(proj_dir,"ExternalV_performance/mortality/")
baseline_model_file  <- "/SOFA/Prediction_RF.csv"
comprison_model_file1 <- "/APACHE/Prediction_RF.csv"
comprison_model_file2 <- "/SelectedClinicalFeature15Vars/Prediction_RF.csv"

reclass_res1 <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1,cutoff = c(0,0.5,1))
colnames(reclass_res1)[2] <- paste0("APACHEvsSOFA_",colnames(reclass_res1)[2])
reclass_res2 <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file2,cutoff = c(0,0.5,1))
colnames(reclass_res2)[2] <- paste0("SelectedClinicalFeature15VarsvsSOFA_",colnames(reclass_res2)[2])

#compare clinical model and APAHCE
reclass_res3 <- compute_IDI_NRI_func(perf_dir,comprison_model_file1,comprison_model_file2,cutoff = c(0,0.5,1))
colnames(reclass_res3)[2] <- paste0("SelectedClinicalFeature15VarsvsAPACHE_",colnames(reclass_res3)[2])
comb_res <- cbind(reclass_res1,reclass_res2,reclass_res3)
write.csv(comb_res,paste0(perf_dir,"UTSW_SelectedClinicalFeature15Vars_Mortality_ReclassResults_RF.csv"))

###############################################################
#2. For MAKE, compare models with IDI, NRI 
###############################################################
#1.UK
perf_dir <- paste0(proj_dir,"CV_performance/make120_drop50/")
baseline_model_file  <- "/KDIGO/Prediction_RF.csv"
comprison_model_file1 <- "/SelectedClinicalFeature14Vars/Prediction_RF.csv"

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1,cutoff = c(0,0.5,1))
colnames(reclass_res)[2] <- paste0("SelectedClinicalFeature14VarsvsKDIGO_",colnames(reclass_res)[2])
write.csv(reclass_res,paste0(perf_dir,"UK_SelectedClinicalFeature14Vars_MAKE_ReclassResults_RF.csv"))

#UTSW
perf_dir <- paste0(proj_dir,"ExternalV_performance/make120_drop50/")
baseline_model_file  <- "/KDIGO/Prediction_RF.csv"
comprison_model_file1 <- "/SelectedClinicalFeature14Vars/Prediction_RF.csv"

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1,cutoff = c(0,0.5,1))
colnames(reclass_res)[2] <- paste0("SelectedClinicalFeature14VarsvsKDIGO_",colnames(reclass_res)[2])
write.csv(reclass_res,paste0(perf_dir,"UTSW_SelectedClinicalFeature14Vars_MAKE_ReclassResults_RF.csv"))



###############################################################
#'@ADDITONAL MAKE for survivors
#3. For MAKE, compare models with IDI, NRI 
###############################################################
#1.UK
perf_dir <- paste0(proj_dir,"CV_performance/Surviors_make120_drop50/")
baseline_model_file  <- "/KDIGO/Prediction_RF.csv"
comprison_model_file1 <- "/SelectedClinicalFeature14Vars/Prediction_RF.csv"

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1,cutoff = c(0,0.5,1))
colnames(reclass_res)[2] <- paste0("SelectedClinicalFeature14VarsvsKDIGO_",colnames(reclass_res)[2])
write.csv(reclass_res,paste0(perf_dir,"UK_SelectedClinicalFeature14Vars_MAKE_ReclassResults_RF.csv"))

#UTSW
perf_dir <- paste0(proj_dir,"ExternalV_performance/Surviors_make120_drop50/")
baseline_model_file  <- "/KDIGO/Prediction_RF.csv"
comprison_model_file1 <- "/SelectedClinicalFeature14Vars/Prediction_RF.csv"

reclass_res <- compute_IDI_NRI_func(perf_dir,baseline_model_file,comprison_model_file1,cutoff = c(0,0.5,1))
colnames(reclass_res)[2] <- paste0("SelectedClinicalFeature14VarsvsKDIGO_",colnames(reclass_res)[2])
write.csv(reclass_res,paste0(perf_dir,"UTSW_SelectedClinicalFeature14Vars_MAKE_ReclassResults_RF.csv"))


