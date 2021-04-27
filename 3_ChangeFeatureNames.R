#This scrip change features name 
source("TAKI_Ultility.R")

#These functions change feautre names and update the df
update_clinicalmodel_mortality_func <-function(input_df){
  input_df <- change_feature_name_func(input_df,"Bicarbonate_Low","Bicarbonate_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Bilirubin","Bilirubin_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"FiO2_High","FiO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"HeartRate_High","HeartRate_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"MAP_low","MAP_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pCO2_high","PCO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"pH_low","PH_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Platelets","PLATELETS_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pO2_low","PO2_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Sodium_high","SODIUM_D1_HIGH")
  return(input_df)
}


update_clinical_withTraj_mortality_func <-function(input_df){
  input_df <- change_feature_name_func(input_df,"Bicarbonate_Low","Bicarbonate_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Bilirubin","Bilirubin_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"FiO2_High","FiO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"HeartRate_High","HeartRate_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"MAP_low","MAP_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pCO2_high","PCO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"pH_low","PH_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Platelets","PLATELETS_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pO2_low","PO2_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Sodium_high","SODIUM_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"peak_KDIGO","MaxKDIGO_D14")
  input_df <- change_feature_name_func(input_df,"KDIGO_at_EndOfWindow","LastKDIGO_D14")
  return(input_df)
}


update_clinicalmodel_MAKE_func <-function(input_df){
  input_df <- change_feature_name_func(input_df,"Bicarbonate_Low","Bicarbonate_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Bilirubin","Bilirubin_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"BUN","BUN_D0TOD3_HIGH")
  input_df <- change_feature_name_func(input_df,"FiO2_High","FiO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"HeartRate_High","HeartRate_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"Hemoglobin_low","HEMOGLOBIN_D1_LOW")
  input_df <- change_feature_name_func(input_df,"MAP_low","MAP_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pCO2_high","PCO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"pH_low","PH_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Platelets","PLATELETS_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pO2_low","PO2_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Sodium_high","SODIUM_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"Temperature_low","TEMPERATURE_D1_LOW")

  return(input_df)
}


update_clinical_withTraj_MAKE_func <-function(input_df){
  input_df <- change_feature_name_func(input_df,"Bicarbonate_Low","Bicarbonate_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Bilirubin","Bilirubin_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"BUN","BUN_D0TOD3_HIGH")
  input_df <- change_feature_name_func(input_df,"FiO2_High","FiO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"HeartRate_High","HeartRate_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"Hemoglobin_low","HEMOGLOBIN_D1_LOW")
  input_df <- change_feature_name_func(input_df,"MAP_low","MAP_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pCO2_high","PCO2_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"pH_low","PH_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Platelets","PLATELETS_D1_LOW")
  input_df <- change_feature_name_func(input_df,"pO2_low","PO2_D1_LOW")
  input_df <- change_feature_name_func(input_df,"Sodium_high","SODIUM_D1_HIGH")
  input_df <- change_feature_name_func(input_df,"Temperature_low","TEMPERATURE_D1_LOW")
  input_df <- change_feature_name_func(input_df,"peak_KDIGO","MaxKDIGO_D14")
  input_df <- change_feature_name_func(input_df,"KDIGO_at_EndOfWindow","LastKDIGO_D14")
  return(input_df)
}


#User input
#data dir
data_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/features/"
UK_data_dir <- paste0(data_dir,"uky/")
UTSW_data_dir <- paste0(data_dir,"utsw/")

#out dir
out_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/TAKI_Feature/Updated_features/"
UK_outdir<-  paste0(out_dir,"uky/")
UTSW_outdir<-  paste0(out_dir,"utsw/")


####################################################################################################
##### I. Mortality Features
##### A. clinical_model_mortality_norm.csv
##### B. clinical_model_mortality_wTrajectory_norm.csv
##### C. selected feature model 
##### D. SOFA_SUM
##### E. APACHE_SUM
#####
#'@NOTE: 1.Albumin wasnot sure what it was, only says ALBUMIN_VALUE
#'2.Q: Min_KDIGO? FluidOverload?,Urine_output?
####################################################################################################
##############################################################################################
##################                1. UK                           ############################ 
##############################################################################################
#1. Load clinical_model_mortality_norm.csv
feature_ClinicalModel_UK <- read.csv(paste0(UK_data_dir,"clinical_model_mortality_norm.csv"),stringsAsFactors = F)
updated_feature_ClinicalModel_UK <- update_clinicalmodel_mortality_func(feature_ClinicalModel_UK)
write.csv(updated_feature_ClinicalModel_UK,paste0(UK_outdir,"updated_clinical_model_mortality_norm.csv"),row.names = F)

#2. Load clinical_model_mortality_wTrajectory_norm.csv
feature_Clinical_withTraj_UK <- read.csv(paste0(UK_data_dir,"clinical_model_mortality_wTrajectory_norm.csv"),stringsAsFactors = F)
updated_feature_Clinical_withTraj_UK <- update_clinical_withTraj_mortality_func(feature_Clinical_withTraj_UK)
write.csv(updated_feature_Clinical_withTraj_UK,paste0(UK_outdir,"updated_clinical_model_mortality_wTrajectory_norm.csv"),row.names = F)

#3. Selected features from note "Feature_Importance0222_031221_jan.docx"
selected_feature_names <- c("LastKDIGO_D14","MaxKDIGO_D03","unPlannedAdmission","Age",
                            "Bilirubin_D1_HIGH","FluidOverload","Septic","MechHemodynamicSupport",
                            "MechanicalVentilation","PH_D1_LOW","Vasopress_exp","PLATELETS_D1_LOW")
selected_feature_df_UK <- updated_feature_Clinical_withTraj_UK[,c("STUDY_PATIENT_ID",selected_feature_names)]
write.csv(selected_feature_df_UK,paste0(UK_outdir,"Mortality_selected_features_norm.csv"),row.names = F)


##############################################################################################
##################                2. UTSW                         ############################ 
##############################################################################################
#1. Load clinical_model_mortality_norm.csv
feature_ClinicalModel_UTSW <- read.csv(paste0(UTSW_data_dir,"clinical_model_mortality_norm.csv"),stringsAsFactors = F)
updated_feature_ClinicalModel_UTSW <- update_clinicalmodel_mortality_func(feature_ClinicalModel_UTSW)
write.csv(updated_feature_ClinicalModel_UTSW,paste0(UTSW_outdir,"updated_clinical_model_mortality_norm.csv"),row.names = F)

#2. Load clinical_model_mortality_wTrajectory_norm.csv
feature_Clinical_withTraj_UTSW <- read.csv(paste0(UTSW_data_dir,"clinical_model_mortality_wTrajectory_norm.csv"),stringsAsFactors = F)
updated_feature_Clinical_withTraj_UTSW <- update_clinical_withTraj_mortality_func(feature_Clinical_withTraj_UTSW)
write.csv(updated_feature_Clinical_withTraj_UTSW,paste0(UTSW_outdir,"updated_clinical_model_mortality_wTrajectory_norm.csv"),row.names = F)

#3. Selected features from note "Feature_Importance0222_031221_jan.docx"
selected_feature_names <- c("LastKDIGO_D14","MaxKDIGO_D03","unPlannedAdmission","Age",
                            "Bilirubin_D1_HIGH","FluidOverload","Septic","MechHemodynamicSupport",
                            "MechanicalVentilation","PH_D1_LOW","Vasopress_exp","PLATELETS_D1_LOW")
selected_feature_df_UTSW <- updated_feature_Clinical_withTraj_UTSW[,c("STUDY_PATIENT_ID",selected_feature_names)]
write.csv(selected_feature_df_UTSW,paste0(UTSW_outdir,"Mortality_selected_features_norm.csv"),row.names = F)


####################################################################################################
##### II. MAKE Features
##### A. clinical_model_make_norm.csv
##### B. clinical_model_make_wTrajectory_norm.csv
##### C. selected feature model 
##### E. max_kdigo_d03_norm.csv
#####
##############################################################################################
##################                1. UK                           ############################ 
##############################################################################################
#1. Load clinical_model_make_norm.csv
feature_ClinicalModel_UK2 <- read.csv(paste0(UK_data_dir,"clinical_model_make_norm.csv"),stringsAsFactors = F)
updated_feature_ClinicalModel_UK2 <- update_clinicalmodel_MAKE_func(feature_ClinicalModel_UK2)
write.csv(updated_feature_ClinicalModel_UK2,paste0(UK_outdir,"updated_clinical_model_make_norm.csv"),row.names = F)

#2. Load clinical_model_make_wTrajectory_norm.csv
feature_Clinical_withTraj_UK2 <- read.csv(paste0(UK_data_dir,"clinical_model_make_wTrajectory_norm.csv"),stringsAsFactors = F)
updated_feature_Clinical_withTraj_UK2 <- update_clinical_withTraj_MAKE_func(feature_Clinical_withTraj_UK2)
write.csv(updated_feature_Clinical_withTraj_UK2,paste0(UK_outdir,"updated_clinical_model_make_wTrajectory_norm.csv"),row.names = F)

#3. Selected features from note "Feature_Importance0222_031221_jan.docx"
#option1
selected_feature_names_option1 <- c("LastKDIGO_D14","MaxKDIGO_D03","unPlannedAdmission","Age",
                            "Bilirubin_D1_HIGH","FluidOverload","Septic","MechanicalVentilation",
                            "Urine_output","HEMOGLOBIN_D1_LOW","PLATELETS_D1_LOW","PH_D1_LOW")
selected_feature_df_UK_option1 <- updated_feature_Clinical_withTraj_UK2[,c("STUDY_PATIENT_ID",selected_feature_names_option1)]
write.csv(selected_feature_df_UK_option1,paste0(UK_outdir,"MAKE_selected_features_norm_option1_withPH.csv.csv"),row.names = F)

#option2
selected_feature_names_option2 <- c("LastKDIGO_D14","MaxKDIGO_D03","unPlannedAdmission","Age",
                                    "Bilirubin_D1_HIGH","FluidOverload","Septic","MechanicalVentilation",
                                    "Urine_output","HEMOGLOBIN_D1_LOW","PLATELETS_D1_LOW","MechHemodynamicSupport")
#Get MechHemodynamicSupport from mortality file
matchedId_order <- match(feature_ClinicalModel_UK[,"STUDY_PATIENT_ID"], updated_feature_Clinical_withTraj_UK2[,"STUDY_PATIENT_ID"])
updated_feature_Clinical_withTraj_UK2$MechHemodynamicSupport <- feature_ClinicalModel_UK[matchedId_order,"MechHemodynamicSupport"]

selected_feature_df_UK_option2 <- updated_feature_Clinical_withTraj_UK2[,c("STUDY_PATIENT_ID",selected_feature_names_option2)]
write.csv(selected_feature_df_UK_option2,paste0(UK_outdir,"MAKE_selected_features_norm_option2_withMechHemoSup.csv"),row.names = F)


##############################################################################################
##################                2. UTSW                           ############################ 
##############################################################################################
#1. Load clinical_model_make_norm.csv
feature_ClinicalModel_UTSW2 <- read.csv(paste0(UTSW_data_dir,"clinical_model_make_norm.csv"),stringsAsFactors = F)
updated_feature_ClinicalModel_UTSW2 <- update_clinicalmodel_MAKE_func(feature_ClinicalModel_UTSW2)
write.csv(updated_feature_ClinicalModel_UTSW2,paste0(UTSW_outdir,"updated_clinical_model_make_norm.csv"),row.names = F)

#2. Load clinical_model_make_wTrajectory_norm.csv
feature_Clinical_withTraj_UTSW2 <- read.csv(paste0(UTSW_data_dir,"clinical_model_make_wTrajectory_norm.csv"),stringsAsFactors = F)
updated_feature_Clinical_withTraj_UTSW2 <- update_clinical_withTraj_MAKE_func(feature_Clinical_withTraj_UTSW2)
write.csv(updated_feature_Clinical_withTraj_UTSW2,paste0(UTSW_outdir,"updated_clinical_model_make_wTrajectory_norm.csv"),row.names = F)

#3. Selected features from note "Feature_Importance0222_031221_jan.docx"
#option1
selected_feature_names_option1 <- c("LastKDIGO_D14","MaxKDIGO_D03","unPlannedAdmission","Age",
                                    "Bilirubin_D1_HIGH","FluidOverload","Septic","MechanicalVentilation",
                                    "Urine_output","HEMOGLOBIN_D1_LOW","PLATELETS_D1_LOW","PH_D1_LOW")
selected_feature_df_UTSW_option1 <- updated_feature_Clinical_withTraj_UTSW2[,c("STUDY_PATIENT_ID",selected_feature_names_option1)]
write.csv(selected_feature_df_UTSW_option1,paste0(UTSW_outdir,"MAKE_selected_features_norm_option1_withPH.csv.csv"),row.names = F)

#option2
selected_feature_names_option2 <- c("LastKDIGO_D14","MaxKDIGO_D03","unPlannedAdmission","Age",
                                    "Bilirubin_D1_HIGH","FluidOverload","Septic","MechanicalVentilation",
                                    "Urine_output","HEMOGLOBIN_D1_LOW","PLATELETS_D1_LOW","MechHemodynamicSupport")
#Get MechHemodynamicSupport from mortality file
matchedId_order <- match(feature_ClinicalModel_UTSW[,"STUDY_PATIENT_ID"], updated_feature_Clinical_withTraj_UTSW2[,"STUDY_PATIENT_ID"])
updated_feature_Clinical_withTraj_UTSW2$MechHemodynamicSupport <- feature_ClinicalModel_UTSW[matchedId_order,"MechHemodynamicSupport"]

selected_feature_df_UTSW_option2 <- updated_feature_Clinical_withTraj_UTSW2[,c("STUDY_PATIENT_ID",selected_feature_names_option2)]
write.csv(selected_feature_df_UTSW_option2,paste0(UTSW_outdir,"MAKE_selected_features_norm_option2_withMechHemoSup.csv"),row.names = F)


