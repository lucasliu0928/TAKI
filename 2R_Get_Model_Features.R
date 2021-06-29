library(lubridate)
source("TAKI_Ultility.R")

get_feature_forPt <- function(pt_id, input_df,feature_name){
  feature_value <- input_df[which(input_df[,"STUDY_PATIENT_ID"] == pt_id),feature_name]
  return(feature_value)
}


#data dir
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#3.Load feature not imputed data
All_Scr_df <-read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)
All_Demo_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)
All_Anemia_df <-read.csv(paste0(outdir,"All_Anemia_usingImputedLabs.csv"),stringsAsFactors = F)
All_LAB_df <-read.csv(paste0(outdir,"All_LAB_NOTimputed.csv"),stringsAsFactors = F)
All_BMI_df <-read.csv(paste0(outdir,"All_BMI_NOTimputed.csv"),stringsAsFactors = F)
All_HT_WT_RESP_FIO2_df <-read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_NOTimputed.csv"),stringsAsFactors = F)
All_Fluid_Overload_df <-read.csv(paste0(outdir,"All_FluidOverLoad_NOTImputed.csv"),stringsAsFactors = F)
All_ECMO_IABP_MV_VAD_df <-read.csv(paste0(outdir,"All_ECMO_IABP_MV_VAD_ICUD0toD3.csv"),stringsAsFactors = F)
All_MAP_TEMP_HR_df <-read.csv(paste0(outdir,"All_MAP_TEMP_HR_NOTimputed.csv"),stringsAsFactors = F)

All_Nephrotoxin_Vasopressor_df <-read.csv(paste0(outdir,"All_Nephrotoxin_Vasopressor.csv"),stringsAsFactors = F)
All_pO2_pCO2_pH_df <-read.csv(paste0(outdir,"All_pO2_pCO2_pH_NOTimputed.csv"),stringsAsFactors = F)
All_septic_df <-read.csv(paste0(outdir,"All_sepsis_Before_Or_At_Admission.csv"),stringsAsFactors = F)
All_UrineOutput_df <-read.csv(paste0(outdir,"All_UrineOutput_NOTimputed.csv"),stringsAsFactors = F)
All_onRRT_df <-read.csv(paste0(outdir,"All_onRRT_ICUD0toD3.csv"),stringsAsFactors = F)
All_KDIGO_df <-read.csv(paste0(outdir,"KDIGO_Admit_MAX_LAST_ICU_D0D3_df.csv"),stringsAsFactors = F)

All_ELIX_df <-read.csv(paste0(outdir,"All_ELIXHAUSER_df.csv"),stringsAsFactors = F)

#Recode gender and race
M_idxes <- which(All_Demo_df$GENDER == "M")
All_Demo_df[M_idxes,"GENDER"] <- 1
All_Demo_df[-M_idxes,"GENDER"] <- 0
All_Demo_df$GENDER <- as.numeric(All_Demo_df$GENDER)

table(All_Demo_df$RACE)
white_idxes <- which(All_Demo_df$RACE == "WHITE")
black_idxes <- which(All_Demo_df$RACE == "BLACK/AFR AMERI")
  
All_Demo_df[white_idxes,"RACE"] <- 0
All_Demo_df[black_idxes,"RACE"] <- 1
All_Demo_df[-c(white_idxes,black_idxes),"RACE"] <- 2
All_Demo_df$RACE <- as.numeric(All_Demo_df$RACE)
table(All_Demo_df$RACE)

##########################################################################################
#1. Get feature df
##########################################################################################
Feature_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 72))
colnames(Feature_df) <- c("STUDY_PATIENT_ID",
                          "Admit_sCr",
                          "AGE",
                          "Anemia_D1",
                          "Baseline_sCr",
                          "Bicarbonate_D1_LOW","Bicarbonate_D1_HIGH",
                          "Bilirubin_D1_HIGH",
                          "BMI",
                          "BUN_D0toD3_HIGH",
                          "FI02_D1_LOW","FI02_D1_HIGH",
                          "FluidOverload_inPercentage",
                          "GENDER",
                          "HR_D1_LOW","HR_D1_HIGH",
                          "Hematocrit_D1_LOW","Hematocrit_D1_HIGH",
                          "Hemoglobin_D1_LOW","Hemoglobin_D1_HIGH",
                          "ECMO_ICUD0toD3","IABP_ICUD0toD3","MV_ICUD0toD3","VAD_ICUD0toD3",
                          "MAP_D1_LOW","MAP_D1_HIGH",
                          "Admit_KDIGO_ICU",
                          "Nephrotoxin_ICUD0toD3","Vasopressor_ICUD0toD3",
                          "PCO2_D1_LOW" , "PCO2_D1_HIGH",
                          "Peak_SCr_inICU_D0_D3",
                          "PH_D1_LOW","PH_D1_HIGH",
                          "Platelets_D1_LOW",
                          "PO2_D1_LOW", "PO2_D1_HIGH",
                          "Potassium_D1_LOW","Potassium_D1_HIGH",
                          "RACE",
                          "RESP_RATE_D1_LOW","RESP_RATE_D1_HIGH",
                          "Sepsis_Before_or_At_Admission",
                          "Sodium_D1_LOW","Sodium_D1_HIGH",
                          "Temperature_D1_LOW","Temperature_D1_HIGH",
                          "UrineOutput_D0toD3","UrineFlow_D0toD3",
                          "WBC_D1_LOW","WBC_D1_HIGH",
                          "HEIGHT_Meters","INITIAL_WEIGHT_KG",
                          "Hours_inICUD0toD3",
                          "Mechanical_Hemodynamic_Support",
                          "Unplanned_Admission",
                          "MAX_KDIGO_ICU_D0toD3",
                          "onRRT_ICUD0toD3",
                          "LAST_KDIGO_ICU_D0toD3",
                          "ELX_GRP_1","ELX_GRP_5","ELX_GRP_6","ELX_GRP_7","ELX_GRP_10","ELX_GRP_11","ELX_GRP_12",
                          "ELX_GRP_15","ELX_GRP_16","ELX_GRP_17","ELX_GRP_19","ELX_GRP_21","ELX_GRP_31")


for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  Feature_df[i,"STUDY_PATIENT_ID"] <- curr_id
  #sCr
  Feature_df[i,"Admit_sCr"] <- get_feature_forPt(curr_id,All_Scr_df,"AdmitICU_SCr")
  Feature_df[i,"Baseline_sCr"] <- get_feature_forPt(curr_id,All_Scr_df,"Baseline_SCr")
  Feature_df[i,"Peak_SCr_inICU_D0_D3"] <- get_feature_forPt(curr_id,All_Scr_df,"Peak_SCr_inICU_D0_D3")

  #demo
  Feature_df[i,"AGE"] <- get_feature_forPt(curr_id,All_Demo_df,"AGE")
  Feature_df[i,"RACE"] <- get_feature_forPt(curr_id,All_Demo_df,"RACE")
  Feature_df[i,"GENDER"] <- get_feature_forPt(curr_id,All_Demo_df,"GENDER")
  
  #anemia
  Feature_df[i,"Anemia_D1"] <- get_feature_forPt(curr_id,All_Anemia_df,"Anemia")
  
  #Fluid Overload
  Feature_df[i,"FluidOverload_inPercentage"] <- get_feature_forPt(curr_id,All_Fluid_Overload_df,"FluidOverload_inPercentage")
  
  #BMI
  Feature_df[i,"BMI"] <- get_feature_forPt(curr_id,All_BMI_df,"BMI")
  
  #All_ECMO_IABP_MV_VAD_df
  Feature_df[i,"ECMO_ICUD0toD3"] <- get_feature_forPt(curr_id,All_ECMO_IABP_MV_VAD_df,"ECMO_ICUD0toD3")
  Feature_df[i,"IABP_ICUD0toD3"] <- get_feature_forPt(curr_id,All_ECMO_IABP_MV_VAD_df,"IABP_ICUD0toD3")
  Feature_df[i,"MV_ICUD0toD3"] <- get_feature_forPt(curr_id,All_ECMO_IABP_MV_VAD_df,"MV_ICUD0toD3")
  Feature_df[i,"VAD_ICUD0toD3"] <- get_feature_forPt(curr_id,All_ECMO_IABP_MV_VAD_df,"VAD_ICUD0toD3")
  #"Mechanical_Hemodynamic_Support"
  if (Feature_df[i,"VAD_ICUD0toD3"]  == 1 | Feature_df[i,"IABP_ICUD0toD3"] == 1 | Feature_df[i,"ECMO_ICUD0toD3"] == 1 ){
    Feature_df[i,"Mechanical_Hemodynamic_Support"] <- 1
  }else{
    Feature_df[i,"Mechanical_Hemodynamic_Support"] <- 0
  }
  
  #KDIGO
  Feature_df[i,"Admit_KDIGO_ICU"] <- get_feature_forPt(curr_id,All_KDIGO_df,"Admit_KDIGO_ICU")
  Feature_df[i,"MAX_KDIGO_ICU_D0toD3"] <- get_feature_forPt(curr_id,All_KDIGO_df,"MAX_KDIGO_ICU_D0toD3")
  Feature_df[i,"LAST_KDIGO_ICU_D0toD3"] <- get_feature_forPt(curr_id,All_KDIGO_df,"LAST_KDIGO_ICU_D0toD3")
  
  
  #All_HT_WT_RESP_FIO2_df
  Feature_df[i,"FI02_D1_LOW"] <- get_feature_forPt(curr_id,All_HT_WT_RESP_FIO2_df,"FI02_D1_LOW")
  Feature_df[i,"FI02_D1_HIGH"] <- get_feature_forPt(curr_id,All_HT_WT_RESP_FIO2_df,"FI02_D1_HIGH")
  Feature_df[i,"RESP_RATE_D1_LOW"] <- get_feature_forPt(curr_id,All_HT_WT_RESP_FIO2_df,"RESP_RATE_D1_LOW")
  Feature_df[i,"RESP_RATE_D1_HIGH"] <- get_feature_forPt(curr_id,All_HT_WT_RESP_FIO2_df,"RESP_RATE_D1_HIGH")
  Feature_df[i,"HEIGHT_Meters"] <- get_feature_forPt(curr_id,All_HT_WT_RESP_FIO2_df,"HEIGHT_Meters")
  Feature_df[i,"INITIAL_WEIGHT_KG"] <- get_feature_forPt(curr_id,All_HT_WT_RESP_FIO2_df,"INITIAL_WEIGHT_KG")
  
  
  #All_MAP_TEMP_HR_df
  Feature_df[i,"HR_D1_LOW"] <- get_feature_forPt(curr_id,All_MAP_TEMP_HR_df,"HR_D1_LOW")
  Feature_df[i,"HR_D1_HIGH"] <- get_feature_forPt(curr_id,All_MAP_TEMP_HR_df,"HR_D1_HIGH")
  Feature_df[i,"MAP_D1_LOW"] <- get_feature_forPt(curr_id,All_MAP_TEMP_HR_df,"MAP_D1_LOW")
  Feature_df[i,"MAP_D1_HIGH"] <- get_feature_forPt(curr_id,All_MAP_TEMP_HR_df,"MAP_D1_HIGH")
  Feature_df[i,"Temperature_D1_LOW"] <- get_feature_forPt(curr_id,All_MAP_TEMP_HR_df,"Temperature_D1_LOW")
  Feature_df[i,"Temperature_D1_HIGH"] <- get_feature_forPt(curr_id,All_MAP_TEMP_HR_df,"Temperature_D1_HIGH")
  
  #All_septic_df
  Feature_df[i,"Sepsis_Before_or_At_Admission"] <- get_feature_forPt(curr_id,All_septic_df,"Sepsis_Before_or_At_Admission")
  
  #All_UrineOutput_df
  Feature_df[i,"UrineOutput_D0toD3"] <- get_feature_forPt(curr_id,All_UrineOutput_df,"UrineOutput")
  Feature_df[i,"UrineFlow_D0toD3"] <- get_feature_forPt(curr_id,All_UrineOutput_df,"UrineFlow")

  #All_onRRT_df
  Feature_df[i,"onRRT_ICUD0toD3"] <- get_feature_forPt(curr_id,All_onRRT_df,"onRRT_ICUD0toD3")
  
  #All_Nephrotoxin_Vasopressor_df
  Feature_df[i,"Nephrotoxin_ICUD0toD3"] <- get_feature_forPt(curr_id,All_Nephrotoxin_Vasopressor_df,"Nephrotoxin_ICUD0toD3")
  Feature_df[i,"Vasopressor_ICUD0toD3"] <- get_feature_forPt(curr_id,All_Nephrotoxin_Vasopressor_df,"Vasopressor_ICUD0toD3")
 
  #All_pO2_pCO2_pH_df
  Feature_df[i,"PO2_D1_LOW"] <- get_feature_forPt(curr_id,All_pO2_pCO2_pH_df,"PO2_D1_LOW")
  Feature_df[i,"PO2_D1_HIGH"] <- get_feature_forPt(curr_id,All_pO2_pCO2_pH_df,"PO2_D1_HIGH")
  Feature_df[i,"PCO2_D1_LOW"] <- get_feature_forPt(curr_id,All_pO2_pCO2_pH_df,"PCO2_D1_LOW")
  Feature_df[i,"PCO2_D1_HIGH"] <- get_feature_forPt(curr_id,All_pO2_pCO2_pH_df,"PCO2_D1_HIGH")
  Feature_df[i,"PH_D1_LOW"] <- get_feature_forPt(curr_id,All_pO2_pCO2_pH_df,"PH_D1_LOW")
  Feature_df[i,"PH_D1_HIGH"] <- get_feature_forPt(curr_id,All_pO2_pCO2_pH_df,"PH_D1_HIGH")
  
  #Labs
  Feature_df[i,"Bilirubin_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"Bilirubin_D1_HIGH")
  Feature_df[i,"Platelets_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"Platelets_D1_LOW")
  Feature_df[i,"Sodium_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"Sodium_D1_LOW")
  Feature_df[i,"Sodium_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"Sodium_D1_HIGH")
  
  Feature_df[i,"Potassium_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"Potassium_D1_LOW")
  Feature_df[i,"Potassium_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"Potassium_D1_HIGH")
  
  Feature_df[i,"Hematocrit_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"Hematocrit_D1_LOW")
  Feature_df[i,"Hematocrit_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"Hematocrit_D1_HIGH")
  
  Feature_df[i,"Hemoglobin_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"Hemoglobin_D1_LOW")
  Feature_df[i,"Hemoglobin_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"Hemoglobin_D1_HIGH")
  
  Feature_df[i,"WBC_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"WBC_D1_LOW")
  Feature_df[i,"WBC_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"WBC_D1_HIGH")
  
  Feature_df[i,"Bicarbonate_D1_LOW"] <- get_feature_forPt(curr_id,All_LAB_df,"Bicarbonate_D1_LOW")
  Feature_df[i,"Bicarbonate_D1_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"Bicarbonate_D1_HIGH")
  
  
  Feature_df[i,"BUN_D0toD3_HIGH"] <- get_feature_forPt(curr_id,All_LAB_df,"BUN_D0toD3_HIGH")

  #All_ELIX_df
  elix_names <- c("ELX_GRP_1","ELX_GRP_5","ELX_GRP_6","ELX_GRP_7","ELX_GRP_10","ELX_GRP_11","ELX_GRP_12",
                  "ELX_GRP_15","ELX_GRP_16","ELX_GRP_17","ELX_GRP_19","ELX_GRP_21","ELX_GRP_31")
  Feature_df[i,elix_names] <- get_feature_forPt(curr_id,All_ELIX_df,elix_names)
  
  #"Hours_inICUD0toD3"
  Feature_df[i,"Hours_inICUD0toD3"] <- get_feature_forPt(curr_id,All_time_df,"Actual_ICUHours_D0toD3")
  

}

#4. Compute missing before imputation
missing_table <- get_missing_rate_table(Feature_df,colnames(Feature_df))
missing_table
write.csv(Feature_df,paste0(outdir,"Model_Feature_Outcome/All_Feature_NOTimputed.csv"),row.names = F)

#5.imputation median
features_toimputed <- colnames(Feature_df)[-which(colnames(Feature_df) %in% c("Unplanned_Admission","STUDY_PATIENT_ID"))]
Final_Feature_df <- median_imputation_func(Feature_df,features_toimputed)
missing_table2 <- get_missing_rate_table(Final_Feature_df,features_toimputed)
missing_table2

write.csv(Feature_df,paste0(outdir,"Model_Feature_Outcome/All_Feature_imputed.csv"),row.names = F)

