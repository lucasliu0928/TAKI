library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
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

#3.Load feature data
All_pO2_pCO2_pH_df <-read.csv(paste0(outdir,"All_pO2_pCO2_pH_NOTimputed.csv"),stringsAsFactors = F)
All_HT_WT_RESP_FIO2_df <-read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_NOTimputed.csv"),stringsAsFactors = F)
All_GCS_df <-read.csv(paste0(outdir,"All_GCS_NOTimputed.csv"),stringsAsFactors = F)
All_MAP_TEMP_HR_df <-read.csv(paste0(outdir,"All_MAP_TEMP_HR_NOTimputed.csv"),stringsAsFactors = F)
All_LAB_df <-read.csv(paste0(outdir,"All_LAB_NOTimputed.csv"),stringsAsFactors = F)
All_Scr_df <-read.csv(paste0(outdir,"Scr_Baseline_Admit_Peak_NUM_ICU_D0D3_df.csv"),stringsAsFactors = F)
All_Demo_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)
All_Nephrotoxin_Vasopressor_df <-read.csv(paste0(outdir,"All_Nephrotoxin_Vasopressor.csv"),stringsAsFactors = F)
All_ECMO_IABP_MV_VAD_df <-read.csv(paste0(outdir,"All_ECMO_IABP_MV_VAD_ICUD0toD3.csv"),stringsAsFactors = F)

##########################################################################################
#1. get SOFA and apahce features
#In cases where you can’t find the information required, zero points are given for that particular condition. 
#In cases where more than one parameters match, you’ll pick the highest number. 
#In the end, you’ll have to report the sum of every condition below
##########################################################################################
#1.Get all features for SOFA in one df
SOFA_APACHE_Feature_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 34))
colnames(SOFA_APACHE_Feature_df) <- c("STUDY_PATIENT_ID",
                               "PO2_D1_LOW", "PO2_D1_HIGH",
                               "PCO2_D1_LOW" , "PCO2_D1_HIGH",
                               "FI02_D1_LOW","FI02_D1_HIGH",
                               "PH_D1_LOW","PH_D1_HIGH",
                               "Sodium_D1_LOW","Sodium_D1_HIGH",
                               "Potassium_D1_LOW","Potassium_D1_HIGH",
                               "Hematocrit_D1_LOW","Hematocrit_D1_HIGH",
                               "WBC_D1_LOW","WBC_D1_HIGH",
                               "onMV_inICU_D0toD3",
                               "GCS",
                               "MAP_D1_LOW","MAP_D1_HIGH",
                               "HR_D1_LOW","HR_D1_HIGH",
                               "Use_of_Dopamine_OR_Dobutamine_OR_Milrinone",
                               "Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin",
                               "Bilirubin_D1_HIGH",
                               "Platelets_D1_LOW",
                               "Peak_SCr_inICU_D0toD3","Lowest_SCr_inICU_D0_D3",
                               "Temperature_D1_LOW","Temperature_D1_HIGH",
                               "RESP_RATE_D1_LOW","RESP_RATE_D1_HIGH",
                               "AGE")

for (i in 1:length(analysis_ID)){
  if(i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  SOFA_APACHE_Feature_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #get pt df
  curr_pO2_pCO2_pH_df <- All_pO2_pCO2_pH_df[which(All_pO2_pCO2_pH_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_HT_WT_RESP_FIO2_BMI_df <- All_HT_WT_RESP_FIO2_df[which(All_HT_WT_RESP_FIO2_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_ECMO_IABP_MV_VAD_df <- All_ECMO_IABP_MV_VAD_df[which(All_ECMO_IABP_MV_VAD_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_gcs_df <- All_GCS_df[which(All_GCS_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_MAP_TEMP_HR_df <- All_MAP_TEMP_HR_df[which(All_MAP_TEMP_HR_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_Nephrotoxin_Vasopressor_df <- All_Nephrotoxin_Vasopressor_df[which(All_Nephrotoxin_Vasopressor_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_lab_df <-  All_LAB_df[which(All_LAB_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_scr_df <-  All_Scr_df[which(All_Scr_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_demo_df <-  All_Demo_df[which(All_Demo_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Get feature
  SOFA_APACHE_Feature_df[i,"PO2_D1_LOW"] <- curr_pO2_pCO2_pH_df[,"PO2_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"PO2_D1_HIGH"] <- curr_pO2_pCO2_pH_df[,"PO2_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"PCO2_D1_LOW"] <- curr_pO2_pCO2_pH_df[,"PCO2_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"PCO2_D1_HIGH"] <- curr_pO2_pCO2_pH_df[,"PCO2_D1_HIGH"]

  SOFA_APACHE_Feature_df[i,"FI02_D1_LOW"] <- curr_HT_WT_RESP_FIO2_BMI_df[,"FI02_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"FI02_D1_HIGH"] <- curr_HT_WT_RESP_FIO2_BMI_df[,"FI02_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"PH_D1_LOW"] <- curr_pO2_pCO2_pH_df[,"PH_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"PH_D1_HIGH"] <- curr_pO2_pCO2_pH_df[,"PH_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"Sodium_D1_LOW"] <-  curr_lab_df[,"Sodium_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"Sodium_D1_HIGH"] <-  curr_lab_df[,"Sodium_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"Potassium_D1_LOW"] <-  curr_lab_df[,"Potassium_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"Potassium_D1_HIGH"] <-  curr_lab_df[,"Potassium_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"Hematocrit_D1_LOW"] <-  curr_lab_df[,"Hematocrit_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"Hematocrit_D1_HIGH"] <-  curr_lab_df[,"Hematocrit_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"WBC_D1_LOW"] <-  curr_lab_df[,"WBC_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"WBC_D1_HIGH"] <-  curr_lab_df[,"WBC_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"onMV_inICU_D0toD3"] <-  curr_ECMO_IABP_MV_VAD_df[,"MV_ICUD0toD3"]
  SOFA_APACHE_Feature_df[i,"GCS"] <-  curr_gcs_df[,"Updated_GCS_D1_LOW"]
  
  SOFA_APACHE_Feature_df[i,"MAP_D1_LOW"] <-  curr_MAP_TEMP_HR_df[,"MAP_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"MAP_D1_HIGH"] <-  curr_MAP_TEMP_HR_df[,"MAP_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"HR_D1_LOW"] <-  curr_MAP_TEMP_HR_df[,"HR_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"HR_D1_HIGH"] <-  curr_MAP_TEMP_HR_df[,"HR_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"Bilirubin_D1_HIGH"] <-  curr_lab_df[,"Bilirubin_D1_HIGH"]
  SOFA_APACHE_Feature_df[i,"Platelets_D1_LOW"] <-  curr_lab_df[,"Platelets_D1_LOW"]
  
  SOFA_APACHE_Feature_df[i,"Peak_SCr_inICU_D0toD3"] <-  curr_scr_df[,"Peak_SCr_inICU_D0_D3"]
  SOFA_APACHE_Feature_df[i,"Lowest_SCr_inICU_D0_D3"] <-  curr_scr_df[,"Lowest_SCr_inICU_D0_D3"]
  
  
  SOFA_APACHE_Feature_df[i,"Temperature_D1_LOW"] <-  curr_MAP_TEMP_HR_df[,"Temperature_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"Temperature_D1_HIGH"] <-  curr_MAP_TEMP_HR_df[,"Temperature_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"RESP_RATE_D1_LOW"] <- curr_HT_WT_RESP_FIO2_BMI_df[,"RESP_RATE_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"RESP_RATE_D1_HIGH"] <- curr_HT_WT_RESP_FIO2_BMI_df[,"RESP_RATE_D1_HIGH"]
  
  
  SOFA_APACHE_Feature_df[i,"AGE"] <-  curr_demo_df[,"AGE"]
  
  search_string1 <- "Dopamine|Dobutamine|Milrinone"
  search_string2 <- "Epinephrine|Norepinephrine|Phenylephrine|Vasopressin"
  
  if (grepl(search_string1,curr_Nephrotoxin_Vasopressor_df[,"Vasopressor_Meds_ICUD0toD3"],ignore.case = T)==T){
    SOFA_APACHE_Feature_df[i,"Use_of_Dopamine_OR_Dobutamine_OR_Milrinone"] <- 1
  }else{
    SOFA_APACHE_Feature_df[i,"Use_of_Dopamine_OR_Dobutamine_OR_Milrinone"] <- 0
  }
  
  if (grepl(search_string2,curr_Nephrotoxin_Vasopressor_df[,"Vasopressor_Meds_ICUD0toD3"],ignore.case = T)){
    SOFA_APACHE_Feature_df[i,"Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin"] <- 1
  }else{
    SOFA_APACHE_Feature_df[i,"Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin"] <- 0
  }
  
}

##########################################################################################
#Compute SOFA score columns
##########################################################################################
SOFA_SCORE_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 7))
colnames(SOFA_SCORE_df) <- c("STUDY_PATIENT_ID","SOFA_pO2_FiO2","SOFA_GCS","SOFA_MAP_Pressors",
                                    "SOFA_Bilirubin","SOFA_Platelets","SOFA_sCr")

for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  SOFA_SCORE_df[i,"STUDY_PATIENT_ID"] <- curr_id
  curr_feature_df <- SOFA_APACHE_Feature_df[which(SOFA_APACHE_Feature_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
   curr_PO2_D1_LOW <- curr_feature_df[,"PO2_D1_LOW"]
   curr_FI02_D1_HIGH <- curr_feature_df[,"FI02_D1_HIGH"]
   curr_onMV <- curr_feature_df[,"onMV_inICU_D0toD3"]
   curr_gcs <- curr_feature_df[,"GCS"]
   curr_map <- curr_feature_df[,"MAP_D1_LOW"]
   curr_Use_DDM <- curr_feature_df[,"Use_of_Dopamine_OR_Dobutamine_OR_Milrinone"]
   curr_Use_ENPV <- curr_feature_df[,"Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin"]
   curr_Bilirubin_D1_HIGH <- curr_feature_df[,"Bilirubin_D1_HIGH"]
   curr_Platelets_D1_LOW <- curr_feature_df[,"Platelets_D1_LOW"]
   curr_Peak_SCr_inICU_D0toD3 <- curr_feature_df[,"Peak_SCr_inICU_D0toD3"]

   SOFA_SCORE_df[i,"SOFA_pO2_FiO2"] <-  sofa1_func(curr_PO2_D1_LOW,curr_FI02_D1_HIGH,curr_onMV)
   SOFA_SCORE_df[i,"SOFA_GCS"] <-  sofa2_func(curr_gcs)
   SOFA_SCORE_df[i,"SOFA_MAP_Pressors"] <-  sofa3_func(curr_map,curr_Use_DDM,curr_Use_ENPV)
   SOFA_SCORE_df[i,"SOFA_Bilirubin"] <-  sofa4_func(curr_Bilirubin_D1_HIGH)
   SOFA_SCORE_df[i,"SOFA_Platelets"] <-  sofa5_func(curr_Platelets_D1_LOW)
   SOFA_SCORE_df[i,"SOFA_sCr"] <-  sofa6_func(curr_Peak_SCr_inICU_D0toD3)
   

}

#Total Score
SOFA_SCORE_df$SOFA_TOTAL <- rowSums(SOFA_SCORE_df[,2:7])

##########################################################################################
#Compute APACHE score columns
##########################################################################################
APACHE_SCORE_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 14))
colnames(APACHE_SCORE_df) <- c("STUDY_PATIENT_ID","APACHE_Temp","APACHE_MAP","APACHE_HR","APACHE_Resp",
                               "APACHE_OxygentionPO2",
                               "APACHE_PH","APACHE_Sodium","APACHE_Potassium","APACHE_sCr",
                               "APACHE_Hematocrit","APACHE_WBC","APACHE_GCS","APACHE_AGE")
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  APACHE_SCORE_df[i,"STUDY_PATIENT_ID"] <- curr_id
  curr_feature_df <- SOFA_APACHE_Feature_df[which(SOFA_APACHE_Feature_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Apache temp: use both low and high value, use the one give the highest score
  curr_Temperature_D1_LOW <- curr_feature_df[,"Temperature_D1_LOW"]
  curr_Temperature_D1_HIGH <- curr_feature_df[,"Temperature_D1_HIGH"]
  curr_apache_temp_low <- apache1_func(curr_Temperature_D1_LOW)
  curr_apache_temp_high <- apache1_func(curr_Temperature_D1_HIGH)
  APACHE_SCORE_df[i,"APACHE_Temp"]<- max(c(curr_apache_temp_low,curr_apache_temp_high), na.rm = T)
  
  #Apache MAP:
  curr_map_low <- curr_feature_df[,"MAP_D1_LOW"]
  curr_map_high <- curr_feature_df[,"MAP_D1_HIGH"]
  curr_apache_map_low <- apache2_func(curr_map_low)
  curr_apache_map_high <- apache2_func(curr_map_high)
  APACHE_SCORE_df[i,"APACHE_MAP"]<- max(c(curr_apache_map_low,curr_apache_map_high), na.rm = T)
  
  #Apache HR:
  curr_hr_low <- curr_feature_df[,"HR_D1_LOW"]
  curr_hr_high <- curr_feature_df[,"HR_D1_HIGH"]
  curr_apache_hr_low <- apache3_func(curr_hr_low)
  curr_apache_hr_high <- apache3_func(curr_hr_high)
  APACHE_SCORE_df[i,"APACHE_HR"]<- max(c(curr_apache_hr_low,curr_apache_hr_high), na.rm = T)
  

  #Apache Resp Rate:
  curr_resp_low <- curr_feature_df[,"RESP_RATE_D1_LOW"]
  curr_resp_high <- curr_feature_df[,"RESP_RATE_D1_HIGH"]
  curr_apache_resp_low <- apache4_func(curr_resp_low)
  curr_apache_resp_high <- apache4_func(curr_resp_high)
  APACHE_SCORE_df[i,"APACHE_Resp"]<- max(c(curr_apache_resp_low,curr_apache_resp_high), na.rm = T)


  #Apache OxygentionPO2:
  curr_FI02_D1_low<- curr_feature_df[,"FI02_D1_LOW"]
  curr_FI02_D1_high <- curr_feature_df[,"FI02_D1_HIGH"]
  
  curr_pCO2_low <- curr_feature_df[,"PCO2_D1_LOW"]
  curr_pCO2_high <- curr_feature_df[,"PCO2_D1_HIGH"]
  
  curr_po2_low <- curr_feature_df[,"PO2_D1_LOW"]
  curr_po2_high <- curr_feature_df[,"PO2_D1_HIGH"]
  
  curr_apache5_score1 <- apache5_func(curr_FI02_D1_low,curr_pCO2_low,curr_po2_low)
  curr_apache5_score2 <- apache5_func(curr_FI02_D1_low,curr_pCO2_high,curr_po2_low)
  curr_apache5_score3 <- apache5_func(curr_FI02_D1_low,curr_pCO2_low,curr_po2_high)
  curr_apache5_score4 <- apache5_func(curr_FI02_D1_low,curr_pCO2_high,curr_po2_high)
  curr_apache5_score5 <- apache5_func(curr_FI02_D1_high,curr_pCO2_low,curr_po2_low)
  curr_apache5_score6 <- apache5_func(curr_FI02_D1_high,curr_pCO2_high,curr_po2_low)
  curr_apache5_score7 <- apache5_func(curr_FI02_D1_high,curr_pCO2_low,curr_po2_high)
  curr_apache5_score8 <- apache5_func(curr_FI02_D1_high,curr_pCO2_high,curr_po2_high)
  apache5_socre_list <- c(curr_apache5_score1,curr_apache5_score2,curr_apache5_score3,curr_apache5_score4,
                         curr_apache5_score5,curr_apache5_score6,curr_apache5_score7,curr_apache5_score8)

  APACHE_SCORE_df[i,"APACHE_OxygentionPO2"]<- max(apache5_socre_list, na.rm = T)
  
  
  
  #6. Apache PH:
  curr_ph_low <- curr_feature_df[,"PH_D1_LOW"]
  curr_ph_high <- curr_feature_df[,"PH_D1_HIGH"]
  curr_apache_ph_low <- apache6_func(curr_ph_low)
  curr_apache_ph_high <- apache6_func(curr_ph_high)
  APACHE_SCORE_df[i,"APACHE_PH"]<- max(c(curr_apache_ph_low,curr_apache_ph_high), na.rm = T)
  
  

  
  #7. Apache sodium:
  curr_sodium_low <- curr_feature_df[,"Sodium_D1_LOW"]
  curr_sodium_high <- curr_feature_df[,"Sodium_D1_HIGH"]
  curr_apache_sodium_low <- apache7_func(curr_sodium_low)
  curr_apache_sodium_high <- apache7_func(curr_sodium_high)
  APACHE_SCORE_df[i,"APACHE_Sodium"]<- max(c(curr_apache_sodium_low,curr_apache_sodium_high), na.rm = T)
  
  

  
  
  #8. Apache potassium:
  curr_pot_low <- curr_feature_df[,"Potassium_D1_LOW"]
  curr_pot_high <- curr_feature_df[,"Potassium_D1_HIGH"]
  curr_apache_pot_low <- apache8_func(curr_pot_low)
  curr_apache_pot_high <- apache8_func(curr_pot_high)
  APACHE_SCORE_df[i,"APACHE_Potassium"]<- max(c(curr_apache_pot_low,curr_apache_pot_high), na.rm = T)
  

  #9. Apache sCR: #use min and peak
  curr_Lowest_SCr_inICU_D0toD3 <- curr_feature_df[,"Lowest_SCr_inICU_D0_D3"]
  curr_Peak_SCr_inICU_D0toD3 <- curr_feature_df[,"Peak_SCr_inICU_D0toD3"]
  curr_apache_Scr_low <- apache9_func(curr_Lowest_SCr_inICU_D0toD3)
  curr_apache_Scr_high <- apache9_func(curr_Peak_SCr_inICU_D0toD3)
  APACHE_SCORE_df[i,"APACHE_sCr"]<- max(c(curr_apache_Scr_low,curr_apache_Scr_high), na.rm = T)
  

  

  #10. Apache APACHE_Hematocrit
  curr_Hematocrit_low <- curr_feature_df[,"Hematocrit_D1_LOW"]
  curr_Hematocrit_high <- curr_feature_df[,"Hematocrit_D1_HIGH"]
  curr_apache_hemat_low <- apache10_func(curr_Hematocrit_low)
  curr_apache_hemat_high <- apache10_func(curr_Hematocrit_high)
  APACHE_SCORE_df[i,"APACHE_Hematocrit"]<- max(c(curr_apache_hemat_low,curr_apache_hemat_high), na.rm = T)
  

  #11. Apache APACHE_WBC
  curr_wbc_low <- curr_feature_df[,"WBC_D1_LOW"]
  curr_wbc_high <- curr_feature_df[,"WBC_D1_HIGH"]
  curr_apache_wbc_low <- apache11_func(curr_wbc_low)
  curr_apache_wbc_high <- apache11_func(curr_wbc_high)
  APACHE_SCORE_df[i,"APACHE_WBC"]<- max(c(curr_apache_wbc_low,curr_apache_wbc_high), na.rm = T)

  
  #12. Apache APACHE_GCS
  curr_gcs <- curr_feature_df[,"GCS"]
  APACHE_SCORE_df[i,"APACHE_GCS"] <- apache12_func(curr_gcs)
  
  #13. Apache APACHE_AGE
  curr_age <- curr_feature_df[,"AGE"]
  APACHE_SCORE_df[i,"APACHE_AGE"] <- apache13_func(curr_age)
  


}

APACHE_SCORE_df$APACHE_TOTAL <- rowSums(APACHE_SCORE_df[,2:14])


##########################################################################################
#Combine SOFA and APACHE and features 
##########################################################################################
#Check ID order
identical(SOFA_SCORE_df$STUDY_PATIENT_ID, APACHE_SCORE_df$STUDY_PATIENT_ID)
#combine
SOFA_APACHE_SCORE_df <- cbind(SOFA_SCORE_df,APACHE_SCORE_df[,-1])

#Combine with feature
identical(SOFA_APACHE_SCORE_df$STUDY_PATIENT_ID,SOFA_APACHE_Feature_df$STUDY_PATIENT_ID)
SOFA_APACHE_With_Feature_df <- cbind(SOFA_APACHE_SCORE_df, SOFA_APACHE_Feature_df[,-1])

write.csv(SOFA_APACHE_With_Feature_df,paste0(outdir,"All_SOFA_APACHE_With_NotImputedFeature.csv"),row.names = F)

