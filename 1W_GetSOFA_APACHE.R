library(lubridate)
source("TAKI_Ultility.R")

sofa1_func <- function(po2,fio2,onMV_flag){
  if (is.na(po2) == T & is.na(fio2) == T){ #if one value is not available
    highest_score <- 0
  }else{
    po2_fio2_ratio <- (po2/fio2)*100
    
    if (po2_fio2_ratio >= 400){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    if(po2_fio2_ratio < 400){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    if(po2_fio2_ratio < 300){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    if(po2_fio2_ratio < 200 & onMV_flag == 1){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    if(po2_fio2_ratio < 100 & onMV_flag == 1){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}

sofa2_func <- function(GCS_value){
  if (is.na(GCS_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    if (GCS_value > 14){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if(GCS_value >=13 & GCS_value <= 14 ){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    if(GCS_value >=10 & GCS_value < 13 ){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    if(GCS_value >=6 & GCS_value < 10 ){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    if(GCS_value < 6){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}

sofa3_func <- function(MAP_value,Use_DDM,UseENPV){
  
  
  if (is.na(MAP_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    if (MAP_value >= 70){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if(MAP_value < 70){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if(Use_DDM == 1){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if(UseENPV == 1){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}

sofa4_func <- function(bilirubin_value){
  if (is.na(bilirubin_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (bilirubin_value < 1.2){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if(bilirubin_value  >= 1.2 & bilirubin_value < 2){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if(bilirubin_value  >= 2 & bilirubin_value < 6){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if(bilirubin_value  >= 6.0 & bilirubin_value < 12){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    
    if(bilirubin_value  >= 12){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}

sofa5_func <- function(Platelets_value){
  if (is.na(Platelets_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (Platelets_value >= 150){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if(Platelets_value < 150){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if(Platelets_value < 100){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if(Platelets_value < 50){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    
    if(Platelets_value < 20){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}

sofa6_func <- function(sCr_Value){
  if (is.na(sCr_Value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (sCr_Value < 1.2){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if(sCr_Value >= 1.2 & sCr_Value < 2){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if(sCr_Value >= 2 & sCr_Value < 3.5){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if(sCr_Value >= 3.5 & sCr_Value < 5){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    
    if(sCr_Value >= 5){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}


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
All_pO2_pCO2_pH_df <-read.csv(paste0(outdir,"All_pO2_pCO2_pH_imputed.csv"),stringsAsFactors = F)
All_HT_WT_RESP_FIO2_BMI_df <-read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_BMI_df.csv"),stringsAsFactors = F)
All_GCS_df <-read.csv(paste0(outdir,"All_GCS_df.csv"),stringsAsFactors = F)
All_MAP_TEMP_HR_df <-read.csv(paste0(outdir,"All_MAP_TEMP_HR_imputed.csv"),stringsAsFactors = F)
All_LAB_df <-read.csv(paste0(outdir,"All_LAB_imputed.csv"),stringsAsFactors = F)
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
SOFA_APACHE_Feature_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 30))
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
                               "MAP_D1_LOW","Use_of_Dopamine_OR_Dobutamine_OR_Milrinone",
                               "Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin",
                               "Bilirubin_D1_HIGH",
                               "Platelets_D1_LOW",
                               "Peak_SCr_inICU_D0toD3",
                               "Temperature_D1_LOW","Temperature_D1_HIGH",
                               "RESP_RATE_D1_LOW","RESP_RATE_D1_HIGH",
                               "AGE")


for (i in 1:length(analysis_ID)){
  if(i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  SOFA_APACHE_Feature_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #get pt df
  curr_pO2_pCO2_pH_df <- All_pO2_pCO2_pH_df[which(All_pO2_pCO2_pH_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_HT_WT_RESP_FIO2_BMI_df <- All_HT_WT_RESP_FIO2_BMI_df[which(All_HT_WT_RESP_FIO2_BMI_df[,"STUDY_PATIENT_ID"] == curr_id),]
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
  SOFA_APACHE_Feature_df[i,"Bilirubin_D1_HIGH"] <-  curr_lab_df[,"Bilirubin_D1_HIGH"]
  SOFA_APACHE_Feature_df[i,"Platelets_D1_LOW"] <-  curr_lab_df[,"Platelets_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"Peak_SCr_inICU_D0toD3"] <-  curr_scr_df[,"Peak_SCr_inICU_D0_D3"]

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
  
   curr_PO2_D1_HIGH <- curr_feature_df[,"PO2_D1_HIGH"]
   curr_FI02_D1_HIGH <- curr_feature_df[,"FI02_D1_HIGH"]
   curr_onMV <- curr_feature_df[,"onMV_inICU_D0toD3"]
   curr_gcs <- curr_feature_df[,"GCS"]
   curr_map <- curr_feature_df[,"MAP_D1_LOW"]
   curr_Use_DDM <- curr_feature_df[,"Use_of_Dopamine_OR_Dobutamine_OR_Milrinone"]
   curr_Use_ENPV <- curr_feature_df[,"Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin"]
   curr_Bilirubin_D1_HIGH <- curr_feature_df[,"Bilirubin_D1_HIGH"]
   curr_Platelets_D1_LOW <- curr_feature_df[,"Platelets_D1_LOW"]
   curr_Peak_SCr_inICU_D0toD3 <- curr_feature_df[,"Peak_SCr_inICU_D0toD3"]
   

   SOFA_SCORE_df[i,"SOFA_pO2_FiO2"] <-  sofa1_func(curr_PO2_D1_HIGH,curr_FI02_D1_HIGH,curr_onMV)
   SOFA_SCORE_df[i,"SOFA_GCS"] <-  sofa2_func(curr_gcs)
   SOFA_SCORE_df[i,"SOFA_MAP_Pressors"] <-  sofa3_func(curr_map,curr_Use_DDM,curr_Use_ENPV)
   SOFA_SCORE_df[i,"SOFA_Bilirubin"] <-  sofa4_func(curr_Bilirubin_D1_HIGH)
   SOFA_SCORE_df[i,"SOFA_Platelets"] <-  sofa5_func(curr_Platelets_D1_LOW)
   SOFA_SCORE_df[i,"SOFA_sCr"] <-  sofa6_func(curr_Peak_SCr_inICU_D0toD3)
   

}

SOFA_SCORE_df$SOFA_TOTAL <- rowSums(SOFA_SCORE_df[,2:7])


#Combine feature df and sofa df for check
SOFA_With_Feature_df <- cbind(SOFA_SCORE_df, SOFA_APACHE_Feature_df[,-1])


write.csv(SOFA_With_Feature_df,paste0(outdir,"All_SOFA_With_Feature_df.csv"),row.names = F)

##########################################################################################
#Compute APACHE score columns
##########################################################################################
APACHE_SCORE_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 14))
colnames(APACHE_SCORE_df) <- c("STUDY_PATIENT_ID","APACHE_Temp","APACHE_MAP","APACHE_HR","APACHE_Resp","APACHE_OxygentionPO2",
                               "APACHE_PH","APACHE_Sodium","APACHE_Potassium","APACHE_sCr",
                               "APACHE_Hematocrit","APACHE_WBC","APACHE_GCS","APACHE_AGE")

