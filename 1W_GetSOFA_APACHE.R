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


apche1_func <- function(temp){
  if (is.na(temp) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (temp >= 36 & temp < 38.5){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if( (temp >= 38.5 & temp < 39) | (temp >=34 & temp < 36)){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if(temp >=32 & temp < 34 ){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if( (temp >=39 & temp < 41) | (temp >=30 & temp < 32)){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    if(temp >=41 | temp < 30){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}

apche2_func <- function(map_value){
  if (is.na(map_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (map_value >= 70 & map_value < 109){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if( (map_value >= 110 & map_value < 130) | (map_value >=50 & map_value < 70)){
      scoreB <- 2
    }else{
      scoreB <- NA
    }
    
    if(map_value >=130 & map_value < 160 ){
      scoreC <- 3
    }else{
      scoreC <- NA
    }
    
    if( map_value >= 160 |  map_value < 50){
      scoreD <- 4
    }else{
      scoreD <- NA
    }
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}


apche3_func <- function(hr_value){
  if (is.na(hr_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (hr_value >= 70 & hr_value < 109){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if( (hr_value >= 110 & hr_value < 140) | (hr_value >=55 & hr_value < 70)){
      scoreB <- 2
    }else{
      scoreB <- NA
    }
    
    if( (hr_value >= 140 & hr_value < 180) | (hr_value >=40 & hr_value < 55) ){
      scoreC <- 3
    }else{
      scoreC <- NA
    }
    
    if( hr_value >= 180 |  hr_value < 40){
      scoreD <- 4
    }else{
      scoreD <- NA
    }
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}
apche4_func <- function(resp_value){
  if (is.na(resp_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (resp_value >= 12 & resp_value < 25){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if( (resp_value >= 25 & resp_value < 35) | (resp_value >=10 & resp_value < 11)){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if( resp_value >= 6 & resp_value < 10 ){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if(resp_value >= 35 & resp_value < 50 ){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    
    if(resp_value >= 50 & resp_value < 6){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}
apche5_func <- function(fio2_value,pco2_value,po2_value){
  if (is.na(fio2_value) == T | is.na(pco2_value) == T | is.na(po2_value) == T ){ #if one value is not available
    highest_score <- 0
  }else{
    
    
    if (fio2_value >= 0.5){ #use A-aDO2 formula 
      A_aDO2_score <- fio2_value*713 - (pco2_value/0.8) - po2_value
      
      if (A_aDO2_score < 200 ){
        highest_score <- 0
      }else if (A_aDO2_score >= 200 & A_aDO2_score < 350) {
        highest_score <- 2
      }else if (A_aDO2_score >= 350 & A_aDO2_score < 500) {
        highest_score <- 3
      }else if (A_aDO2_score >= 500 ) {
        highest_score <- 4
      }
      
    }else if (fio2_value < 0.5){ #use PO2
      
      if (po2_value >= 70 ){
        highest_score <- 0
      }else if (po2_value >= 61 & po2_value < 70) {
        highest_score <- 1
      }else if (po2_value >= 55 & po2_value < 61) {
        highest_score <- 3
      }else if (po2_value < 55) {
        highest_score <- 4
      }
    }
    
  }
  return(highest_score)
}
apche6_func <- function(ph_value){
  if (is.na(ph_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (ph_value >= 7.33 & ph_value < 7.50){
      scoreA <- 0
    }else{
      scoreA <- NA
    }
    
    if (ph_value >= 7.50 & ph_value < 7.6){
      scoreB <- 1
    }else{
      scoreB <- NA
    }
    
    if (ph_value >= 7.25 & ph_value < 7.33){
      scoreC <- 2
    }else{
      scoreC <- NA
    }
    
    if( (ph_value >= 7.6 & ph_value < 7.7) | (ph_value > 7.15 & ph_value < 7.25)){
      scoreD <- 3
    }else{
      scoreD <- NA
    }
    
    if(ph_value >= 7.7 & ph_value <= 7.15){
      scoreE <- 4
    }else{
      scoreE <- NA
    }
    
    
    all_possbile_scores <- c(scoreA,scoreB,scoreC,scoreD,scoreE)
    highest_score <- max(all_possbile_scores,na.rm = T)
  }
  return(highest_score)
}
apche7_func <- function(sodium_value){
  if (is.na(sodium_value) == T){ #if one value is not available
    highest_score <- 0
  }else{
    
    if (sodium_value >= 130 & sodium_value < 150){
      highest_score <- 0
    }else if (sodium_value >= 150 & sodium_value < 155){
      highest_score <- 1
    } else if ( (sodium_value >= 155 & sodium_value < 160) | (sodium_value >= 120 & sodium_value < 130)){
      highest_score <- 2
    }else if( (sodium_value >= 160 & sodium_value < 180) | (sodium_value > 110 & sodium_value < 120)){
      highest_score <- 3
    }else if(sodium_value >= 180 | sodium_value <= 110){
      highest_score <- 4
    }else{
      highest_score <- NA
    }
    
  }
  return(highest_score)
}
apche8_func <- function(pot_value){
  if (is.na(pot_value) == T){ #if one value is not available
    apche_score <- 0
  }else{
    
    if (pot_value >= 3.5 & pot_value < 5.5){
      apche_score <- 0
    }else if ( (pot_value >= 5.5 & pot_value < 6.0) | (pot_value >= 3.0 & pot_value < 3.5)){
      apche_score <- 1
    }else if (pot_value >= 2.5 & pot_value < 3.0){
      apche_score <- 2
    }else if (pot_value >= 6.0 & pot_value < 7){
      apche_score <- 3
    }else if(pot_value >= 7 | pot_value < 2.5){
      apche_score <- 4
    }else{
      apche_score <- NA
    }
    
  }
  return(apche_score)
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
SOFA_APACHE_Feature_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 33))
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
                               "MAP_D1_LOW","MAP_D1__HIGH",
                               "HR_D1_LOW","HR_D1_HIGH",
                               "Use_of_Dopamine_OR_Dobutamine_OR_Milrinone",
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
  SOFA_APACHE_Feature_df[i,"MAP_D1_HIGH"] <-  curr_MAP_TEMP_HR_df[,"MAP_D1_HIGH"]
  
  SOFA_APACHE_Feature_df[i,"HR_D1_LOW"] <-  curr_MAP_TEMP_HR_df[,"HR_D1_LOW"]
  SOFA_APACHE_Feature_df[i,"HR_D1_HIGH"] <-  curr_MAP_TEMP_HR_df[,"HR_D1_HIGH"]
  
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


#write.csv(SOFA_With_Feature_df,paste0(outdir,"All_SOFA_With_Feature_df.csv"),row.names = F)

##########################################################################################
#Compute APACHE score columns
##########################################################################################
APACHE_SCORE_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 14))
colnames(APACHE_SCORE_df) <- c("STUDY_PATIENT_ID","APACHE_Temp","APACHE_MAP","APACHE_HR","APACHE_Resp",
                               "APACHE_OxygentionPO2",
                               "APACHE_PH","APACHE_Sodium","APACHE_Potassium","APACHE_sCr",
                               "APACHE_Hematocrit","APACHE_WBC","APACHE_GCS","APACHE_AGE")
for (i in 1:length(analysis_ID)){
  i <- 1
  curr_id <- analysis_ID[i]
  APACHE_SCORE_df[i,"STUDY_PATIENT_ID"] <- curr_id
  curr_feature_df <- SOFA_APACHE_Feature_df[which(SOFA_APACHE_Feature_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  #Apache temp: use both low and high value, use the one give the highest score
  curr_Temperature_D1_LOW <- curr_feature_df[,"Temperature_D1_LOW"]
  curr_Temperature_D1_HIGH <- curr_feature_df[,"Temperature_D1_HIGH"]
  curr_apche_temp_low <- apche1_func(curr_Temperature_D1_LOW)
  curr_apache_temp_high <- apche1_func(curr_Temperature_D1_HIGH)
  APACHE_SCORE_df[i,"APACHE_Temp"]<- max(c(curr_apche_temp_low,curr_apache_temp_high), na.rm = T)
  
  #Apache MAP:
  curr_map_low <- curr_feature_df[,"MAP_D1_LOW"]
  curr_map_high <- curr_feature_df[,"MAP_D1_HIGH"]
  curr_apche_map_low <- apche2_func(curr_map_low)
  curr_apache_map_high <- apche2_func(curr_map_high)
  APACHE_SCORE_df[i,"APACHE_MAP"]<- max(c(curr_apche_map_low,curr_apache_map_high), na.rm = T)
  
  #Apache HR:
  curr_hr_low <- curr_feature_df[,"HR_D1_LOW"]
  curr_hr_high <- curr_feature_df[,"HR_D1_HIGH"]
  curr_apche_hr_low <- apche3_func(curr_hr_low)
  curr_apache_hr_high <- apche3_func(curr_hr_high)
  APACHE_SCORE_df[i,"APACHE_HR"]<- max(c(curr_apche_hr_low,curr_apache_hr_high), na.rm = T)
  

  #Apache Resp Rate:
  curr_resp_low <- curr_feature_df[,"RESP_RATE_D1_LOW"]
  curr_resp_high <- curr_feature_df[,"RESP_RATE_D1_HIGH"]
  curr_apche_resp_low <- apche4_func(curr_resp_low)
  curr_apache_resp_high <- apche4_func(curr_resp_high)
  APACHE_SCORE_df[i,"APACHE_Resp"]<- max(c(curr_apche_resp_low,curr_apache_resp_high), na.rm = T)


  #Apache OxygentionPO2:
  curr_FI02_D1_low<- curr_feature_df[,"FI02_D1_LOW"]
  curr_FI02_D1_high <- curr_feature_df[,"FI02_D1_HIGH"]
  
  curr_pCO2_low <- curr_feature_df[,"PCO2_D1_LOW"]
  curr_pCO2_high <- curr_feature_df[,"PCO2_D1_HIGH"]
  
  curr_po2_low <- curr_feature_df[,"PO2_D1_LOW"]
  curr_po2_high <- curr_feature_df[,"PO2_D1_HIGH"]
  
  curr_apche5_score1 <- apche5_func(curr_FI02_D1_low,curr_pCO2_low,curr_po2_low)
  curr_apche5_score2 <- apche5_func(curr_FI02_D1_low,curr_pCO2_high,curr_po2_low)
  curr_apche5_score3 <- apche5_func(curr_FI02_D1_low,curr_pCO2_low,curr_po2_high)
  curr_apche5_score4 <- apche5_func(curr_FI02_D1_low,curr_pCO2_high,curr_po2_high)
  curr_apche5_score5 <- apche5_func(curr_FI02_D1_high,curr_pCO2_low,curr_po2_low)
  curr_apche5_score6 <- apche5_func(curr_FI02_D1_high,curr_pCO2_high,curr_po2_low)
  curr_apche5_score7 <- apche5_func(curr_FI02_D1_high,curr_pCO2_low,curr_po2_high)
  curr_apche5_score8 <- apche5_func(curr_FI02_D1_high,curr_pCO2_high,curr_po2_high)
  apche5_socre_list <- c(curr_apche5_score1,curr_apche5_score2,curr_apche5_score3,curr_apche5_score4,
                         curr_apche5_score5,curr_apche5_score6,curr_apche5_score7,curr_apche5_score8)

  APACHE_SCORE_df[i,"APACHE_OxygentionPO2"]<- max(apche5_socre_list, na.rm = T)
  
  
  
  #6. Apache PH:
  curr_ph_low <- curr_feature_df[,"PH_D1_LOW"]
  curr_ph_high <- curr_feature_df[,"PH_D1_HIGH"]
  curr_apche_ph_low <- apche6_func(curr_ph_low)
  curr_apache_ph_high <- apche6_func(curr_ph_high)
  APACHE_SCORE_df[i,"APACHE_PH"]<- max(c(curr_apche_ph_low,curr_apache_ph_high), na.rm = T)
  
  

  
  #7. Apache sodium:
  curr_sodium_low <- curr_feature_df[,"Sodium_D1_LOW"]
  curr_sodium_high <- curr_feature_df[,"Sodium_D1_HIGH"]
  curr_apche_sodium_low <- apche7_func(curr_sodium_low)
  curr_apche_sodium_high <- apche7_func(curr_sodium_high)
  APACHE_SCORE_df[i,"APACHE_Sodium"]<- max(c(curr_apche_sodium_low,curr_apche_sodium_high), na.rm = T)
  
  

  
  
  #8. Apache potassium:
  curr_pot_low <- curr_feature_df[,"Potassium_D1_LOW"]
  curr_pot_high <- curr_feature_df[,"Potassium_D1_HIGH"]
  curr_apche_pot_low <- apche8_func(curr_pot_low)
  curr_apche_pot_high <- apche8_func(curr_pot_high)
  APACHE_SCORE_df[i,"APACHE_Potassium"]<- max(c(curr_apche_pot_low,curr_apche_pot_high), na.rm = T)
  
  
  #9. Apache sCR: #use min and peak
  curr_Lowest_SCr_inICU_D0toD3 <- curr_feature_df[,"Lowest_SCr_inICU_D0_D3"]
  curr_Peak_SCr_inICU_D0toD3 <- curr_feature_df[,"Peak_SCr_inICU_D0toD3"]
  curr_apche_Scr_low <- apche9_func(curr_Lowest_SCr_inICU_D0toD3)
  curr_apche_Scr_high <- apche9_func(curr_Peak_SCr_inICU_D0toD3)
  APACHE_SCORE_df[i,"APACHE_sCr"]<-  apche9_func(curr_Peak_SCr_inICU_D0toD3)
  
  apche9_func <- function(sCr_Value){
    if (is.na(sCr_Value) == T){ #if one value is not available
      apche_score <- 0
    }else{
      
      if (sCr_Value >= 0.6 & sCr_Value < 1.5){
        apche_score <- 0
      }else if ( (sCr_Value >= 1.5 & sCr_Value < 2.0) | ( sCr_Value < 0.6)){
        apche_score <- 2
      }else if (sCr_Value >= 2.0 & sCr_Value < 3.5){
        apche_score <- 3
      }else if (sCr_Value >= 3.5){
        apche_score <- 4
      }else{
        apche_score <- NA
      }
      
    }
    return(apche_score)
  }
  
  
  #10. Apache APACHE_Hematocrit

  
  #####
  curr_gcs <- curr_feature_df[,"GCS"]
  curr_Use_DDM <- curr_feature_df[,"Use_of_Dopamine_OR_Dobutamine_OR_Milrinone"]
  curr_Use_ENPV <- curr_feature_df[,"Use_of_Epinephrine_Or_Norepinephrine_Or_Phenylephrine_Or_Vasopressin"]
  

  
  
}

SOFA_SCORE_df$SOFA_TOTAL <- rowSums(SOFA_SCORE_df[,2:7])


#Combine feature df and sofa df for check
SOFA_With_Feature_df <- cbind(SOFA_SCORE_df, SOFA_APACHE_Feature_df[,-1])

