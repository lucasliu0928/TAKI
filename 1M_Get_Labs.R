library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#3.Load Labs 
##########################################################################################
raw_LAB1_df <- read.csv(paste0(raw_dir,"LABS_SET1.csv"),stringsAsFactors = F)
raw_LAB2_df <- read.csv(paste0(raw_dir,"LABS_SET2.csv"),stringsAsFactors = F)

##########################################################################################
#4.Get ICU stays in terms of D0, D1, D2, D3
##########################################################################################
ICU_D0_D3_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID) ,ncol = 2))
colnames(ICU_D0_D3_df) <- c("STUDY_PATIENT_ID", "ICU_Stays_inDays")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  ICU_D0_D3_df[i,"STUDY_PATIENT_ID"] <- curr_id

  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  #Get ICU D0 to D3 start and end time
  curr_ICU_D0D3_time_df <- get_ICUD0_D3_dates_func(curr_icu_start,curr_icu_end)
  
  #non NA days
  non_NA_idxes <-   which(is.na(curr_ICU_D0D3_time_df[,"Day_start"]) == F)
  non_NA_Days <-  curr_ICU_D0D3_time_df[non_NA_idxes,"Day"]
  ICU_D0_D3_df[i,c("ICU_Stays_inDays")] <- paste0(paste0("D",non_NA_Days),collapse = "$$")

}

write.csv(ICU_D0_D3_df,paste0(outdir,"ICU_D0toD3_Days_df.csv"))

##########################################################################################
#Load UK raw LABS_SET1
#Features to extract :  1. Bilirubin D1 High
#                       2. Platelets D1 Low
#                       3. Sodium D1 High/Low
#                       4. Potassium D1 High/Low
#                       5. Hematocrit D1 High/Low
#                       6. Hemoglobin D1 High/Low
#                       7. WBC D1 High/Low
#                       8. BUN D0-D3 High (Take the max of d0 to d3 high)
#                       9. Bicarbonate D1 High/Low
#Load UK raw LABS_SET2
#Features to extract :  1. Albumin (NOT SURE is high or low and on which day)
#                       2. Lactate (NOT SURE is high or low and on which day)
##########################################################################################
#'@TODO: NEED to find out if D3 is ICU D3 for each patient , use get_ICUdates function
raw_LAB1_df$BUN_D0TOD3_HIGH_VALUE <- NA
for (i in 1:nrow(raw_LAB1_df)){
  curr_df <-  raw_LAB1_df[i,c("BUN_D0_HIGH_VALUE","BUN_D1_HIGH_VALUE","BUN_D2_HIGH_VALUE","BUN_D3_HIGH_VALUE")]
  if (all(is.na(curr_df)==F)){  #if at least one not NA
    curr_max <- max(curr_df, na.rm = T)
  }else{ #if all NA
    curr_max <- NA
  }
  raw_LAB1_df$BUN_D0TOD3_HIGH_VALUE[i] <- curr_max
}

#Get features
Bilirubin_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Bilirubin_D1_HIGH","BILIRUBIN_D1_HIGH_VALUE")

PLATELETS_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Platelets_D1_LOW","PLATELETS_D1_LOW_VALUE")

SODIUM_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Sodium_D1_LOW","SODIUM_D1_LOW_VALUE")
SODIUM_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Sodium_D1_HIGH","SODIUM_D1_HIGH_VALUE")

Potassium_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Potassium_D1_LOW","POTASSIUM_D1_LOW_VALUE")
Potassium_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Potassium_D1_HIGH","POTASSIUM_D1_HIGH_VALUE")

Hematocrit_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Hematocrit_D1_LOW","HEMATOCRIT_D1_LOW_VALUE")
Hematocrit_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Hematocrit_D1_HIGH","HEMATOCRIT_D1_HIGH_VALUE")

Hemoglobin_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Hemoglobin_D1_LOW","HEMOGLOBIN_D1_LOW_VALUE")
Hemoglobin_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Hemoglobin_D1_HIGH","HEMOGLOBIN_D1_HIGH_VALUE")

WBC_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"WBC_D1_LOW","WBC_D1_LOW_VALUE")
WBC_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"WBC_D1_HIGH","WBC_D1_HIGH_VALUE")

BUN_d0tod3_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"BUN_D0TOD3_HIGH","BUN_D0TOD3_HIGH_VALUE")

CO2_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Bicarbonate_D1_LOW","CO2_D1_LOW_VALUE")
CO2_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Bicarbonate_D1_HIGH","CO2_D1_HIGH_VALUE")

Albumin <- get_raw_var_values_1option_func(raw_LAB2_df,analysis_ID,"Albumin","ALBUMIN_VALUE")
Lactate <- get_raw_var_values_1option_func(raw_LAB2_df,analysis_ID,"Lactate","LACTATE_SYRINGE_ION_VALUE")

All_LAB_df <- cbind(Bilirubin_d1_High,PLATELETS_d1_Low,SODIUM_d1_Low,SODIUM_d1_High,
                    Potassium_d1_Low,Potassium_d1_High,Hematocrit_d1_Low,Hematocrit_d1_High,
                    Hemoglobin_d1_Low,Hemoglobin_d1_High,WBC_d1_Low,WBC_d1_High,BUN_d0tod3_High,
                    CO2_d1_Low,CO2_d1_High,Albumin,Lactate)
All_LAB_df <- All_LAB_df[,-seq(3,33,2)] #remove duplicated columns
write.csv(All_LAB_df,paste0(outdir,"All_LAB_df.csv"))

