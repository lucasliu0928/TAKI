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

#3.Load Labs 
raw_LAB1_df <- read.csv(paste0(raw_dir,"LABS_SET1.csv"),stringsAsFactors = F)
raw_LAB2_df <- read.csv(paste0(raw_dir,"LABS_SET2.csv"),stringsAsFactors = F)

#4. Load demo for compute Anemia
All_RACE_GENDER_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

##########################################################################################
#Load UK raw LABS_SET1
#Features to extract :  1. Bilirubin D1 High
#                       2. Platelets D1 Low
#                       3. Sodium D1 High/Low
#                       4. Potassium D1 High/Low
#                       5. Hematocrit D1 High/Low
#                       6. Hemoglobin D1 High/Low
#                       7. WBC D1 High/Low
#                       8. Bicarbonate D1 High/Low
#                       9. BUN D0-D3 High (Take the max of d0 to d3 high and make sure d0-d3 values in recorded in ICU)
#                       10. Anemia (Compute using Lowest Hematocrit  and lowest Hemoglobin)
#Load UK raw LABS_SET2
#Features to extract :  1. Albumin (NOT SURE is high or low and on which day)
#                       2. Lactate (NOT SURE is high or low and on which day)
#Steps: 1. Get raw available values
#       2. Filter out values if patient not in ICU on that day
#       3. Remove outlier values
#       4. Compute missing 
#       5. impute with median
##########################################################################################
#1. Get raw available values
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

CO2_d1_Low <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Bicarbonate_D1_LOW","CO2_D1_LOW_VALUE")
CO2_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"Bicarbonate_D1_HIGH","CO2_D1_HIGH_VALUE")

#BUN
BUN_d0_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"BUN_D0_HIGH","BUN_D0_HIGH_VALUE")
BUN_d1_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"BUN_D1_HIGH","BUN_D1_HIGH_VALUE")
BUN_d2_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"BUN_D2_HIGH","BUN_D2_HIGH_VALUE")
BUN_d3_High <- get_raw_var_values_1option_func(raw_LAB1_df,analysis_ID,"BUN_D3_HIGH","BUN_D3_HIGH_VALUE")

Albumin <- get_raw_var_values_1option_func(raw_LAB2_df,analysis_ID,"Albumin","ALBUMIN_VALUE")
Lactate <- get_raw_var_values_1option_func(raw_LAB2_df,analysis_ID,"Lactate","LACTATE_SYRINGE_ION_VALUE")

All_LAB_df <- cbind(Bilirubin_d1_High,PLATELETS_d1_Low,SODIUM_d1_Low,SODIUM_d1_High,
                    Potassium_d1_Low,Potassium_d1_High,Hematocrit_d1_Low,Hematocrit_d1_High,
                    Hemoglobin_d1_Low,Hemoglobin_d1_High,WBC_d1_Low,WBC_d1_High,
                    CO2_d1_Low,CO2_d1_High,
                    BUN_d0_High,BUN_d1_High,BUN_d2_High,BUN_d3_High,
                    Albumin,Lactate)

All_LAB_df <- All_LAB_df[,-which(colnames(All_LAB_df) == "STUDY_PATIENT_ID")[-1]] #remove redudant ID columns except the 1st one


#2.code the corresponding faetuere value as NA If patient does not have in ICU on that day 
updated_LAB_df <- remove_featureValue(All_LAB_df,All_time_df)
table(updated_LAB_df$Excluded_Feature)

#3.Get MAX BUN in ICU D0-D3 with values only in ICU (in updated_LAB_df, the value not in ICU is excluded)
updated_LAB_df$BUN_D0toD3_HIGH <- NA
for (i in 1:nrow(updated_LAB_df)){
  curr_df <- updated_LAB_df[i,c("BUN_D0_HIGH", "BUN_D1_HIGH" , "BUN_D2_HIGH" ,"BUN_D3_HIGH")]
  updated_LAB_df[i,"BUN_D0toD3_HIGH"] <- max(curr_df,na.rm = T)
}


#3.Remove outlier
#Bicarbonate_D1_LOW
updated_LAB_df_OutlierExcluded <- remove_values_byValue(updated_LAB_df,"Bicarbonate_D1_LOW",5,"Less Than")
updated_LAB_df_OutlierExcluded <- remove_values_byValue(updated_LAB_df_OutlierExcluded,"Bicarbonate_D1_LOW",50,"Greater Than")

#BUN
updated_LAB_df_OutlierExcluded <- remove_values_byValue(updated_LAB_df_OutlierExcluded,"BUN_D0toD3_HIGH",5,"Less Than")

#Potassium_D1_LOW
updated_LAB_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_LAB_df_OutlierExcluded,"Potassium_D1_LOW","TOP")
#Potassium_D1_HIGH
updated_LAB_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_LAB_df_OutlierExcluded,"Potassium_D1_HIGH","TOP")

#WBC_D1_LOW
updated_LAB_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_LAB_df_OutlierExcluded,"WBC_D1_LOW","TOP")

#WBC_D1_HIGH
updated_LAB_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_LAB_df_OutlierExcluded,"WBC_D1_HIGH","TOP")



#4. Compute missing
feature_columns <-  c("Bilirubin_D1_HIGH", "Platelets_D1_LOW", "Sodium_D1_LOW",
                      "Sodium_D1_HIGH","Potassium_D1_LOW", "Potassium_D1_HIGH", "Hematocrit_D1_LOW",
                      "Hematocrit_D1_HIGH", "Hemoglobin_D1_LOW", "Hemoglobin_D1_HIGH" , "WBC_D1_LOW",
                      "WBC_D1_HIGH" ,"Bicarbonate_D1_LOW", "Bicarbonate_D1_HIGH","BUN_D0toD3_HIGH")
missing_table <- get_missing_rate_table(updated_LAB_df_OutlierExcluded,feature_columns)
missing_table


#5.imputation median
Final_LAB_df <- median_imputation_func(updated_LAB_df_OutlierExcluded,feature_columns)
missing_table2 <- get_missing_rate_table(Final_LAB_df,feature_columns)
missing_table2

##########################################################################################
# Compute Anemia and add to LAB_df
##########################################################################################
Final_LAB_df$Anemia <- NA
for (i in 1:nrow(Final_LAB_df)){
  curr_id <- Final_LAB_df[i,"STUDY_PATIENT_ID"]
  curr_lowest_Hematocrit <- Final_LAB_df[i,"Hematocrit_D1_LOW"]
  curr_lowest_Hemoglobin <- Final_LAB_df[i,"Hemoglobin_D1_LOW"]
  curr_gender <- All_RACE_GENDER_df[which(All_RACE_GENDER_df[,"STUDY_PATIENT_ID"] == curr_id),"GENDER"]
  
  if (curr_gender == "M" & (curr_lowest_Hematocrit < 39 | curr_lowest_Hemoglobin < 18)){
    curr_flag <- 1
  }else if (curr_gender == "F" & (curr_lowest_Hematocrit < 36 | curr_lowest_Hemoglobin < 12)){
    curr_flag <- 1
  }else {
    curr_flag <- 0
  }
  
  Final_LAB_df[i,"Anemia"] <- curr_flag
}

#remove features not needed for model
exlucde_indxes <- which(colnames(Final_LAB_df) %in% c("BUN_D0_HIGH","BUN_D1_HIGH","BUN_D2_HIGH","BUN_D3_HIGH","Albumin","Lactate","Excluded_Feature"))
Final_LAB_df <- Final_LAB_df[,-exlucde_indxes]
write.csv(Final_LAB_df,paste0(outdir,"All_LAB_imputed.csv"),row.names = F)

