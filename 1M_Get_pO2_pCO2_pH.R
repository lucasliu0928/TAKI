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


#3.Load blood gas
raw_BLOOD_GAS_df <- read.csv(paste0(raw_dir,"BLOOD_GAS.csv"),stringsAsFactors = F)


##########################################################################################
#Load UK raw BLOOD_GAS
#Features to extract :  1. pO2 D1 High/Low
#                       2. pCO2 D1 High/Low
#                       3. pH D1 High/Low
#Steps: 1. Get raw available values
#       2. Filter out values if patient not in ICU on that day
#       3. Remove outlier values
#       4. Compute missing 
#       5. impute with median
##########################################################################################
#1. Get raw available values
po2_d1_Low <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PO2_D1_LOW","PO2_D1_LOW_VALUE")
po2_d1_High <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PO2_D1_HIGH","PO2_D1_HIGH_VALUE")

pco2_d1_Low <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PCO2_D1_LOW","PCO2_D1_LOW_VALUE")
pco2_d1_High <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PCO2_D1_HIGH","PCO2_D1_HIGH_VALUE")

ph_d1_Low <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PH_D1_LOW","PH_D1_LOW_VALUE")
ph_d1_High <- get_raw_var_values_1option_func(raw_BLOOD_GAS_df,analysis_ID,"PH_D1_HIGH","PH_D1_HIGH_VALUE")


All_BloodGAS_df <- cbind(po2_d1_Low,po2_d1_High,pco2_d1_Low,pco2_d1_High,ph_d1_Low,ph_d1_High)
All_BloodGAS_df <- All_BloodGAS_df[,-which(colnames(All_BloodGAS_df) == "STUDY_PATIENT_ID")[-1]] #remove redudant ID columns except the 1st one


#2.code the corresponding faetuere value as NA If patient does not have in ICU on that day 
updated_BloodGAS_df <- remove_featureValue(All_BloodGAS_df,All_time_df)
table(updated_BloodGAS_df$Excluded_Feature) #0

#3.Remove outlier
#PCO2_D1_LOW
updated_BloodGAS_df_OutlierExcluded <- remove_values_byValue(updated_BloodGAS_df,"PCO2_D1_LOW",0,"Less Than")
updated_BloodGAS_df_OutlierExcluded <- remove_values_byValue(updated_BloodGAS_df_OutlierExcluded,"PCO2_D1_LOW",125,"Greater Than")

#PCO2_D1_HIGH
updated_BloodGAS_df_OutlierExcluded <- remove_values_byValue(updated_BloodGAS_df_OutlierExcluded,"PCO2_D1_HIGH",0,"Less Than")
updated_BloodGAS_df_OutlierExcluded <- remove_values_byValue(updated_BloodGAS_df_OutlierExcluded,"PCO2_D1_HIGH",125,"Greater Than")


#4. Compute missing
feature_columns <-  c("PCO2_D1_LOW", "PCO2_D1_HIGH","PH_D1_LOW", "PH_D1_HIGH","PO2_D1_LOW","PO2_D1_HIGH")
missing_table <- get_missing_rate_table(updated_BloodGAS_df_OutlierExcluded,feature_columns)
missing_table


#5.imputation median
Final_BloodGAS_df <- median_imputation_func(updated_BloodGAS_df_OutlierExcluded,feature_columns)
missing_table2 <- get_missing_rate_table(Final_BloodGAS_df,feature_columns)
missing_table2

write.csv(Final_BloodGAS_df,paste0(outdir,"All_pO2_pCO2_pH_imputed.csv"),row.names = F)
