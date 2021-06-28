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

#3.Load CLINICAL_VITALS 
raw_Vitals_df <- read.csv(paste0(raw_dir,"CLINICAL_VITALS.csv"),stringsAsFactors = F)


##########################################################################################
#4. Extract Features
#Features to extract :  1. Temperature D1 High/Low
#                       2. MAP D1 High/Low
#                       3. Heart Rate D1 High/Low
#Steps: 1. Get raw available values
#       2. Filter out values if patient not in ICU on that day
#       3. remove outliers
#       4. compute missing
#       5. Impudation median
##########################################################################################
#1. Get raw available values
#MAP
MAP_d1_Low  <- get_raw_var_values_2options_func(raw_Vitals_df,analysis_ID,"MAP_D1_LOW","ART_MEAN_D1_LOW_VALUE","CUFF_MEAN_D1_LOW_VALUE")
MAP_d1_High <- get_raw_var_values_2options_func(raw_Vitals_df,analysis_ID,"MAP_D1_HIGH","ART_MEAN_D1_HIGH_VALUE","CUFF_MEAN_D1_HIGH_VALUE")
#Temp
Temp_d1_Low <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"Temperature_D1_LOW","TEMPERATURE_D1_LOW_VALUE")
Temp_d1_High <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"Temperature_D1_HIGH","TEMPERATURE_D1_HIGH_VALUE")
#Convert to celsius
Temp_d1_Low[,"Temperature_D1_LOW"] <- (Temp_d1_Low[,"Temperature_D1_LOW"] -32)*(5/9)
Temp_d1_High[,"Temperature_D1_HIGH"] <- (Temp_d1_High[,"Temperature_D1_HIGH"] -32)*(5/9)
#HR
HR_d1_Low <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"HR_D1_LOW","HEART_RATE_D1_LOW_VALUE")
HR_d1_High <- get_raw_var_values_1option_func(raw_Vitals_df,analysis_ID,"HR_D1_HIGH","HEART_RATE_D1_HIGH_VALUE")

All_Vital_df <- cbind(MAP_d1_Low,MAP_d1_High,Temp_d1_Low,Temp_d1_High,HR_d1_Low,HR_d1_High)
All_Vital_df <- All_Vital_df[,-c(3,5,7,9,11)] #remove redudant ID columns



#2.If patient does not have D1 in All_time_df$Actual_ICU_Stays, code the corresponding value as NA
#2.1.Update
updated_Vital_df <- remove_featureValue(All_Vital_df,All_time_df)

#2.2 Check agreement pts has no D1, and the values were excluded 
indxes_noD1 <- which(grepl("D1",All_time_df$Actual_ICU_Stays) == F)
length(indxes_noD1) #0
table(updated_Vital_df$Excluded_Feature) #0

#3.Remove outliers
#MAP_D1_LOW
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df,"MAP_D1_LOW",0,"Less Than")
updated_Vital_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_Vital_df_OutlierExcluded,"MAP_D1_LOW","BOTTOM")

#MAP_D1_HIGH
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"MAP_D1_HIGH",0,"Less Than")
updated_Vital_df_OutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_Vital_df_OutlierExcluded,"MAP_D1_HIGH","TOP")

#Temperature_D1_LOW
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"Temperature_D1_LOW",30,"Less Than")
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"Temperature_D1_LOW",45,"Greater Than")
#Temperature_D1_HIGH
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"Temperature_D1_HIGH",30,"Less Than")
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"Temperature_D1_HIGH",45,"Greater Than")

#HR_D1_LOW
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"HR_D1_LOW",25,"Less Than")
updated_Vital_df_OutlierExcluded <- remove_values_byValue(updated_Vital_df_OutlierExcluded,"HR_D1_HIGH",25,"Less Than")


#4. Compute missing before imputation
feature_columns <-  c("MAP_D1_LOW", "MAP_D1_HIGH", "Temperature_D1_LOW",
                      "Temperature_D1_HIGH","HR_D1_LOW", "HR_D1_HIGH")
missing_table <- get_missing_rate_table(updated_Vital_df_OutlierExcluded,feature_columns)

write.csv(updated_Vital_df_OutlierExcluded,paste0(outdir,"All_MAP_TEMP_HR_NOTimputed.csv"),row.names = F)


#5.imputation median
Final_Vital_df <- median_imputation_func(updated_Vital_df_OutlierExcluded,feature_columns)
missing_table2 <- get_missing_rate_table(Final_Vital_df,feature_columns)

#6.remove features not needed for model
exlucde_indxes <- which(colnames(Final_Vital_df) %in% c("Excluded_Feature"))
Final_Vital_df <- Final_Vital_df[,-exlucde_indxes]

write.csv(Final_Vital_df,paste0(outdir,"All_MAP_TEMP_HR_imputed.csv"),row.names = F)
