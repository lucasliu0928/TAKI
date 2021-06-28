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

#3.Load raw data
raw_CLINICAL_OTHERS_df <- read.csv(paste0(raw_dir,"CLINICAL_OTHERS.csv"),stringsAsFactors = F)


##########################################################################################
#Load UK raw CLINICAL_OTHERS
#Features to extract :  1. Height
#                       2. Weight
#                       3. Respiration D1 Low/High
#                       4. FIO2 D1 Low/High
#                       5. BMI
#Steps: 1. Get raw available values
#       2. Filter out values if patient not in ICU on that day
#       3. Remove outlier values
#       4. Compute missing 
#       5. impute with median
##########################################################################################
#Get Features
height <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"HEIGHT_CM","HEIGHT_CM_VALUE")
#comvert to meters
height$HEIGHT_Meters <-  height$HEIGHT_CM/100
height <- height[,-which(colnames(height) == "HEIGHT_CM")]

weight <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"INITIAL_WEIGHT_KG","INITIAL_WEIGHT_KG")

resp_d1_Low <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"RESP_RATE_D1_LOW","RESP_RATE_D1_LOW_VALUE")
resp_d1_High <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"RESP_RATE_D1_HIGH","RESP_RATE_D1_HIGH_VALUE")

fio2_d1_Low <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"FI02_D1_LOW","FI02_D1_LOW_VALUE")
fio2_d1_High <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"FI02_D1_HIGH","FI02_D1_HIGH_VALUE")

#All features except height,weight since these two has no D0,D1 in names, do not have to filter in the following step
All_ClinicalOthers_df <- cbind(resp_d1_Low,resp_d1_High,fio2_d1_Low,fio2_d1_High)
All_ClinicalOthers_df <- All_ClinicalOthers_df[,-which(colnames(All_ClinicalOthers_df) == "STUDY_PATIENT_ID")[-1]] #remove redudant ID columns except the 1st one

#2.code the corresponding faetuere value as NA If patient does not have in ICU on that day 
updated_ClinicalOthers_df <- remove_featureValue(All_ClinicalOthers_df,All_time_df)
table(updated_ClinicalOthers_df$Excluded_Feature) #0

#3. Add height and weight back to the updated df
updated_ClinicalOthers_df <- cbind(updated_ClinicalOthers_df,height,weight)
updated_ClinicalOthers_df <- updated_ClinicalOthers_df[,-which(colnames(updated_ClinicalOthers_df) == "STUDY_PATIENT_ID")[-1]] #remove redudant ID columns except the 1st one

#3.Remove outlier
#FI02_D1_LOW
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_df,"FI02_D1_LOW",21,"Less Than")
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"FI02_D1_LOW",100,"Greater Than")

#FI02_D1_HIGH
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"FI02_D1_HIGH",21,"Less Than")
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"FI02_D1_HIGH",100,"Greater Than")

#RESP_RATE_D1_LOW
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"RESP_RATE_D1_LOW",5,"Less Than")
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"RESP_RATE_D1_LOW",70,"Greater Than")

#RESP_RATE_D1_HIGH
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"RESP_RATE_D1_HIGH",5,"Less Than")
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"RESP_RATE_D1_HIGH",70,"Greater Than")

#HEIGHT_Meters
updated_ClinicalOthers_dfOutlierExcluded <- remove_outlier_BOTOrTOP_5perc(updated_ClinicalOthers_dfOutlierExcluded,"HEIGHT_Meters","BOTTOM and TOP")

#INITIAL_WEIGHT_KG
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"INITIAL_WEIGHT_KG",30,"Less Than")
updated_ClinicalOthers_dfOutlierExcluded <- remove_values_byValue(updated_ClinicalOthers_dfOutlierExcluded,"INITIAL_WEIGHT_KG",200,"Greater Than")

#4. Compute missing
feature_columns <-  c("FI02_D1_LOW", "FI02_D1_HIGH","RESP_RATE_D1_LOW", "RESP_RATE_D1_HIGH",
                      "HEIGHT_Meters","INITIAL_WEIGHT_KG")
missing_table <- get_missing_rate_table(updated_ClinicalOthers_dfOutlierExcluded,feature_columns)
missing_table

write.csv(updated_ClinicalOthers_dfOutlierExcluded,paste0(outdir,"All_HT_WT_RESP_FIO2_NOTimputed.csv"),row.names = F)


#5.imputation median
Final_ClinicalOthers_df <- median_imputation_func(updated_ClinicalOthers_dfOutlierExcluded,feature_columns)
missing_table2 <- get_missing_rate_table(Final_ClinicalOthers_df,feature_columns)
missing_table2

#6.remove features not needed for model
exlucde_indxes <- which(colnames(Final_ClinicalOthers_df) %in% c("Excluded_Feature"))
Final_ClinicalOthers_df <- Final_ClinicalOthers_df[,-exlucde_indxes]

write.csv(Final_ClinicalOthers_df,paste0(outdir,"All_HT_WT_RESP_FIO2_Imputed.csv"),row.names = F)


