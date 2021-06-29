library(lubridate)
source("TAKI_Ultility.R")

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

#3.Load raw data
#imputed_HT_WT_df <- read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_Imputed.csv"),stringsAsFactors = F)
notimputed_HT_WT_df <- read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_NOTimputed.csv"),stringsAsFactors = F)

# Compute BMI and using not Imputed values
BMI_df <- as.data.frame(matrix(NA, nrow = nrow(notimputed_HT_WT_df), ncol = 2))
colnames(BMI_df) <- c("STUDY_PATIENT_ID","BMI")

for (i in 1:nrow(notimputed_HT_WT_df)){
  curr_wt <- notimputed_HT_WT_df[i,"INITIAL_WEIGHT_KG"]
  curr_ht <- notimputed_HT_WT_df[i,"HEIGHT_Meters"]
  curr_id <- notimputed_HT_WT_df[i,"STUDY_PATIENT_ID"]
  
  curr_BMI <- curr_wt/(curr_ht^2)
  BMI_df[i,"STUDY_PATIENT_ID"] <- curr_id
  BMI_df[i,"BMI"] <- curr_BMI
}

#4. Compute missing
feature_columns <-  c("BMI")
missing_table <- get_missing_rate_table(BMI_df,feature_columns)
missing_table

write.csv(BMI_df,paste0(outdir,"All_BMI_NOTimputed.csv"),row.names = F)


#5.imputation median for feature column
Final_BMI_df <- median_imputation_func(BMI_df,feature_columns)
missing_table2 <- get_missing_rate_table(Final_BMI_df,feature_columns)
missing_table2

write.csv(BMI_df,paste0(outdir,"All_BMI_Imputed.csv"),row.names = F)

