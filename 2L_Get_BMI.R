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
HT_WT_df <- read.csv(paste0(outdir,"All_HT_WT_RESP_FIO2_Imputed.csv"),stringsAsFactors = F)

# Compute BMI and using Imputed values
BMI_df <- as.data.frame(matrix(NA, nrow = nrow(HT_WT_df), ncol = 2))
colnames(BMI_df) <- c("STUDY_PATIENT_ID","BMI")

for (i in 1:nrow(HT_WT_df)){
  curr_wt <- HT_WT_df[i,"INITIAL_WEIGHT_KG"]
  curr_ht <- HT_WT_df[i,"HEIGHT_Meters"]
  curr_id <- HT_WT_df[i,"STUDY_PATIENT_ID"]
  
  curr_BMI <- curr_wt/(curr_ht^2)
  BMI_df[i,"STUDY_PATIENT_ID"] <- curr_id
  BMI_df[i,"BMI"] <- curr_BMI
}

write.csv(BMI_df,paste0(outdir,"All_BMI_usingImputedHT_WT.csv"),row.names = F)

