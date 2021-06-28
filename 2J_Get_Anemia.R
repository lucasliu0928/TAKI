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

#3.Load Imputed Labs 
Imputed_LAB_df <- read.csv(paste0(outdir,"All_LAB_imputed.csv"),stringsAsFactors = F)

#4. Load demo for compute Anemia
All_RACE_GENDER_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)





##########################################################################################
# Compute Anemia using imputed Hematocrit_D1_LOW and Hemoglobin_D1_LOW
##########################################################################################
Anemia_df <- as.data.frame(matrix(NA, nrow = nrow(Final_LAB_df), ncol = 2))
colnames(Anemia_df) <- c("STUDY_PATIENT_ID","Anemia")

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
  
  Anemia_df[i,"STUDY_PATIENT_ID"] <- curr_id
  Anemia_df[i,"Anemia"] <- curr_flag
}

write.csv(Anemia_df,paste0(outdir,"All_Anemia_usingImputedLabs.csv"),row.names = F)

