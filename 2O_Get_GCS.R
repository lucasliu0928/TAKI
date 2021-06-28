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
#Features to extract :  1. GCS

#Steps: 1. Get raw available values
#       2. Clean values
#       3. Filter out values if patient not in ICU on that day
#       4. Compute missing 
#       5. impute with median
##########################################################################################
#1. Get raw available values
GCS_D1_LOW_df <- get_raw_var_values_1option_func(raw_CLINICAL_OTHERS_df,analysis_ID,"GCS_D1_LOW","GLASGOW_SCORE_D1_LOW_VALUE")

#2.First code blank as NA
blank_idxes <- which(GCS_D1_LOW_df$GCS_D1_LOW=="")
GCS_D1_LOW_df[blank_idxes,"GCS_D1_LOW"] <- NA

#2.Clean the values by taking the lowest end (e.g, 8-10 = 8)
GCS_D1_LOW_df$Updated_GCS_D1_LOW <- NA
for (i in 1:nrow(GCS_D1_LOW_df)){
  curr_val <- GCS_D1_LOW_df[i,"GCS_D1_LOW"]
  if (is.na(curr_val) == F){
    updated_val <- unlist(strsplit(curr_val,split = "-"))[1] #take the first one which is lowest
    GCS_D1_LOW_df[i,"Updated_GCS_D1_LOW"] <-  updated_val
  }
  
}

#3.code the corresponding faetuere value as NA If patient does not have in ICU on that day 
updated_GCS_df <- remove_featureValue(GCS_D1_LOW_df,All_time_df)
table(updated_GCS_df$Excluded_Feature) #0

#4. Compute missing
feature_columns <-  c("Updated_GCS_D1_LOW","GCS_D1_LOW")
missing_table <- get_missing_rate_table(updated_GCS_df,feature_columns)
missing_table

write.csv(updated_GCS_df,paste0(outdir,"All_GCS_df.csv"),row.names = F)
