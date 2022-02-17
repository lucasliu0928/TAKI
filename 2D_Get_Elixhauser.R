library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Analysis Id after exclusion
analysis_ID_df <-read.csv(paste0(outdir,"Final_Analysis_ID.csv"),stringsAsFactors = F)
analysis_ID <- unique(analysis_ID_df[,"STUDY_PATIENT_ID"]) #7354

#2. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)
All_time_df <- All_time_df[which(All_time_df$STUDY_PATIENT_ID %in% analysis_ID),] #filter for anlaysis Id only

#3 Load raw ELIXHAUSER_SCORE data
raw_ELIXHAUSER_df <- read.csv(paste0(raw_dir,"ELIXHAUSER_SCORE.csv"),stringsAsFactors = F)


##########################################################################################
#Features to extract :  1. ELIXHAUSER_SCORE
##########################################################################################
ELIXHAUSER_df <- raw_ELIXHAUSER_df[which(raw_ELIXHAUSER_df[,"STUDY_PATIENT_ID"] %in% analysis_ID),]
ELIXHAUSER_df <- ELIXHAUSER_df[match(analysis_ID,ELIXHAUSER_df[,"STUDY_PATIENT_ID"]),] #reorder to match ID

#4. Compute missing
feature_columns <-  colnames(ELIXHAUSER_df)[2:32]
missing_table <- get_missing_rate_table(ELIXHAUSER_df,feature_columns)
missing_table

write.csv(ELIXHAUSER_df,paste0(outdir,"All_ELIXHAUSER_df.csv"))