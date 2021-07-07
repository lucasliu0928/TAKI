library(lubridate)
source("TAKI_Ultility.R")

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#Inclusion IDs
#1.	Have hospital admission and discharge dates
#2.	Have age, gender, and race
##########################################################################################
#1. Corrected Time df 
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#2.Load  demo df
Demo_df <-read.csv(paste0(outdir,"All_RACE_GENDER_AGE_df.csv"),stringsAsFactors = F)

#3.inclusion IDs
inclusionIDs <-intersect(All_time_df[,"STUDY_PATIENT_ID"],Demo_df[,"STUDY_PATIENT_ID"])


#5. add ENCNTER_ID and MRN
inclusionIDs_df <- as.data.frame(inclusionIDs)
colnames(inclusionIDs_df)[1] <- "STUDY_PATIENT_ID"
nrow(inclusionIDs_df) #36017
write.csv(inclusionIDs_df,paste0(outdir,"Inclusion_IDs.csv"),row.names = F)
