library(lubridate)
source("TAKI_Ultility.R")
#This script get each patient ICU days info
#For each patient, get the days avaiable, so that we know if a feature is acutally captured in ICU
#e.g, BUN03, some patient not in ICU for 3 days, but has a BUND3, it is because the way they extracted data is: 
#BUND3 means 3 days from ICU start, does not have to be in the 3rd day does not have to be in ICU
#For this example, the patinet has value after ICU start + 3 days, but the pt does not in ICU anymore at this time

#Raw data dir
raw_dir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/Taylors_Data/UKY/raw_csv_files/"
outdir <- "/Volumes/LJL_ExtPro/Data/AKI_Data/TAKI_Data_Extracted/uky/"

##########################################################################################
#1. Load data
##########################################################################################
#1. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

##########################################################################################
#2. Analysis Id for pts has corrected HOSP ADMISSION time
##########################################################################################
analysis_ID <- unique(All_time_df[,"STUDY_PATIENT_ID"])

##########################################################################################
#4.Get ICU stays in terms of D0, D1, D2, D3
##########################################################################################
ICU_D0_D3_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID) ,ncol = 2))
colnames(ICU_D0_D3_df) <- c("STUDY_PATIENT_ID", "ICU_Stays_inDays")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){print(i)}
  
  curr_id <- analysis_ID[i]
  ICU_D0_D3_df[i,"STUDY_PATIENT_ID"] <- curr_id
  
  #Time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df[,"Updated_ICU_ADMIT_DATE"])
  curr_icu_end <- ymd_hms(curr_time_df[,"Updated_ICU_DISCHARGE_DATE"])
  #Get ICU D0 to D3 start and end time
  curr_ICU_D0D3_time_df <- get_ICUD0_D3_dates_func(curr_icu_start,curr_icu_end)
  
  #non NA days
  non_NA_idxes <-   which(is.na(curr_ICU_D0D3_time_df[,"Day_start"]) == F)
  non_NA_Days <-  curr_ICU_D0D3_time_df[non_NA_idxes,"Day"]
  ICU_D0_D3_df[i,c("ICU_Stays_inDays")] <- paste0(paste0("D",non_NA_Days),collapse = "$$")
  
}

write.csv(ICU_D0_D3_df,paste0(outdir,"ICU_D0toD3_Days_df.csv"),row.names = F)
