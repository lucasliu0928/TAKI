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


#2. Corrected Time df for analysis ID
All_time_df <-read.csv(paste0(outdir,"All_Corrected_Timeinfo.csv"),stringsAsFactors = F)

#3. surgery df
surgery_df <- read.csv(paste0(raw_dir,"SURGERY_INDX.csv"),stringsAsFactors = F)

##########################################################################################
#Match the date of surgery with date of ICU admission
#1.	 Planned admission: if date of surgery is either within 24 hours before or after admission (window of 48h), 
#                         then is considered PLANNED ADMISSION

#1.Updated:   Planned admission:  if pts has any surgery in this range: ICU_start - 24h  <= surgery date <= ICU_start + 24h
#2.	          Unplanned admission: the rest of the pts

##########################################################################################
Admission_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID),ncol = 3))
colnames(Admission_df) <- c("STUDY_PATIENT_ID","unplanned_Admission","planned_admission")
for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_ID[i]
  Admission_df[i,"STUDY_PATIENT_ID"]    <- curr_id
  
  #time info
  curr_time_df <- All_time_df[which(All_time_df[,"STUDY_PATIENT_ID"] == curr_id),]
  curr_icu_start <- ymd_hms(curr_time_df$Updated_ICU_ADMIT_DATE)
  curr_icu_start_plus24h <- curr_icu_start + hours(24)
  curr_icu_start_minus24h <- curr_icu_start - hours(24)
  
  #surgery info 
  curr_surgery_df <- surgery_df[which(surgery_df[,"STUDY_PATIENT_ID"] == curr_id),]
  
  if (nrow(curr_surgery_df) > 0){ #if any surgery
    curr_surgery_performed_dates <- mdy(unique(curr_surgery_df[,"SURGERY_PERFORMED_DATE"]))
    #For each date, check if it is within ICU start +- 24 hours, then it is a planned admission
    surgery_flag <- NA
    for (t in 1:length(curr_surgery_performed_dates)){
      curr_date <- curr_surgery_performed_dates[t]
      if (curr_date >= curr_icu_start_minus24h & curr_date <= curr_icu_start_plus24h){
        surgery_flag[t] <- "planned"
      }else{
        surgery_flag[t] <- "unplanned"
      }
    }
    
    planned_indxes <- which(surgery_flag == "planned")
    if (length(planned_indxes) > 0){
      Admission_df[i,"planned_admission"] <- 1
      Admission_df[i,"unplanned_Admission"] <- 0
    }else{
      Admission_df[i,"planned_admission"] <- 0
      Admission_df[i,"unplanned_Admission"] <- 1
    }
  }else{ #no surgery at all
    Admission_df[i,"planned_admission"] <- 0
    Admission_df[i,"unplanned_Admission"] <- 0
  }

}

table(Admission_df$unplanned_Admission)
write.csv(Admission_df,paste0(outdir,"All_unplanned_Admission.csv"),row.names=FALSE)

table(Admission_df$unplanned_Admission) #6219 1135
